# =============================================================================
# engine.R : Artha HE calculation engine in R
# Translates engine.js into R.
# =============================================================================
library(jsonlite)

# --- Math & Formatting ---
sum_arr <- function(x) { sum(x, na.rm = TRUE) }

rdirichlet <- function(alpha) {
  g <- sapply(alpha, function(a) if (a > 0) rgamma(1, shape = a, rate = 1) else 0)
  s <- sum(g)
  if (s == 0) s <- 1
  g / s
}

betaMS <- function(m, se) {
  if (se <= 0) return(list(a = m * 1e6, b = (1 - m) * 1e6))
  v <- se * se
  t <- m * (1 - m) / v - 1
  list(a = max(0.01, m * t), b = max(0.01, (1 - m) * t))
}

gammaMS <- function(m, se) {
  if (se <= 0) return(list(shape = 1e6, scale = m / 1e6))
  v <- se * se
  list(shape = m * m / v, scale = v / m)
}

# --- Costing ---
COST_CATEGORIES <- c("Direct medical", "Direct non-medical", "Indirect (productivity)")

inflate_cost <- function(c, fy, ty, r) {
  if (is.null(fy) || is.na(fy)) return(c)
  c * (1 + r)^(ty - fy)
}

microCost <- function(rows, ty, infl) {
  if (is.null(rows) || length(rows) == 0) return(list(lines=list(), total=0, byCat=list()))
  
  lines <- lapply(rows, function(r) {
    u <- inflate_cost(as.numeric(r$unit_cost), as.numeric(r$year), ty, infl)
    qty <- as.numeric(r$quantity)
    r$unit_cost_adj <- u
    r$line_cost <- qty * u
    r
  })
  
  total <- sum(sapply(lines, function(l) ifelse(is.null(l$line_cost) || is.na(l$line_cost), 0, l$line_cost)))
  
  byCat <- lapply(COST_CATEGORIES, function(cat) {
    cat_lines <- Filter(function(l) !is.null(l$category) && l$category == cat, lines)
    if (length(cat_lines) == 0) return(NULL)
    cost <- sum(sapply(cat_lines, function(l) as.numeric(l$line_cost)))
    if (cost > 0) {
      list(category = cat, cost = cost, share = ifelse(total > 0, cost / total, 0))
    } else {
      NULL
    }
  })
  byCat <- Filter(Negate(is.null), byCat)
  
  list(lines = lines, total = total, byCat = byCat)
}

annualisationFactor <- function(d, L) {
  if (is.null(L) || is.na(L) || L <= 0) return(1)
  if (d <= 0) return(1 / L)
  f <- (1 + d)^L
  d * f / (f - 1)
}

advancedCost <- function(a) {
  if (is.null(a)) a <- list()
  MH <- if (!is.null(a$monthlyHours) && as.numeric(a$monthlyHours) > 0) as.numeric(a$monthlyHours) else 160
  d <- if (!is.null(a$discount) && as.numeric(a$discount) >= 0) as.numeric(a$discount) else 0.03
  
  staff <- lapply(a$staff, function(s) {
    h <- if(!is.null(s$hours)) as.numeric(s$hours) else 0
    ses <- if(!is.null(s$sessions) && as.numeric(s$sessions) > 0) as.numeric(s$sessions) else 1
    line <- (as.numeric(s$salary) / MH) * h * ses
    list(role = s$role, salary = as.numeric(s$salary), hours = h, sessions = ses, apportion = h * ses / MH, line = line)
  })
  
  equip <- lapply(a$equip, function(e) {
    L <- as.numeric(e$life)
    AF <- annualisationFactor(d, L)
    usage <- if(!is.null(e$usagePct) && e$usagePct != "") as.numeric(e$usagePct) else 1
    qty <- if(!is.null(e$qty) && as.numeric(e$qty) > 0) as.numeric(e$qty) else 1
    price <- as.numeric(e$price)
    annualCapital <- price * AF * qty
    annualMaint <- price * qty * (if(!is.null(e$maintPct)) as.numeric(e$maintPct) else 0)
    line <- (annualCapital + annualMaint) * usage
    list(item = e$item, price = price, qty = qty, life = L, af = AF, annualCapital = annualCapital, annualMaint = annualMaint, usage = usage, line = line)
  })
  
  consum <- lapply(a$consum, function(c) {
    qty <- if(!is.null(c$qty)) as.numeric(c$qty) else 0
    unit <- if(!is.null(c$unit)) as.numeric(c$unit) else 0
    list(item = c$item, qty = qty, unit = unit, line = qty * unit)
  })
  
  space <- lapply(a$space, function(s) {
    ann <- if(!is.null(s$annual)) as.numeric(s$annual) else 0
    list(item = s$item, annual = ann, line = ann)
  })
  
  sumL <- function(arr) sum(sapply(arr, function(x) if(!is.null(x$line)) x$line else 0))
  
  groups <- list()
  if (length(staff) > 0) groups <- append(groups, list(list(key = "staff", label = "Human resources", items = staff, total = sumL(staff))))
  if (length(equip) > 0) groups <- append(groups, list(list(key = "equip", label = "Equipment / capital (annualised)", items = equip, total = sumL(equip))))
  if (length(consum) > 0) groups <- append(groups, list(list(key = "consum", label = "Consumables / recurring", items = consum, total = sumL(consum))))
  if (length(space) > 0) groups <- append(groups, list(list(key = "space", label = "Space", items = space, total = sumL(space))))
  
  total <- sum(sapply(groups, function(g) g$total))
  output <- if(!is.null(a$output) && as.numeric(a$output) > 0) as.numeric(a$output) else 1
  
  breakdown <- lapply(groups, function(g) {
    list(component = g$label, amount = g$total, share = if(total > 0) g$total / total else 0)
  })
  
  list(method = "advanced", total = total, perUnit = total / output, output = output, discount = d, monthlyHours = MH, groups = groups, breakdown = breakdown)
}

# --- OOP ---
oopRun <- function(o) {
  items <- lapply(o$items, function(i) {
    i$amount <- as.numeric(i$amount)
    i
  })
  total <- sum(sapply(items, function(i) i$amount))
  
  byCat <- lapply(COST_CATEGORIES, function(cat) {
    cat_items <- Filter(function(i) !is.null(i$category) && i$category == cat, items)
    cost <- sum(sapply(cat_items, function(i) i$amount))
    if (cost > 0) {
      list(category = cat, cost = cost, share = ifelse(total > 0, cost / total, 0))
    } else {
      NULL
    }
  })
  byCat <- Filter(Negate(is.null), byCat)
  
  pctInc <- if (!is.null(o$income) && as.numeric(o$income) > 0) total / as.numeric(o$income) else 0
  pctCTP <- if (!is.null(o$nonFood) && as.numeric(o$nonFood) > 0) total / as.numeric(o$nonFood) else 0
  
  list(items = items, total = total, byCat = byCat, pctInc = pctInc, pctCTP = pctCTP, che10 = pctInc > 0.10, che25 = pctInc > 0.25, che40 = pctCTP > 0.40)
}

# --- Evaluation & ICER ---
icerIncremental <- function(strats) {
  d <- lapply(strats, function(s) { s$cost <- as.numeric(s$cost); s$effect <- as.numeric(s$effect); s })
  d <- d[order(sapply(d, function(x) x$cost))]
  for (i in seq_along(d)) d[[i]]$status <- "frontier"
  
  for (i in seq_along(d)) {
    for (j in seq_along(d)) {
      if (d[[j]]$cost < d[[i]]$cost && d[[j]]$effect >= d[[i]]$effect) {
        d[[i]]$status <- "dominated"
      }
    }
  }
  
  fr <- Filter(function(x) x$status != "dominated", d)
  if (length(fr) > 0) {
    fr[[1]]$incCost <- NA
    fr[[1]]$incEff <- NA
    fr[[1]]$icer <- NA
    if (length(fr) > 1) {
      for (i in 2:length(fr)) {
        fr[[i]]$incCost <- fr[[i]]$cost - fr[[i-1]]$cost
        fr[[i]]$incEff <- fr[[i]]$effect - fr[[i-1]]$effect
        fr[[i]]$icer <- if(fr[[i]]$incEff != 0) fr[[i]]$incCost / fr[[i]]$incEff else NA
      }
    }
    if (length(fr) > 2) {
      for (i in 3:length(fr)) {
        if (!is.na(fr[[i]]$icer) && !is.na(fr[[i-1]]$icer) && fr[[i]]$icer < fr[[i-1]]$icer) {
          fr[[i-1]]$status <- "extended"
        }
      }
    }
  }
  
  for (i in seq_along(d)) {
    match_idx <- which(sapply(fr, function(x) x$strategy == d[[i]]$strategy))
    if (length(match_idx) > 0) {
      f <- fr[[match_idx[1]]]
      d[[i]]$incCost <- f$incCost
      d[[i]]$incEff <- f$incEff
      d[[i]]$icer <- f$icer
      d[[i]]$status <- f$status
    }
  }
  d
}

nmb <- function(c, e, w) e * w - c

evalRequirements <- function(type, strats) {
  allCost <- all(sapply(strats, function(s) as.numeric(s$cost) > 0))
  allEff <- all(sapply(strats, function(s) !is.null(s$effect) && s$effect != "" && as.numeric(s$effect) != 0))
  eff <- sapply(strats, function(s) as.numeric(s$effect))
  mean_eff <- mean(eff)
  equal <- if (mean_eff == 0) TRUE else (max(eff) - min(eff)) / abs(mean_eff) < 0.03
  
  items <- list(list(label = "Total cost for each option", ok = allCost))
  if (type == "CMA") items <- append(items, list(list(label = "Outcomes equal across options", ok = equal, warn = if(!equal) "Outcomes differ" else NULL)))
  else if (type == "CBA") items <- append(items, list(list(label = "Monetised benefit (\u20B9) for each option", ok = allEff)))
  else if (type == "CCA") items <- append(items, list(list(label = "At least one outcome measure", ok = allEff)))
  else if (type == "CUA") items <- append(items, list(list(label = "QALYs (utility \u00D7 time) for each option", ok = allEff)))
  else items <- append(items, list(list(label = "A common natural-unit effect for each option", ok = allEff)))
  
  list(items = items, missing = sum(!sapply(items, function(i) i$ok)))
}

# --- Markov ---
fixModel <- function(m) {
  n <- length(m$states)
  m$strategies <- lapply(m$strategies, function(s) {
    M <- matrix(0, nrow=n, ncol=n)
    for (i in 1:n) {
      for (j in 1:n) {
        v <- if (!is.null(s$matrix) && length(s$matrix) >= i && length(s$matrix[[i]]) >= j && !is.null(s$matrix[[i]][[j]])) as.numeric(s$matrix[[i]][[j]]) else (if (i==j) 1 else 0)
        M[i, j] <- v
      }
    }
    s$matrix <- M
    
    s$cost <- sapply(1:n, function(i) {
      st <- m$states[[i]]
      as.numeric(st$cost %||% 0) + (if(i==1) as.numeric(s$cost %||% 0) else 0)
    })
    s$util <- sapply(1:n, function(i) {
      st <- m$states[[i]]
      as.numeric(st$util %||% 0) + (if(i==1) as.numeric(s$util %||% 0) else 0)
    })
    s$daly <- sapply(1:n, function(i) {
      st <- m$states[[i]]
      as.numeric(st$daly %||% 0) + (if(i==1) as.numeric(s$daly %||% 0) else 0)
    })
    s
  })
  
  for (i in 1:n) {
    m$states[[i]]$dead <- as.logical(m$states[[i]]$dead %||% FALSE)
  }
  
  m
}

BG_MORT <- matrix(c(0,0.028, 1,0.003, 5,0.001, 15,0.0015, 25,0.002, 35,0.003, 45,0.006, 55,0.013, 65,0.030, 75,0.070, 85,0.15), ncol=2, byrow=TRUE)
bgMortAnnual <- function(age) {
  q <- BG_MORT[1,2]
  for (i in 1:nrow(BG_MORT)) {
    if (age >= BG_MORT[i,1]) q <- BG_MORT[i,2]
  }
  q
}

markovGeneric <- function(m, strat) {
  n <- length(m$states)
  P <- strat$matrix
  nC <- round(as.numeric(m$horizon) / as.numeric(m$cycle))
  deadIdxFirst <- which(sapply(m$states, function(s) !is.null(s$absorbing) && s$absorbing))[1]
  useBg <- !is.null(m$bgMortality) && m$bgMortality && !is.na(deadIdxFirst)
  startAge <- if (!is.null(m$startAge) && as.numeric(m$startAge) > 0) as.numeric(m$startAge) else 30
  
  trace <- matrix(0, nrow=nC+1, ncol=n)
  trace[1, 1] <- 1
  
  for (t in 1:nC) {
    cur <- trace[t, ]
    nx <- as.numeric(cur %*% P)
    if (useBg) {
      age <- startAge + (t-1) * as.numeric(m$cycle)
      pCycle <- 1 - (1 - bgMortAnnual(age))^as.numeric(m$cycle)
      for (j in 1:n) {
        if (!m$states[[j]]$absorbing) {
          mv <- nx[j] * pCycle
          nx[j] <- nx[j] - mv
          nx[deadIdxFirst] <- nx[deadIdxFirst] + mv
        }
      }
    }
    trace[t+1, ] <- nx
  }
  
  Cc <- 0; Q <- 0; YLD <- 0
  for (t in 1:nC) {
    occ <- (trace[t,] + trace[t+1,]) / 2
    dc <- (1 + as.numeric(m$dCost))^((t-1) * as.numeric(m$cycle))
    de <- (1 + as.numeric(m$dEff))^((t-1) * as.numeric(m$cycle))
    for (i in 1:n) {
      st <- m$states[[i]]
      cc <- as.numeric(st$cost) + (if(!is.null(st$absorbing) && st$absorbing) 0 else (if(!is.null(strat$addCost)) as.numeric(strat$addCost) else 0))
      Cc <- Cc + occ[i] * cc * as.numeric(m$cycle) / dc
      Q <- Q + occ[i] * as.numeric(st$util) * as.numeric(m$cycle) / de
      YLD <- YLD + occ[i] * (if(!is.null(st$dw)) as.numeric(st$dw) else 0) * as.numeric(m$cycle) / de
    }
  }
  
  deadIdx <- which(sapply(m$states, function(s) !is.null(s$absorbing) && s$absorbing))
  YLL <- 0
  for (t in 1:nC) {
    nd <- 0
    if (length(deadIdx) > 0) {
      for (i in deadIdx) nd <- nd + (trace[t+1, i] - trace[t, i])
    }
    YLL <- YLL + nd * as.numeric(m$lifeExp) / ((1 + as.numeric(m$dEff))^((t-1) * as.numeric(m$cycle)))
  }
  
  list(cost = Cc, qaly = Q, daly = YLD + YLL, yll = YLL, trace = trace, nC = nC)
}

modelRunAll <- function(m) {
  m <- fixModel(m)
  lapply(m$strategies, function(s) {
    r <- markovGeneric(m, s)
    list(name = s$name, cost = r$cost, qaly = r$qaly, daly = r$daly, trace = r$trace)
  })
}

modelIncremental <- function(arr, lowerBetter) {
  d <- lapply(arr, function(s) s)
  d <- d[order(sapply(d, function(x) x$cost))]
  for (i in seq_along(d)) d[[i]]$status <- "frontier"
  
  for (i in seq_along(d)) {
    for (j in seq_along(d)) {
      cond <- if(lowerBetter) d[[j]]$eff <= d[[i]]$eff else d[[j]]$eff >= d[[i]]$eff
      if (d[[j]]$cost < d[[i]]$cost && cond) {
        d[[i]]$status <- "dominated"
      }
    }
  }
  
  fr <- Filter(function(x) x$status != "dominated", d)
  if (length(fr) > 0) {
    fr[[1]]$incCost <- NA
    fr[[1]]$incEff <- NA
    fr[[1]]$icer <- NA
    if (length(fr) > 1) {
      for (i in 2:length(fr)) {
        fr[[i]]$incCost <- fr[[i]]$cost - fr[[i-1]]$cost
        de <- if(lowerBetter) (fr[[i-1]]$eff - fr[[i]]$eff) else (fr[[i]]$eff - fr[[i-1]]$eff)
        fr[[i]]$incEff <- de
        fr[[i]]$icer <- if(de != 0) fr[[i]]$incCost / de else NA
      }
    }
    if (length(fr) > 2) {
      for (i in 3:length(fr)) {
        if (!is.na(fr[[i]]$icer) && !is.na(fr[[i-1]]$icer) && fr[[i]]$icer < fr[[i-1]]$icer) {
          fr[[i-1]]$status <- "extended"
        }
      }
    }
  }
  
  for (i in seq_along(d)) {
    match_idx <- which(sapply(fr, function(x) x$name == d[[i]]$name))
    if (length(match_idx) > 0) {
      f <- fr[[match_idx[1]]]
      d[[i]]$incCost <- f$incCost
      d[[i]]$incEff <- f$incEff
      d[[i]]$icer <- f$icer
      d[[i]]$status <- f$status
    }
  }
  d
}

# --- Sensitivity (PSA) ---
psaModel <- function(m, N, iRef, iCmp, cv, mode, params) {
  cv <- if(!is.null(cv) && cv > 0) cv else 0.2
  uSE <- min(0.08, 0.4 * cv)
  K_global <- max(4, 1 / (cv * cv))
  m <- fixModel(m)
  draws <- list()
  
  for (it in 1:N) {
    mm <- jsonlite::fromJSON(jsonlite::toJSON(m, auto_unbox=TRUE), simplifyVector = FALSE)
    
    if (!is.null(mode) && mode == "per_parameter" && !is.null(params)) {
      for (i in seq_along(mm$states)) {
        st <- mm$states[[i]]
        pSt <- if(!is.null(params$states) && length(params$states) >= i) params$states[[i]] else list()
        
        if (as.numeric(st$cost) > 0 && !is.null(pSt$cost)) {
          if (!is.null(pSt$cost$dist) && pSt$cost$dist == "gamma") {
            g <- gammaMS(as.numeric(pSt$cost$mean), as.numeric(pSt$cost$se))
            mm$states[[i]]$cost <- rgamma(1, shape = g$shape, scale = g$scale)
          } else if (!is.null(pSt$cost$dist) && pSt$cost$dist == "normal") {
            mm$states[[i]]$cost <- rnorm(1, mean = as.numeric(pSt$cost$mean), sd = as.numeric(pSt$cost$se))
          } else if (!is.null(pSt$cost$dist) && pSt$cost$dist == "fixed") {
            mm$states[[i]]$cost <- as.numeric(pSt$cost$mean)
          }
        }
        
        if (as.numeric(st$util) > 0 && as.numeric(st$util) < 1 && !is.null(pSt$util)) {
          if (!is.null(pSt$util$dist) && pSt$util$dist == "beta") {
            b <- betaMS(as.numeric(pSt$util$mean), as.numeric(pSt$util$se))
            mm$states[[i]]$util <- rbeta(1, b$a, b$b)
          } else if (!is.null(pSt$util$dist) && pSt$util$dist == "normal") {
            mm$states[[i]]$util <- max(0, min(1, rnorm(1, mean = as.numeric(pSt$util$mean), sd = as.numeric(pSt$util$se))))
          } else if (!is.null(pSt$util$dist) && pSt$util$dist == "fixed") {
            mm$states[[i]]$util <- as.numeric(pSt$util$mean)
          }
        }
      }
      
      for (i in seq_along(mm$strategies)) {
        s <- mm$strategies[[i]]
        pStr <- if(!is.null(params$strategies) && length(params$strategies) >= i) params$strategies[[i]] else list()
        
        if (!is.null(s$addCost) && as.numeric(s$addCost) > 0 && !is.null(pStr$addCost)) {
          if (!is.null(pStr$addCost$dist) && pStr$addCost$dist == "gamma") {
            g <- gammaMS(as.numeric(pStr$addCost$mean), as.numeric(pStr$addCost$se))
            mm$strategies[[i]]$addCost <- rgamma(1, shape = g$shape, scale = g$scale)
          } else if (!is.null(pStr$addCost$dist) && pStr$addCost$dist == "normal") {
            mm$strategies[[i]]$addCost <- rnorm(1, mean = as.numeric(pStr$addCost$mean), sd = as.numeric(pStr$addCost$se))
          } else if (!is.null(pStr$addCost$dist) && pStr$addCost$dist == "fixed") {
            mm$strategies[[i]]$addCost <- as.numeric(pStr$addCost$mean)
          }
        }
        
        if (!is.null(pStr$matrix)) {
          new_mat <- matrix(0, nrow=length(s$matrix), ncol=length(s$matrix))
          for (rIdx in 1:length(s$matrix)) {
            row <- s$matrix[rIdx, ]
            if (sum(row) == 0) {
              new_mat[rIdx, ] <- row
            } else {
              mRow <- if(length(pStr$matrix) >= rIdx) pStr$matrix[[rIdx]] else list()
              if (!is.null(mRow$dist) && mRow$dist == "dirichlet") {
                new_mat[rIdx, ] <- rdirichlet(sapply(row, function(p) max(0.0001, as.numeric(p)) * (if(!is.null(mRow$K) && as.numeric(mRow$K)>0) as.numeric(mRow$K) else 100)))
              } else {
                new_mat[rIdx, ] <- row
              }
            }
          }
          mm$strategies[[i]]$matrix <- new_mat
        }
      }
    } else {
      for (i in seq_along(mm$states)) {
        st <- mm$states[[i]]
        if (as.numeric(st$cost) > 0) {
          g <- gammaMS(as.numeric(st$cost), as.numeric(st$cost) * cv)
          mm$states[[i]]$cost <- rgamma(1, shape = g$shape, scale = g$scale)
        }
        if (as.numeric(st$util) > 0 && as.numeric(st$util) < 1) {
          b <- betaMS(as.numeric(st$util), uSE)
          mm$states[[i]]$util <- rbeta(1, b$a, b$b)
        }
      }
      for (i in seq_along(mm$strategies)) {
        s <- mm$strategies[[i]]
        new_mat <- matrix(0, nrow=nrow(s$matrix), ncol=ncol(s$matrix))
        for (rIdx in 1:nrow(s$matrix)) {
          row <- s$matrix[rIdx, ]
          if (sum(row) == 0) {
            new_mat[rIdx, ] <- row
          } else {
            new_mat[rIdx, ] <- rdirichlet(sapply(row, function(p) max(0.0001, as.numeric(p)) * K_global))
          }
        }
        mm$strategies[[i]]$matrix <- new_mat
      }
    }
    
    ref <- markovGeneric(mm, mm$strategies[[iRef + 1]])
    cmp <- markovGeneric(mm, mm$strategies[[iCmp + 1]])
    incEff <- if (m$outcome == "QALY") (cmp$qaly - ref$qaly) else (ref$daly - cmp$daly)
    draws <- append(draws, list(list(incCost = cmp$cost - ref$cost, incEff = incEff)))
  }
  draws
}

ceac <- function(d, ws) {
  lapply(ws, function(w) {
    prob <- sum(sapply(d, function(x) nmb(x$incCost, x$incEff, w) > 0)) / length(d)
    list(wtp = w, prob = prob)
  })
}

evpi <- function(d, w) {
  nb <- lapply(d, function(x) c(0, nmb(x$incCost, x$incEff, w)))
  mm <- sum(sapply(nb, max)) / length(nb)
  mx <- max(sum(sapply(nb, function(x) x[1])) / length(nb), sum(sapply(nb, function(x) x[2])) / length(nb))
  max(0, mm - mx)
}

# --- BIA ---
biaRun <- function(b) {
  horizon <- as.numeric(b$horizon)
  startYear <- as.numeric(b$startYear)
  years <- startYear + 0:(horizon - 1)
  
  rows <- list()
  cum <- 0
  for (i in seq_along(years)) {
    yr <- years[i]
    uptake <- min(as.numeric(b$maxUptake), as.numeric(b$maxUptake) * i / horizon)
    treated <- as.numeric(b$population) * as.numeric(b$eligible) * uptake
    onOld <- as.numeric(b$population) * as.numeric(b$eligible) - treated
    worldNew <- treated * as.numeric(b$costNew) + onOld * as.numeric(b$costOld)
    worldOld <- as.numeric(b$population) * as.numeric(b$eligible) * as.numeric(b$costOld)
    impact <- worldNew - worldOld
    cum <- cum + impact
    rows <- append(rows, list(list(year = yr, uptake = uptake, treated = treated, worldOld = worldOld, worldNew = worldNew, impact = impact, cum = cum)))
  }
  rows
}
