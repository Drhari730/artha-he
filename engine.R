# =============================================================================
# engine.R : Artha HE calculation engine (pure R, no package dependencies
# beyond base + stats). Every public function is covered by tests/testthat.
# =============================================================================

# --- Input helpers -------------------------------------------------------------

# Raise an input-validation error. plumber.R turns these into HTTP 400 with the
# message shown to the user (other errors are 500).
input_error <- function(msg) {
  stop(structure(class = c("input_error", "error", "condition"),
                 list(message = msg, call = NULL)))
}

# Safe numeric scalar: NULL / "" / NA / non-numeric -> default.
num <- function(x, default = 0) {
  if (is.null(x) || length(x) == 0) return(default)
  v <- suppressWarnings(as.numeric(x[[1]]))
  if (length(v) == 0 || is.na(v)) default else v
}

# Absorbing flag that tolerates a missing field (NULL), "true"/"false", 0/1.
isAbs <- function(s) isTRUE(as.logical(s$absorbing))

# Sum a field over a (possibly empty) list without the sapply(list()) trap.
sumv <- function(lst, f) {
  if (length(lst) == 0) return(0)
  sum(vapply(lst, function(x) num(f(x)), numeric(1)))
}

sum_arr <- function(x) sum(x, na.rm = TRUE)

# --- Distributions ----------------------------------------------------------------

# Dirichlet draw that (a) keeps structural zeros at zero, so impossible
# transitions (e.g. Dead -> Healthy) stay impossible, and (b) can take shared
# uniforms `u` (common random numbers) so that the same row in two strategies
# is sampled in a correlated way.
rdirichlet <- function(alpha, u = NULL) {
  alpha <- as.numeric(alpha)
  pos <- alpha > 0
  out <- numeric(length(alpha))
  if (sum(pos) <= 1) { out[pos] <- 1; return(out) }
  if (is.null(u)) u <- stats::runif(length(alpha))
  g <- stats::qgamma(pmin(pmax(u[pos], 1e-12), 1 - 1e-12), shape = alpha[pos], rate = 1)
  if (!all(is.finite(g)) || sum(g) <= 0) { out[pos] <- alpha[pos] / sum(alpha[pos]); return(out) }
  out[pos] <- g / sum(g)
  out
}

# Method-of-moments Beta parameters from mean and SE.
betaMS <- function(m, se) {
  if (se <= 0) return(list(a = m * 1e6, b = (1 - m) * 1e6))
  v <- se * se
  if (v >= m * (1 - m)) v <- m * (1 - m) * 0.99   # keep parameters valid
  t <- m * (1 - m) / v - 1
  list(a = max(0.01, m * t), b = max(0.01, (1 - m) * t))
}

# Method-of-moments Gamma parameters from mean and SE.
gammaMS <- function(m, se) {
  if (se <= 0 || m <= 0) return(list(shape = 1e6, scale = max(m, 0) / 1e6))
  v <- se * se
  list(shape = m * m / v, scale = v / m)
}

# --- Costing --------------------------------------------------------------------
COST_CATEGORIES <- c("Direct medical", "Direct non-medical", "Indirect (productivity)")

inflate_cost <- function(c, fy, ty, r) {
  if (is.null(fy) || is.na(fy) || is.null(ty) || is.na(ty) || is.na(r)) return(c)
  c * (1 + r)^(ty - fy)
}

byCategory <- function(items, amountOf, total) {
  out <- lapply(COST_CATEGORIES, function(cat) {
    ci <- Filter(function(l) identical(as.character(l$category), cat), items)
    cost <- sumv(ci, amountOf)
    if (cost > 0) list(category = cat, cost = cost, share = if (total > 0) cost / total else 0) else NULL
  })
  Filter(Negate(is.null), out)
}

microCost <- function(rows, ty, infl) {
  if (is.null(rows) || length(rows) == 0) return(list(lines = list(), total = 0, byCat = list()))
  lines <- lapply(rows, function(r) {
    u <- inflate_cost(num(r$unit_cost), num(r$year, NA), ty, num(infl))
    r$unit_cost_adj <- u
    r$line_cost <- num(r$quantity) * u
    r
  })
  total <- sumv(lines, function(l) l$line_cost)
  list(lines = lines, total = total, byCat = byCategory(lines, function(l) l$line_cost, total))
}

annualisationFactor <- function(d, L) {
  if (is.null(L) || is.na(L) || L <= 0) return(1)
  if (d <= 0) return(1 / L)
  f <- (1 + d)^L
  d * f / (f - 1)
}

advancedCost <- function(a) {
  if (is.null(a)) a <- list()
  MH <- if (num(a$monthlyHours) > 0) num(a$monthlyHours) else 160
  d <- if (!is.null(a$discount) && num(a$discount, -1) >= 0) num(a$discount) else 0.03

  staff <- lapply(a$staff, function(s) {
    h <- num(s$hours)
    ses <- if (num(s$sessions) > 0) num(s$sessions) else 1
    list(role = s$role, salary = num(s$salary), hours = h, sessions = ses,
         apportion = h * ses / MH, line = (num(s$salary) / MH) * h * ses)
  })
  equip <- lapply(a$equip, function(e) {
    L <- num(e$life, NA)
    AF <- annualisationFactor(d, L)
    usage <- num(e$usagePct, 1)
    qty <- if (num(e$qty) > 0) num(e$qty) else 1
    price <- num(e$price)
    annualCapital <- price * AF * qty
    annualMaint <- price * qty * num(e$maintPct)
    list(item = e$item, price = price, qty = qty, life = L, af = AF, annualCapital = annualCapital,
         annualMaint = annualMaint, usage = usage, line = (annualCapital + annualMaint) * usage)
  })
  consum <- lapply(a$consum, function(c) {
    qty <- num(c$qty); unit <- num(c$unit)
    list(item = c$item, qty = qty, unit = unit, line = qty * unit)
  })
  space <- lapply(a$space, function(s) {
    ann <- num(s$annual)
    list(item = s$item, annual = ann, line = ann)
  })

  lineOf <- function(x) x$line
  groups <- list()
  if (length(staff))  groups <- append(groups, list(list(key = "staff",  label = "Human resources", items = staff, total = sumv(staff, lineOf))))
  if (length(equip))  groups <- append(groups, list(list(key = "equip",  label = "Equipment / capital (annualised)", items = equip, total = sumv(equip, lineOf))))
  if (length(consum)) groups <- append(groups, list(list(key = "consum", label = "Consumables / recurring", items = consum, total = sumv(consum, lineOf))))
  if (length(space))  groups <- append(groups, list(list(key = "space",  label = "Space", items = space, total = sumv(space, lineOf))))

  total <- sumv(groups, function(g) g$total)
  output <- if (num(a$output) > 0) num(a$output) else 1
  breakdown <- lapply(groups, function(g)
    list(component = g$label, amount = g$total, share = if (total > 0) g$total / total else 0))

  list(method = "advanced", total = total, perUnit = total / output, output = output, discount = d,
       monthlyHours = MH, groups = groups, breakdown = breakdown)
}

# --- Out-of-pocket & catastrophic health expenditure -------------------------------
# CHE follows SDG 3.8.2 / WHO: only out-of-pocket payments for health goods and
# services (category "Direct medical") count. Transport, food and lost income
# are reported in the total but excluded from the CHE ratios.
oopRun <- function(o) {
  if (is.null(o)) o <- list()
  items <- lapply(o$items, function(i) { i$amount <- num(i$amount); i })
  amt <- function(i) i$amount
  total <- sumv(items, amt)
  health <- sumv(Filter(function(i) identical(as.character(i$category), "Direct medical"), items), amt)
  income <- num(o$income); ctp <- num(o$nonFood)
  pctInc <- if (income > 0) health / income else 0
  pctCTP <- if (ctp > 0) health / ctp else 0
  list(items = items, total = total, health = health, nonHealth = total - health,
       byCat = byCategory(items, amt, total),
       pctInc = pctInc, pctCTP = pctCTP, pctIncAll = if (income > 0) total / income else 0,
       che10 = pctInc > 0.10, che25 = pctInc > 0.25, che40 = pctCTP > 0.40)
}

# --- Incremental analysis (shared by Evaluation and Modeling) ----------------------
# Standard algorithm: sort by cost, remove strongly dominated options, then
# repeatedly remove extendedly dominated options (ICERs must increase along the
# frontier) and recompute ICERs against the previous non-dominated option.
# `eff` must be "higher is better".
cea_frontier <- function(cost, eff) {
  n <- length(cost)
  if (n == 0) return(list(order = integer(0), status = character(0), incCost = numeric(0), incEff = numeric(0), icer = numeric(0)))
  if (any(!is.finite(cost)) || any(!is.finite(eff))) input_error("Every option needs a numeric cost and effect.")
  status <- rep("frontier", n)
  incCost <- incEff <- icer <- rep(NA_real_, n)
  ord <- order(cost, -eff)

  # Strong dominance (costs no less, effective no more, and worse on one).
  for (i in seq_len(n)) for (j in seq_len(n)) if (i != j) {
    if (cost[j] <= cost[i] && eff[j] >= eff[i] && (cost[j] < cost[i] || eff[j] > eff[i])) status[i] <- "dominated"
  }
  # Exact duplicates: keep the first one in cost order.
  for (a in seq_along(ord)) for (b in seq_len(a - 1)) {
    i <- ord[a]; j <- ord[b]
    if (status[i] == "frontier" && status[j] == "frontier" && cost[i] == cost[j] && eff[i] == eff[j]) status[i] <- "dominated"
  }
  # Extended dominance, iterated until ICERs are strictly increasing.
  repeat {
    fr <- ord[status[ord] == "frontier"]
    if (length(fr) < 3) break
    ic <- vapply(2:length(fr), function(k) (cost[fr[k]] - cost[fr[k - 1]]) / (eff[fr[k]] - eff[fr[k - 1]]), numeric(1))
    bad <- which(diff(ic) < 0)
    if (!length(bad)) break
    status[fr[bad[1] + 1]] <- "extended"
  }
  fr <- ord[status[ord] == "frontier"]
  for (k in seq_along(fr)[-1]) {
    i <- fr[k]; p <- fr[k - 1]
    incCost[i] <- cost[i] - cost[p]
    incEff[i] <- eff[i] - eff[p]
    icer[i] <- incCost[i] / incEff[i]
  }
  list(order = ord, status = status, incCost = incCost, incEff = incEff, icer = icer)
}

# Optimal option = highest net monetary benefit at the threshold (ties -> cheaper).
# This is equivalent to walking the frontier until the ICER exceeds the WTP.
best_index <- function(cost, eff, wtp) {
  nb <- eff * wtp - cost
  cand <- which(nb == max(nb))
  cand[which.min(cost[cand])]
}

# Logical NA serialises to JSON null (numeric NA would become the string "NA").
na_null <- function(x) if (is.na(x)) NA else x

apply_frontier <- function(d, fr) {
  out <- d[fr$order]
  for (k in seq_along(fr$order)) {
    i <- fr$order[k]
    out[[k]]$status <- fr$status[i]
    out[[k]]$incCost <- na_null(fr$incCost[i])
    out[[k]]$incEff <- na_null(fr$incEff[i])
    out[[k]]$icer <- na_null(fr$icer[i])
  }
  out
}

icerIncremental <- function(strats) {
  d <- lapply(strats, function(s) { s$cost <- num(s$cost, NA); s$effect <- num(s$effect, NA); s })
  fr <- cea_frontier(vapply(d, function(x) x$cost, numeric(1)), vapply(d, function(x) x$effect, numeric(1)))
  apply_frontier(d, fr)
}

nmb <- function(c, e, w) e * w - c

evalRequirements <- function(type, strats) {
  hasNum <- function(x) !is.null(x) && !identical(x, "") && !is.na(suppressWarnings(as.numeric(x)))
  allCost <- length(strats) > 0 && all(vapply(strats, function(s) hasNum(s$cost) && as.numeric(s$cost) >= 0, logical(1)))
  allEff <- length(strats) > 0 && all(vapply(strats, function(s) hasNum(s$effect), logical(1)))
  eff <- vapply(strats, function(s) num(s$effect), numeric(1))
  mean_eff <- if (length(eff)) mean(eff) else 0
  equal <- if (mean_eff == 0) TRUE else (max(eff) - min(eff)) / abs(mean_eff) < 0.03

  items <- list(list(label = "At least two options to compare", ok = length(strats) >= 2),
                list(label = "Total cost for each option", ok = allCost))
  if (type == "CMA") items <- append(items, list(list(label = "Outcomes equal across options", ok = equal, warn = if (!equal) "Outcomes differ" else NULL)))
  else if (type == "CBA") items <- append(items, list(list(label = "Monetised benefit (₹) for each option", ok = allEff)))
  else if (type == "CCA") items <- append(items, list(list(label = "At least one outcome measure", ok = allEff)))
  else if (type == "CUA") items <- append(items, list(list(label = "QALYs (utility × time) for each option", ok = allEff)))
  else items <- append(items, list(list(label = "A common natural-unit effect for each option", ok = allEff)))

  list(items = items, missing = sum(!vapply(items, function(i) isTRUE(i$ok), logical(1))))
}

# --- Markov ---------------------------------------------------------------------
LIMITS <- list(states = 50, strategies = 20, cycles = 1200, psaN = 10000)
ROW_TOL <- 1e-3   # rows within this of 1 are renormalised; beyond it, an error

# Build n x n numeric matrices, then validate the model. Idempotent.
fixModel <- function(m) {
  n <- length(m$states)
  if (n < 2) input_error("The model needs at least two health states.")
  if (n > LIMITS$states) input_error(paste0("At most ", LIMITS$states, " health states are supported."))
  if (length(m$strategies) < 1) input_error("The model needs at least one strategy.")
  if (length(m$strategies) > LIMITS$strategies) input_error(paste0("At most ", LIMITS$strategies, " strategies are supported."))
  cyc <- num(m$cycle); hor <- num(m$horizon)
  if (cyc <= 0) input_error("Cycle length must be greater than zero.")
  if (hor <= 0) input_error("Time horizon must be greater than zero.")
  if (round(hor / cyc) < 1) input_error("The time horizon must be at least one cycle long.")
  if (round(hor / cyc) > LIMITS$cycles) input_error(paste0("At most ", LIMITS$cycles, " cycles are supported (horizon / cycle length)."))

  m$strategies <- lapply(m$strategies, function(s) {
    if (!(is.matrix(s$matrix) && nrow(s$matrix) == n && ncol(s$matrix) == n)) {
      M <- matrix(0, nrow = n, ncol = n)
      for (i in 1:n) for (j in 1:n) {
        has <- !is.null(s$matrix) && length(s$matrix) >= i && !is.null(s$matrix[[i]]) &&
               length(s$matrix[[i]]) >= j && !is.null(s$matrix[[i]][[j]])
        M[i, j] <- if (has) num(s$matrix[[i]][[j]], NA) else (if (i == j) 1 else 0)
      }
      s$matrix <- M
    }
    M <- s$matrix
    nm <- if (is.null(s$name)) "strategy" else s$name
    if (any(is.na(M))) input_error(paste0("Transition matrix for '", nm, "' has a blank or non-numeric cell."))
    if (any(M < 0) || any(M > 1)) input_error(paste0("Transition probabilities for '", nm, "' must be between 0 and 1."))
    rs <- rowSums(M)
    for (i in 1:n) {
      if (abs(rs[i] - 1) > ROW_TOL)
        input_error(sprintf("In '%s', the row for '%s' sums to %.4f; each row must sum to 1.", nm, m$states[[i]]$name, rs[i]))
    }
    s$matrix <- M / rs
    s
  })
  m
}

# Indicative annual all-cause mortality by age (crude; replace with SRS life table).
BG_MORT <- matrix(c(0,0.028, 1,0.003, 5,0.001, 15,0.0015, 25,0.002, 35,0.003, 45,0.006, 55,0.013, 65,0.030, 75,0.070, 85,0.15), ncol = 2, byrow = TRUE)
bgMortAnnual <- function(age) {
  idx <- max(which(BG_MORT[, 1] <= age), 1)
  BG_MORT[idx, 2]
}

# Cohort state-transition model for one strategy.
# Conventions: cohort starts in state 1; within-cycle (half-cycle) correction by
# the trapezoidal rule; cycle t (t = 1..nC) is discounted at (1+r)^((t-1)*cycle).
# Background mortality, when on, is applied to survivors after the disease
# transitions (i.e. the matrix death column is treated as disease-specific).
markovGeneric <- function(m, strat) {
  n <- length(m$states)
  P <- strat$matrix
  cyc <- num(m$cycle)
  nC <- round(num(m$horizon) / cyc)
  absIdx <- which(vapply(m$states, isAbs, logical(1)))
  alive <- !(seq_len(n) %in% absIdx)
  useBg <- isTRUE(as.logical(m$bgMortality)) && length(absIdx) > 0
  startAge <- if (num(m$startAge) > 0) num(m$startAge) else 30

  trace <- matrix(0, nrow = nC + 1, ncol = n)
  trace[1, 1] <- 1
  for (t in 1:nC) {
    nx <- as.numeric(trace[t, ] %*% P)
    if (useBg) {
      pCycle <- 1 - (1 - bgMortAnnual(startAge + (t - 1) * cyc))^cyc
      mv <- nx[alive] * pCycle
      nx[alive] <- nx[alive] - mv
      nx[absIdx[1]] <- nx[absIdx[1]] + sum(mv)
    }
    trace[t + 1, ] <- nx
  }

  occ <- (trace[1:nC, , drop = FALSE] + trace[2:(nC + 1), , drop = FALSE]) / 2
  addC <- num(strat$addCost)
  cvec <- vapply(m$states, function(s) num(s$cost), numeric(1)) + ifelse(alive, addC, 0)
  uvec <- vapply(m$states, function(s) num(s$util), numeric(1))
  dwvec <- vapply(m$states, function(s) num(s$dw), numeric(1))
  tt <- (0:(nC - 1)) * cyc
  dc <- (1 + num(m$dCost))^tt
  de <- (1 + num(m$dEff))^tt

  Cc <- sum((occ %*% cvec) * cyc / dc)
  Q <- sum((occ %*% uvec) * cyc / de)
  LY <- sum(rowSums(occ[, alive, drop = FALSE]) * cyc / de)
  YLD <- sum((occ %*% dwvec) * cyc / de)
  deaths <- if (length(absIdx)) rowSums(trace[2:(nC + 1), absIdx, drop = FALSE] - trace[1:nC, absIdx, drop = FALSE]) else rep(0, nC)
  YLL <- sum(deaths * num(m$lifeExp) / de)

  list(cost = Cc, qaly = Q, ly = LY, daly = YLD + YLL, yll = YLL, yld = YLD, trace = trace, nC = nC)
}

modelRunAll <- function(m) {
  m <- fixModel(m)
  lapply(m$strategies, function(s) {
    r <- markovGeneric(m, s)
    list(name = s$name, cost = r$cost, qaly = r$qaly, ly = r$ly, daly = r$daly, trace = r$trace)
  })
}

# arr: list with $name, $cost, $eff. lowerBetter = TRUE for DALYs (incEff = DALYs averted).
modelIncremental <- function(arr, lowerBetter) {
  e <- vapply(arr, function(x) num(x$eff, NA), numeric(1))
  fr <- cea_frontier(vapply(arr, function(x) num(x$cost, NA), numeric(1)), if (lowerBetter) -e else e)
  apply_frontier(arr, fr)
}

# --- Sensitivity (PSA) -------------------------------------------------------------

sampleParam <- function(p, base, kind) {
  dist <- if (is.null(p$dist)) "" else p$dist
  mu <- num(p$mean, base); se <- num(p$se)
  if (dist == "fixed") return(mu)
  if (kind == "cost") {
    if (dist == "gamma") { g <- gammaMS(mu, se); return(stats::rgamma(1, shape = g$shape, scale = g$scale)) }
    if (dist == "normal") return(max(0, stats::rnorm(1, mu, se)))
  } else {
    if (dist == "beta") { b <- betaMS(mu, se); return(stats::rbeta(1, b$a, b$b)) }
    if (dist == "normal") return(max(0, min(1, stats::rnorm(1, mu, se))))
  }
  base
}

# One PSA draw of the whole model. Transition rows use common random numbers
# (the same uniforms U for every strategy), so arms that share a row stay
# correlated and the incremental results are not inflated by independent noise.
psaDraw <- function(m, cv, mode, params) {
  n <- length(m$states)
  uSE <- min(0.08, 0.4 * cv)
  K_global <- max(4, 1 / (cv * cv))
  perParam <- identical(mode, "per_parameter") && !is.null(params)
  U <- matrix(stats::runif(n * n), n, n)
  mm <- m

  for (i in seq_len(n)) {
    st <- m$states[[i]]
    cst <- num(st$cost); utl <- num(st$util)
    pSt <- if (perParam && length(params$states) >= i) params$states[[i]] else NULL
    if (cst > 0) {
      mm$states[[i]]$cost <- if (perParam) { if (!is.null(pSt$cost)) sampleParam(pSt$cost, cst, "cost") else cst }
                             else { g <- gammaMS(cst, cst * cv); stats::rgamma(1, shape = g$shape, scale = g$scale) }
    }
    if (utl > 0 && utl < 1) {
      mm$states[[i]]$util <- if (perParam) { if (!is.null(pSt$util)) sampleParam(pSt$util, utl, "util") else utl }
                             else { b <- betaMS(utl, uSE); stats::rbeta(1, b$a, b$b) }
    }
  }

  for (k in seq_along(m$strategies)) {
    s <- m$strategies[[k]]
    pStr <- if (perParam && length(params$strategies) >= k) params$strategies[[k]] else NULL
    ac <- num(s$addCost)
    if (ac > 0 && perParam && !is.null(pStr$addCost)) mm$strategies[[k]]$addCost <- sampleParam(pStr$addCost, ac, "cost")
    P <- s$matrix
    newP <- P
    for (r in seq_len(n)) {
      if (perParam) {
        mRow <- if (!is.null(pStr$matrix) && length(pStr$matrix) >= r) pStr$matrix[[r]] else NULL
        if (is.null(mRow) || !identical(mRow$dist, "dirichlet")) next
        K <- if (num(mRow$K) > 0) num(mRow$K) else 100
      } else K <- K_global
      newP[r, ] <- rdirichlet(P[r, ] * K, U[r, ])
    }
    mm$strategies[[k]]$matrix <- newP
  }
  mm
}

psaModel <- function(m, N, iRef, iCmp, cv, mode, params, seed = NULL) {
  cv <- if (!is.null(cv) && cv > 0) cv else 0.2
  N <- as.integer(N)
  if (is.na(N) || N < 1) input_error("PSA needs at least one iteration.")
  if (N > LIMITS$psaN) input_error(paste0("At most ", LIMITS$psaN, " PSA iterations are supported."))
  if (!is.null(seed) && !is.na(seed)) set.seed(as.integer(seed))
  m <- fixModel(m)
  qaly <- identical(m$outcome, "QALY") || is.null(m$outcome)
  lapply(seq_len(N), function(it) {
    mm <- psaDraw(m, cv, mode, params)
    ref <- markovGeneric(mm, mm$strategies[[iRef + 1]])
    cmp <- markovGeneric(mm, mm$strategies[[iCmp + 1]])
    list(incCost = cmp$cost - ref$cost,
         incEff = if (qaly) cmp$qaly - ref$qaly else ref$daly - cmp$daly)
  })
}

ceac <- function(d, ws) {
  lapply(ws, function(w) {
    prob <- mean(vapply(d, function(x) nmb(x$incCost, x$incEff, w) > 0, logical(1)))
    list(wtp = w, prob = prob)
  })
}

# Per-patient EVPI for a two-option decision: E[max NB] - max E[NB].
evpi <- function(d, w) {
  inb <- vapply(d, function(x) nmb(x$incCost, x$incEff, w), numeric(1))
  max(0, mean(pmax(0, inb)) - max(0, mean(inb)))
}

# --- Deterministic one-way sensitivity (tornado) -------------------------------------
dsaModel <- function(m, w, iRef, iCmp) {
  m <- fixModel(m)
  qaly <- identical(m$outcome, "QALY") || is.null(m$outcome)
  nmbPair <- function(mm) {
    r <- markovGeneric(mm, mm$strategies[[iRef + 1]])
    cc <- markovGeneric(mm, mm$strategies[[iCmp + 1]])
    incE <- if (qaly) (cc$qaly - r$qaly) else (r$daly - cc$daly)
    incE * w - (cc$cost - r$cost)
  }
  base <- nmbPair(m)
  params <- list()
  for (si in seq_along(m$strategies)) {
    ac <- num(m$strategies[[si]]$addCost)
    if (ac > 0) params <- append(params, list(list(label = paste0(m$strategies[[si]]$name, " added cost"), kind = "strat", idx = si, field = "addCost", lo = ac * 0.7, hi = ac * 1.3)))
  }
  for (si in seq_along(m$states)) {
    st <- m$states[[si]]
    cst <- num(st$cost); utl <- num(st$util)
    if (cst > 0) params <- append(params, list(list(label = paste0(st$name, " cost"), kind = "state", idx = si, field = "cost", lo = cst * 0.7, hi = cst * 1.3)))
    if (utl > 0) params <- append(params, list(list(label = paste0(st$name, " utility"), kind = "state", idx = si, field = "util", lo = max(0, utl - 0.1), hi = min(1, utl + 0.1))))
  }
  out <- lapply(params, function(p) {
    mlo <- m; mhi <- m
    if (p$kind == "strat") { mlo$strategies[[p$idx]][[p$field]] <- p$lo; mhi$strategies[[p$idx]][[p$field]] <- p$hi }
    else { mlo$states[[p$idx]][[p$field]] <- p$lo; mhi$states[[p$idx]][[p$field]] <- p$hi }
    nL <- nmbPair(mlo); nH <- nmbPair(mhi)
    list(label = p$label, low = min(nL, nH), high = max(nL, nH), swing = abs(nH - nL), base = base)
  })
  if (length(out) > 0) out <- out[order(vapply(out, function(x) -x$swing, numeric(1)))]
  if (length(out) > 8) out <- out[1:8]
  out
}

# --- Budget impact ------------------------------------------------------------------
biaRun <- function(b) {
  horizon <- round(num(b$horizon))
  if (horizon < 1) input_error("Budget-impact horizon must be at least 1 year.")
  if (horizon > 50) input_error("Budget-impact horizon is limited to 50 years.")
  startYear <- num(b$startYear, as.numeric(format(Sys.Date(), "%Y")))
  pop <- num(b$population); elig <- num(b$eligible); maxU <- num(b$maxUptake)
  cNew <- num(b$costNew); cOld <- num(b$costOld)
  eligible <- pop * elig
  rows <- vector("list", horizon)
  cum <- 0
  for (i in seq_len(horizon)) {
    uptake <- maxU * i / horizon
    treated <- eligible * uptake
    worldNew <- treated * cNew + (eligible - treated) * cOld
    worldOld <- eligible * cOld
    impact <- worldNew - worldOld
    cum <- cum + impact
    rows[[i]] <- list(year = startYear + i - 1, uptake = uptake, treated = treated, worldOld = worldOld,
                      worldNew = worldNew, impact = impact, cum = cum)
  }
  rows
}
