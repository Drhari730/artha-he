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

# Price index lookup: cpi = list(list(year=, index=), ...). Returns NULL when absent.
cpi_table <- function(cpi) {
  if (is.null(cpi) || length(cpi) == 0) return(NULL)
  y <- vapply(cpi, function(r) num(r$year, NA), numeric(1)); v <- vapply(cpi, function(r) num(r$index, NA), numeric(1))
  ok <- !is.na(y) & !is.na(v) & v > 0
  if (!any(ok)) return(NULL)
  stats::setNames(v[ok], as.character(y[ok]))
}
# Convert a cost from price year fy to ty: by the CPI ratio when both years are
# in the index, otherwise by the constant annual rate.
price_convert <- function(c, fy, ty, r, cpi = NULL) {
  if (!is.null(cpi) && !is.na(fy) && !is.na(ty)) {
    a <- cpi[as.character(fy)]; b <- cpi[as.character(ty)]
    if (!is.na(a) && !is.na(b)) return(c * b / a)
  }
  inflate_cost(c, fy, ty, r)
}

microCost <- function(rows, ty, infl, cpi = NULL) {
  if (is.null(rows) || length(rows) == 0) return(list(lines = list(), total = 0, byCat = list(), cpiUsed = 0))
  cpi <- cpi_table(cpi)
  used <- 0
  lines <- lapply(rows, function(r) {
    fy <- num(r$year, NA)
    viaCpi <- !is.null(cpi) && !is.na(fy) && !is.na(ty) && !is.na(cpi[as.character(fy)]) && !is.na(cpi[as.character(ty)])
    if (viaCpi && fy != ty) used <<- used + 1
    r$priceMethod <- if (viaCpi) "CPI" else "rate"
    u <- price_convert(num(r$unit_cost), fy, ty, num(infl), cpi)
    r$unit_cost_adj <- u
    r$line_cost <- num(r$quantity) * u
    r
  })
  total <- sumv(lines, function(l) l$line_cost)
  list(lines = lines, total = total, byCat = byCategory(lines, function(l) l$line_cost, total), cpiUsed = used)
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

# A strategy either has its own transition matrix, or derives one from a
# baseline strategy by applying a treatment effect to chosen transitions:
#   effect = list(baseline = <0-based strategy index>, measure = "hr"|"rr"|"or",
#                 value = <point estimate>, lo = <95% CI low>, hi = <95% CI high>,
#                 transitions = list(c(from, to), ...))   # 0-based state indices
# The probability removed from (or added to) a transition goes to / comes from
# staying in the same state (the diagonal).
has_effect <- function(s) !is.null(s$effect) && !is.null(s$effect$transitions) && length(s$effect$transitions) > 0

apply_effect_p <- function(p, measure, v) {
  switch(measure,
    hr = 1 - (1 - p)^v,
    rr = min(1, p * v),
    or = { if (p >= 1) 1 else { o <- p / (1 - p) * v; o / (1 + o) } },
    input_error(paste0("Unknown effect measure '", measure, "' (use hr, rr or or).")))
}

effect_matrix <- function(Pbase, eff, value, sname, stateNames) {
  P <- Pbase
  measure <- tolower(if (is.null(eff$measure)) "hr" else eff$measure)
  n <- nrow(P)
  for (tr in eff$transitions) {
    i <- as.integer(num(tr[[1]], NA)) + 1L; j <- as.integer(num(tr[[2]], NA)) + 1L
    if (is.na(i) || is.na(j) || i < 1 || j < 1 || i > n || j > n || i == j)
      input_error(paste0("'", sname, "': each treatment-effect transition must be between two different states."))
    p0 <- P[i, j]
    p1 <- apply_effect_p(p0, measure, value)
    P[i, j] <- p1
    P[i, i] <- P[i, i] - (p1 - p0)
    if (P[i, i] < -1e-12)
      input_error(sprintf("'%s': the treatment effect makes the probability of staying in '%s' negative. Use a smaller effect or a different measure.", sname, stateNames[i]))
    P[i, i] <- max(0, P[i, i])
  }
  P
}

# Fill each derived strategy's matrix from its (current) baseline matrix.
resolve_effects <- function(m, values = NULL) {
  nms <- vapply(m$states, function(s) as.character(if (is.null(s$name)) "" else s$name), character(1))
  for (k in seq_along(m$strategies)) {
    s <- m$strategies[[k]]
    if (!has_effect(s)) next
    b <- as.integer(num(s$effect$baseline, 0)) + 1L
    v <- if (!is.null(values) && !is.na(values[k])) values[k] else num(s$effect$value, NA)
    m$strategies[[k]]$matrix <- effect_matrix(m$strategies[[b]]$matrix, s$effect, v, s$name, nms)
  }
  m
}

as_matrix <- function(x, n) {
  if (is.matrix(x) && nrow(x) == n && ncol(x) == n) return(x)
  M <- matrix(0, nrow = n, ncol = n)
  for (i in 1:n) for (j in 1:n) {
    has <- !is.null(x) && length(x) >= i && !is.null(x[[i]]) && length(x[[i]]) >= j && !is.null(x[[i]][[j]])
    M[i, j] <- if (has) num(x[[i]][[j]], NA) else (if (i == j) 1 else 0)
  }
  M
}

# Build n x n numeric matrices, resolve treatment effects, validate. Idempotent.
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
  corr <- if (is.null(m$correction)) "half-cycle" else as.character(m$correction)
  if (!(corr %in% c("half-cycle", "simpson"))) input_error("Within-cycle correction must be 'half-cycle' or 'simpson'.")
  m$correction <- corr

  nS <- length(m$strategies)
  for (k in seq_len(nS)) {
    s <- m$strategies[[k]]
    nm <- if (is.null(s$name)) paste("Strategy", k) else s$name
    if (has_effect(s)) {
      b <- as.integer(num(s$effect$baseline, NA)) + 1L
      if (is.na(b) || b < 1 || b > nS || b == k) input_error(paste0("'", nm, "': choose a different strategy as the baseline for its treatment effect."))
      if (has_effect(m$strategies[[b]])) input_error(paste0("'", nm, "': the baseline strategy must have its own transition matrix, not another treatment effect."))
      v <- num(s$effect$value, NA)
      if (is.na(v) || v < 0) input_error(paste0("'", nm, "': the treatment effect must be a number of 0 or more."))
      lo <- num(s$effect$lo, NA); hi <- num(s$effect$hi, NA)
      if (!is.na(lo) && !is.na(hi) && !(lo > 0 && lo <= v && v <= hi)) input_error(paste0("'", nm, "': the 95% CI must satisfy 0 < low <= estimate <= high."))
      next
    }
    M <- as_matrix(s$matrix, n)
    if (any(is.na(M))) input_error(paste0("Transition matrix for '", nm, "' has a blank or non-numeric cell."))
    if (any(M < 0) || any(M > 1)) input_error(paste0("Transition probabilities for '", nm, "' must be between 0 and 1."))
    rs <- rowSums(M)
    for (i in 1:n) if (abs(rs[i] - 1) > ROW_TOL)
      input_error(sprintf("In '%s', the row for '%s' sums to %.4f; each row must sum to 1.", nm, m$states[[i]]$name, rs[i]))
    m$strategies[[k]]$matrix <- M / rs
  }
  m <- resolve_effects(m)

  # Optional per-strategy reward settings.
  for (k in seq_len(nS)) {
    s <- m$strategies[[k]]
    if (!is.null(s$addCostStates)) {
      idx <- vapply(s$addCostStates, function(x) as.integer(num(x, NA)), integer(1))
      if (any(is.na(idx)) || any(idx < 0) || any(idx >= n)) input_error(paste0("'", s$name, "': treatment-cost states are not valid."))
    }
    if (!is.null(s$utilOverride) && length(s$utilOverride) > 0) {
      u <- vapply(seq_len(n), function(i) if (length(s$utilOverride) >= i) num(s$utilOverride[[i]], NA) else NA_real_, numeric(1))
      if (any(!is.na(u) & (u < -1 | u > 1))) input_error(paste0("'", s$name, "': utility overrides must be between -1 and 1."))
    }
  }
  m
}

# Background mortality: a life table of annual death probabilities by age.
# The built-in table is crude and indicative only; supply m$lifeTable =
# list(list(age=, q=), ...) from an official source (e.g. SRS abridged life
# tables for India, or WHO GHO life tables) for formal analyses.
BG_MORT_DEFAULT <- matrix(c(0,0.028, 1,0.003, 5,0.001, 15,0.0015, 25,0.002, 35,0.003, 45,0.006, 55,0.013, 65,0.030, 75,0.070, 85,0.15), ncol = 2, byrow = TRUE)
lifeTableOf <- function(m) {
  lt <- m$lifeTable
  if (is.null(lt) || length(lt) == 0) return(BG_MORT_DEFAULT)
  M <- t(vapply(lt, function(r) c(num(r$age, NA), num(r$q, NA)), numeric(2)))
  if (any(is.na(M))) input_error("Every life-table row needs a numeric age and annual death probability (q).")
  if (any(M[, 2] < 0 | M[, 2] > 1)) input_error("Life-table death probabilities (q) must be between 0 and 1.")
  M[order(M[, 1]), , drop = FALSE]
}
bgMortAnnual <- function(age, table = BG_MORT_DEFAULT) {
  idx <- max(which(table[, 1] <= age), 1)
  table[idx, 2]
}

# Within-cycle correction weights for trace rows t = 0..n (sum to n).
# half-cycle: 1/2, 1, ..., 1, 1/2.  simpson: composite Simpson 1/3, with the
# 3/8 rule over the last three cycles when n is odd (as in DARTH v0.3.0+).
wcc_weights <- function(n, method = "half-cycle") {
  if (identical(method, "simpson") && n >= 2) {
    s13 <- function(k) { w <- ifelse((0:k) %% 2 == 1, 4/3, 2/3); w[c(1, k + 1)] <- 1/3; w }
    if (n %% 2 == 0) return(s13(n))
    w <- numeric(n + 1)
    if (n >= 5) w[1:(n - 2)] <- s13(n - 3)
    w[(n - 2):(n + 1)] <- w[(n - 2):(n + 1)] + c(3/8, 9/8, 9/8, 3/8)
    return(w)
  }
  c(0.5, rep(1, n - 1), 0.5)
}

# Per-state cost and utility vectors for one strategy.
strategy_rewards <- function(m, strat) {
  n <- length(m$states)
  alive <- !vapply(m$states, isAbs, logical(1))
  cst <- vapply(m$states, function(s) num(s$cost), numeric(1))
  utl <- vapply(m$states, function(s) num(s$util), numeric(1))
  addC <- num(strat$addCost)
  onStates <- if (!is.null(strat$addCostStates)) (seq_len(n) - 1L) %in% vapply(strat$addCostStates, function(x) as.integer(num(x)), integer(1)) else alive
  cvec <- cst + ifelse(onStates, addC, 0)
  if (!is.null(strat$utilOverride) && length(strat$utilOverride) > 0) {
    for (i in seq_len(min(n, length(strat$utilOverride)))) {
      u <- num(strat$utilOverride[[i]], NA)
      if (!is.na(u)) utl[i] <- u
    }
  }
  list(cost = cvec, util = utl, dw = vapply(m$states, function(s) num(s$dw), numeric(1)), alive = alive)
}

# Cohort state-transition model for one strategy.
# Conventions (same as DARTH): the cohort starts in state 1; trace row t
# (t = 0..n, time t*cycle) is weighted by the within-cycle correction and
# discounted at (1+r)^(t*cycle). Per-cycle rewards are multiplied by the cycle
# length. Background mortality, when on, is applied to survivors after the
# disease transitions (the matrix death column is treated as disease-specific).
markovGeneric <- function(m, strat) {
  n <- length(m$states)
  P <- strat$matrix
  cyc <- num(m$cycle)
  nC <- round(num(m$horizon) / cyc)
  rw <- strategy_rewards(m, strat)
  alive <- rw$alive
  absIdx <- which(!alive)
  useBg <- isTRUE(as.logical(m$bgMortality)) && length(absIdx) > 0
  lt <- if (useBg) lifeTableOf(m) else NULL
  startAge <- if (num(m$startAge) > 0) num(m$startAge) else 30

  trace <- matrix(0, nrow = nC + 1, ncol = n)
  trace[1, 1] <- 1
  for (t in 1:nC) {
    nx <- as.numeric(trace[t, ] %*% P)
    if (useBg) {
      pCycle <- 1 - (1 - bgMortAnnual(startAge + (t - 1) * cyc, lt))^cyc
      mv <- nx[alive] * pCycle
      nx[alive] <- nx[alive] - mv
      nx[absIdx[1]] <- nx[absIdx[1]] + sum(mv)
    }
    trace[t + 1, ] <- nx
  }

  trace_rewards(m, strat, trace)
}

modelRunAll <- function(m) {
  m <- prepareModel(m)
  lapply(seq_along(m$strategies), function(k) {
    s <- m$strategies[[k]]
    r <- runStrategy(m, k)
    out <- list(name = s$name, cost = r$cost, qaly = r$qaly, ly = r$ly, daly = r$daly, undisc = r$undisc)
    if (!is.null(r$costByState)) { out$costByState <- as.list(r$costByState); out$qalyByState <- as.list(r$qalyByState) }
    if (!is.null(r$trace)) out$trace <- r$trace
    if (!is.null(r$effect)) out$effect <- r$effect
    if (!is.null(r$paths)) out$paths <- r$paths
    if (isTRUE(r$crossing)) out$crossing <- TRUE
    out
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

# Log-scale SE of a ratio from its 95% CI (NA when no CI is given).
effect_se_log <- function(eff) {
  lo <- num(eff$lo, NA); hi <- num(eff$hi, NA)
  if (is.na(lo) || is.na(hi) || lo <= 0 || hi <= lo) return(NA_real_)
  (log(hi) - log(lo)) / (2 * stats::qnorm(0.975))
}

# One PSA draw of the whole model. Own matrices use Dirichlet rows with common
# random numbers (the same uniforms for every strategy). Strategies defined by a
# treatment effect are rebuilt from the *sampled* baseline matrix with a
# lognormal draw of the effect (from its 95% CI), which keeps the arms
# correlated the way the evidence implies.
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

  effVals <- rep(NA_real_, length(m$strategies))
  for (k in seq_along(m$strategies)) {
    s <- m$strategies[[k]]
    pStr <- if (perParam && length(params$strategies) >= k) params$strategies[[k]] else NULL
    ac <- num(s$addCost)
    if (ac > 0) {
      if (perParam) { if (!is.null(pStr$addCost)) mm$strategies[[k]]$addCost <- sampleParam(pStr$addCost, ac, "cost") }
      else { g <- gammaMS(ac, ac * cv); mm$strategies[[k]]$addCost <- stats::rgamma(1, shape = g$shape, scale = g$scale) }
    }
    if (has_effect(s)) {
      se <- effect_se_log(s$effect)
      effVals[k] <- if (is.na(se)) num(s$effect$value) else exp(stats::rnorm(1, log(num(s$effect$value)), se))
      next
    }
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
  # A large sampled effect can make a row invalid; cap it rather than fail the run.
  out <- tryCatch(resolve_effects(mm, effVals), input_error = function(e) {
    for (k in which(!is.na(effVals))) effVals[k] <- num(m$strategies[[k]]$effect$value)
    resolve_effects(mm, effVals)
  })
  out$.eff <- effVals
  out
}

psaModel <- function(m, N, iRef, iCmp, cv, mode, params, seed = NULL) {
  P <- psaAll(m, N, cv, mode, params, seed, which = c(iRef + 1L, iCmp + 1L))
  lapply(seq_len(nrow(P$C)), function(i) list(incCost = P$C[i, 2] - P$C[i, 1], incEff = P$E[i, 2] - P$E[i, 1]))
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
# Ranges: the 2.5th-97.5th percentiles of the per-parameter distributions when
# they are set; the 95% CI of each treatment effect; discount rates 0%-5%;
# otherwise an assumed +/-30% (costs) or +/-0.1 (utilities), flagged as assumed.
param_range <- function(p, base, kind) {
  if (!is.null(p) && !is.null(p$dist) && num(p$se) > 0) {
    mu <- num(p$mean, base); se <- num(p$se)
    if (p$dist == "gamma" && mu > 0) { g <- gammaMS(mu, se); return(list(lo = stats::qgamma(0.025, shape = g$shape, scale = g$scale), hi = stats::qgamma(0.975, shape = g$shape, scale = g$scale), src = "95% interval")) }
    if (p$dist == "beta") { b <- betaMS(mu, se); return(list(lo = stats::qbeta(0.025, b$a, b$b), hi = stats::qbeta(0.975, b$a, b$b), src = "95% interval")) }
    if (p$dist == "normal") { lo <- mu - 1.96 * se; hi <- mu + 1.96 * se
      if (kind == "util") { lo <- max(0, lo); hi <- min(1, hi) } else lo <- max(0, lo)
      return(list(lo = lo, hi = hi, src = "95% interval")) }
  }
  if (kind == "cost") list(lo = base * 0.7, hi = base * 1.3, src = "assumed ±30%")
  else list(lo = max(0, base - 0.1), hi = min(1, base + 0.1), src = "assumed ±0.1")
}

dsaModel <- function(m, w, iRef, iCmp, params = NULL) {
  m <- prepareModel(m)
  type <- model_type(m)
  nmbPair <- function(mm) {
    if (type == "markov") mm <- resolve_effects(mm)
    r <- runStrategy(mm, iRef + 1L); cc <- runStrategy(mm, iCmp + 1L)
    (nb_effect(mm, cc) - nb_effect(mm, r)) * w - (cc$cost - r$cost)
  }
  base <- nmbPair(m)
  specs <- list()
  add <- function(label, set, rg) specs[[length(specs) + 1]] <<- list(label = label, set = set, lo = rg$lo, hi = rg$hi, src = rg$src)

  if (type != "tree") {
    for (si in seq_along(m$strategies)) {
      s <- m$strategies[[si]]
      ac <- num(s$addCost)
      pStr <- if (!is.null(params$strategies) && length(params$strategies) >= si) params$strategies[[si]] else NULL
      if (ac > 0) local({ k <- si; add(paste0(s$name, " added cost"), function(mm, v) { mm$strategies[[k]]$addCost <- v; mm }, param_range(pStr$addCost, ac, "cost")) })
      if (type == "markov" && has_effect(s)) {
        lo <- num(s$effect$lo, NA); hi <- num(s$effect$hi, NA)
        if (!is.na(lo) && !is.na(hi)) local({ k <- si
          add(paste0(s$name, " treatment effect (", toupper(if (is.null(s$effect$measure)) "hr" else s$effect$measure), ")"),
              function(mm, v) { mm$strategies[[k]]$effect$value <- v; mm }, list(lo = lo, hi = hi, src = "95% CI")) })
      }
      if (type == "psm") for (ep in c("os", "pfs")) {
        spec <- s[[ep]]
        if (!is.null(spec$ref) && !is.na(num(spec$lo, NA)) && !is.na(num(spec$hi, NA))) local({ k <- si; e <- ep
          add(paste0(s$name, " ", toupper(ep), " hazard ratio"), function(mm, v) { mm$strategies[[k]][[e]]$hr <- v; mm },
              list(lo = num(spec$lo), hi = num(spec$hi), src = "95% CI")) })
      }
    }
    for (si in seq_along(m$states)) {
      st <- m$states[[si]]
      cst <- num(st$cost); utl <- num(st$util)
      pSt <- if (!is.null(params$states) && length(params$states) >= si) params$states[[si]] else NULL
      if (cst > 0) local({ k <- si; add(paste0(st$name, " cost"), function(mm, v) { mm$states[[k]]$cost <- v; mm }, param_range(pSt$cost, cst, "cost")) })
      if (utl > 0) local({ k <- si; add(paste0(st$name, " utility"), function(mm, v) { mm$states[[k]]$util <- v; mm }, param_range(pSt$util, utl, "util")) })
    }
    add("Discount rate, costs & effects", function(mm, v) { mm$dCost <- v; mm$dEff <- v; mm }, list(lo = 0, hi = 0.05, src = "scenario range"))
  } else {
    for (si in seq_along(m$strategies)) {
      tp <- tree_parse(m$strategies[[si]])
      for (i in seq_along(tp$nodes)) {
        nd <- tp$nodes[[i]]
        if (i != tp$root) {
          sib <- which(tp$par == tp$par[i])
          hasHash <- any(vapply(sib, function(j) identical(as.character(tp$nodes[[j]]$p), "#"), logical(1)))
          pv <- num(nd$p, NA)
          if (hasHash && !is.na(pv) && pv > 0 && pv < 1) local({ k <- si; ii <- i
            add(paste0(tp$name, ": P(", nd$label, ")"), function(mm, v) { mm$strategies[[k]]$nodes[[ii]]$p <- v; mm },
                list(lo = max(0, pv * 0.8), hi = min(1, pv * 1.2), src = "assumed \u00B120%")) })
        }
        cv0 <- num(nd$cost)
        if (cv0 > 0) local({ k <- si; ii <- i
          add(paste0(tp$name, ": cost of ", nd$label), function(mm, v) { mm$strategies[[k]]$nodes[[ii]]$cost <- v; mm }, param_range(NULL, cv0, "cost")) })
      }
    }
  }

  out <- lapply(specs, function(p) {
    nL <- tryCatch(nmbPair(p$set(m, p$lo)), input_error = function(e) NA_real_)
    nH <- tryCatch(nmbPair(p$set(m, p$hi)), input_error = function(e) NA_real_)
    if (is.na(nL) || is.na(nH)) return(NULL)
    list(label = p$label, low = min(nL, nH), high = max(nL, nH), swing = abs(nH - nL), base = base,
         loVal = p$lo, hiVal = p$hi, source = p$src, nmbAtLo = nL, nmbAtHi = nH)
  })
  out <- Filter(Negate(is.null), out)
  if (length(out) > 0) out <- out[order(vapply(out, function(x) -x$swing, numeric(1)))]
  if (length(out) > 10) out <- out[1:10]
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

# --- Further model types --------------------------------------------------------------
# Survival analysis, partitioned survival and decision trees live in sibling
# files, sourced from the same directory as this file.
.engine_dir <- (function() {
  for (i in rev(seq_len(sys.nframe()))) { f <- sys.frame(i)$ofile; if (!is.null(f)) return(dirname(f)) }
  "."
})()
for (.f in c("engine_survival.R", "engine_models.R", "engine_analysis.R")) source(file.path(.engine_dir, .f), local = environment())
rm(.f)
