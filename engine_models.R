# =============================================================================
# engine_models.R : partitioned survival and decision-tree models, plus the
# dispatch layer that lets Markov, PSM and tree models share incremental
# analysis, PSA (including multi-strategy CEAC / CEAF / EVPI) and DSA.
# Requires engine.R and engine_survival.R.
# =============================================================================

model_type <- function(m) { t <- if (is.null(m$type)) "markov" else as.character(m$type); if (!(t %in% c("markov", "psm", "tree"))) input_error("Model type must be markov, psm or tree."); t }

check_time <- function(m) {
  cyc <- num(m$cycle); hor <- num(m$horizon)
  if (cyc <= 0) input_error("Cycle length must be greater than zero.")
  if (hor <= 0) input_error("Time horizon must be greater than zero.")
  if (round(hor / cyc) < 1) input_error("The time horizon must be at least one cycle long.")
  if (round(hor / cyc) > LIMITS$cycles) input_error(paste0("At most ", LIMITS$cycles, " cycles are supported (horizon / cycle length)."))
  corr <- if (is.null(m$correction)) "half-cycle" else as.character(m$correction)
  if (!(corr %in% c("half-cycle", "simpson"))) input_error("Within-cycle correction must be 'half-cycle' or 'simpson'.")
  corr
}

# ---- partitioned survival -------------------------------------------------------
# Three states: progression-free (PF), progressed disease (PD), dead.
# PF = S_PFS(t), alive = S_OS(t), PD = OS - PF, dead = 1 - OS (PFS is capped at OS).
fixPSM <- function(m) {
  if (length(m$states) != 3) input_error("A partitioned survival model has exactly three states: progression-free, progressed, dead.")
  m$states[[3]]$absorbing <- TRUE
  m$states[[1]]$absorbing <- FALSE; m$states[[2]]$absorbing <- FALSE
  if (length(m$strategies) < 1) input_error("The model needs at least one strategy.")
  if (length(m$strategies) > LIMITS$strategies) input_error(paste0("At most ", LIMITS$strategies, " strategies are supported."))
  m$correction <- check_time(m)
  nS <- length(m$strategies)
  for (k in seq_len(nS)) {
    s <- m$strategies[[k]]
    nm <- if (is.null(s$name)) paste("Strategy", k) else s$name
    for (ep in c("os", "pfs")) {
      if (is.null(s[[ep]])) input_error(paste0("'", nm, "': add the ", toupper(ep), " curve."))
      check_curve(s[[ep]], paste0("'", nm, "' ", toupper(ep)))
      if (!is.null(s[[ep]]$ref)) {
        r <- as.integer(num(s[[ep]]$ref, NA)) + 1L
        if (is.na(r) || r < 1 || r > nS || r == k) input_error(paste0("'", nm, "' ", toupper(ep), ": choose another strategy as the reference for the hazard ratio."))
      }
    }
    if (is.null(s$addCostStates)) m$strategies[[k]]$addCostStates <- list(0)   # on treatment until progression
  }
  # resolve once to surface reference loops early
  for (k in seq_len(nS)) for (ep in c("os", "pfs")) curve_S(m, k, ep, 0)
  m
}

psmTrace <- function(m, k, hrOverride = NULL) {
  cyc <- num(m$cycle); nC <- round(num(m$horizon) / cyc)
  t <- (0:nC) * cyc
  os <- pmin(pmax(curve_S(m, k, "os", t, hrOverride), 0), 1)
  pfs <- pmin(pmax(curve_S(m, k, "pfs", t, hrOverride), 0), 1)
  pf <- pmin(pfs, os)
  cbind(pf, os - pf, 1 - os)
}

# ---- shared reward calculation (trace -> costs, QALYs, LYs, DALYs) ----------------
trace_rewards <- function(m, strat, trace) {
  n <- length(m$states); nC <- nrow(trace) - 1
  cyc <- num(m$cycle)
  rw <- strategy_rewards(m, strat)
  alive <- rw$alive; absIdx <- which(!alive)
  w <- wcc_weights(nC, if (is.null(m$correction)) "half-cycle" else m$correction)
  tt <- (0:nC) * cyc
  dc <- w * cyc / (1 + num(m$dCost))^tt
  de <- w * cyc / (1 + num(m$dEff))^tt
  wu <- w * cyc
  occC <- colSums(trace * dc); occE <- colSums(trace * de); occU <- colSums(trace * wu)
  costByState <- occC * rw$cost
  qalyByState <- occE * rw$util
  YLD <- sum(occE * rw$dw)
  deaths <- if (length(absIdx)) rowSums(trace[2:(nC + 1), absIdx, drop = FALSE] - trace[1:nC, absIdx, drop = FALSE]) else rep(0, nC)
  YLL <- sum(deaths * num(m$lifeExp) / (1 + num(m$dEff))^((0:(nC - 1)) * cyc))
  list(cost = sum(costByState), qaly = sum(qalyByState), ly = sum(occE[alive]),
       daly = YLD + YLL, yll = YLL, yld = YLD,
       undisc = list(cost = sum(occU * rw$cost), qaly = sum(occU * rw$util), ly = sum(occU[alive])),
       costByState = costByState, qalyByState = qalyByState, trace = trace, nC = nC)
}

psmRun <- function(m, k, hrOverride = NULL) {
  r <- trace_rewards(m, m$strategies[[k]], psmTrace(m, k, hrOverride))
  s <- m$strategies[[k]]
  cyc <- num(m$cycle); nC <- round(num(m$horizon) / cyc); t <- (0:nC) * cyc
  r$crossing <- any(curve_S(m, k, "pfs", t, hrOverride) > curve_S(m, k, "os", t, hrOverride) + 1e-9)
  r
}

# ---- decision tree ----------------------------------------------------------------
# strategy$nodes = list(list(id=, parent=, label=, p=, cost=, effect=)), one root
# (parent empty). p is a probability, or "#" for "the rest" (1 - siblings).
# Costs and effects accumulate along each path; expected values are the
# path-probability-weighted sums over the terminal (leaf) nodes.
tree_parse <- function(s) {
  nodes <- s$nodes
  nm <- if (is.null(s$name)) "strategy" else s$name
  if (length(nodes) < 2) input_error(paste0("'", nm, "': the tree needs a root and at least one branch."))
  if (length(nodes) > 500) input_error("At most 500 nodes per tree are supported.")
  id <- vapply(nodes, function(x) as.character(if (is.null(x$id)) "" else x$id), character(1))
  par <- vapply(nodes, function(x) { p <- x$parent; if (is.null(p) || length(p) == 0 || is.na(p[[1]])) "" else as.character(p[[1]]) }, character(1))
  if (any(id == "") || any(duplicated(id))) input_error(paste0("'", nm, "': every node needs a unique id."))
  roots <- which(par == "")
  if (length(roots) != 1) input_error(paste0("'", nm, "': the tree must have exactly one root (a node with no parent)."))
  if (any(!(par[-roots] %in% id))) input_error(paste0("'", nm, "': a node's parent does not exist."))
  list(nodes = nodes, id = id, par = par, root = roots, name = nm)
}

tree_probs <- function(tp, pOverride = NULL) {
  n <- length(tp$nodes); p <- rep(NA_real_, n); hash <- rep(FALSE, n)
  for (i in seq_len(n)) {
    if (i == tp$root) { p[i] <- 1; next }
    raw <- tp$nodes[[i]]$p
    if (!is.null(pOverride) && !is.na(pOverride[i])) p[i] <- pOverride[i]
    else if (identical(as.character(raw), "#")) hash[i] <- TRUE
    else p[i] <- num(raw, NA)
  }
  for (pid in unique(tp$par[-tp$root])) {
    kids <- which(tp$par == pid)
    lab <- tp$nodes[[which(tp$id == pid)]]$label
    if (sum(hash[kids]) > 1) input_error(sprintf("'%s': only one branch under '%s' can be '#' (the rest).", tp$name, lab))
    if (any(is.na(p[kids] [!hash[kids]]))) input_error(sprintf("'%s': every branch under '%s' needs a probability (or '#').", tp$name, lab))
    known <- sum(p[kids][!hash[kids]])
    if (any(p[kids][!hash[kids]] < 0 | p[kids][!hash[kids]] > 1)) input_error(sprintf("'%s': probabilities under '%s' must be between 0 and 1.", tp$name, lab))
    if (any(hash[kids])) {
      rest <- 1 - known
      if (rest < -1e-9) input_error(sprintf("'%s': probabilities under '%s' add up to more than 1.", tp$name, lab))
      p[kids[hash[kids]]] <- max(0, rest)
    } else if (abs(known - 1) > 1e-6) input_error(sprintf("'%s': probabilities under '%s' add up to %.4f; they must add up to 1 (or use '#' for the rest).", tp$name, lab, known))
  }
  p
}

treeRun <- function(m, k, pOverride = NULL, cOverride = NULL, eOverride = NULL) {
  tp <- tree_parse(m$strategies[[k]])
  p <- tree_probs(tp, pOverride)
  n <- length(tp$nodes)
  cst <- vapply(seq_len(n), function(i) if (!is.null(cOverride) && !is.na(cOverride[i])) cOverride[i] else num(tp$nodes[[i]]$cost), numeric(1))
  eff <- vapply(seq_len(n), function(i) if (!is.null(eOverride) && !is.na(eOverride[i])) eOverride[i] else num(tp$nodes[[i]]$effect), numeric(1))
  pathP <- rep(NA_real_, n); accC <- rep(NA_real_, n); accE <- rep(NA_real_, n); depth <- rep(0L, n)
  visit <- function(i, pp, cc, ee, d) {
    if (d > 60) input_error(paste0("'", tp$name, "': the tree is too deep or has a loop."))
    pathP[i] <<- pp * p[i]; accC[i] <<- cc + cst[i]; accE[i] <<- ee + eff[i]; depth[i] <<- d
    for (j in which(tp$par == tp$id[i])) visit(j, pathP[i], accC[i], accE[i], d + 1L)
  }
  visit(tp$root, 1, 0, 0, 0L)
  if (any(is.na(pathP))) input_error(paste0("'", tp$name, "': some nodes are not connected to the root."))
  leaf <- !(tp$id %in% tp$par)
  out <- list(cost = sum(pathP[leaf] * accC[leaf]), effect = sum(pathP[leaf] * accE[leaf]))
  out$paths <- lapply(which(leaf), function(i) {
    lab <- character(0); j <- i
    while (j != tp$root) { lab <- c(tp$nodes[[j]]$label, lab); j <- which(tp$id == tp$par[j]) }
    list(path = paste(lab, collapse = " → "), prob = pathP[i], cost = accC[i], effect = accE[i])
  })
  out
}

fixTree <- function(m) {
  if (length(m$strategies) < 1) input_error("The model needs at least one strategy.")
  if (length(m$strategies) > LIMITS$strategies) input_error(paste0("At most ", LIMITS$strategies, " strategies are supported."))
  for (k in seq_along(m$strategies)) treeRun(m, k)
  m
}

# ---- dispatch -------------------------------------------------------------------
prepareModel <- function(m) {
  switch(model_type(m), markov = fixModel(m), psm = fixPSM(m), tree = fixTree(m))
}

# Run strategy k of a prepared model. Returns cost / qaly / ly / daly (+ details).
runStrategy <- function(m, k) {
  switch(model_type(m),
    markov = markovGeneric(m, m$strategies[[k]]),
    psm = psmRun(m, k, m$.hr),
    tree = {
      r <- treeRun(m, k, m$.p[[k]], m$.c[[k]], m$.e[[k]])
      isD <- identical(m$outcome, "DALY")
      list(cost = r$cost, qaly = if (isD) 0 else r$effect, daly = if (isD) r$effect else 0, ly = NA_real_,
           effect = r$effect, paths = r$paths, undisc = list(cost = r$cost, qaly = if (isD) 0 else r$effect, ly = NA_real_))
    })
}

# Effect on the "higher is better" scale used for net benefit.
nb_effect <- function(m, r) if (identical(m$outcome, "DALY")) -r$daly else r$qaly

# ---- PSA draws for PSM and tree --------------------------------------------------
psaDrawPSM <- function(m, cv) {
  uSE <- min(0.08, 0.4 * cv)
  mm <- m
  for (i in 1:3) {
    cst <- num(m$states[[i]]$cost); utl <- num(m$states[[i]]$util)
    if (cst > 0) { g <- gammaMS(cst, cst * cv); mm$states[[i]]$cost <- stats::rgamma(1, shape = g$shape, scale = g$scale) }
    if (utl > 0 && utl < 1) { b <- betaMS(utl, uSE); mm$states[[i]]$util <- stats::rbeta(1, b$a, b$b) }
  }
  hr <- list()
  for (k in seq_along(m$strategies)) {
    s <- m$strategies[[k]]
    ac <- num(s$addCost)
    if (ac > 0) { g <- gammaMS(ac, ac * cv); mm$strategies[[k]]$addCost <- stats::rgamma(1, shape = g$shape, scale = g$scale) }
    for (ep in c("os", "pfs")) {
      spec <- s[[ep]]
      if (!is.null(spec$ref)) {
        se <- effect_se_log(spec)
        hr[[paste0(k, ep)]] <- if (is.na(se)) num(spec$hr) else exp(stats::rnorm(1, log(num(spec$hr)), se))
      } else mm$strategies[[k]][[ep]]$par <- sample_curve_par(spec)
    }
  }
  mm$.hr <- hr
  mm
}

psaDrawTree <- function(m, cv) {
  uSE <- min(0.08, 0.4 * cv)
  mm <- m
  mm$.p <- list(); mm$.c <- list(); mm$.e <- list()
  for (k in seq_along(m$strategies)) {
    tp <- tree_parse(m$strategies[[k]])
    n <- length(tp$nodes)
    pd <- rep(NA_real_, n); cd <- rep(NA_real_, n); ed <- rep(NA_real_, n)
    for (pid in unique(tp$par[-tp$root])) {
      kids <- which(tp$par == pid)
      raw <- lapply(kids, function(i) tp$nodes[[i]]$p)
      isHash <- vapply(raw, function(x) identical(as.character(x), "#"), logical(1))
      v <- vapply(seq_along(kids), function(j) if (isHash[j]) NA_real_ else num(raw[[j]], NA), numeric(1))
      for (j in which(!isHash & v > 0 & v < 1)) {
        se <- min(cv * v[j], 0.9 * sqrt(v[j] * (1 - v[j])))
        b <- betaMS(v[j], se); v[j] <- stats::rbeta(1, b$a, b$b)
      }
      if (any(isHash)) {
        tot <- sum(v[!isHash])
        if (tot > 1) v[!isHash] <- v[!isHash] / tot   # keep the rest non-negative
        v[isHash] <- max(0, 1 - sum(v[!isHash]))
      } else v <- v / sum(v)
      pd[kids] <- v
    }
    for (i in seq_len(n)) {
      cst <- num(tp$nodes[[i]]$cost); ef <- num(tp$nodes[[i]]$effect)
      if (cst > 0) { g <- gammaMS(cst, cst * cv); cd[i] <- stats::rgamma(1, shape = g$shape, scale = g$scale) }
      if (ef > 0 && ef < 1) { b <- betaMS(ef, uSE); ed[i] <- stats::rbeta(1, b$a, b$b) }
      else if (ef >= 1) { g <- gammaMS(ef, ef * cv); ed[i] <- stats::rgamma(1, shape = g$shape, scale = g$scale) }
    }
    pd[tp$root] <- 1
    mm$.p[[k]] <- pd; mm$.c[[k]] <- cd; mm$.e[[k]] <- ed
  }
  mm
}

psaDrawAny <- function(m, cv, mode, params) {
  switch(model_type(m), markov = psaDraw(m, cv, mode, params), psm = psaDrawPSM(m, cv), tree = psaDrawTree(m, cv))
}

# PSA over all (or selected) strategies: returns N x K matrices of cost and of
# effect on the net-benefit scale.
psaAll <- function(m, N, cv, mode, params, seed = NULL, which = NULL) {
  cv <- if (!is.null(cv) && cv > 0) cv else 0.2
  N <- as.integer(N)
  if (is.na(N) || N < 1) input_error("PSA needs at least one iteration.")
  if (N > LIMITS$psaN) input_error(paste0("At most ", LIMITS$psaN, " PSA iterations are supported."))
  if (!is.null(seed) && !is.na(seed)) set.seed(as.integer(seed))
  m <- prepareModel(m)
  K <- length(m$strategies); idx <- if (is.null(which)) seq_len(K) else which
  C <- matrix(NA_real_, N, length(idx)); E <- matrix(NA_real_, N, length(idx))
  for (it in seq_len(N)) {
    mm <- psaDrawAny(m, cv, mode, params)
    for (j in seq_along(idx)) { r <- runStrategy(mm, idx[j]); C[it, j] <- r$cost; E[it, j] <- nb_effect(m, r) }
  }
  list(C = C, E = E, idx = idx, names = vapply(m$strategies[idx], function(s) as.character(s$name), character(1)))
}

# Multi-strategy CEAC (probability each strategy has the highest net benefit),
# cost-effectiveness acceptability frontier (strategy with the highest expected
# net benefit) and per-patient EVPI.
ceac_multi <- function(P, ws) {
  K <- ncol(P$C)
  curves <- lapply(seq_len(K), function(k) list(name = P$names[k], points = list()))
  frontier <- vector("list", length(ws)); evpis <- numeric(length(ws))
  for (i in seq_along(ws)) {
    NB <- P$E * ws[i] - P$C
    win <- max.col(NB, ties.method = "first")
    pr <- tabulate(win, nbins = K) / nrow(NB)
    for (k in seq_len(K)) curves[[k]]$points[[i]] <- list(wtp = ws[i], prob = pr[k])
    eNB <- colMeans(NB); b <- which.max(eNB)
    frontier[[i]] <- list(wtp = ws[i], strategy = P$names[b], index = b - 1L, prob = pr[b])
    evpis[i] <- mean(apply(NB, 1, max)) - max(eNB)
  }
  list(curves = curves, frontier = frontier, evpi = lapply(seq_along(ws), function(i) list(wtp = ws[i], evpi = max(0, evpis[i]))))
}
