# =============================================================================
# engine_analysis.R : value of information and scenario analysis
#   - records the sampled value of every uncertain parameter in each PSA draw
#   - EVPPI by non-parametric regression (Strong, Oakley & Brennan, Med Decis
#     Making 2014): smoothing spline for one parameter, loess for 2-4
#   - population EVPI / EVPPI
#   - scenario analysis: run alternative versions of the model side by side
# =============================================================================

# Named numeric vectors of the parameters drawn in one PSA iteration.
psa_record <- function(m, mm) {
  rec <- list()
  put <- function(name, v) rec[[name]] <<- as.numeric(v)
  type <- model_type(m)
  if (type != "tree") {
    for (i in seq_along(m$states)) {
      nm <- m$states[[i]]$name
      if (num(m$states[[i]]$cost) > 0) put(paste0("Cost: ", nm), num(mm$states[[i]]$cost))
      u <- num(m$states[[i]]$util); if (u > 0 && u < 1) put(paste0("Utility: ", nm), num(mm$states[[i]]$util))
    }
    for (k in seq_along(m$strategies)) {
      s <- m$strategies[[k]]; sn <- s$name
      if (num(s$addCost) > 0) put(paste0("Treatment cost: ", sn), num(mm$strategies[[k]]$addCost))
      if (type == "markov") {
        if (has_effect(s)) { if (!is.null(mm$.eff) && !is.na(mm$.eff[k])) put(paste0("Treatment effect: ", sn), mm$.eff[k]) }
        else {
          P <- mm$strategies[[k]]$matrix
          for (r in seq_len(nrow(P))) {
            free <- which(s$matrix[r, ] > 0)
            if (length(free) >= 2) put(paste0("Transitions from ", m$states[[r]]$name, " (", sn, ")"), P[r, free[-length(free)]])
          }
        }
      } else {
        for (ep in c("os", "pfs")) {
          spec <- s[[ep]]
          if (!is.null(spec$ref)) { h <- mm$.hr[[paste0(k, ep)]]; if (!is.null(h)) put(paste0(toupper(ep), " hazard ratio: ", sn), h) }
          else if (!is.null(spec$vcov)) put(paste0(toupper(ep), " curve: ", sn), unlist(mm$strategies[[k]][[ep]]$par))
        }
      }
    }
  } else {
    for (k in seq_along(m$strategies)) {
      tp <- tree_parse(m$strategies[[k]])
      for (i in seq_along(tp$nodes)) {
        lab <- paste0(tp$name, ": ", tp$nodes[[i]]$label)
        if (i != tp$root && !is.null(mm$.p[[k]]) && !identical(as.character(tp$nodes[[i]]$p), "#")) put(paste0("P(", lab, ")"), mm$.p[[k]][i])
        if (!is.null(mm$.c[[k]]) && !is.na(mm$.c[[k]][i])) put(paste0("Cost: ", lab), mm$.c[[k]][i])
        if (!is.null(mm$.e[[k]]) && !is.na(mm$.e[[k]][i])) put(paste0("Outcome: ", lab), mm$.e[[k]][i])
      }
    }
  }
  rec
}

# Stack per-draw records into a list of N x d matrices (constant ones dropped).
stack_records <- function(recs) {
  nms <- unique(unlist(lapply(recs, names)))
  out <- list()
  for (nm in nms) {
    rows <- lapply(recs, function(r) r[[nm]])
    if (any(vapply(rows, is.null, logical(1)))) next
    X <- do.call(rbind, rows)
    if (all(apply(X, 2, stats::sd) < 1e-12)) next
    # The same sampled values under two names (e.g. a matrix row shared by two
    # strategies through common random numbers) are one parameter.
    dup <- Find(function(k) identical(dim(out[[k]]), dim(X)) && max(abs(out[[k]] - X)) < 1e-12, names(out))
    if (!is.null(dup)) {
      merged <- sub("\\)$", paste0(", ", sub("^.*\\(", "", sub("\\)$", "", nm)), ")"), dup)
      if (!grepl("\\)$", dup)) merged <- paste0(dup, " / ", nm)
      names(out)[names(out) == dup] <- merged
      next
    }
    out[[nm]] <- X
  }
  out
}

# EVPPI for parameter(s) X (N x d), given net benefits NB (N x K).
evppi_one <- function(NB, X) {
  X <- as.matrix(X); N <- nrow(NB); K <- ncol(NB)
  keep <- apply(X, 2, function(x) stats::sd(x) > 1e-12 && length(unique(x)) >= 5)
  X <- X[, keep, drop = FALSE]
  d <- ncol(X)
  if (d == 0 || d > 4 || N < 50) return(NA_real_)
  INB <- NB[, -1, drop = FALSE] - NB[, 1]
  fitted <- matrix(0, N, K - 1)
  for (k in seq_len(K - 1)) {
    y <- INB[, k]
    fitted[, k] <- tryCatch({
      if (d == 1) stats::predict(stats::smooth.spline(X[, 1], y), X[, 1])$y
      else {
        df <- data.frame(y = y, X); names(df) <- c("y", paste0("x", seq_len(d)))
        f <- stats::as.formula(paste("y ~", paste(paste0("x", seq_len(d)), collapse = " + ")))
        stats::fitted(stats::loess(f, data = df, span = 0.75, degree = if (d <= 2) 2 else 1, control = stats::loess.control(surface = "direct")))
      }
    }, error = function(e) rep(mean(y), N))
  }
  v <- mean(apply(cbind(0, fitted), 1, max)) - max(0, colMeans(fitted))
  max(0, v)
}

evppi_all <- function(NB, params, maxParams = 30) {
  if (!length(params)) return(list())
  if (length(params) > maxParams) {   # rank by a quick linear screen, keep the most influential
    INB <- NB[, ncol(NB)] - NB[, 1]
    sc <- vapply(params, function(X) { X <- as.matrix(X); max(abs(suppressWarnings(stats::cor(X, INB))), na.rm = TRUE) }, numeric(1))
    params <- params[order(-sc)][seq_len(maxParams)]
  }
  out <- lapply(names(params), function(nm) list(parameter = nm, evppi = evppi_one(NB, params[[nm]]), dims = ncol(as.matrix(params[[nm]]))))
  out <- Filter(function(x) !is.na(x$evppi), out)
  out[order(vapply(out, function(x) -x$evppi, numeric(1)))]
}

# Population value: per-patient value x discounted number of patients affected
# over the period the decision applies.
population_factor <- function(perYear, years, rate = 0.03) {
  perYear <- num(perYear); years <- round(num(years))
  if (perYear <= 0 || years <= 0) return(0)
  sum(perYear / (1 + num(rate))^(0:(years - 1)))
}

# ---- scenario analysis ------------------------------------------------------------
# scenarios: list(list(name=, model=)). Each is run in full; errors are reported
# per scenario. iRef / iCmp pick the pairwise comparison shown as the key ICER.
run_scenarios <- function(scenarios, wtp, iRef = 0, iCmp = 1) {
  if (!length(scenarios)) input_error("Add at least one scenario.")
  if (length(scenarios) > 40) input_error("At most 40 scenarios can be run at once.")
  lapply(scenarios, function(sc) {
    nm <- if (is.null(sc$name)) "Scenario" else as.character(sc$name)
    tryCatch({
      m <- sc$model
      res <- modelRunAll(m)
      isQ <- !identical(m$outcome, "DALY")
      eff <- vapply(res, function(r) if (isQ) r$qaly else -r$daly, numeric(1))
      cost <- vapply(res, function(r) r$cost, numeric(1))
      b <- best_index(cost, eff, num(wtp))
      r1 <- min(iRef + 1L, length(res)); r2 <- min(iCmp + 1L, length(res))
      dC <- cost[r2] - cost[r1]; dE <- eff[r2] - eff[r1]
      list(name = nm, ok = TRUE, best = res[[b]]$name,
           strategies = lapply(seq_along(res), function(k) list(name = res[[k]]$name, cost = cost[k], effect = if (isQ) eff[k] else -eff[k])),
           incCost = dC, incEff = dE, icer = if (abs(dE) > 1e-12) dC / dE else NA, inmb = dE * num(wtp) - dC,
           refName = res[[r1]]$name, cmpName = res[[r2]]$name)
    }, error = function(e) list(name = nm, ok = FALSE, error = conditionMessage(e)))
  })
}

# Standard HTA scenarios generated from a base model.
standard_scenarios <- function(m) {
  type <- model_type(m)
  sc <- list()
  add <- function(name, f) sc[[length(sc) + 1]] <<- list(name = name, model = f(m))
  if (type != "tree") {
    add("Discount rate 0% (costs and outcomes)", function(x) { x$dCost <- 0; x$dEff <- 0; x })
    add("Discount rate 5% (costs and outcomes)", function(x) { x$dCost <- 0.05; x$dEff <- 0.05; x })
    add("Outcomes not discounted (costs 3%)", function(x) { x$dCost <- 0.03; x$dEff <- 0; x })
    add("Time horizon halved", function(x) { x$horizon <- max(num(x$cycle), num(x$horizon) / 2); x })
    add(if (identical(m$correction, "simpson")) "Half-cycle correction" else "Simpson's 1/3 correction",
        function(x) { x$correction <- if (identical(m$correction, "simpson")) "half-cycle" else "simpson"; x })
    if (type == "markov" && length(m$states) && any(vapply(m$states, isAbs, logical(1))))
      add(if (isTRUE(as.logical(m$bgMortality))) "Without background mortality" else "With background mortality",
          function(x) { x$bgMortality <- !isTRUE(as.logical(m$bgMortality)); x })
  }
  sc
}
