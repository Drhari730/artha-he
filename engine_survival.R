# =============================================================================
# engine_survival.R : parametric survival analysis (base R only)
#   - Kaplan-Meier from individual patient data (IPD)
#   - Guyot et al. (2012) reconstruction of IPD from a digitised KM curve
#   - maximum-likelihood fits with right censoring: exponential, Weibull,
#     Gompertz, log-logistic, log-normal (parametrised as in flexsurv)
#   - survival / hazard functions, restricted means, parameter sampling for PSA
# =============================================================================

SURV_DISTS <- c("exponential", "weibull", "gompertz", "llogis", "lnorm")
SURV_LABELS <- c(exponential = "Exponential", weibull = "Weibull", gompertz = "Gompertz",
                 llogis = "Log-logistic", lnorm = "Log-normal")
# Parameter names on the natural scale (flexsurv conventions) and whether each
# is estimated on the log scale (positive) or as-is.
SURV_PARS <- list(
  exponential = list(names = "rate", log = TRUE),
  weibull     = list(names = c("shape", "scale"), log = c(TRUE, TRUE)),
  gompertz    = list(names = c("shape", "rate"), log = c(FALSE, TRUE)),
  llogis      = list(names = c("shape", "scale"), log = c(TRUE, TRUE)),
  lnorm       = list(names = c("meanlog", "sdlog"), log = c(FALSE, TRUE)))

TIME_UNITS <- c(days = 365.25, weeks = 365.25 / 7, months = 12, years = 1)  # per year

# ---- survival and hazard ------------------------------------------------------
surv_S <- function(dist, p, t) {
  t <- pmax(t, 0)
  switch(dist,
    exponential = exp(-p[["rate"]] * t),
    weibull = exp(-(t / p[["scale"]])^p[["shape"]]),
    gompertz = { a <- p[["shape"]]; b <- p[["rate"]]
      if (abs(a) < 1e-10) exp(-b * t) else exp(-(b / a) * (exp(a * t) - 1)) },
    llogis = 1 / (1 + (t / p[["scale"]])^p[["shape"]]),
    lnorm = ifelse(t <= 0, 1, stats::pnorm((log(pmax(t, 1e-300)) - p[["meanlog"]]) / p[["sdlog"]], lower.tail = FALSE)),
    input_error(paste0("Unknown survival distribution '", dist, "'.")))
}

surv_logh <- function(dist, p, t) {
  t <- pmax(t, 1e-10)
  switch(dist,
    exponential = rep(log(p[["rate"]]), length(t)),
    weibull = { a <- p[["shape"]]; b <- p[["scale"]]; log(a) - log(b) + (a - 1) * (log(t) - log(b)) },
    gompertz = log(p[["rate"]]) + p[["shape"]] * t,
    llogis = { a <- p[["shape"]]; b <- p[["scale"]]; z <- (t / b)^a
      log(a) - log(b) + (a - 1) * (log(t) - log(b)) - log1p(z) },
    lnorm = { z <- (log(t) - p[["meanlog"]]) / p[["sdlog"]]
      stats::dnorm(z, log = TRUE) - log(t) - log(p[["sdlog"]]) - stats::pnorm(z, lower.tail = FALSE, log.p = TRUE) })
}

surv_logS <- function(dist, p, t) {
  if (dist == "lnorm") {
    z <- (log(pmax(t, 1e-300)) - p[["meanlog"]]) / p[["sdlog"]]
    return(ifelse(t <= 0, 0, stats::pnorm(z, lower.tail = FALSE, log.p = TRUE)))
  }
  log(pmax(surv_S(dist, p, t), 1e-300))
}

theta_to_par <- function(dist, theta) {
  sp <- SURV_PARS[[dist]]
  v <- ifelse(sp$log, exp(theta), theta)
  stats::setNames(as.numeric(v), sp$names)
}
par_to_theta <- function(dist, p) {
  sp <- SURV_PARS[[dist]]
  as.numeric(ifelse(sp$log, log(unlist(p[sp$names])), unlist(p[sp$names])))
}

# ---- Kaplan-Meier --------------------------------------------------------------
km_estimate <- function(time, event) {
  o <- order(time, -event); time <- time[o]; event <- event[o]
  ut <- unique(time[event == 1])
  if (!length(ut)) return(data.frame(time = 0, surv = 1, n.risk = length(time), n.event = 0))
  n.risk <- vapply(ut, function(u) sum(time >= u), numeric(1))
  n.event <- vapply(ut, function(u) sum(time == u & event == 1), numeric(1))
  surv <- cumprod(1 - n.event / n.risk)
  data.frame(time = c(0, ut), surv = c(1, surv), n.risk = c(length(time), n.risk), n.event = c(0, n.event))
}

# ---- maximum likelihood ----------------------------------------------------------
surv_start <- function(dist, time, event) {
  lam <- max(sum(event), 0.5) / max(sum(time), 1e-8)
  switch(dist,
    exponential = log(lam),
    weibull = c(0, log(1 / lam)),
    gompertz = c(0.001, log(lam)),
    llogis = c(0, log(1 / lam)),
    lnorm = c(log(1 / lam), 0))
}

surv_fit_one <- function(dist, time, event) {
  nll <- function(theta) {
    p <- theta_to_par(dist, theta)
    v <- -(sum(surv_logh(dist, p, time[event == 1])) + sum(surv_logS(dist, p, time)))
    if (!is.finite(v)) 1e100 else v
  }
  th0 <- surv_start(dist, time, event)
  fit <- if (length(th0) == 1) {
    o <- stats::optimize(nll, interval = th0 + c(-15, 15))
    list(par = o$minimum, value = o$objective, convergence = 0)
  } else {
    o <- stats::optim(th0, nll, method = "Nelder-Mead", control = list(maxit = 5000, reltol = 1e-12))
    o2 <- stats::optim(o$par, nll, method = "BFGS", control = list(maxit = 1000, reltol = 1e-14))
    if (o2$value <= o$value) o2 else o
  }
  H <- tryCatch(stats::optimHess(fit$par, nll), error = function(e) NULL)
  vc <- if (!is.null(H)) tryCatch(solve(H), error = function(e) NULL) else NULL
  if (!is.null(vc) && any(!is.finite(vc) | diag(as.matrix(vc)) <= 0)) vc <- NULL
  k <- length(th0); n <- length(time); ll <- -fit$value
  list(dist = dist, label = SURV_LABELS[[dist]], par = as.list(theta_to_par(dist, fit$par)),
       theta = as.numeric(fit$par), vcov = if (is.null(vc)) NULL else unname(as.matrix(vc)),
       loglik = ll, aic = 2 * k - 2 * ll, bic = k * log(n) - 2 * ll, converged = fit$convergence == 0)
}

# Restricted mean survival up to T (numerical integration of S).
surv_rmst <- function(dist, p, T) {
  if (T <= 0) return(0)
  stats::integrate(function(t) surv_S(dist, p, t), 0, T, subdivisions = 500L, rel.tol = 1e-8)$value
}
surv_median <- function(dist, p) {
  f <- function(t) surv_S(dist, p, t) - 0.5
  hi <- 1
  while (f(hi) > 0 && hi < 1e6) hi <- hi * 2
  if (f(hi) > 0) return(NA_real_)
  stats::uniroot(f, c(0, hi), tol = 1e-9)$root
}

# Validate IPD and fit all distributions.
clean_ipd <- function(ipd) {
  if (is.null(ipd) || length(ipd) == 0) input_error("Add the patient-level data (time, event) for this arm.")
  time <- vapply(ipd, function(r) num(r$time, NA), numeric(1))
  event <- vapply(ipd, function(r) num(r$event, NA), numeric(1))
  if (any(is.na(time)) || any(is.na(event))) input_error("Every row needs a numeric time and an event indicator (1 = event, 0 = censored).")
  if (any(time < 0)) input_error("Times cannot be negative.")
  if (!all(event %in% c(0, 1))) input_error("The event column must be 1 (event) or 0 (censored).")
  if (sum(event) < 2) input_error("At least two events are needed to fit a survival curve.")
  if (length(time) > 20000) input_error("At most 20,000 patients per arm are supported.")
  list(time = pmax(time, 1e-6), event = event)
}

surv_fit_all <- function(ipd, dists = SURV_DISTS, horizon = NULL) {
  d <- clean_ipd(ipd)
  fits <- lapply(dists, function(ds) tryCatch(surv_fit_one(ds, d$time, d$event), error = function(e) NULL))
  fits <- Filter(Negate(is.null), fits)
  if (!length(fits)) input_error("None of the survival distributions could be fitted to these data.")
  T <- if (is.null(horizon) || num(horizon) <= 0) max(d$time) * 3 else num(horizon)
  fits <- lapply(fits, function(f) {
    f$median <- surv_median(f$dist, unlist(f$par))
    f$rmst <- surv_rmst(f$dist, unlist(f$par), T)
    f
  })
  best <- which.min(vapply(fits, function(f) f$aic, numeric(1)))
  list(km = km_estimate(d$time, d$event), fits = fits, best = fits[[best]]$dist, horizon = T,
       n = length(d$time), events = sum(d$event), maxTime = max(d$time))
}

# ---- Guyot reconstruction ---------------------------------------------------------
# digitised: list(list(time=, surv=)) read off a published KM plot (surv on 0-1);
# atRisk: list(list(time=, n=)) from the numbers-at-risk table under the plot;
# totalEvents: optional. Returns IPD as list(list(time, event)).
guyot_ipd <- function(digitised, atRisk, totalEvents = NULL) {
  tS <- vapply(digitised, function(r) num(r$time, NA), numeric(1))
  S <- vapply(digitised, function(r) num(r$surv, NA), numeric(1))
  if (any(is.na(tS)) || any(is.na(S))) input_error("Every digitised point needs a time and a survival value.")
  if (any(S > 1.5)) S <- S / 100     # entered as percentages
  o <- order(tS); tS <- tS[o]; S <- S[o]
  keep <- !duplicated(tS, fromLast = TRUE); tS <- tS[keep]; S <- S[keep]
  if (tS[1] > 0) { tS <- c(0, tS); S <- c(1, S) } else S[1] <- 1
  S <- cummin(pmin(pmax(S, 0), 1))
  tR <- vapply(atRisk, function(r) num(r$time, NA), numeric(1))
  nR <- vapply(atRisk, function(r) num(r$n, NA), numeric(1))
  if (!length(tR) || any(is.na(tR)) || any(is.na(nR))) input_error("Add the numbers-at-risk table (time, number at risk).")
  o <- order(tR); tR <- tR[o]; nR <- round(nR[o])
  if (tR[1] != 0) input_error("The numbers-at-risk table must start at time 0.")
  tR <- tR[tR <= max(tS)]; nR <- nR[seq_along(tR)]
  n.int <- length(nR)
  lower <- vapply(tR, function(x) which(tS >= x)[1], integer(1))
  upper <- c(lower[-1] - 1L, length(tS))
  if (any(upper < lower)) input_error("Each interval of the at-risk table needs at least one digitised point; add more points.")
  n.t <- upper[n.int]
  n.censor <- rep(0, n.int); n.hat <- rep(nR[1] + 1, n.t + 1); cen <- rep(0, n.t); d <- rep(0, n.t)
  KM.hat <- rep(1, n.t); last.i <- rep(1L, n.int)
  bin <- function(x, br) { if (length(br) < 2) return(numeric(0)); as.numeric(table(cut(x, br, right = TRUE, include.lowest = TRUE))) }
  step <- function(i, from, to, last) {
    for (k in from:to) {
      if (i == 1 && k == from) { d[k] <<- 0; KM.hat[k] <<- 1 }
      else {
        d[k] <<- if (KM.hat[last] != 0) round(n.hat[k] * (1 - S[k] / KM.hat[last])) else 0
        d[k] <<- max(0, min(d[k], n.hat[k]))
        KM.hat[k] <<- KM.hat[last] * (1 - if (n.hat[k] > 0) d[k] / n.hat[k] else 0)
      }
      n.hat[k + 1] <<- n.hat[k] - d[k] - cen[k]
      if (n.hat[k + 1] < 0) { n.hat[k + 1] <<- 0; cen[k] <<- n.hat[k] - d[k] }
      if (d[k] != 0) last <- k
    }
    last
  }
  if (n.int > 1) {
    for (i in 1:(n.int - 1)) {
      n.censor[i] <- round(nR[i] * S[lower[i + 1]] / S[lower[i]] - nR[i + 1])
      iter <- 0
      repeat {
        if (n.censor[i] <= 0) { cen[lower[i]:upper[i]] <- 0; n.censor[i] <- 0 }
        else {
          ct <- tS[lower[i]] + (1:n.censor[i]) * (tS[lower[i + 1]] - tS[lower[i]]) / (n.censor[i] + 1)
          cen[lower[i]:upper[i]] <- bin(ct, tS[lower[i]:lower[i + 1]])
        }
        n.hat[lower[i]] <- nR[i]
        last <- step(i, lower[i], upper[i], last.i[i])
        diff <- n.hat[lower[i + 1]] - nR[i + 1]
        iter <- iter + 1
        if (!((diff > 0) || (diff < 0 && n.censor[i] > 0)) || iter > 200) break
        n.censor[i] <- n.censor[i] + diff
      }
      if (n.hat[lower[i + 1]] < nR[i + 1]) nR[i + 1] <- n.hat[lower[i + 1]]
      last.i[i + 1] <- last
    }
  }
  # last interval: censor at the average rate seen so far
  n.censor[n.int] <- if (n.int > 1) min(round(sum(n.censor[1:(n.int - 1)]) * (tS[upper[n.int]] - tS[lower[n.int]]) /
                                              max(tS[upper[n.int - 1]] - tS[lower[1]], 1e-12)), nR[n.int]) else 0
  if (upper[n.int] > lower[n.int]) {
    if (n.censor[n.int] <= 0) cen[lower[n.int]:(upper[n.int] - 1)] <- 0
    else {
      ct <- tS[lower[n.int]] + (1:n.censor[n.int]) * (tS[upper[n.int]] - tS[lower[n.int]]) / (n.censor[n.int] + 1)
      cen[lower[n.int]:(upper[n.int] - 1)] <- bin(ct, tS[lower[n.int]:upper[n.int]])
    }
  }
  n.hat[lower[n.int]] <- nR[n.int]
  invisible(step(n.int, lower[n.int], upper[n.int], last.i[n.int]))
  # optional: match the reported total number of events
  if (!is.null(totalEvents) && !is.na(num(totalEvents, NA)) && sum(d) > num(totalEvents)) {
    excess <- sum(d) - num(totalEvents)
    for (k in rev(which(d > 0))) { take <- min(d[k], excess); d[k] <- d[k] - take; cen[k] <- cen[k] + take; excess <- excess - take; if (excess <= 0) break }
  }
  ev <- rep(tS[1:n.t], d[1:n.t]); ce <- rep(tS[1:n.t], cen[1:n.t])
  rest <- max(0, n.hat[n.t + 1])
  out <- c(lapply(ev, function(t) list(time = t, event = 1)), lapply(ce, function(t) list(time = t, event = 0)),
           lapply(rep(tS[n.t], rest), function(t) list(time = t, event = 0)))
  if (sum(vapply(out, function(r) r$event, numeric(1))) < 2) input_error("Too few events could be reconstructed from this curve; check the digitised points and the at-risk table.")
  out
}

# ---- curve specification used by the partitioned-survival model ------------------
# spec = list(dist=, par=list(...), unit="months", vcov=, theta=) for a fitted /
# published curve, or list(ref=<0-based strategy index>, endpoint="os"|"pfs",
# hr=, lo=, hi=) for a proportional-hazards adjustment of another strategy's curve.
curve_unit_factor <- function(spec) {
  u <- if (is.null(spec$unit)) "years" else as.character(spec$unit)
  if (!(u %in% names(TIME_UNITS))) input_error(paste0("Unknown time unit '", u, "'."))
  TIME_UNITS[[u]]
}

check_curve <- function(spec, what) {
  if (!is.null(spec$ref)) {
    hr <- num(spec$hr, NA)
    if (is.na(hr) || hr <= 0) input_error(paste0(what, ": the hazard ratio must be greater than 0."))
    lo <- num(spec$lo, NA); hi <- num(spec$hi, NA)
    if (!is.na(lo) && !is.na(hi) && !(lo > 0 && lo <= hr && hr <= hi)) input_error(paste0(what, ": the 95% CI must satisfy 0 < low <= HR <= high."))
    return(invisible(TRUE))
  }
  dist <- as.character(if (is.null(spec$dist)) "" else spec$dist)
  if (!(dist %in% SURV_DISTS)) input_error(paste0(what, ": choose a survival curve (fit one in the Survival tab, or enter a distribution and its parameters)."))
  nm <- SURV_PARS[[dist]]$names
  v <- vapply(nm, function(k) num(spec$par[[k]], NA), numeric(1))
  if (any(is.na(v))) input_error(paste0(what, ": enter every parameter of the ", SURV_LABELS[[dist]], " curve (", paste(nm, collapse = ", "), ")."))
  pos <- SURV_PARS[[dist]]$log
  if (any(v[pos] <= 0)) input_error(paste0(what, ": ", paste(nm[pos], collapse = " and "), " must be greater than 0."))
  curve_unit_factor(spec)
  invisible(TRUE)
}

# Survival at model times t (years) for strategy k's endpoint, following
# proportional-hazards references (S_trt = S_ref ^ HR).
curve_S <- function(m, k, endpoint, tYears, hrOverride = NULL, depth = 0) {
  if (depth > 5) input_error("Hazard-ratio references form a loop; point each one at a strategy with its own curve.")
  s <- m$strategies[[k]]
  spec <- s[[endpoint]]
  if (!is.null(spec$ref)) {
    r <- as.integer(num(spec$ref)) + 1L
    hr <- if (!is.null(hrOverride) && !is.null(hrOverride[[paste0(k, endpoint)]])) hrOverride[[paste0(k, endpoint)]] else num(spec$hr)
    ep <- if (is.null(spec$endpoint)) endpoint else spec$endpoint
    return(curve_S(m, r, ep, tYears, hrOverride, depth + 1)^hr)
  }
  surv_S(spec$dist, unlist(spec$par), tYears * curve_unit_factor(spec))
}

# Sample curve parameters (multivariate normal on the estimation scale).
sample_curve_par <- function(spec) {
  if (is.null(spec$vcov) || is.null(spec$theta)) return(spec$par)
  V <- matrix(unlist(spec$vcov), nrow = length(spec$theta), byrow = TRUE)
  th <- as.numeric(unlist(spec$theta))
  L <- tryCatch(chol(V), error = function(e) NULL)
  if (is.null(L)) return(spec$par)
  z <- stats::rnorm(length(th))
  as.list(theta_to_par(spec$dist, th + as.numeric(t(L) %*% z)))
}
