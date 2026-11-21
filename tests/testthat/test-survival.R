sim_ipd <- function(n = 300, shape = 1.4, scale = 20, cens = 40, seed = 1) {
  set.seed(seed); t <- rweibull(n, shape, scale); c <- runif(n, 0, cens)
  lapply(seq_len(n), function(i) list(time = min(t[i], c[i]), event = as.numeric(t[i] <= c[i])))
}
ipd_vec <- function(ipd) list(time = vapply(ipd, function(r) r$time, 0), event = vapply(ipd, function(r) r$event, 0))

test_that("MLE fits match survival::survreg", {
  skip_if_not_installed("survival")
  ipd <- sim_ipd(); v <- ipd_vec(ipd)
  r <- surv_fit_all(ipd)
  f <- setNames(r$fits, vapply(r$fits, function(x) x$dist, ""))
  sr <- function(d) survival::survreg(survival::Surv(v$time, v$event) ~ 1, dist = d)
  s <- sr("weibull");     expect_equal(f$weibull$par$shape, 1 / s$scale, tolerance = 1e-4); expect_equal(f$weibull$par$scale, unname(exp(coef(s))), tolerance = 1e-4); expect_equal(f$weibull$loglik, s$loglik[1], tolerance = 1e-6)
  s <- sr("lognormal");   expect_equal(f$lnorm$par$meanlog, unname(coef(s)), tolerance = 1e-4); expect_equal(f$lnorm$par$sdlog, s$scale, tolerance = 1e-4); expect_equal(f$lnorm$loglik, s$loglik[1], tolerance = 1e-6)
  s <- sr("loglogistic"); expect_equal(f$llogis$par$shape, 1 / s$scale, tolerance = 1e-4); expect_equal(f$llogis$loglik, s$loglik[1], tolerance = 1e-6)
  s <- sr("exponential"); expect_equal(f$exponential$par$rate, unname(exp(-coef(s))), tolerance = 1e-5); expect_equal(f$exponential$loglik, s$loglik[1], tolerance = 1e-6)
  expect_equal(r$best, "weibull")
})

test_that("Gompertz MLE is a maximum and recovers the truth", {
  set.seed(4); n <- 2000; a <- 0.15; b <- 0.05
  u <- runif(n); t <- log(1 - a * log(u) / b) / a; c <- runif(n, 0, 30)
  ipd <- lapply(seq_len(n), function(i) list(time = min(t[i], c[i]), event = as.numeric(t[i] <= c[i])))
  f <- surv_fit_one("gompertz", pmin(t, c), as.numeric(t <= c))
  expect_equal(f$par$shape, a, tolerance = 0.15); expect_equal(f$par$rate, b, tolerance = 0.15)
  nll <- function(th) { p <- theta_to_par("gompertz", th); -(sum(surv_logh("gompertz", p, pmin(t, c)[t <= c])) + sum(surv_logS("gompertz", p, pmin(t, c)))) }
  for (d in list(c(1e-3, 0), c(-1e-3, 0), c(0, 1e-3), c(0, -1e-3))) expect_gte(nll(f$theta + d), nll(f$theta))
})

test_that("Kaplan-Meier matches survival::survfit", {
  skip_if_not_installed("survival")
  v <- ipd_vec(sim_ipd()); km <- km_estimate(v$time, v$event)
  ref <- summary(survival::survfit(survival::Surv(v$time, v$event) ~ 1), times = km$time[-1])
  expect_equal(km$surv[-1], ref$surv, tolerance = 1e-12)
})

test_that("Guyot reconstruction reproduces the KM curve and the fit", {
  ipd <- sim_ipd(n = 400, seed = 7); v <- ipd_vec(ipd); km <- km_estimate(v$time, v$event)
  dig <- lapply(seq_len(nrow(km)), function(i) list(time = km$time[i], surv = km$surv[i]))
  rt <- seq(0, 35, by = 5)
  risk <- lapply(rt, function(x) list(time = x, n = sum(v$time >= x)))
  rec <- guyot_ipd(dig, risk)
  rv <- ipd_vec(rec); km2 <- km_estimate(rv$time, rv$event)
  grid <- seq(0, 35, by = 0.5)
  step <- function(k, x) k$surv[findInterval(x, k$time)]
  expect_lt(max(abs(step(km, grid) - step(km2, grid))), 0.02)
  expect_equal(length(rec), length(ipd), tolerance = 0.02)
  f1 <- surv_fit_one("weibull", v$time, v$event); f2 <- surv_fit_one("weibull", rv$time, rv$event)
  expect_equal(f2$par$shape, f1$par$shape, tolerance = 0.05); expect_equal(f2$par$scale, f1$par$scale, tolerance = 0.05)
})

test_that("bad survival input is rejected clearly", {
  expect_error(surv_fit_all(list(list(time = 1, event = 1))), "two events", class = "input_error")
  expect_error(surv_fit_all(list(list(time = -1, event = 1), list(time = 2, event = 1))), "negative", class = "input_error")
  expect_error(guyot_ipd(list(list(time = 0, surv = 1), list(time = 5, surv = 0.5)), list(list(time = 2, n = 10))), "time 0", class = "input_error")
})
