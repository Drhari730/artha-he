psm_model <- function(lo = 0.10, lp = 0.30, hr = NULL) {
  base <- list(name = "Control", addCost = 1000,
               os = list(dist = "exponential", par = list(rate = lo), unit = "years"),
               pfs = list(dist = "exponential", par = list(rate = lp), unit = "years"))
  trt <- if (is.null(hr)) base else list(name = "New", addCost = 5000,
               os = list(ref = 0, hr = hr, lo = 0.5, hi = 0.95), pfs = list(ref = 0, hr = hr))
  trt$name <- "New"
  list(type = "psm", cycle = 1, horizon = 20, dCost = 0.03, dEff = 0.03, outcome = "QALY",
       states = list(list(name = "PF", cost = 2000, util = 0.8), list(name = "PD", cost = 6000, util = 0.6), list(name = "Dead", cost = 0, util = 0)),
       strategies = list(base, trt))
}

test_that("partitioned survival matches an independent calculation", {
  m <- psm_model(hr = 0.7)
  res <- modelRunAll(m)
  t <- 0:20; w <- c(0.5, rep(1, 19), 0.5); d <- w / 1.03^t
  for (k in 1:2) {
    h <- if (k == 1) 1 else 0.7
    os <- exp(-0.1 * t)^h; pf <- pmin(exp(-0.3 * t)^h, os); pd <- os - pf
    add <- if (k == 1) 1000 else 5000
    expect_equal(res[[k]]$cost, sum(d * (pf * (2000 + add) + pd * 6000)), tolerance = 1e-10)
    expect_equal(res[[k]]$qaly, sum(d * (pf * 0.8 + pd * 0.6)), tolerance = 1e-10)
    expect_equal(res[[k]]$ly, sum(d * os), tolerance = 1e-10)
  }
})

test_that("PSM curves in months and PFS crossing OS are handled", {
  m <- psm_model()
  m$strategies[[1]]$os <- list(dist = "exponential", par = list(rate = 0.1 / 12), unit = "months")
  expect_equal(modelRunAll(m)[[1]]$ly, modelRunAll(psm_model())[[1]]$ly, tolerance = 1e-12)
  m <- psm_model(lo = 0.3, lp = 0.1)
  r <- modelRunAll(m)[[1]]
  expect_true(isTRUE(r$crossing))
  expect_true(all(r$trace[, 2] >= -1e-12))
})

test_that("PSM input errors", {
  m <- psm_model(); m$strategies[[1]]$os <- NULL
  expect_error(prepareModel(m), "OS curve", class = "input_error")
  m <- psm_model(hr = 0.7); m$strategies[[2]]$os$ref <- 1
  expect_error(prepareModel(m), "reference", class = "input_error")
  m <- psm_model(); m$strategies[[1]]$pfs$par$rate <- -1
  expect_error(prepareModel(m), "greater than 0", class = "input_error")
})

tree_model <- function() {
  node <- function(id, parent, label, p, cost = 0, effect = 0) list(id = id, parent = parent, label = label, p = p, cost = cost, effect = effect)
  list(type = "tree", outcome = "QALY", strategies = list(
    list(name = "Watchful waiting", nodes = list(
      node("r", "", "Start", 1),
      node("a", "r", "Recover", 0.6, 0, 0.95),
      node("b", "r", "Complication", "#", 5000, 0.6))),
    list(name = "Surgery", nodes = list(
      node("r", "", "Start", 1, 20000),
      node("s", "r", "Success", 0.9, 0, 0.97),
      node("f", "r", "Failure", "#", 0),
      node("f1", "f", "Revision ok", 0.5, 15000, 0.85),
      node("f2", "f", "Disability", 0.5, 30000, 0.5)))))
}

test_that("decision tree rolls back expected cost and effect", {
  res <- modelRunAll(tree_model())
  expect_equal(res[[1]]$cost, 0.4 * 5000); expect_equal(res[[1]]$qaly, 0.6 * 0.95 + 0.4 * 0.6)
  expect_equal(res[[2]]$cost, 20000 + 0.1 * 0.5 * 15000 + 0.1 * 0.5 * 30000)
  expect_equal(res[[2]]$qaly, 0.9 * 0.97 + 0.05 * 0.85 + 0.05 * 0.5)
  expect_equal(sum(vapply(res[[2]]$paths, function(p) p$prob, 0)), 1)
})

test_that("decision tree validation", {
  m <- tree_model(); m$strategies[[1]]$nodes[[3]]$p <- 0.3
  expect_error(prepareModel(m), "add up to 0.9000", class = "input_error")
  m <- tree_model(); m$strategies[[1]]$nodes[[2]]$parent <- "zzz"
  expect_error(prepareModel(m), "parent", class = "input_error")
  m <- tree_model(); m$strategies[[2]]$nodes[[4]]$p <- "#"; m$strategies[[2]]$nodes[[5]]$p <- "#"
  expect_error(prepareModel(m), "only one branch", class = "input_error")
})

test_that("PSA and DSA run for PSM and tree models", {
  for (m in list(psm_model(hr = 0.7), tree_model())) {
    d <- psaModel(m, 30, 0, 1, 0.2, "global", NULL, seed = 1)
    expect_length(d, 30)
    expect_true(all(vapply(d, function(x) is.finite(x$incCost) && is.finite(x$incEff), logical(1))))
    t <- dsaModel(m, 50000, 0, 1)
    expect_gt(length(t), 0)
  }
  lab <- vapply(dsaModel(psm_model(hr = 0.7), 50000, 0, 1), function(x) x$label, "")
  expect_true(any(grepl("OS hazard ratio", lab)))
})

test_that("multi-strategy CEAC, CEAF and EVPI", {
  P <- psaAll(darth_model(), 200, 0.2, "global", NULL, seed = 3)
  ws <- c(0, 50000, 100000, 200000)
  cm <- ceac_multi(P, ws)
  for (i in seq_along(ws)) expect_equal(sum(vapply(cm$curves, function(c) c$points[[i]]$prob, 0)), 1)
  NB <- P$E * 100000 - P$C
  expect_equal(cm$frontier[[3]]$index + 1L, which.max(colMeans(NB)))
  expect_equal(cm$evpi[[3]]$evpi, mean(apply(NB, 1, max)) - max(colMeans(NB)))
  # with two strategies the CEAC equals the pairwise probability
  d <- psaModel(darth_model(strategies = c("SoC", "B")), 100, 0, 1, 0.2, "global", NULL, seed = 2)
  P2 <- psaAll(darth_model(strategies = c("SoC", "B")), 100, 0.2, "global", NULL, seed = 2)
  expect_equal(ceac_multi(P2, 100000)$curves[[2]]$points[[1]]$prob, ceac(d, 100000)[[1]]$prob)
})
