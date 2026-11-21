test_that("Dirichlet sampling keeps structural zeros and absorbing rows", {
  set.seed(1)
  for (i in 1:200) {
    x <- rdirichlet(c(0, 0, 100))
    expect_identical(x, c(0, 0, 1))
    y <- rdirichlet(c(85, 0, 15))
    expect_equal(y[2], 0); expect_equal(sum(y), 1)
  }
})

test_that("per-parameter mode runs (was: subscript out of bounds)", {
  m <- darth_model(strategies = c("SoC", "B"))
  params <- list(states = list(list(cost = list(dist = "gamma", mean = 2000, se = 200))),
                 strategies = list(list(matrix = lapply(1:4, function(i) list(dist = "dirichlet", K = 200))),
                                   list(addCost = list(dist = "gamma", mean = 13000, se = 1300),
                                        matrix = lapply(1:4, function(i) list(dist = "dirichlet", K = 200)))))
  d <- psaModel(m, 20, 0, 1, 0.2, "per_parameter", params, seed = 7)
  expect_length(d, 20)
  expect_true(all(vapply(d, function(x) is.finite(x$incCost) && is.finite(x$incEff), logical(1))))
})

test_that("PSA is reproducible with a seed", {
  a <- psaModel(darth_model(), 15, 0, 1, 0.2, "global", NULL, seed = 42)
  b <- psaModel(darth_model(), 15, 0, 1, 0.2, "global", NULL, seed = 42)
  expect_identical(a, b)
})

test_that("identical arms give zero incremental results (common random numbers)", {
  m <- darth_model(strategies = c("SoC", "SoC")); m$strategies[[2]]$name <- "copy"
  d <- psaModel(m, 25, 0, 1, 0.3, "global", NULL, seed = 3)
  expect_true(all(abs(vapply(d, function(x) x$incCost, numeric(1))) < 1e-8))
  expect_true(all(abs(vapply(d, function(x) x$incEff, numeric(1))) < 1e-10))
})

test_that("PSA iteration limit is enforced", {
  expect_error(psaModel(darth_model(), 1e7, 0, 1, 0.2, "global", NULL), class = "input_error")
})

test_that("CEAC and EVPI", {
  d <- list(list(incCost = 100, incEff = 1), list(incCost = 100, incEff = 3))
  expect_equal(ceac(d, c(0, 50, 200))[[2]]$prob, 0.5)
  # NB at w=50: -50, 50 -> E[max(0,.)] = 25, max(0, mean)=0 -> EVPI 25
  expect_equal(evpi(d, 50), 25)
})

test_that("DSA returns sorted swings and uses CI-based ranges", {
  m <- darth_model(strategies = c("SoC", "SoC"))
  m$strategies[[2]] <- list(name = "Trt", addCost = 13000, addCostStates = list(1, 2),
                            effect = list(baseline = 0, measure = "hr", value = 0.6, lo = 0.4, hi = 0.9, transitions = list(list(1, 2))))
  params <- list(states = list(NULL, list(util = list(dist = "beta", mean = 0.75, se = 0.05))))
  t <- dsaModel(m, 100000, 0, 1, params)
  sw <- vapply(t, function(x) x$swing, numeric(1))
  expect_true(all(diff(sw) <= 0))
  lab <- vapply(t, function(x) x$label, character(1))
  eff <- t[[which(grepl("treatment effect", lab))]]
  expect_equal(c(eff$loVal, eff$hiVal), c(0.4, 0.9)); expect_equal(eff$source, "95% CI")
  u <- t[[which(lab == "Sick utility")]]
  expect_equal(u$source, "95% interval")
  b <- betaMS(0.75, 0.05); expect_equal(u$loVal, qbeta(0.025, b$a, b$b))
  expect_true(any(lab == "Discount rate, costs & effects"))
})

test_that("PSA samples the treatment effect and keeps arms correlated", {
  m <- darth_model(strategies = c("SoC", "SoC"))
  m$strategies[[2]] <- list(name = "Trt", addCost = 0,
                            effect = list(baseline = 0, measure = "hr", value = 1, transitions = list(list(1, 2))))
  # HR fixed at 1 (no CI): treatment is identical to baseline in every draw.
  d <- psaModel(m, 20, 0, 1, 0.3, "global", NULL, seed = 5)
  expect_true(all(abs(vapply(d, function(x) x$incEff, numeric(1))) < 1e-10))
  # With a CI, the effect varies and is centred near the point estimate.
  m$strategies[[2]]$effect <- list(baseline = 0, measure = "hr", value = 0.6, lo = 0.4, hi = 0.9, transitions = list(list(1, 2)))
  set.seed(9); f <- fixModel(m)
  hr <- replicate(400, { mm <- psaDraw(f, 0.2, "global", NULL); P0 <- mm$strategies[[1]]$matrix; P1 <- mm$strategies[[2]]$matrix; log(1 - P1[2, 3]) / log(1 - P0[2, 3]) })
  expect_gt(sd(hr), 0.05)
  expect_lt(abs(median(hr) - 0.6), 0.05)
})
