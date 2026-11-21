test_that("EVPPI matches analytic values (Strong et al. regression method)", {
  set.seed(1); N <- 4000; X <- rnorm(N); Y <- rnorm(N)
  NB <- cbind(0, X + Y)                        # INB = X + Y
  expect_equal(evppi_one(NB, X), 1 / sqrt(2 * pi), tolerance = 0.03)        # E[max(0, X)]
  expect_equal(evppi_one(NB, cbind(X, Y)), 1 / sqrt(pi), tolerance = 0.03)  # = EVPI
  Z <- rnorm(N)
  expect_lt(evppi_one(NB, Z), 0.03)                                         # irrelevant parameter
})

test_that("EVPPI is recorded for each model type and never exceeds EVPI", {
  for (m in list(darth_model(strategies = c("SoC", "B")), psm_model(hr = 0.7), tree_model())) {
    P <- psaAll(m, 400, 0.2, "global", NULL, seed = 2, record = TRUE)
    expect_gt(length(P$params), 0)
    NB <- P$E * 100000 - P$C
    evpi <- mean(apply(NB, 1, max)) - max(colMeans(NB))
    ev <- evppi_all(NB, P$params)
    expect_true(all(vapply(ev, function(e) e$evppi <= evpi * 1.05 + 1, logical(1))))
  }
  P <- psaAll(darth_model(), 50, 0.2, "global", NULL, seed = 3, record = TRUE)
  expect_true(any(grepl("Standard of care, Strategy A", names(P$params))))   # shared rows merged
})

test_that("population value factor", {
  expect_equal(population_factor(1000, 1), 1000)
  expect_equal(population_factor(1000, 3, 0.03), 1000 * (1 + 1 / 1.03 + 1 / 1.03^2))
  expect_equal(population_factor(0, 5), 0)
})

test_that("CPI-based price conversion", {
  cpi <- list(list(year = 2020, index = 100), list(year = 2024, index = 125))
  r <- microCost(list(list(item = "a", category = "Direct medical", quantity = 1, unit_cost = 400, year = 2020)), 2024, 0.05, cpi)
  expect_equal(r$total, 500); expect_equal(r$cpiUsed, 1)
  r <- microCost(list(list(item = "a", category = "Direct medical", quantity = 1, unit_cost = 400, year = 2019)), 2024, 0.05, cpi)
  expect_equal(r$total, 400 * 1.05^5); expect_equal(r$cpiUsed, 0)          # year not in index -> rate
})

test_that("scenarios run side by side and report errors per scenario", {
  base <- darth_model(strategies = c("SoC", "B"))
  sc <- c(list(list(name = "Base case", model = base)), standard_scenarios(base))
  bad <- base; bad$horizon <- -1
  sc[[length(sc) + 1]] <- list(name = "Broken", model = bad)
  r <- run_scenarios(sc, 100000, 0, 1)
  expect_equal(r[[1]]$icer, darth_published$icer[["B"]], tolerance = 1, scale = 1)
  nm <- vapply(r, function(x) x$name, "")
  d0 <- r[[which(nm == "Discount rate 0% (costs and outcomes)")]]
  expect_true(d0$ok); expect_false(isTRUE(all.equal(d0$icer, r[[1]]$icer)))
  expect_false(r[[length(r)]]$ok); expect_match(r[[length(r)]]$error, "horizon")
  expect_true(length(standard_scenarios(tree_model())) == 0)
})
