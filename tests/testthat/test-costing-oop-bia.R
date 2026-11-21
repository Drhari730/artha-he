test_that("empty inputs do not crash", {
  expect_equal(oopRun(list(items = list(), income = 100))$total, 0)
  expect_equal(oopRun(NULL)$total, 0)
  expect_equal(advancedCost(list(staff = list(), equip = list()))$total, 0)
  expect_equal(microCost(list(), 2024, 0.05)$total, 0)
})

test_that("micro-costing treats missing quantity as zero instead of failing", {
  r <- microCost(list(list(item = "a", category = "Direct medical", quantity = "", unit_cost = 100, year = 2024),
                      list(item = "b", category = "Direct medical", quantity = 3, unit_cost = 50, year = 2024)), 2024, 0)
  expect_equal(r$total, 150)
  r <- microCost(list(list(item = "a", category = "Direct medical", quantity = 2, unit_cost = 100, year = 2022)), 2024, 0.05)
  expect_equal(r$total, 200 * 1.05^2)
})

test_that("catastrophic expenditure counts direct medical payments only", {
  o <- oopRun(list(income = 100000, nonFood = 50000, items = list(
    list(category = "Direct medical", amount = 8000),
    list(category = "Direct non-medical", amount = 5000),
    list(category = "Indirect (productivity)", amount = 20000))))
  expect_equal(o$total, 33000); expect_equal(o$health, 8000)
  expect_equal(o$pctInc, 0.08); expect_false(o$che10)
  expect_equal(o$pctCTP, 0.16); expect_false(o$che40)
})

test_that("annualisation factor (equivalent annual cost)", {
  expect_equal(annualisationFactor(0.03, 10), 0.03 * 1.03^10 / (1.03^10 - 1))
  expect_equal(annualisationFactor(0, 5), 0.2)
})

test_that("budget impact", {
  r <- biaRun(list(population = 1000, eligible = 0.1, maxUptake = 0.5, horizon = 2, startYear = 2025, costNew = 300, costOld = 100))
  expect_equal(r[[1]]$impact, 100 * 0.25 * 200)
  expect_equal(r[[2]]$cum, 100 * 0.25 * 200 + 100 * 0.5 * 200)
  expect_error(biaRun(list(horizon = 0)), class = "input_error")
})
