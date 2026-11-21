st <- function(...) { a <- list(...); lapply(seq(1, length(a), 3), function(i) list(strategy = a[[i]], cost = a[[i+1]], effect = a[[i+2]])) }
get <- function(d, nm) Filter(function(x) x$strategy == nm, d)[[1]]

test_that("extended dominance removes the option and recomputes the ICER", {
  d <- icerIncremental(st("A", 0, 0, "B", 100, 0.5, "C", 150, 1.5))
  expect_equal(get(d, "B")$status, "extended")
  expect_true(is.na(get(d, "B")$icer))
  expect_equal(get(d, "C")$icer, 100)        # vs A, not vs B (was 50)
  expect_equal(get(d, "C")$incEff, 1.5)
})

test_that("extended dominance is iterated until ICERs increase", {
  # B then C are both extendedly dominated by the A-D line.
  d <- icerIncremental(st("A", 0, 0, "B", 100, 0.2, "C", 200, 0.5, "D", 300, 3))
  expect_equal(get(d, "B")$status, "extended")
  expect_equal(get(d, "C")$status, "extended")
  expect_equal(get(d, "D")$icer, 100)
})

test_that("strong dominance, ties and duplicates", {
  d <- icerIncremental(st("A", 40000, 3.5, "B", 50000, 3.2))
  expect_equal(get(d, "B")$status, "dominated")
  d <- icerIncremental(st("A", 100, 1, "B", 100, 2))     # same cost, less effect
  expect_equal(get(d, "A")$status, "dominated")
  d <- icerIncremental(st("A", 100, 1, "B", 100, 1))     # exact duplicate
  expect_equal(sum(vapply(d, function(x) x$status == "frontier", logical(1))), 1)
  d <- icerIncremental(st("A", 40000, 3.5, "B", 85000, 4.4))
  expect_equal(get(d, "B")$icer, 50000)
})

test_that("best option maximises net monetary benefit", {
  cost <- c(0, 100, 150); eff <- c(0, 0.5, 1.5)
  expect_equal(best_index(cost, eff, 50), 1)     # ICER C vs A = 100 > 50
  expect_equal(best_index(cost, eff, 120), 3)
})

test_that("DALY frontier treats lower DALYs as better", {
  arr <- list(list(name = "Old", cost = 100, eff = 10), list(name = "New", cost = 300, eff = 8))
  d <- modelIncremental(arr, lowerBetter = TRUE)
  nw <- Filter(function(x) x$name == "New", d)[[1]]
  expect_equal(nw$incEff, 2); expect_equal(nw$icer, 100)
})

test_that("evaluation requirements accept zero cost / effect for a do-nothing option", {
  r <- evalRequirements("CEA", st("Do nothing", 0, 0, "New", 100, 1))
  expect_equal(r$missing, 0)
})
