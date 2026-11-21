test_that("two-state model matches the closed-form series (half-cycle)", {
  r <- modelRunAll(two_state(d = 0))[[1]]
  q0 <- (1 - 0.9^11) / 0.1 - 0.5 * (1 + 0.9^10)
  expect_equal(r$qaly, q0, tolerance = 1e-12)
  expect_equal(r$cost, 1000 * q0, tolerance = 1e-9)
  expect_equal(r$ly, q0, tolerance = 1e-12)
  rr <- 0.9 / 1.03
  r3 <- modelRunAll(two_state(d = 0.03))[[1]]
  expect_equal(r3$qaly, (1 - rr^11) / (1 - rr) - 0.5 * (1 + rr^10), tolerance = 1e-12)
  expect_equal(r3$undisc$qaly, q0, tolerance = 1e-12)
})

test_that("Simpson weights sum to n and integrate cubics exactly", {
  for (n in 2:12) {
    w <- wcc_weights(n, "simpson")
    expect_equal(sum(w), n, tolerance = 1e-12)
    expect_equal(sum(w * (0:n)^3), n^4 / 4, tolerance = 1e-9)
  }
  expect_equal(wcc_weights(4, "simpson"), c(1, 4, 2, 4, 1) / 3)
})

test_that("reproduces the published DARTH Sick-Sicker base case", {
  res <- modelRunAll(darth_model())
  names(res) <- c("SoC", "A", "B", "AB")
  for (k in names(res)) {
    expect_equal(res[[k]]$cost, darth_published$cost[[k]], tolerance = 0.6, scale = 1)   # to the dollar
    expect_equal(round(res[[k]]$qaly, 2), darth_published$qaly[[k]])
  }
  arr <- lapply(res, function(s) { s$eff <- s$qaly; s })
  inc <- modelIncremental(unname(arr), FALSE)
  get <- function(nm) Filter(function(x) x$name == nm, inc)[[1]]
  expect_equal(get("Strategy A")$status, "dominated")
  expect_equal(get("Strategy B")$icer, darth_published$icer[["B"]], tolerance = 1, scale = 1)
  expect_equal(get("Strategy AB")$icer, darth_published$icer[["AB"]], tolerance = 1, scale = 1)
})

test_that("DARTH model agrees with an independent implementation to machine precision", {
  m <- darth_model()
  res <- modelRunAll(m)
  w <- wcc_weights(75, "simpson")
  u0 <- c(1, 0.75, 0.5, 0); uA <- c(1, 0.95, 0.5, 0); c0 <- c(2000, 4000, 15000, 0)
  refs <- list(reference_cstm(darth_matrix(darth), c0, u0, 75, 0.03, w),
               reference_cstm(darth_matrix(darth), c0 + c(0, 12000, 12000, 0), uA, 75, 0.03, w),
               reference_cstm(darth_matrix(darth, TRUE), c0 + c(0, 13000, 13000, 0), u0, 75, 0.03, w),
               reference_cstm(darth_matrix(darth, TRUE), c0 + c(0, 25000, 25000, 0), uA, 75, 0.03, w))
  for (k in 1:4) {
    expect_equal(res[[k]]$cost, refs[[k]]$cost, tolerance = 1e-12)
    expect_equal(res[[k]]$qaly, refs[[k]]$qaly, tolerance = 1e-12)
  }
  expect_equal(sum(unlist(res[[2]]$costByState)), res[[2]]$cost, tolerance = 1e-12)
})

test_that("treatment effects (HR / RR / OR) rebuild the matrix from the baseline", {
  m <- darth_model(strategies = c("SoC", "SoC"))
  m$strategies[[2]] <- list(name = "HR", addCost = 0,
                            effect = list(baseline = 0, measure = "hr", value = 0.6, transitions = list(list(1, 2))))
  f <- fixModel(m)
  P0 <- f$strategies[[1]]$matrix; P1 <- f$strategies[[2]]$matrix
  expect_equal(P1[2, 3], 1 - (1 - P0[2, 3])^0.6)
  expect_equal(sum(P1[2, ]), 1)
  expect_equal(P1[2, 2], P0[2, 2] + P0[2, 3] - P1[2, 3])
  expect_equal(P1[-2, ], P0[-2, ])
  m$strategies[[2]]$effect$measure <- "rr"; m$strategies[[2]]$effect$value <- 2
  expect_equal(fixModel(m)$strategies[[2]]$matrix[2, 3], 2 * P0[2, 3])
  m$strategies[[2]]$effect$measure <- "or"; m$strategies[[2]]$effect$value <- 1
  expect_equal(fixModel(m)$strategies[[2]]$matrix, P0)
  m$strategies[[2]]$effect$measure <- "rr"; m$strategies[[2]]$effect$value <- 50
  expect_error(fixModel(m), "negative", class = "input_error")
  m$strategies[[2]]$effect$baseline <- 1
  expect_error(fixModel(m), "baseline", class = "input_error")
})

test_that("cohort mass is conserved, with and without background mortality", {
  m <- darth_model(); m$bgMortality <- TRUE; m$startAge <- 25
  f <- fixModel(m)
  for (s in f$strategies) expect_equal(rowSums(markovGeneric(f, s)$trace), rep(1, 76), tolerance = 1e-12)
})

test_that("custom life table is used for background mortality", {
  m <- two_state(p = 0); m$bgMortality <- TRUE; m$startAge <- 40
  m$lifeTable <- list(list(age = 0, q = 0.5))
  r <- modelRunAll(m)[[1]]
  expect_equal(r$trace[2, 1], 0.5)
  m$lifeTable <- list(list(age = 0, q = 2))
  expect_error(modelRunAll(m), class = "input_error")
})

test_that("missing 'absorbing' field does not crash background mortality", {
  m <- two_state(); m$states[[1]]$absorbing <- NULL; m$bgMortality <- TRUE
  expect_silent(modelRunAll(m))
})

test_that("invalid inputs are rejected with a clear message", {
  m <- two_state(); m$strategies[[1]]$matrix <- list(list(0.8, 0.1), list(0, 1))
  expect_error(modelRunAll(m), "sums to 0.9000", class = "input_error")
  m <- two_state(); m$strategies[[1]]$matrix <- list(list(1.2, -0.2), list(0, 1))
  expect_error(modelRunAll(m), "between 0 and 1", class = "input_error")
  m <- two_state(); m$horizon <- 0
  expect_error(modelRunAll(m), class = "input_error")
  m <- two_state(); m$correction <- "magic"
  expect_error(modelRunAll(m), class = "input_error")
  m <- two_state(); m$strategies[[1]]$matrix <- list(list(0.6666, 0.3333), list(0, 1))   # rounding: accepted
  expect_silent(modelRunAll(m))
})

test_that("fixModel is idempotent", {
  f <- fixModel(darth_model())
  expect_identical(fixModel(f)$strategies[[3]]$matrix, f$strategies[[3]]$matrix)
})
