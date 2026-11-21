test_that("two-state model matches the closed-form geometric series", {
  r <- modelRunAll(two_state(d = 0))[[1]]
  q0 <- 0.95 * (1 - 0.9^10) / 0.1
  expect_equal(r$qaly, q0, tolerance = 1e-12)
  expect_equal(r$cost, 1000 * q0, tolerance = 1e-9)
  expect_equal(r$ly, q0, tolerance = 1e-12)
  r3 <- modelRunAll(two_state(d = 0.03))[[1]]
  rr <- 0.9 / 1.03
  expect_equal(r3$qaly, 0.95 * (1 - rr^10) / (1 - rr), tolerance = 1e-12)
})

test_that("DARTH Sick-Sicker parameters agree with an independent implementation", {
  m <- darth_model()
  res <- modelRunAll(m)
  uvec <- c(darth$u_H, darth$u_S1, darth$u_S2, 0)
  ref_soc <- reference_cstm(darth_matrix(darth), c(darth$c_H, darth$c_S1, darth$c_S2, 0), uvec, darth$n_cycles, darth$d)
  ref_b <- reference_cstm(darth_matrix(darth, TRUE), c(darth$c_H, darth$c_S1, darth$c_S2, 0) + c(rep(darth$c_trtB, 3), 0), uvec, darth$n_cycles, darth$d)
  expect_equal(res[[1]]$cost, ref_soc$cost, tolerance = 1e-10)
  expect_equal(res[[1]]$qaly, ref_soc$qaly, tolerance = 1e-10)
  expect_equal(res[[2]]$cost, ref_b$cost, tolerance = 1e-10)
  expect_equal(res[[2]]$qaly, ref_b$qaly, tolerance = 1e-10)
  expect_gt(res[[2]]$qaly, res[[1]]$qaly)        # slowing progression gains QALYs
  expect_gt(res[[2]]$cost, res[[1]]$cost)
})

test_that("cohort mass is conserved, with and without background mortality", {
  m <- darth_model(); m$bgMortality <- TRUE; m$startAge <- 25
  f <- fixModel(m)
  for (s in f$strategies) expect_equal(rowSums(markovGeneric(f, s)$trace), rep(1, 76), tolerance = 1e-12)
})

test_that("missing 'absorbing' field does not crash background mortality", {
  m <- two_state(); m$states[[1]]$absorbing <- NULL; m$bgMortality <- TRUE
  expect_silent(modelRunAll(m))
})

test_that("invalid matrices are rejected with a clear message", {
  m <- two_state(); m$strategies[[1]]$matrix <- list(list(0.8, 0.1), list(0, 1))
  expect_error(modelRunAll(m), "sums to 0.9000", class = "input_error")
  m <- two_state(); m$strategies[[1]]$matrix <- list(list(1.2, -0.2), list(0, 1))
  expect_error(modelRunAll(m), "between 0 and 1", class = "input_error")
  m <- two_state(); m$horizon <- 0
  expect_error(modelRunAll(m), class = "input_error")
  m <- two_state(); m$strategies[[1]]$matrix <- list(list(0.6666, 0.3333), list(0, 1))   # rounding: accepted
  expect_silent(modelRunAll(m))
})

test_that("fixModel is idempotent", {
  f <- fixModel(darth_model())
  expect_identical(fixModel(f)$strategies[[1]]$matrix, f$strategies[[1]]$matrix)
})
