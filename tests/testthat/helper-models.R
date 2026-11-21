# Shared fixtures. engine.R is sourced by tests/run_tests.R (or here if needed).
if (!exists("markovGeneric")) {
  for (p in c("../../engine.R", "engine.R")) if (file.exists(p)) { source(p); break }
}

two_state <- function(p = 0.1, horizon = 10, d = 0.03, cost = 1000) {
  list(states = list(list(name = "Alive", cost = cost, util = 1, dw = 0),
                     list(name = "Dead", cost = 0, util = 0, dw = 0, absorbing = TRUE)),
       strategies = list(list(name = "x", addCost = 0, matrix = list(list(1 - p, p), list(0, 1)))),
       cycle = 1, horizon = horizon, dCost = d, dEff = d, lifeExp = 0, outcome = "QALY")
}

# DARTH Sick-Sicker (time-independent) parameters, from Alarid-Escudero et al.,
# "An Introductory Tutorial on Cohort State-Transition Models in R Using a
# Cost-Effectiveness Analysis Example", Med Decis Making 2023.
darth <- list(n_cycles = 75, r_HS1 = 0.15, r_S1H = 0.5, r_S1S2 = 0.105, r_HD = 0.002,
              hr_S1 = 3, hr_S2 = 10, hr_S1S2_trtB = 0.6,
              c_H = 2000, c_S1 = 4000, c_S2 = 15000, c_trtB = 13000,
              u_H = 1, u_S1 = 0.75, u_S2 = 0.5, d = 0.03)

darth_matrix <- function(p, trtB = FALSE) {
  r2p <- function(r) 1 - exp(-r)
  p_HD <- r2p(p$r_HD); p_S1D <- r2p(p$r_HD * p$hr_S1); p_S2D <- r2p(p$r_HD * p$hr_S2)
  p_HS1 <- r2p(p$r_HS1); p_S1H <- r2p(p$r_S1H)
  p_S1S2 <- r2p(p$r_S1S2 * (if (trtB) p$hr_S1S2_trtB else 1))
  M <- matrix(0, 4, 4)
  M[1, ] <- c((1 - p_HD) * (1 - p_HS1), (1 - p_HD) * p_HS1, 0, p_HD)
  M[2, ] <- c((1 - p_S1D) * p_S1H, (1 - p_S1D) * (1 - (p_S1H + p_S1S2)), (1 - p_S1D) * p_S1S2, p_S1D)
  M[3, ] <- c(0, 0, 1 - p_S2D, p_S2D)
  M[4, ] <- c(0, 0, 0, 1)
  M
}

darth_model <- function(p = darth) {
  asList <- function(M) lapply(seq_len(nrow(M)), function(i) as.list(M[i, ]))
  list(states = list(list(name = "Healthy", cost = p$c_H, util = p$u_H),
                     list(name = "Sick", cost = p$c_S1, util = p$u_S1),
                     list(name = "Sicker", cost = p$c_S2, util = p$u_S2),
                     list(name = "Dead", cost = 0, util = 0, absorbing = TRUE)),
       strategies = list(list(name = "Standard of care", addCost = 0, matrix = asList(darth_matrix(p))),
                         list(name = "Strategy B", addCost = p$c_trtB, matrix = asList(darth_matrix(p, TRUE)))),
       cycle = 1, horizon = p$n_cycles, dCost = p$d, dEff = p$d, lifeExp = 0, outcome = "QALY")
}

# Independent reference: matrix powers + trapezoid weights + discount vector,
# written without any engine code. Same conventions as the engine
# (start in state 1, trapezoidal half-cycle, discount cycle t at (1+d)^(t-1)).
reference_cstm <- function(P, cvec, uvec, n, d) {
  tr <- t(sapply(0:n, function(k) { v <- c(1, rep(0, nrow(P) - 1)); if (k > 0) for (i in 1:k) v <- v %*% P; v }))
  # Each interval [t-1, t] contributes 0.5*(row t-1 + row t), discounted at t-1.
  occ <- (tr[1:n, ] + tr[2:(n + 1), ]) / 2
  dd <- (1 + d)^-(0:(n - 1))
  list(cost = sum(dd * (occ %*% cvec)), qaly = sum(dd * (occ %*% uvec)))
}
