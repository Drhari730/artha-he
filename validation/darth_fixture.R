# validation/darth_fixture.R
# DARTH Sick-Sicker model (time-independent) expressed as an Artha HE model,
# plus the published base-case results. Used by the test suite, the
# validation script and the live self-checks on the Methods page.
# Source: DARTH-git/cohort-modeling-tutorial-intro, analysis/cSTM_time_indep.R
# and NEWS.md (v0.3.0, corrected Simpson weights).

# DARTH Sick-Sicker (time-independent) parameters, from Alarid-Escudero et al.,
# "An Introductory Tutorial on Cohort State-Transition Models in R Using a
# Cost-Effectiveness Analysis Example", Med Decis Making 2023.
darth <- list(n_cycles = 75, c_trtA = 12000, u_trtA = 0.95, r_HS1 = 0.15, r_S1H = 0.5, r_S1S2 = 0.105, r_HD = 0.002,
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

asList <- function(M) lapply(seq_len(nrow(M)), function(i) as.list(M[i, ]))

# DARTH base case with all four strategies, as in analysis/cSTM_time_indep.R:
# treatment costs apply to Sick and Sicker only; treatment A raises the
# utility of Sick to u_trtA; treatment B slows progression (HR on the rate).
darth_model <- function(p = darth, correction = "simpson", strategies = c("SoC", "A", "B", "AB")) {
  P0 <- asList(darth_matrix(p)); PB <- asList(darth_matrix(p, TRUE))
  uA <- list(NA, p$u_trtA, NA, NA)
  all <- list(
    SoC = list(name = "Standard of care", addCost = 0, matrix = P0),
    A   = list(name = "Strategy A", addCost = p$c_trtA, addCostStates = list(1, 2), utilOverride = uA, matrix = P0),
    B   = list(name = "Strategy B", addCost = p$c_trtB, addCostStates = list(1, 2), matrix = PB),
    AB  = list(name = "Strategy AB", addCost = p$c_trtA + p$c_trtB, addCostStates = list(1, 2), utilOverride = uA, matrix = PB))
  list(states = list(list(name = "Healthy", cost = p$c_H, util = p$u_H),
                     list(name = "Sick", cost = p$c_S1, util = p$u_S1),
                     list(name = "Sicker", cost = p$c_S2, util = p$u_S2),
                     list(name = "Dead", cost = 0, util = 0, absorbing = TRUE)),
       strategies = unname(all[strategies]),
       cycle = 1, horizon = p$n_cycles, dCost = p$d, dEff = p$d, lifeExp = 0, outcome = "QALY",
       correction = correction)
}

# Published DARTH base case (cohort-modeling-tutorial-intro v0.3.0 NEWS.md,
# corrected Simpson weights): costs in $, QALYs to 2 dp, ICERs in $/QALY.
darth_published <- list(
  cost = c(SoC = 152282, A = 285889, B = 260222, AB = 380378),
  qaly = c(SoC = 21.03, A = 21.83, B = 22.50, AB = 23.46),
  icer = c(B = 73279, AB = 125302))

# Independent reference written without engine code, using the DARTH
# conventions: trace by repeated multiplication, row t weighted by w[t] and
# discounted at (1+d)^t.
reference_cstm <- function(P, cvec, uvec, n, d, w) {
  tr <- matrix(0, n + 1, nrow(P)); tr[1, 1] <- 1
  for (k in 1:n) tr[k + 1, ] <- tr[k, ] %*% P
  dd <- w / (1 + d)^(0:n)
  list(cost = sum(dd * (tr %*% cvec)), qaly = sum(dd * (tr %*% uvec)))
}
