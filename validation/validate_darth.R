# validation/validate_darth.R
# Runs the DARTH Sick-Sicker model (time-independent version) through the
# Artha HE engine and checks it against a separately written reference
# implementation. Exits with an error if they disagree.
#
#   Rscript validation/validate_darth.R        (from the repository root)
#
# Parameters: Alarid-Escudero F, Krijkamp EM, Enns EA, et al. "An Introductory
# Tutorial on Cohort State-Transition Models in R Using a Cost-Effectiveness
# Analysis Example." Med Decis Making 2023;43(1):3-20.
#
# Note: this checks that the engine computes its stated method correctly. It is
# NOT a reproduction of the published DARTH result tables, which use Simpson's
# 1/3 within-cycle correction and state-specific treatment costs/utilities.

if (!file.exists("engine.R")) stop("Run this from the repository root.")
source("engine.R")
source("tests/testthat/helper-models.R")

m <- darth_model()
res <- modelRunAll(m)
uvec <- c(darth$u_H, darth$u_S1, darth$u_S2, 0)
cst <- c(darth$c_H, darth$c_S1, darth$c_S2, 0)
ref <- list(reference_cstm(darth_matrix(darth), cst, uvec, darth$n_cycles, darth$d),
            reference_cstm(darth_matrix(darth, TRUE), cst + c(rep(darth$c_trtB, 3), 0), uvec, darth$n_cycles, darth$d))

cat("DARTH Sick-Sicker (time-independent), 75 annual cycles, 3% discounting\n")
cat(sprintf("%-18s %14s %14s %10s %10s\n", "Strategy", "Cost (engine)", "Cost (ref)", "QALY (eng)", "QALY (ref)"))
ok <- TRUE
for (i in 1:2) {
  cat(sprintf("%-18s %14.2f %14.2f %10.4f %10.4f\n", res[[i]]$name, res[[i]]$cost, ref[[i]]$cost, res[[i]]$qaly, ref[[i]]$qaly))
  ok <- ok && abs(res[[i]]$cost - ref[[i]]$cost) < 1e-6 * ref[[i]]$cost && abs(res[[i]]$qaly - ref[[i]]$qaly) < 1e-9
}
icer <- (res[[2]]$cost - res[[1]]$cost) / (res[[2]]$qaly - res[[1]]$qaly)
cat(sprintf("ICER (Strategy B vs SoC): %.0f per QALY\n", icer))
if (!ok) stop("Engine and reference implementation disagree.")
cat("PASS: engine matches the reference implementation.\n")
