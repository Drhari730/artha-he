# validation/validate_darth.R
# Runs the DARTH Sick-Sicker model (time-independent, 4 strategies) through the
# Artha HE engine and compares it with the published base case.
#
#   Rscript validation/validate_darth.R        (from the repository root)
#
# Model and published results: DARTH-git/cohort-modeling-tutorial-intro,
# analysis/cSTM_time_indep.R and NEWS.md v0.3.0 (corrected Simpson's 1/3
# weights). Article: Alarid-Escudero F, Krijkamp EM, Enns EA, et al.
# Med Decis Making 2023;43(1):3-20.

# Text in the engine (₹, ±, –) is UTF-8; make sure R reads and serialises it as such.
if (!l10n_info()$`UTF-8`) invisible(Sys.setlocale("LC_CTYPE", "C.UTF-8"))
if (!file.exists("engine.R")) stop("Run this from the repository root.")
source("engine.R")
source("validation/darth_fixture.R")

res <- modelRunAll(darth_model())
keys <- c("SoC", "A", "B", "AB")
cat("DARTH Sick-Sicker, 75 annual cycles, 3% discounting, Simpson's 1/3 correction\n\n")
cat(sprintf("%-18s %14s %14s %10s %10s\n", "Strategy", "Cost (Artha)", "Cost (DARTH)", "QALY (Artha)", "QALY (DARTH)"))
ok <- TRUE
for (k in seq_along(keys)) {
  pc <- darth_published$cost[[keys[k]]]; pq <- darth_published$qaly[[keys[k]]]
  cat(sprintf("%-18s %14.2f %14.0f %12.4f %12.2f\n", res[[k]]$name, res[[k]]$cost, pc, res[[k]]$qaly, pq))
  ok <- ok && abs(res[[k]]$cost - pc) < 1 && round(res[[k]]$qaly, 2) == pq
}
inc <- modelIncremental(lapply(res, function(s) { s$eff <- s$qaly; s }), FALSE)
cat("\nIncremental analysis:\n")
for (s in inc) cat(sprintf("  %-18s %-10s ICER %s\n", s$name, s$status, if (is.na(s$icer)) "-" else formatC(s$icer, format = "f", digits = 0, big.mark = ",")))
icer <- vapply(inc, function(s) if (is.na(s$icer)) NA_real_ else s$icer, numeric(1)); names(icer) <- vapply(inc, function(s) s$name, "")
ok <- ok && abs(icer[["Strategy B"]] - darth_published$icer[["B"]]) < 1 && abs(icer[["Strategy AB"]] - darth_published$icer[["AB"]]) < 1
cat(sprintf("\nPublished ICERs: B $%s, AB $%s; Strategy A dominated.\n", format(darth_published$icer[["B"]], big.mark = ","), format(darth_published$icer[["AB"]], big.mark = ",")))
if (!ok) stop("Artha HE does not reproduce the published DARTH results.")
cat("PASS: Artha HE reproduces the published DARTH base case.\n")
