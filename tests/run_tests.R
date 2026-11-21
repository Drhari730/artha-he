# Run from the repository root:  Rscript tests/run_tests.R
library(testthat)
# Text in the engine (₹, ±, –) is UTF-8; make sure R reads and serialises it as such.
if (!l10n_info()$`UTF-8`) invisible(Sys.setlocale("LC_CTYPE", "C.UTF-8"))
if (!file.exists("engine.R")) stop("Run this from the repository root.")
source("engine.R")
test_dir("tests/testthat", reporter = "summary", stop_on_failure = TRUE)
