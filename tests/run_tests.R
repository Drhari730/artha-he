# Run from the repository root:  Rscript tests/run_tests.R
library(testthat)
if (!file.exists("engine.R")) stop("Run this from the repository root.")
source("engine.R")
test_dir("tests/testthat", reporter = "summary", stop_on_failure = TRUE)
