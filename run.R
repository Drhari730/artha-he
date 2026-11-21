library(plumber)

options_plumber(maxRequestSize = 2 * 1024 * 1024)  # 2 MB per request
pr <- pr("plumber.R")

pr_set_error(pr, function(req, res, err) {
  # Input-validation problems are the user's to fix (400); anything else is a bug (500).
  res$status <- if (inherits(err, "input_error")) 400 else 500
  msg <- conditionMessage(err)
  cat("[API ERROR]", msg, "|call:", paste(deparse(err$call), collapse=" "), "\n", file = stderr())
  list(error = msg)
})

port <- Sys.getenv("PORT")
if (port == "") {
  port <- "8080"
}

pr_run(pr, host = "0.0.0.0", port = as.numeric(port))
