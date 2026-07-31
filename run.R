library(plumber)

pr <- pr("plumber.R")

pr_set_error(pr, function(req, res, err) {
  res$status <- 500
  msg <- conditionMessage(err)
  cat("[API ERROR]", msg, "|call:", paste(deparse(err$call), collapse=" "), "\n", file = stderr())
  list(error = msg)
})

port <- Sys.getenv("PORT")
if (port == "") {
  port <- "8080"
}

pr_run(pr, host = "0.0.0.0", port = as.numeric(port))
