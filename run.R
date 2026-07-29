library(plumber)

pr <- pr("plumber.R")

pr_set_error(pr, function(req, res, err) {
  res$status <- 500
  list(error = err$message)
})

port <- Sys.getenv("PORT")
if (port == "") {
  port <- "8080"
}

pr_run(pr, host = "0.0.0.0", port = as.numeric(port))
