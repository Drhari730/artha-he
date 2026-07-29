library(plumber)

pr <- pr("plumber.R")

port <- Sys.getenv("PORT")
if (port == "") {
  port <- "8080"
}

pr_run(pr, host = "0.0.0.0", port = as.numeric(port))
