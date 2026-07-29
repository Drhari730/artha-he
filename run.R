library(plumber)

pr <- pr("plumber.R")
pr <- pr_static(pr, "/", "./public")
pr_run(pr, host = "0.0.0.0", port = as.numeric(Sys.getenv("PORT", 8088)))
