# Shared fixtures. engine.R is sourced by tests/run_tests.R (or here if needed).
if (!exists("markovGeneric")) {
  for (p in c("../../engine.R", "engine.R")) if (file.exists(p)) { source(p); break }
}
for (p in c("../../validation/darth_fixture.R", "validation/darth_fixture.R")) if (file.exists(p)) { source(p); break }

two_state <- function(p = 0.1, horizon = 10, d = 0.03, cost = 1000) {
  list(states = list(list(name = "Alive", cost = cost, util = 1, dw = 0),
                     list(name = "Dead", cost = 0, util = 0, dw = 0, absorbing = TRUE)),
       strategies = list(list(name = "x", addCost = 0, matrix = list(list(1 - p, p), list(0, 1)))),
       cycle = 1, horizon = horizon, dCost = d, dEff = d, lifeExp = 0, outcome = "QALY")
}


psm_model <- function(lo = 0.10, lp = 0.30, hr = NULL) {
  base <- list(name = "Control", addCost = 1000,
               os = list(dist = "exponential", par = list(rate = lo), unit = "years"),
               pfs = list(dist = "exponential", par = list(rate = lp), unit = "years"))
  trt <- if (is.null(hr)) base else list(name = "New", addCost = 5000,
               os = list(ref = 0, hr = hr, lo = 0.5, hi = 0.95), pfs = list(ref = 0, hr = hr))
  trt$name <- "New"
  list(type = "psm", cycle = 1, horizon = 20, dCost = 0.03, dEff = 0.03, outcome = "QALY",
       states = list(list(name = "PF", cost = 2000, util = 0.8), list(name = "PD", cost = 6000, util = 0.6), list(name = "Dead", cost = 0, util = 0)),
       strategies = list(base, trt))
}


tree_model <- function() {
  node <- function(id, parent, label, p, cost = 0, effect = 0) list(id = id, parent = parent, label = label, p = p, cost = cost, effect = effect)
  list(type = "tree", outcome = "QALY", strategies = list(
    list(name = "Watchful waiting", nodes = list(
      node("r", "", "Start", 1),
      node("a", "r", "Recover", 0.6, 0, 0.95),
      node("b", "r", "Complication", "#", 5000, 0.6))),
    list(name = "Surgery", nodes = list(
      node("r", "", "Start", 1, 20000),
      node("s", "r", "Success", 0.9, 0, 0.97),
      node("f", "r", "Failure", "#", 0),
      node("f1", "f", "Revision ok", 0.5, 15000, 0.85),
      node("f2", "f", "Disability", 0.5, 30000, 0.5)))))
}

