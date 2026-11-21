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

