# validation/validate_darth.R
# Validates the Artha HE R engine against the DARTH Sick-Sicker model.
source("../engine.R")

# DARTH Sick-Sicker Model Parameters
# States: 1=Healthy, 2=Sick, 3=Sicker, 4=Dead
# Cycle: 1 year, Horizon: 30 years
# Discounting: 3% for costs and QALYs

darth_model <- list(
  states = list(
    list(name = "Healthy", cost = 2000, util = 1.0, absorbing = FALSE),
    list(name = "Sick", cost = 4000, util = 0.75, absorbing = FALSE),
    list(name = "Sicker", cost = 15000, util = 0.50, absorbing = FALSE),
    list(name = "Dead", cost = 0, util = 0.0, absorbing = TRUE)
  ),
  strategies = list(
    list(
      name = "Standard of Care",
      addCost = 0,
      matrix = list(
        list(0.85, 0.10, 0.00, 0.05),
        list(0.00, 0.70, 0.20, 0.10),
        list(0.00, 0.00, 0.80, 0.20),
        list(0.00, 0.00, 0.00, 1.00)
      )
    ),
    list(
      name = "Treatment A",
      addCost = 12000,
      matrix = list(
        list(0.85, 0.10, 0.00, 0.05),
        list(0.00, 0.70, 0.20, 0.10),
        list(0.00, 0.00, 0.80, 0.20),
        list(0.00, 0.00, 0.00, 1.00)
      )
    )
  ),
  cycle = 1,
  horizon = 30,
  dCost = 0.03,
  dEff = 0.03,
  bgMortality = FALSE
)

# Run Artha HE Engine
res <- modelRunAll(darth_model)

cat("DARTH Sick-Sicker Validation\n")
cat("----------------------------\n")
cat("Standard of Care Cost:", res[[1]]$cost, "\n")
cat("Standard of Care QALYs:", res[[1]]$qaly, "\n")
cat("Treatment A Cost:", res[[2]]$cost, "\n")
cat("Treatment A QALYs:", res[[2]]$qaly, "\n")

icer <- (res[[2]]$cost - res[[1]]$cost) / (res[[2]]$qaly - res[[1]]$qaly)
cat("ICER:", icer, "\n")
