# =============================================================================
# Artha HE  —  Health Economics Workbench
# global.R : libraries, sourced helpers, and shared constants
# =============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(dplyr)
  library(tidyr)
  library(DT)
  library(ggplot2)
  library(scales)
  library(readxl)
  library(writexl)
})

# ---- Source helper functions and modules -----------------------------------
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f, local = FALSE)

# ---- App-wide constants -----------------------------------------------------

APP_NAME    <- "Artha HE"
APP_TAGLINE <- "Health Economics Workbench"
APP_VERSION <- "0.1.0"

# India / LMIC defaults (editable in the UI). GDP per capita in INR (approx 2024).
DEFAULTS <- list(
  currency        = "INR",
  gdp_per_capita  = 200000,    # ~ India GDP per capita (INR), used for WTP thresholds
  wtp_multiplier  = c(`1x GDP` = 1, `3x GDP` = 3),
  discount_rate   = 0.03,      # 3% per year, standard in HE
  price_year      = 2024,
  currency_symbol = "₹"   # rupee sign
)

# Cost perspective categories (used by the costing module)
COST_CATEGORIES <- c(
  "Direct medical",
  "Direct non-medical",
  "Indirect (productivity)"
)

# Distribution choices for probabilistic sensitivity analysis (Phase 4)
PSA_DISTRIBUTIONS <- c("Beta", "Gamma", "Lognormal", "Normal", "Dirichlet", "Fixed")
