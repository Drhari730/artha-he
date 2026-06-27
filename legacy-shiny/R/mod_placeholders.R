# =============================================================================
# mod_placeholders.R : scaffolds for modules built in later phases.
# Each renders a clear "what's coming" panel so the app runs end-to-end now.
# =============================================================================

.coming_soon <- function(title, phase, bullets) {
  card(
    card_header(title),
    card_body(
      tags$p(tags$em(sprintf("Planned for %s.", phase))),
      tags$p("This module will include:"),
      tags$ul(lapply(bullets, tags$li))
    )
  )
}

# ---- Modeling (decision tree / Markov / partitioned survival) ---------------
modelUI <- function(id) {
  .coming_soon(
    "Decision-analytic modeling", "Phase 3",
    c("One-click Markov / state-transition model (cycle length, half-cycle correction, time horizon, discounting)",
      "Decision tree builder",
      "Partitioned survival model for oncology (survival extrapolation via flexsurv/survHE)",
      "Cohort trace plots and per-strategy cost/QALY outputs",
      "Built on the hesim and heemod engines")
  )
}
modelServer <- function(id) moduleServer(id, function(input, output, session) {})

# ---- Sensitivity analysis (DSA / PSA) ---------------------------------------
sensitivityUI <- function(id) {
  .coming_soon(
    "Sensitivity analysis", "Phase 4",
    c("One-way deterministic analysis with tornado diagram",
      "Probabilistic sensitivity analysis (Monte Carlo)",
      "Distributions: Beta, Gamma, Lognormal, Dirichlet",
      "Cost-effectiveness acceptability curves (CEAC) and frontier (CEAF)",
      "Scatter on the CE plane and expected value of perfect information (EVPI)")
  )
}
sensitivityServer <- function(id) moduleServer(id, function(input, output, session) {})

# ---- Budget impact analysis -------------------------------------------------
biaUI <- function(id) {
  .coming_soon(
    "Budget impact analysis", "Phase 5",
    c("Eligible population sizing with uptake scenarios over a multi-year horizon",
      "Current vs new mix of treatments",
      "Annual and cumulative budget impact",
      "Payer-facing tables and charts",
      "CHEERS 2022-compliant report export to Word/PDF")
  )
}
biaServer <- function(id) moduleServer(id, function(input, output, session) {})
