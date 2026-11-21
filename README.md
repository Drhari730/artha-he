# Artha HE — Health Economics Workbench

A friendly, browser-based tool that turns your data into full health-economic
analyses — **costing, out-of-pocket burden, economic evaluation (CMA/CEA/CUA/CBA/CCA),
decision-analytic (Markov) modeling, probabilistic sensitivity analysis, and
budget impact** — with validated formulae running in the background.

Built for **researchers, teaching, and HTA/payer analysis**, with defaults tuned
for the **Indian / LMIC** context (GDP-based WTP thresholds, DALYs, ₹, 3% discounting).

> *Artha* (अर्थ) is Sanskrit for *wealth / economics*.

## What's inside

| Module | What it does |
|--------|--------------|
| **★ Project (start here)** | Describe the decision (population, intervention, comparators, outcome, perspective). Artha recommends the method, applies the reference case (India: HTAIn-style defaults), and guides you step by step, with progress, plain-language checks and an auto-filled **CHEERS 2022** checklist |
| **Costing** | Micro-costing (bottom-up) & gross-costing (top-down), inflation, category breakdown, CSV import/template |
| **Out-of-Pocket** | Direct patient spending and catastrophic-health-expenditure (CHE) checks (10% income / 40% capacity-to-pay) |
| **Evaluation** | CMA · CEA · CUA · CBA · CCA, with a built-in **advisor** that recommends the right method and lists missing data; ICER, dominance, net benefit, CE plane |
| **Survival** | Fit exponential, Weibull, Gompertz, log-logistic and log-normal curves (maximum likelihood, AIC/BIC) to patient-level data, or to a **published Kaplan-Meier curve** rebuilt with the Guyot et al. (2012) algorithm; extrapolate, and send the chosen curve to the model |
| **Modeling** | Three model types: **Markov** (any states, matrices or treatment effects), **partitioned survival** (OS/PFS curves or hazard ratios, as in oncology HTA) and **decision trees** (screening, diagnostics, acute care); QALYs and DALYs |
| **Sensitivity** | Probabilistic SA for every model type (Gamma / Beta / Dirichlet / multivariate-normal curve parameters / lognormal hazard ratios), CEAC for all strategies with the acceptability frontier, EVPI by threshold, **EVPPI** (which parameters are worth researching) and **population EVPI**, one-way tornado, and a **scenario manager** (standard HTA scenarios generated automatically, your own saved scenarios, alternative survival extrapolations) |
| **Reference library** | Ships with verified India figures: GDP per capita FY 2025-26 (₹2,43,803; RBI/NSO), CPI-IW 2020-21 to 2025-26 (Labour Bureau), **all SRS 2020-24 abridged life tables** (India + 22 states/UTs, by residence and sex; ORGI) and CGHS consultation rates (Oct 2025); every shipped unit cost has a cited source. Your own editable, shareable library: GDP per capita, a CPI series (price-year conversion by index), life tables, unit costs and utilities, each with its source and a **verified** flag; one click sends a value into the analysis, and the report lists every value used and whether it was verified |
| **Budget Impact** | Multi-year annual & cumulative budget impact |
| **Methods** | Every formula on display + a live engine-validation panel + glossary |

Plus: **Save/Load projects**, a one-click **combined CHEERS report** (PDF/Word),
and per-analysis export to **Excel (.xlsx) / Word / PDF / PNG**.

## One guided flow

Question → Costs → Outcomes → Evaluation (quick) or Model + Uncertainty (full) → Patient burden → Budget impact → Report.

- **Quick or full:** compare summary totals in minutes, or build a Markov model with PSA for a formal submission. Both live in the same project.
- **Nothing typed twice:** every result (a cost, a QALY, a utility, a transition probability, a model's results) has a **Use this result** button that sends it to the step that needs it. Artha records where each value came from, and the report lists it.
- **Submission-ready report:** includes only the analyses you ran, plus the decision problem, a model-inputs table, undiscounted results, the DSA table, where each input came from, and the CHEERS 2022 checklist.

## How it's built

- **Front end:** plain HTML + CSS + JavaScript in `public/` (no framework). It handles input, charts and export.
- **Engine:** every calculation runs in R (`engine.R`), served as a JSON API by [plumber](https://www.rplumber.io/) (`plumber.R`). The front end calls `/api/*`, so **the app needs the R server running**; opening `index.html` directly will not work.
- **Methods:** standard published formulae (discounting, half-cycle correction, ICER with strong and iterated extended dominance, net monetary benefit, Gamma/Beta/Dirichlet PSA, CEAC, EVPI). Reporting is structured to **CHEERS 2022**.

## Run it locally

You need R (4.3 or later) with `plumber`, `jsonlite` and `testthat`:

```bash
Rscript -e "install.packages(c('plumber','jsonlite','testthat'))"
```

```bash
Rscript run.R
```

Then open http://localhost:8080 (set `PORT` to change the port).

## Tests and validation

```bash
Rscript tests/run_tests.R
```

```bash
Rscript validation/validate_darth.R
```

The engine **reproduces the published DARTH Sick-Sicker base case** (all four strategies; costs to the dollar, QALYs to two decimals, the same ICERs). The `testthat` suite covers survival fitting (checked against R `survival::survreg` / `survfit`, and a Guyot round-trip), the partitioned survival model and decision trees (against independent calculations), multi-strategy CEAC/CEAF/EVPI, EVPPI (against analytic values), CPI conversion and scenarios, the Markov engine (DARTH, closed-form checks, Simpson weights, treatment effects, mass conservation, input validation), the incremental analysis (strong and extended dominance), PSA (reproducibility, absorbing states, correlated sampling, the per-parameter mode), and costing, out-of-pocket and budget impact. CI runs it on every push (`.github/workflows/engine-tests.yml`), and the Docker build fails if a test fails. See `validation/VALIDATION.md` for what is and is not validated.

## Deploy (Railway)

The repo deploys with the `Dockerfile` (pinned `rocker/r-ver:4.4.1`):
1. **New Project → Deploy from GitHub repo** → pick this repo.
2. Railway builds the Dockerfile (`railway.json`) and starts `Rscript run.R` on `$PORT`.
3. Add a public domain under **Settings → Networking → Generate Domain**.

## Project layout

```
engine.R            # calculation engine (pure R): costing, Markov, PSA, DSA, BIA
engine_survival.R   # survival fitting, Kaplan-Meier, Guyot reconstruction
engine_models.R     # partitioned survival, decision trees, multi-strategy PSA
engine_analysis.R   # EVPPI, population EVPI, scenario analysis
plumber.R           # JSON API + report builder
run.R               # starts the server (error handling, request-size limit)
public/data/        # srs_life_tables_2020_24.json (parsed from the ORGI report)
public/             # front end: index.html, app.js (core), models.js (survival, PSM, trees),
                    #   library.js (reference library, scenarios), flow.js (guided flow, CHEERS)
tests/              # testthat suite (tests/run_tests.R)
validation/         # DARTH validation script + VALIDATION.md
Dockerfile          # pinned R image; runs the tests at build time
legacy-shiny/       # earlier R/Shiny prototype (not used by the web app)
```

## Key conventions

- **Trace and discounting (as in DARTH):** the cohort starts in the first state. Trace row *t* (time *t*·cycle) is weighted by the within-cycle correction and discounted at (1+r)^(t·cycle). Per-cycle costs and utilities are multiplied by the cycle length.
- **Within-cycle correction:** half-cycle (trapezoidal, the default) or Simpson's 1/3. For an odd number of cycles, Simpson uses the 3/8 rule over the last three cycles, as DARTH v0.3.0 does.
- **Transition matrices:** rows must sum to 1. Rows within 0.001 of 1 are renormalised; anything further off is rejected with a message.
- **Treatment effects:** a strategy can be defined as a hazard ratio, relative risk or odds ratio applied to chosen transitions of a baseline strategy's matrix. HR gives p′ = 1 − (1 − p)^HR, RR gives p′ = min(1, p·RR), and OR is applied on the odds scale. The probability that moves comes from (or goes to) staying in the same state. In PSA the effect is drawn from a lognormal distribution fitted to its 95% CI, and applied to the *sampled* baseline.
- **Strategy-specific rewards:** a strategy's added cost can apply to chosen states only (default: all non-absorbing states), and a strategy can override the utility of any state.
- **Background mortality:** uses your own life table (age, annual q). The built-in table is crude and for illustration only.
- **Optimal strategy:** the one with the highest net monetary benefit at the willingness-to-pay threshold. The GDP-per-capita threshold is editable and saved with the project.
- **DSA ranges:** the 95% interval of each per-parameter PSA distribution; each treatment effect's 95% CI; discounting from 0% to 5%. Any other parameter uses an assumed ±30% (costs) or ±0.1 (utilities), and the tornado labels which.
- **Catastrophic health expenditure:** counts **direct medical** payments only (SDG 3.8.2 / WHO). Transport and lost income are reported, but excluded from the ratios.
- **PSA reproducibility:** PSA uses a fixed random seed by default, so results are reproducible. You can change the seed in the Sensitivity tab.

---
*For research & teaching. Verify all inputs against your study protocol before use.*
