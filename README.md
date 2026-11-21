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
| **Costing** | Micro-costing (bottom-up) & gross-costing (top-down), inflation, category breakdown, CSV import/template |
| **Out-of-Pocket** | Direct patient spending and catastrophic-health-expenditure (CHE) checks (10% income / 40% capacity-to-pay) |
| **Evaluation** | CMA · CEA · CUA · CBA · CCA, with a built-in **advisor** that recommends the right method and lists missing data; ICER, dominance, net benefit, CE plane |
| **Modeling** | Fully **configurable Markov model** — any states, editable transition matrices, multiple strategies, **QALYs and DALYs**, cohort trace |
| **Sensitivity** | Probabilistic SA (Dirichlet / Gamma / Beta sampling), CEAC, EVPI, one-way tornado |
| **Budget Impact** | Multi-year annual & cumulative budget impact |
| **Methods** | Every formula on display + a live engine-validation panel + glossary |

Plus: **Save/Load projects**, a one-click **combined CHEERS report** (PDF/Word),
and per-analysis export to **Excel (.xlsx) / Word / PDF / PNG**.

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

The `testthat` suite covers the Markov engine (closed-form checks, DARTH Sick-Sicker parameters against a separately written reference implementation, mass conservation, input validation), the incremental analysis (strong and extended dominance), PSA (reproducibility, absorbing states, correlated sampling, the per-parameter mode), and costing, out-of-pocket and budget impact. CI runs it on every push (`.github/workflows/engine-tests.yml`), and the Docker build fails if a test fails. See `validation/VALIDATION.md` for what is and is not validated.

## Deploy (Railway)

The repo deploys with the `Dockerfile` (pinned `rocker/r-ver:4.4.1`):
1. **New Project → Deploy from GitHub repo** → pick this repo.
2. Railway builds the Dockerfile (`railway.json`) and starts `Rscript run.R` on `$PORT`.
3. Add a public domain under **Settings → Networking → Generate Domain**.

## Project layout

```
engine.R            # calculation engine (pure R)
plumber.R           # JSON API + report builder
run.R               # starts the server (error handling, request-size limit)
public/             # front end: index.html, app.js
tests/              # testthat suite (tests/run_tests.R)
validation/         # DARTH validation script + VALIDATION.md
Dockerfile          # pinned R image; runs the tests at build time
legacy-shiny/       # earlier R/Shiny prototype (not used by the web app)
```

## Key conventions

- The Markov cohort starts in the first state. Occupancy uses the trapezoidal (half-cycle) rule, and cycle *t* is discounted at (1+r)^((t−1)·cycle).
- Transition-matrix rows must sum to 1. Rows within 0.001 of 1 are renormalised; anything further off is rejected with a message.
- A strategy's "added cost" applies to every non-absorbing state.
- The optimal strategy is the one with the highest net monetary benefit at the willingness-to-pay threshold.
- Catastrophic health expenditure counts **direct medical** payments only (SDG 3.8.2 / WHO). Transport and lost income are reported, but excluded from the ratios.
- PSA uses a fixed random seed by default, so results are reproducible. You can change the seed in the Sensitivity tab.

---
*For research & teaching. Verify all inputs against your study protocol before use.*
