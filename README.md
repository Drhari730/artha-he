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

Pure client-side **HTML + CSS + JavaScript** (no framework). All calculations use
standard published health-economics formulae — the same methods implemented in the
R toolchain (`hesim`, `heemod`, `dampack`, `BCEA`): discounting, half-cycle correction,
ICER/dominance, PSA, CEAC, EVPI. Reporting is structured to the **CHEERS 2022** checklist.

## Run it locally

**Easiest:** just open `index.html` in any browser (works offline).

**Or via the bundled server:**
```bash
npm start        # serves on http://localhost:8088 (or $PORT)
```

## Deploy (Railway)

This repo is deploy-ready. On [Railway](https://railway.app):
1. **New Project → Deploy from GitHub repo** → pick this repo.
2. Railway auto-detects Node, runs `npm start` (`node server.js`), and serves on `$PORT`.
3. Add a public domain under **Settings → Networking → Generate Domain**.

`server.js` is a zero-dependency static file server; `railway.json` / `Procfile`
pin the start command.

## Project layout

```
ArthaHE/
  index.html        # shell, design system, landing + app screens
  app.js            # engines (costing, ICER, Markov, PSA, BIA…) + UI + export
  server.js         # zero-dependency static server for hosting
  package.json      # start script (node server.js)
  railway.json      # Railway deploy config
  Procfile          # start command
  legacy-shiny/     # earlier R/Shiny prototype (not used by the web app)
```

---
*For research & teaching. Verify all inputs against your study protocol before use.*
