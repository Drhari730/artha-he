# Artha HE — Validation

## What is checked

The R engine (`engine.R`) is covered by an automated `testthat` suite (`tests/testthat`). CI runs it on every push, and the Docker build runs it too.

| Area | Check |
|---|---|
| Markov engine | A two-state model matches the closed-form geometric series exactly (with and without discounting) |
| Markov engine | **Reproduces the published DARTH Sick-Sicker base case** (Alarid-Escudero et al., *Med Decis Making* 2023;43:3–20; corrected results in DARTH v0.3.0 NEWS.md). All four strategies match: costs to the dollar, QALYs to two decimals, ICERs $73,279 (B) and $125,302 (AB), Strategy A dominated |
| Markov engine | Agrees with a separately written reference implementation (no engine code) to machine precision on all four DARTH strategies |
| Markov engine | Simpson's 1/3 (+3/8) weights sum to *n* and integrate cubic polynomials exactly |
| Survival fitting | Exponential, Weibull, log-logistic and log-normal maximum-likelihood fits match R `survival::survreg` (parameters and log-likelihood); Gompertz is checked as a true maximum and recovers simulated parameters; Kaplan-Meier matches `survival::survfit` |
| Guyot reconstruction | Rebuilding patient-level data from a KM curve and its numbers-at-risk table reproduces the curve (max difference < 0.02) and the Weibull fit (within 5%) |
| Partitioned survival | Costs, QALYs and life-years match an independent calculation to machine precision, including hazard ratios, curves in months, and PFS capped at OS |
| Decision trees | Roll-back matches hand calculation; probabilities, '#' (the rest), parents and loops are validated |
| Multi-strategy PSA | CEAC probabilities sum to 1; the frontier is the strategy with the highest expected net benefit; EVPI matches its definition; with two strategies the CEAC equals the pairwise result |
| EVPPI | The regression estimator recovers analytic values: for INB = X + Y with X, Y ~ N(0,1), EVPPI(X) = 1/√(2π) and EVPPI(X, Y) = EVPI = 1/√π (within 3%); an irrelevant parameter gives ≈ 0; EVPPI never exceeds EVPI for Markov, PSM and tree models |
| Costing (CPI) | Price-year conversion uses the index ratio when both years are in the CPI series and falls back to the annual rate otherwise |
| Scenarios | The base case reproduces the DARTH ICER; standard scenarios change the result as expected; a failing scenario reports its own error without stopping the others |
| SRS life tables | All 207 SRS 2020-24 tables (India and 22 states/UTs × residence × sex) parsed from the ORGI report pass l(x+n) = l(x)(1 - nqx); a cohort facing only background mortality reproduces each table's published life expectancy at birth and at 60 to within 0.06-0.28 years |
| Treatment effects | HR, RR and OR give the expected probabilities; the residual moves to the diagonal; invalid effects and baselines are rejected |
| Markov engine | The cohort sums to 1 in every cycle, with and without background mortality |
| Markov engine | Invalid matrices (rows not summing to 1, probabilities outside 0–1, blank cells) are rejected with a clear message |
| Incremental analysis | Strong dominance, ties and duplicates; extended dominance, iterated until ICERs increase; ICERs recomputed against the previous non-dominated option; optimal = maximum net monetary benefit |
| PSA / DSA | The treatment effect is sampled from its CI and centred on the estimate; DSA ranges come from the 95% intervals and CIs; same seed → identical draws; absorbing states and structural zeros stay at zero; identical strategies give exactly zero incremental cost and effect (common random numbers); per-parameter mode runs; the iteration cap is enforced |
| Costing / OOP / BIA | Empty inputs, missing quantities, inflation, the annualisation factor, CHE on direct medical payments only, budget-impact arithmetic |

Run it yourself:

```bash
Rscript tests/run_tests.R
```

```bash
Rscript validation/validate_darth.R
```

The **Methods** tab in the app also runs live self-checks, including the DARTH reproduction. The **DARTH (validation)** example in the Modeling tab loads the model so you can inspect it.

## What this does and does not show

- **It confirms** that the engine reproduces a published, widely used reference model, and that it correctly computes its stated methods (cohort trace, within-cycle correction, discounting, incremental analysis, treatment effects, sampling).
- **Note on the DARTH conventions:** DARTH discounts monthly cycles at (1 + r·cycle)^t; Artha HE uses (1 + r)^(t·cycle). The two are identical for annual cycles, as in the validated base case.
- **It does not** validate that your inputs, assumptions or model structure suit your decision problem. That is the analyst's responsibility.

## Open science

All engine code is in this repository for inspection and peer review.
