# Artha HE — Validation

## What is checked

The R engine (`engine.R`) is covered by an automated `testthat` suite (`tests/testthat`). CI runs it on every push, and the Docker build runs it too.

| Area | Check |
|---|---|
| Markov engine | A two-state model matches the closed-form geometric series exactly (with and without discounting) |
| Markov engine | The **DARTH Sick-Sicker** model (time-independent parameters from Alarid-Escudero et al., *Med Decis Making* 2023;43:3–20) gives the same costs and QALYs as a separately written reference implementation (matrix powers, no engine code), to 1e-10 |
| Markov engine | The cohort sums to 1 in every cycle, with and without background mortality |
| Markov engine | Invalid matrices (rows not summing to 1, probabilities outside 0–1, blank cells) are rejected with a clear message |
| Incremental analysis | Strong dominance, ties and duplicates; extended dominance, iterated until ICERs increase; ICERs recomputed against the previous non-dominated option; optimal = maximum net monetary benefit |
| PSA | Same seed → identical draws; absorbing states and structural zeros stay at zero; identical strategies give exactly zero incremental cost and effect (common random numbers); per-parameter mode runs; the iteration cap is enforced |
| Costing / OOP / BIA | Empty inputs, missing quantities, inflation, the annualisation factor, CHE on direct medical payments only, budget-impact arithmetic |

Run it yourself:

```bash
Rscript tests/run_tests.R
```

```bash
Rscript validation/validate_darth.R
```

The **Methods** tab in the app also runs a set of live self-checks against hand-calculated answers.

## What this does and does not show

- **It confirms** that the engine correctly computes the method it states (cohort trace, half-cycle correction, discounting, incremental analysis, sampling).
- **It is not** a reproduction of the published DARTH result tables. DARTH uses Simpson's 1/3 within-cycle correction, applies treatment costs to the Sick and Sicker states only, and applies a treatment utility to Sick. Artha HE does not support state-specific treatment costs or utilities yet (planned).
- **It does not** validate that your inputs, assumptions or model structure suit your decision problem. That is the analyst's responsibility.

## Open science

All engine code is in this repository for inspection and peer review.
