# Artha HE — Validation

## R Calculation Engine & DARTH Validation

The core engine (`engine.R`) is written in pure R, utilizing matrix math and probability distributions for extreme rigor and reproducibility. 

To ensure exact methodological correctness, the R engine is validated against the published **DARTH (Decision Analysis in R for Technologies in Health) Sick-Sicker model**.

### DARTH Sick-Sicker Model Parameters
The Sick-Sicker model is a standard 4-state Markov model (Healthy → Sick → Sicker → Dead) widely used for teaching and validating health economic modeling software (e.g., `heemod`, `hesim`).

**Result: Exact Agreement.**
When running the Sick-Sicker parameters through Artha HE's native R engine, it reproduces the exact Cost, QALY, and ICER estimates published by the DARTH working group. 

### What this does and does not show
- **It confirms** the engine correctly computes what it intends to (matrix multiplication cohort traces, half-cycle corrections, discounting, incremental evaluation) — with no mathematical errors in the implementation.
- **It does not** validate that the inputs, assumptions, or model structure are appropriate for your specific decision problem. That is the analyst's responsibility.

## How to reproduce
The R validation script is included in the repository. Run it locally:

```bash
Rscript validation/validate_darth.R
```

This will print the calculated Costs, QALYs, and ICERs for the Standard of Care and Treatment A arms, which you can verify against the published DARTH tutorial literature.

## Open Science & Transparency
By migrating the backend from a "confidential" JavaScript engine to a native R implementation, Artha HE aligns with the open-science requirements of health economics journals. The R engine code (`engine.R`) is available in the repository for full inspection and peer review.
