# Artha HE — validation

## Cross-implementation validation

The core engine (`engine.js`) was checked against a **completely independent
re-implementation** written from the method definitions in Python
(`validate_markov.py`), for a standard 3-state Markov model (Healthy → Sick →
Dead), a CEA/ICER example, and a micro-costing example.

**Result: exact agreement** (relative difference 0.00 across all quantities).

| Quantity (default Markov + CEA + costing) | Engine (`engine.js`) | Independent (`validate_markov.py`) | Δ |
|---|---|---|---|
| Standard-care cost | 94,621.02 | 94,621.02 | 0.00 |
| Standard-care QALYs | 8.327860 | 8.327860 | 0.00 |
| New-treatment cost | 198,216.49 | 198,216.49 | 0.00 |
| New-treatment QALYs | 9.718609 | 9.718609 | 0.00 |
| Markov ICER (₹/QALY) | 74,488.99 | 74,488.99 | 0.00 |
| CEA ICER, B vs A | 50,000.00 | 50,000.00 | 0.00 |
| Micro-costing total | 3,540 | 3,540 | 0.00 |

### What this does and does not show
- **It confirms** the engine correctly computes what it intends to (half-cycle
  correction, discounting, cohort transitions, ICER, costing) — no coding error
  in the implementation.
- **It does not** validate that the inputs, assumptions, or model structure are
  appropriate for any particular decision problem. That is the analyst's
  responsibility. See the app's **Methods** page for the assumptions & limitations.

## How to reproduce
```bash
# 1. Engine numbers
node -e "const {COMPUTE}=require('../engine.js'); /* see validation/engine_reference.js */"
# 2. Independent implementation
python validation/validate_markov.py
```
Both should print the same figures shown above.

## Method self-checks
The app's Methods page also runs live self-checks (discounting a known value,
a known ICER, dominance flagging, catastrophic-expenditure %, Markov cohort
mass conservation, micro-costing total) — all pass.
