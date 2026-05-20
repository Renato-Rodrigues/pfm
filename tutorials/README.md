# PFM Tutorials

Step-by-step guides for the Political Feasibility Module. Read them in order the first time; each one builds on the previous.

| # | File | Topic |
|---|---|---|
| 1 | [01-model-overview.md](01-model-overview.md) | What the model is, why it exists, how it works |
| 2 | [02-running-estimation.md](02-running-estimation.md) | Running a model, reading results, saving to disk |
| 3 | [03-model-selection.md](03-model-selection.md) | Automated variable selection, selection paths, workflow options |
| 4 | [04-statistics-and-diagnostics.md](04-statistics-and-diagnostics.md) | Interpreting AIC, BIC, pseudo-R², VIF, separation, coefficients |
| 5 | [05-model-options.md](05-model-options.md) | Comparing estimation families, fixed effects, Firth, lag, log-transform |
| 6 | [06-projections.md](06-projections.md) | Generating adoption and stringency projections from REMIND GDX output |

## Prerequisites

- `pfm` loaded (via `devtools::load_all(".")` from the package root, or `library(pfm)` if installed)
- Panel data available — either from `panelDataHistorical()` (requires madrat cache) or loaded from a pre-computed `modelData.RData`
- For Tutorial 6: a fitted model in `models/` and a REMIND `fulldata.gdx` file
