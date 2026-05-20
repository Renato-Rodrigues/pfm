# Tutorial 3: Model Selection

> Automated variable selection, selection paths, and workflow options

## Table of Contents

1. [Estimation vs Selection](#estimation-vs-selection)
2. [Running model selection](#running-model-selection)
3. [Understanding the selection output](#understanding-the-selection-output)
4. [Selection criterion](#selection-criterion)
5. [Selection mode: incremental vs combinations](#selection-mode-incremental-vs-combinations)
6. [Running multiple configurations](#running-multiple-configurations)
7. [Group analysis](#group-analysis)

---

## Estimation vs Selection

[Tutorial 2](02-running-estimation.md) showed `modelEstimationWorkflow` — you specify exactly which variables to include and the formula is fixed.

`modelSelectionWorkflow` is different: you supply a **candidate pool** of variables and the workflow **searches** for the best-fitting subset, testing combinations and comparing them by an information criterion (AIC by default). Use selection when exploring which governance indicators or actor power drivers improve fit.

```
modelEstimationWorkflow   →  fixed formula, fit once per sector-stage
modelSelectionWorkflow    →  candidate pool, search across subsets, save all candidates
```

All candidate models are saved to `modelDir` as they are evaluated. On re-runs, cached candidates are loaded from disk rather than re-fitted — repeated selection runs are cheap once the candidate cache is populated.

---

## Running model selection

```r
library(pfm)
options(pfm.modelDir = "models/")

load("path/to/modelData.RData")
panelData <- modelData$panelData

result <- modelSelectionWorkflow(
  panelData                 = panelData,

  # Actor Power: composite index
  actorPowerDrivers         = NULL,
  actorPowerIndex           = "Actor Power Index",

  # Candidate pool of IQ drivers — selection finds the best subset
  instQualityDrivers        = c(
    "Government Effectiveness (WGI)",
    "Rule of Law (VDem)",
    "Vertical Accountability (VDem)"
  ),

  # Candidate pool of controls
  controlDrivers            = c(
    "GDP per Capita",
    "Population",
    "Energy Intensity"
  ),

  regionMappingFixedEffects = "regionmapping_EU_OECDp.csv",
  criterion                 = "AIC",         # see Section 4
  testMode                  = "incremental", # see Section 5
  modelDir                  = getOption("pfm.modelDir")
)
```

---

## Understanding the selection output

```r
# Best model summary: one row per sector × stage
print(result$bestModels)

# Full selection path: every model tested, with AIC, BIC, pseudo-R², decision
print(result$selectionPaths)

# Coefficients of the winning model for each sector × stage
print(result$bestCoefficients)

# Group-level ANOVA: how much does each variable group contribute?
print(result$groupAnalysisTable)

# Readable summary of the winning equations
cat(result$workflowSummary)
```

### The selection path

`selectionPaths` is the complete audit trail. Each row is one model evaluated:

| Column | Meaning |
|---|---|
| `step` | Sequential index of the model tested |
| `phase` | Which phase of the selection this belongs to |
| `description` | Human-readable label (what was added/tested) |
| `aic`, `bic`, `aicc`, `hqic` | Information criteria for this model |
| `pseudoR2` | McFadden pseudo-R² |
| `nPredictors` | Total parameters estimated (k) |
| `separation` | Was quasi-complete separation detected? |
| `highVIF` | Were any VIF values > 5? |
| `decision` | Was this model accepted or rejected, and why? |

Models flagged for separation or high VIF are **automatically rejected** regardless of AIC, because their coefficients are unreliable.

---

## Selection criterion

The `criterion` argument controls which metric is used to compare models.

| Criterion | Penalty | Best when |
|---|---|---|
| `"AIC"` | 2k | General use; moderately penalises complexity |
| `"BIC"` | k·log(n) | Prefer parsimony; larger samples increase penalty more |
| `"AICc"` | 2k(k+1)/(n−k−1) + 2k | Small samples (n < 40k); corrects AIC's finite-sample bias |
| `"HQIC"` | 2k·log(log(n)) | Intermediate penalty between AIC and BIC |
| `"pseudoR2"` | none (maximise) | Maximise explanatory power; tends to retain more variables |

Lower is always better for AIC/BIC/AICc/HQIC. Higher is better for pseudoR2.

**Recommendation:** Use `"AIC"` for initial exploration. Switch to `"BIC"` when you want a more parsimonious model or the sample is large. If AIC and BIC point to different models, check whether the extra variables in the AIC winner are theoretically justified — conflict messages appear in the selection log (`result$evalLogs`).

---

## Selection mode: incremental vs combinations

### Incremental mode (default)

Tests variables in four sequential phases, always building on the current best model:

```
Phase 1 — Establish baseline:  Actor Power Index ± region fixed effects
Phase 2 — Add IQ drivers:      Test each IQ driver and subsets incrementally
Phase 3 — Add AP drivers:      Test each AP driver and subsets incrementally
Phase 4 — Add controls:        Test each control variable incrementally
```

Each phase starts from the best model found so far. This is a greedy forward-selection approach — fast and usually finds a good model, but it may miss the global optimum if the best combination involves variables that individually look weak.

**Use when:** The candidate pool is large (> 4 IQ or AP drivers), or computation time is a constraint.

### Combinations mode

Tests **all subsets** of IQ and AP candidates simultaneously, then adds controls incrementally in Phase 4.

**Use when:** You have ≤ 3–4 IQ drivers and ≤ 3–4 AP drivers and want to be sure the incremental approach hasn't missed a good combination.

```r
result_comb <- modelSelectionWorkflow(
  panelData          = panelData,
  actorPowerIndex    = "Actor Power Index",
  instQualityDrivers = c("Government Effectiveness (WGI)", "Rule of Law (VDem)"),
  controlDrivers     = c("GDP per Capita", "Energy Intensity"),
  testMode           = "combinations",
  criterion          = "AIC",
  modelDir           = getOption("pfm.modelDir")
)
```

---

## Running multiple configurations

A common research workflow is to run several model families and compare them — for example, different IQ driver sets (WGI-only, VDem-only, combined):

```r
configs <- list(
  wgi_only = list(
    instQualityDrivers = "Government Effectiveness (WGI)"
  ),
  vdem_only = list(
    instQualityDrivers = c("Rule of Law (VDem)", "Vertical Accountability (VDem)")
  ),
  combined = list(
    instQualityDrivers = c("Government Effectiveness (WGI)", "Rule of Law (VDem)")
  )
)

results <- lapply(configs, function(cfg) {
  modelSelectionWorkflow(
    panelData          = panelData,
    actorPowerIndex    = "Actor Power Index",
    instQualityDrivers = cfg$instQualityDrivers,
    controlDrivers     = c("GDP per Capita", "Population"),
    criterion          = "AIC",
    modelDir           = getOption("pfm.modelDir")
  )
})

# Compare best-model AIC across configurations
comparison <- lapply(names(results), function(nm) {
  bm <- results[[nm]]$bestModels
  bm$config <- nm
  bm[, c("config", "sector", "stage", "aic", "pseudoR2", "nPredictors")]
})
do.call(rbind, comparison)
```

Because all candidate models are cached in `modelDir`, overlapping candidates across configurations (e.g., the baseline model is the same in all three) are fitted only once.

---

## Group analysis

The `groupAnalysisTable` reports ANOVA-style deviance partitioning for the winning model, answering the question: *how much does each variable group contribute to the fit?*

```r
print(result$groupAnalysisTable)
```

| Column | Meaning |
|---|---|
| `group` | Variable group (Actor Power, IQ, Controls, Time Trend, FE) |
| `predictors` | Number of variables from this group in the winning model |
| `ANOVA Chi-Sq` | Deviance reduction from adding this group (drop-one test) |
| `ANOVA p-value` | Significance of the group's contribution |
| `Pseudo-R2 contribution` | Incremental McFadden R² attributable to this group |

A group with a significant chi-squared but small pseudo-R² has a statistically detectable but modest effect. A group with a large pseudo-R² contribution but non-significant chi-squared may be overfitting. Both are useful signals for deciding whether to keep or drop a variable group.

See [Tutorial 4](04-statistics-and-diagnostics.md) for a full guide to interpreting all the statistics.
