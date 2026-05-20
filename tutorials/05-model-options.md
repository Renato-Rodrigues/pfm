# Tutorial 5: Model Options

> Comparing estimation families, fixed effects, Firth, time lag, and log-transform

## Table of Contents

1. [Overview](#overview)
2. [`useFirth` — Firth penalised likelihood](#usefirth--firth-penalised-likelihood)
3. [`family` — GLM family for stringency](#family--glm-family-for-stringency)
4. [`logTransform` — log-transform the dependent variable](#logtransform--log-transform-the-dependent-variable)
5. [`regionMappingFixedEffects` — fixed-effects resolution](#regionmappingfixedeffects--fixed-effects-resolution)
6. [`timeTrend` — linear time trend](#timetrend--linear-time-trend)
7. [`lag` — time lag for predictors](#lag--time-lag-for-predictors)
8. [`includeLaggedECP` / `includeLaggedAdoption` — state dependence](#includelaggedecp--includedlaggedadoption--state-dependence)
9. [`stabilityShift` — numerical stability](#stabilityshift--numerical-stability)
10. [Summary table](#summary-table)

---

## Overview

Every call to `modelEstimationWorkflow`, `modelSelectionWorkflow`, `estimateAdoptionModel`, and `estimatePriceStringencyModel` accepts options that affect how the model is specified and estimated. The defaults are chosen for a typical research run and rarely need changing, but understanding them is important for sensitivity analyses and edge cases.

---

## `useFirth` — Firth penalised likelihood

**Default:** `TRUE` for adoption, `FALSE` for stringency.

**What it does:**

- *Adoption (logit):* Uses `logistf::logistf()` instead of `stats::glm(binomial)`. Firth's method applies a Jeffrey's-prior penalty to the log-likelihood, producing finite coefficient estimates even when quasi-complete separation is present (see [Tutorial 4](04-statistics-and-diagnostics.md)).
- *Stringency (GLM):* Uses `brglm2::brglmFit` for bias-reduced estimation. Only relevant when the stringency sample is very small.

**When to set `useFirth = FALSE` for adoption:**

Only if your sample is large and well-balanced (adoption rate between 30% and 70%), and you have confirmed no separation. Standard MLE (`glm`) is faster and produces more interpretable standard errors under those conditions.

```r
# Standard logistic regression for adoption (no Firth)
estimateAdoptionModel(
  data     = panelData,
  sector   = "Bulk",
  useFirth = FALSE,
  ...
)
```

**Consequence of disabling on a separated dataset:** Coefficients will be ±∞, standard errors will be Inf, and z-values will be NaN. The model will be flagged as failed and rejected during selection.

---

## `family` — GLM family for stringency

**Default:** `"Gamma"`

**Options:** `"Gamma"`, `"gaussian"`

| Family | Link | Variance assumption | When to use |
|---|---|---|---|
| `"Gamma"` | log | Variance ∝ mean² | Carbon prices are positive, right-skewed, heteroskedastic. **Recommended.** |
| `"gaussian"` | log | Constant variance | Use if residual diagnostics show variance is approximately constant. |

Both families use a log link, so coefficients are interpreted identically (log-scale effect on price). The difference is solely in the assumed error distribution.

```r
# Gamma family (default)
estimatePriceStringencyModel(data = panelData, sector = "Bulk", family = "Gamma", ...)

# Gaussian family (OLS on log-prices)
estimatePriceStringencyModel(data = panelData, sector = "Bulk", family = "gaussian", ...)
```

**How to choose:** Fit both and compare AIC. If AIC is similar, prefer Gamma on theoretical grounds. A substantially lower Gaussian AIC suggests constant-variance is a better assumption for your specific data.

---

## `logTransform` — log-transform the dependent variable

**Default:** `TRUE`

Applies `ecp → log(1 + ecp)` to the stringency dependent variable before fitting.

| `logTransform` | `family` | Model estimated |
|---|---|---|
| `TRUE` | `Gamma` | Gamma regression on log(1 + ECP) |
| `TRUE` | `gaussian` | OLS on log(1 + ECP) |
| `FALSE` | `Gamma` | Gamma regression on raw ECP — most theoretically natural |
| `FALSE` | `gaussian` | OLS on raw ECP — rarely appropriate for price data |

**When to set `logTransform = FALSE`:** When using Gamma and you want coefficients on the natural (not doubly-logged) price scale, or when the distribution of positive prices in your sample closely follows a raw Gamma distribution without the additional log compression.

**Note:** With `logTransform = TRUE` and `includeLaggedECP = TRUE`, the lagged ECP predictor is also log-transformed to maintain scale consistency.

---

## `regionMappingFixedEffects` — fixed-effects resolution

**Default:** `"regionmappingH12.csv"` (12 broad REMIND regions)

Specifies the region grouping used to construct fixed-effect dummy variables. Fixed effects absorb all time-invariant heterogeneity within each group — culture, geography, historical institutions.

| File | Groups | Trade-off |
|---|---|---|
| `"regionmapping_EU_OECDp.csv"` | EU + OECD+ as one group | Fewest dummies (1–2); treats EU/OECD as homogeneous |
| `"regionmappingH12.csv"` | 12 REMIND macro-regions | Standard REMIND resolution; balanced |
| `"regionmapping_54.csv"` | 54 country-regions | Highest granularity; many dummies; high kOverN risk |
| `NULL` | No fixed effects | Pooled regression; tests whether FE matter |

**How to choose:**

- **`regionmapping_EU_OECDp.csv`** is the most commonly used in existing runs and adds only 1–2 dummies — a good default for models with limited predictors.
- **`regionmappingH12.csv`** matches the REMIND regional structure and is appropriate for production runs.
- **`regionmapping_54.csv`** should be avoided unless the sample is very large, as 54 dummies substantially raise kOverN.
- **`NULL`** is useful for a clean sensitivity test: does the conclusion change when you remove all regional heterogeneity?

```r
# EU+OECD fixed effect (recommended for exploratory runs)
estimateAdoptionModel(..., regionMappingFixedEffects = "regionmapping_EU_OECDp.csv")

# No fixed effects (sensitivity test)
estimateAdoptionModel(..., regionMappingFixedEffects = NULL)
```

---

## `timeTrend` — linear time trend

**Default:** `TRUE`

Adds a variable `timeTrend` (integer sequence 1, 2, 3, …) capturing any global secular trend not explained by the other variables — for example, the steady increase in carbon pricing adoption throughout the 2000s–2020s.

**When to set `timeTrend = FALSE`:**

- If you suspect the time trend is multicollinear with a predictor that also trends strongly over time (e.g., VRE share, which has grown nearly monotonically). In that case, separating the two effects is difficult and one should be dropped.
- If you are estimating on a very short time window where a trend adds little.
- To test whether observed dynamics are driven by time versus explanatory variables — if results change substantially when removing the trend, the other variables may be proxying for time.

---

## `lag` — time lag for predictors

**Default:** `1` (one year)

All predictor variables are shifted by `lag` years: the model predicts ECP in year t using driver values in year t − lag.

**Why lag?**

1. **Causality direction:** Policy adoption in year t is more plausibly shaped by governance conditions in t − 1 than contemporaneously.
2. **Simultaneity bias:** Carbon prices may themselves affect governance, energy shares, and GDP — a contemporaneous model picks up reverse causality.

**When to change:**

- `lag = 0`: Purely predictive exercise where causal ordering is not a concern.
- `lag = 2`: If political processes take more than one year to respond to driver changes.

```r
# Two-year lag
modelEstimationWorkflow(..., lag = 2)
```

---

## `includeLaggedECP` / `includeLaggedAdoption` — state dependence

**Defaults:** Both `FALSE`.

### `includeLaggedECP`

Adds the lagged carbon price (`lagged_ecp`) as a predictor in the **stringency** model. Captures path dependence — countries with higher prices last year tend to have higher prices this year.

- A significantly positive coefficient close to 1 means prices are highly persistent.
- Including this greatly improves in-sample fit and projection accuracy.
- Coefficients on other variables then represent effects *above and beyond* price persistence, not the total effect.

### `includeLaggedAdoption`

Adds lagged adoption status (`adoption_lagged`) as a predictor in the **adoption** model. Once a country has a carbon price, it almost always keeps it — policy persistence is very strong.

- Including this improves predictive accuracy dramatically.
- It makes other coefficients harder to interpret: they now represent the effect conditional on past adoption status, not the unconditional probability of initial adoption.

**When to include:**

| Goal | Recommendation |
|---|---|
| REMIND projection accuracy | Include both — persistence is real and important for forward projections |
| Structural interpretation of drivers | Exclude — coefficients represent unconditional adoption/stringency effects |
| Sensitivity analysis | Run both and compare |

```r
# Include state dependence for projection runs
modelEstimationWorkflow(
  ...,
  includeLaggedECP      = TRUE,
  includeLaggedAdoption = FALSE  # keep adoption model clean for structural interpretation
)
```

---

## `stabilityShift` — numerical stability

**Default:** `0`

Adds a small constant to the stringency dependent variable before Gamma regression: `ecp + stabilityShift`. This prevents numerical issues when ECP values are very close to zero after filtering for ECP > 0.

**When to use:** Only if the Gamma model produces convergence warnings on specific data vintages. A typical value is `0.1`. This slightly biases the intercept downward but does not materially affect other coefficients.

---

## Summary table

| Option | Default | Applies to | Controls |
|---|---|---|---|
| `useFirth` | `TRUE` | Adoption (+ Stringency if TRUE) | Firth vs MLE estimation |
| `family` | `"Gamma"` | Stringency | Error distribution |
| `logTransform` | `TRUE` | Stringency | Log-transform of ECP before fitting |
| `regionMappingFixedEffects` | `"regionmappingH12.csv"` | Both | Fixed-effects resolution |
| `timeTrend` | `TRUE` | Both | Include global time trend |
| `lag` | `1` | Both | Year lag for all predictors |
| `includeLaggedECP` | `FALSE` | Stringency | Price persistence |
| `includeLaggedAdoption` | `FALSE` | Adoption | Policy persistence |
| `stabilityShift` | `0` | Stringency | Numerical stability constant for Gamma |
| `criterion` | `"AIC"` | Selection only | Model selection criterion |
| `testMode` | `"incremental"` | Selection only | Search strategy (incremental vs combinations) |
