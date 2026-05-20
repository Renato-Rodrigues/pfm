# Tutorial 6: Running Projections

> Generating adoption and stringency predictions for future scenario years using REMIND output

## Table of Contents

1. [Overview](#overview)
2. [What you need](#what-you-need)
3. [Step 1 — Generate scenario panel data](#step-1--generate-scenario-panel-data)
4. [Step 2 — Load the fitted model](#step-2--load-the-fitted-model)
5. [Step 3 — Prepare prediction data](#step-3--prepare-prediction-data)
6. [Step 4 — Predict adoption probability](#step-4--predict-adoption-probability)
7. [Step 5 — Predict price stringency](#step-5--predict-price-stringency)
8. [Step 6 — Attach projections to the saved model](#step-6--attach-projections-to-the-saved-model)
9. [Full worked example](#full-worked-example)
10. [Projection years and SSP scenario](#projection-years-and-ssp-scenario)
11. [Reading projections back](#reading-projections-back)

---

## Overview

After fitting a PFM model on historical data, you can project adoption probabilities and price stringency forward to REMIND's scenario years. The inputs for projection come from REMIND's `fulldata.gdx` file — the same GDX that drives REMIND's energy system model.

Projection is a two-step process:

```
panelDataScenario(gdxFile)   →  scenario magpie object (drivers for future years)
  → preparePanelData()       →  flat data.frame aligned to the model's formula
    → predict(m$model, ...)  →  adoption probability / stringency per region-year
      → addProjections()     →  attaches results to the saved PFMModel
```

Projections are never computed at fit time because `fulldata.gdx` is produced by a separate REMIND run and may not be available when estimating the model.

---

## What you need

| Requirement | Notes |
|---|---|
| A fitted PFMModel | Either from `modelEstimationWorkflow()` or `estimateAdoptionModel()` / `estimatePriceStringencyModel()` |
| `fulldata.gdx` | REMIND run output — provides future energy system variables |
| madrat configured | Same cache and source folder settings used during estimation |
| `pfm.modelDir` set | So `addProjections()` can find and re-save the model |

---

## Step 1 — Generate scenario panel data

`panelDataScenario()` reads REMIND's GDX and assembles a magpie object containing future-year values of the same driver variables used in the historical panel:

```r
library(pfm)
options(pfm.modelDir = "models/")

madrat::setConfig(forcecache = TRUE, cachefolder = "path/to/madrat/cache")

future_mag <- panelDataScenario(
  gdxFile                 = "path/to/fulldata.gdx",
  outputRegionMappingFile = "regionmapping_54.csv"
)
```

The returned magpie object has dimensions:

```
[regions × scenario_years × variables]
```

Default scenario years are `2005, 2010, ..., 2060, 2070, ..., 2110, 2130, 2150`.

**Note:** Governance indicators (WGI, V-Dem) are not directly available in REMIND output. `panelDataScenario()` handles this by:

- **WGI indicators:** Using SSP2 projections from `SSPextensions` (Government Effectiveness, Control of Corruption, Rule of Law) where available; keeping the remaining WGI indicators constant at their last observed historical value.
- **V-Dem indicators:** Keeping constant (no SSP projections exist for V-Dem). The last historical value is extended forward.

---

## Step 2 — Load the fitted model

Load the model by its 12-character ID from `listPFMModels()`:

```r
# See all saved models
idx <- listPFMModels()
print(idx[, c("id", "sector", "stage", "aic", "converged")])

# Load specific models
m_bulk_adoption    <- loadPFMModel("a3f2c1b8e9d0")
m_bulk_stringency  <- loadPFMModel("7e91a4c23f1b")
m_diff_adoption    <- loadPFMModel("3c8d5e2a9f41")
m_diff_stringency  <- loadPFMModel("b6f1e0c74d28")
```

Each loaded model contains `m$model` — the raw `logistf` or `glm` object — which is what `predict()` uses.

---

## Step 3 — Prepare prediction data

`preparePanelData()` converts the scenario magpie object to a data.frame whose columns match the model's formula. Use the **same arguments** you passed during estimation:

```r
future_df_bulk <- preparePanelData(
  data                      = future_mag,
  sector                    = "Bulk",
  actorPowerDrivers         = NULL,
  actorPowerIndex           = "Actor Power Index",
  instQualityDrivers        = "Government Effectiveness (WGI)",
  controlDrivers            = "GDP per Capita",
  regionMappingFixedEffects = "regionmapping_EU_OECDp.csv",
  lag                       = 1
)

future_df_diffuse <- preparePanelData(
  data                      = future_mag,
  sector                    = "Diffuse",
  actorPowerDrivers         = NULL,
  actorPowerIndex           = "Actor Power Index",
  instQualityDrivers        = "Government Effectiveness (WGI)",
  controlDrivers            = "GDP per Capita",
  regionMappingFixedEffects = "regionmapping_EU_OECDp.csv",
  lag                       = 1
)
```

**Important:** The scenario data has no observed ECP, so `future_df$ecp` will be `NA` throughout. This is expected — ECP is the quantity being predicted, not a predictor.

---

## Step 4 — Predict adoption probability

The adoption model is a `logistf` object. Use `predict()` with `type = "response"` to get probabilities on [0, 1]:

```r
# Adoption probability: one value per region-year
future_df_bulk$adoption_prob <- predict(
  m_bulk_adoption$model,
  newdata = future_df_bulk,
  type    = "response"
)

future_df_diffuse$adoption_prob <- predict(
  m_diff_adoption$model,
  newdata = future_df_diffuse,
  type    = "response"
)
```

The output is the probability that a region has an ECP > 0 in that year, given its driver values.

### Confidence intervals for adoption

`logistf` does not natively support `se.fit`. Use the model's variance-covariance matrix and the delta method:

```r
# Linear predictor
X <- model.matrix(m_bulk_adoption$model)
# Note: you need the scenario design matrix — build it from the formula
fml_terms <- delete.response(terms(m_bulk_adoption$formula))
X_new <- model.matrix(fml_terms, data = future_df_bulk)
lp     <- as.numeric(X_new %*% coef(m_bulk_adoption$model))
se_lp  <- sqrt(diag(X_new %*% m_bulk_adoption$vcov %*% t(X_new)))

# Convert to probability scale (logistic CDF)
plogis_lo <- plogis(lp - 1.96 * se_lp)
plogis_hi <- plogis(lp + 1.96 * se_lp)

bulk_adoption_proj <- data.frame(
  region    = future_df_bulk$region,
  year      = future_df_bulk$year,
  estimate  = plogis(lp),
  ci_lo     = plogis_lo,
  ci_hi     = plogis_hi
)
```

---

## Step 5 — Predict price stringency

The stringency model is a `glm` object. `predict()` with `type = "response"` returns fitted values on the original scale (carbon price in $/tCO₂, or `log(1 + ECP)` if `logTransform = TRUE` was used at fit time):

```r
# Point estimate
bulk_str_pred <- predict(
  m_bulk_stringency$model,
  newdata = future_df_bulk,
  type    = "response",
  se.fit  = TRUE
)

bulk_stringency_proj <- data.frame(
  region   = future_df_bulk$region,
  year     = future_df_bulk$year,
  estimate = bulk_str_pred$fit,
  ci_lo    = bulk_str_pred$fit - 1.96 * bulk_str_pred$se.fit,
  ci_hi    = bulk_str_pred$fit + 1.96 * bulk_str_pred$se.fit
)
```

**Note on log-transform:** If the model was fitted with `logTransform = TRUE` (the default), the fitted values are on the `log(1 + ECP)` scale. To back-transform to $/tCO₂:

```r
bulk_stringency_proj$estimate <- exp(bulk_stringency_proj$estimate) - 1
bulk_stringency_proj$ci_lo    <- exp(bulk_stringency_proj$ci_lo)    - 1
bulk_stringency_proj$ci_hi    <- exp(bulk_stringency_proj$ci_hi)    - 1
```

**Conditional interpretation:** The stringency model is estimated only on observations where adoption = 1. The predicted price is therefore the *conditional* expected price — what price to expect **given** the region has a carbon pricing policy. To get an unconditional expected price, multiply by the adoption probability:

```r
bulk_stringency_proj$unconditional_estimate <-
  bulk_stringency_proj$estimate * future_df_bulk$adoption_prob
```

---

## Step 6 — Attach projections to the saved model

Bundle all projection data.frames into a list and attach them to each saved model using `addProjections()`. This re-saves the `.rds` file with the projections included:

```r
bulk_projections <- list(
  bulk_adoption    = bulk_adoption_proj,
  bulk_stringency  = bulk_stringency_proj,
  scenario_data_hash = digest::digest(future_mag, algo = "sha256"),
  gdx_path           = "path/to/fulldata.gdx"
)

addProjections(id = "a3f2c1b8e9d0", projections = bulk_projections)
addProjections(id = "7e91a4c23f1b", projections = bulk_projections)
```

After this call, `loadPFMModel("a3f2c1b8e9d0")$projections` will return the attached list.

---

## Full worked example

```r
library(pfm)
options(pfm.modelDir = "models/")

madrat::setConfig(forcecache = TRUE, cachefolder = "path/to/madrat/cache")

# --- 1. Scenario panel data ---
future_mag <- panelDataScenario(
  gdxFile                 = "path/to/fulldata.gdx",
  outputRegionMappingFile = "regionmapping_54.csv"
)

# --- 2. Load fitted models ---
idx               <- listPFMModels()
m_bulk_adopt      <- loadPFMModel(idx[idx$sector == "Bulk"    & idx$stage == "adoption",    "id"][1])
m_bulk_string     <- loadPFMModel(idx[idx$sector == "Bulk"    & idx$stage == "stringency",  "id"][1])
m_diff_adopt      <- loadPFMModel(idx[idx$sector == "Diffuse" & idx$stage == "adoption",    "id"][1])
m_diff_string     <- loadPFMModel(idx[idx$sector == "Diffuse" & idx$stage == "stringency",  "id"][1])

# --- 3. Prepare prediction data.frames ---
prep_args <- list(
  actorPowerDrivers         = NULL,
  actorPowerIndex           = "Actor Power Index",
  instQualityDrivers        = "Government Effectiveness (WGI)",
  controlDrivers            = "GDP per Capita",
  regionMappingFixedEffects = "regionmapping_EU_OECDp.csv",
  lag                       = 1
)

future_bulk    <- do.call(preparePanelData, c(list(data = future_mag, sector = "Bulk"),    prep_args))
future_diffuse <- do.call(preparePanelData, c(list(data = future_mag, sector = "Diffuse"), prep_args))

# --- 4. Adoption probabilities ---
future_bulk$adoption_prob    <- predict(m_bulk_adopt$model,  newdata = future_bulk,    type = "response")
future_diffuse$adoption_prob <- predict(m_diff_adopt$model,  newdata = future_diffuse, type = "response")

# --- 5. Stringency ---
bulk_str_pred    <- predict(m_bulk_string$model,  newdata = future_bulk,    type = "response", se.fit = TRUE)
diffuse_str_pred <- predict(m_diff_string$model,  newdata = future_diffuse, type = "response", se.fit = TRUE)

# Back-transform from log(1 + ECP) if logTransform = TRUE (default)
bulk_stringency_proj <- data.frame(
  region   = future_bulk$region,
  year     = future_bulk$year,
  estimate = exp(bulk_str_pred$fit) - 1,
  ci_lo    = exp(bulk_str_pred$fit - 1.96 * bulk_str_pred$se.fit) - 1,
  ci_hi    = exp(bulk_str_pred$fit + 1.96 * bulk_str_pred$se.fit) - 1
)
diffuse_stringency_proj <- data.frame(
  region   = future_diffuse$region,
  year     = future_diffuse$year,
  estimate = exp(diffuse_str_pred$fit) - 1,
  ci_lo    = exp(diffuse_str_pred$fit - 1.96 * diffuse_str_pred$se.fit) - 1,
  ci_hi    = exp(diffuse_str_pred$fit + 1.96 * diffuse_str_pred$se.fit) - 1
)

bulk_adoption_proj <- data.frame(
  region   = future_bulk$region,
  year     = future_bulk$year,
  estimate = future_bulk$adoption_prob
)
diffuse_adoption_proj <- data.frame(
  region   = future_diffuse$region,
  year     = future_diffuse$year,
  estimate = future_diffuse$adoption_prob
)

# --- 6. Attach to saved models ---
projections <- list(
  bulk_adoption      = bulk_adoption_proj,
  bulk_stringency    = bulk_stringency_proj,
  diffuse_adoption   = diffuse_adoption_proj,
  diffuse_stringency = diffuse_stringency_proj,
  scenario_data_hash = digest::digest(future_mag, algo = "sha256"),
  gdx_path           = "path/to/fulldata.gdx"
)

addProjections(id = m_bulk_adopt$id,   projections = projections)
addProjections(id = m_bulk_string$id,  projections = projections)
addProjections(id = m_diff_adopt$id,   projections = projections)
addProjections(id = m_diff_string$id,  projections = projections)

message("Projections attached to all four models.")
```

---

## Projection years and SSP scenario

`panelDataScenario()` uses **SSP2** for all socioeconomic variables (GDP per Capita, Population, Energy Intensity, Urban Share, Gini, Gender Inequality) unless the REMIND run was based on a different SSP. The Actor Power drivers (VRE share, electrification, coal share, etc.) come directly from the REMIND GDX and reflect the specific scenario run.

To see the default projection years:

```r
# Default years in panelDataScenario:
c(seq(2005, 2060, 5), seq(2070, 2110, 10), 2130, 2150)
```

To project over a subset of years (e.g., only to 2060):

```r
future_mag <- panelDataScenario(
  gdxFile = "path/to/fulldata.gdx",
  y       = seq(2005, 2060, 5),
  outputRegionMappingFile = "regionmapping_54.csv"
)
```

---

## Reading projections back

Once attached, projections can be retrieved directly from a loaded model:

```r
m <- loadPFMModel("a3f2c1b8e9d0")

# Check projections are present
is.null(m$projections)  # FALSE if addProjections() was called

# Access projection tables
head(m$projections$bulk_adoption)
head(m$projections$bulk_stringency)

# Traceability: which GDX file produced these projections?
m$projections$gdx_path
m$projections$scenario_data_hash
```

The `scenario_data_hash` is a SHA-256 of the `future_mag` object at the time projections were attached. If you re-run REMIND and get a different GDX, the new `panelDataScenario()` call will produce a different hash — a quick way to detect whether cached projections are still valid.
