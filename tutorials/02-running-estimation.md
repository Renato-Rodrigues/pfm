# Tutorial 2: Running Model Estimation

> Fitting, saving, and reading a PFM model

## Table of Contents

1. [Setup](#setup)
2. [Loading panel data](#loading-panel-data)
3. [Running estimation](#running-estimation-modelestimationworkflow)
4. [Reading the results](#reading-the-results)
5. [Estimating a single sector-stage](#estimating-a-single-sector-stage)
6. [Saved models](#saved-models)
7. [Adding scenario projections](#adding-scenario-projections)

---

## Setup

```r
library(pfm)  # or devtools::load_all(".") from the package root

# Configure the model store — each fitted model is saved as its own .rds file.
# Models with the same formula + data are loaded from disk rather than re-fitted.
options(pfm.modelDir = "models/")
```

---

## Loading panel data

Panel data can be obtained two ways.

**Option A — from a pre-computed cache** (fastest, no madrat setup needed):

```r
load("path/to/modelData.RData")
panelData <- modelData$panelData
```

**Option B — compute fresh** (requires madrat configured with source data):

```r
madrat::setConfig(
  forcecache  = TRUE,
  cachefolder = "path/to/madrat/cache"
)
panelData <- panelDataHistorical(
  aggregate               = TRUE,
  y                       = 2000:2022,
  outputRegionMappingFile = "regionmapping_54.csv"
)
```

---

## Running estimation: `modelEstimationWorkflow`

`modelEstimationWorkflow` fits both stages (adoption + stringency) for each requested sector using a **fixed, pre-specified formula**. Use this when you already know which variables you want.

```r
result <- modelEstimationWorkflow(
  panelData                 = panelData,
  sectors                   = c("Bulk", "Diffuse"),

  # Actor Power: use the composite index (NULL = no individual AP drivers)
  actorPowerDrivers         = NULL,
  actorPowerIndex           = "Actor Power Index",

  # Institutional Quality
  instQualityDrivers        = "Government Effectiveness (WGI)",

  # Controls
  controlDrivers            = "GDP per Capita",

  # Region fixed effects grouping
  regionMappingFixedEffects = "regionmapping_EU_OECDp.csv",

  # Model store — saves each model as {id}.rds
  modelDir                  = getOption("pfm.modelDir")
)
```

This produces four models: Bulk adoption, Bulk stringency, Diffuse adoption, Diffuse stringency.

---

## Reading the results

```r
# Summary table: sector, stage, n observations, AIC, convergence
print(result$model_stats)

# Coefficient table: all sectors and stages combined
print(result$coefficients)

# Access a specific model's fit object
bulk_adoption <- result$models[["Bulk"]]$adoption
print(bulk_adoption$coeftest)   # robust coefficient table
print(bulk_adoption$formula)    # the formula used
```

---

## Estimating a single sector-stage

You can call the two underlying functions directly when you only need one sector or stage.

```r
# Stage 1: Adoption probability (Firth logit)
adoption_bulk <- estimateAdoptionModel(
  data                      = panelData,
  sector                    = "Bulk",
  actorPowerDrivers         = NULL,
  actorPowerIndex           = "Actor Power Index",
  instQualityDrivers        = "Government Effectiveness (WGI)",
  controlDrivers            = "GDP per Capita",
  regionMappingFixedEffects = "regionmapping_EU_OECDp.csv",
  modelDir                  = getOption("pfm.modelDir")
)

# Stage 2: Price stringency (Gamma GLM)
stringency_bulk <- estimatePriceStringencyModel(
  data                      = panelData,
  sector                    = "Bulk",
  family                    = "Gamma",
  actorPowerDrivers         = NULL,
  actorPowerIndex           = "Actor Power Index",
  instQualityDrivers        = "Government Effectiveness (WGI)",
  controlDrivers            = "GDP per Capita",
  regionMappingFixedEffects = "regionmapping_EU_OECDp.csv",
  modelDir                  = getOption("pfm.modelDir")
)
```

---

## Saved models

When `modelDir` is set, each model is saved automatically after fitting. The store contains:

```
models/
  a3f2c1b8e9d0.rds   # Bulk adoption
  7e91a4c23f1b.rds   # Bulk stringency
  ...
  index.json         # lightweight catalogue of all saved models
```

### Listing saved models

```r
idx <- listPFMModels()   # reads models/index.json
print(idx[, c("id", "sector", "stage", "aic", "pseudoR2", "converged")])
```

### Loading a saved model

```r
# Load by short ID (first 12 characters shown in the index)
m <- loadPFMModel("a3f2c1b8e9d0")
print(m)              # compact summary
summary(m)            # full details: coefficients, diagnostics, correlations
validate.PFMModel(m)  # checks package version match — important before REMIND coupling
```

### What each saved model contains

```r
m$id              # short cache key (12 chars)
m$id_full         # full SHA-256 hash
m$sector          # "Bulk" or "Diffuse"
m$stage           # "adoption" or "stringency"
m$formula         # the model formula
m$training_years  # c(2000, 2022)
m$training_data   # data.frame used for fitting (rows and columns actually passed to glm/logistf)
m$model           # raw logistf / glm object
m$coeftest        # robust coefficient table (estimate, SE, z, p)
m$vcov            # variance-covariance matrix
m$fitted_values   # in-sample fitted values
m$correlations    # list: $pearson and $spearman matrices of predictor variables
m$diagnostics     # AIC, BIC, pseudo-R2, VIF, separation, convergence, ...
m$projections     # NULL at fit time; populated by addProjections()
```

### Caching: skip re-fitting unchanged models

If you run the same script twice with the same data and formula, models already in `modelDir` are loaded from disk rather than re-fitted:

```
[cache hit] Loading adoption model a3f2c1b8e9d0 from disk.
```

The cache key is a SHA-256 hash of the formula and the training data. Any change to either — different variable set, different year range, updated data source — produces a new ID and triggers a fresh fit.

---

## Adding scenario projections

Projections require REMIND's `fulldata.gdx` and are added after fitting because they are not available at estimation time.

```r
# 1. Generate scenario panel data
scenario_data <- panelDataScenario(
  gdxFile                 = "path/to/fulldata.gdx",
  outputRegionMappingFile = "regionmapping_54.csv"
)

# 2. Produce predictions using the fitted model object
#    (use predict() on the raw model, or a custom prediction wrapper)
projections <- list(
  scenario_data_hash = digest::digest(scenario_data),
  gdx_path           = "path/to/fulldata.gdx",
  bulk_adoption      = data.frame(...)  # region, year, estimate, ci_lo, ci_hi
)

# 3. Attach to the saved model by its short ID — re-saves the .rds with projections added
addProjections(id = "a3f2c1b8e9d0", projections = projections)
```

See [Tutorial 3](03-model-selection.md) for model selection and [Tutorial 4](04-statistics-and-diagnostics.md) for how to interpret all output statistics.
