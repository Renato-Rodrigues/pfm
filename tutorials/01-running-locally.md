# Tutorial 1 — Running locally

How to run a PFM model group on your own machine and render the reports. This is the
fast path for development, small runs, and rendering.

## Prerequisites (one time)

1. **R + the PIK package repo.** In `~/.Rprofile`:
   ```r
   options(repos = c(CRAN = "https://cloud.r-project.org",
                     pik  = "https://rse.pik-potsdam.de/r/packages"))
   ```
2. **Install the packages** (from the `_code` directory):
   ```r
   install.packages(c("future", "future.apply"))     # parallel backend (Tutorial 3)
   devtools::install("mrpfm", upgrade = "never")
   devtools::install("pfm",   upgrade = "never")
   ```
   > After **any** change to `mrpfm`/`pfm` R code, reinstall before running — the session
   > uses the installed package, not the source files.
3. **A populated madrat cache.** The compute layer runs *offline from the madrat cache*
   (`forcecache = TRUE`), so the cache files must already exist. See `MRPFM_EXTERNAL_CACHE_DEPS.md`
   for the list. Point `config.yml` at it (next step).
4. **`config.yml`** in the `pfm-reports` root (copy `config.yml.example`):
   ```yaml
   modelDir:   "cache"      # Fit Cache + madrat cache root (relative to pfm-reports, or absolute)
   resultsDir: "results"    # Run-Group artifacts land here
   gdxPath:    "data/fulldata.gdx"   # for the Projection-Sanity gate (optional)
   group:      "exhaustive" # default Run-Group the reports render against
   ```

## The one-liner

From the **pfm-reports** working directory:

```bash
Rscript start.R --group=guided --mode=guided --nCores=4
```

`start.R` detects it's a local terminal (no SLURM) and runs **in-process**. It builds the
panel, runs the sweep + selection, and the post-processing steps, writing everything to
`results/guided/`. Check it:

```bash
Rscript status.R --group=guided
```

For the full run, use `--mode=exhaustive` (this is heavy locally — prefer the cluster, Tutorial 2):

```bash
Rscript start.R --group=exhaustive --mode=exhaustive --nCores=8
```

## What just happened

`start.R` → `pfm::startRun()` → `pfm::runModelGroup()`, which runs these steps in order,
each writing one artifact into `results/<group>/`:

| Step | Function | Artifact |
|---|---|---|
| sweep + selection | `runSweep` | `sweep.rds`, `selected-models.yml`, `channels-<mode>.yml` |
| robustness | `runRobustness` | `robustness.rds` |
| temporal split | `runTemporalSplit` | `temporal-split.rds` |
| subnational | `runSubnational` | `subnational.rds` |

…plus a `manifest.json` run record (status, timings, per-step metrics — see Tutorial 4).

## Running from R instead of the CLI

The CLI just wraps exported functions; you can call them directly:

```r
library(pfm)
options(pfm.modelDir = "cache", pfm.resultsDir = "results")

# whole pipeline:
runModelGroup("guided", mode = "guided", nCores = 4,
              gdxFile = "data/fulldata.gdx")

# or one step at a time (re-run just robustness without redoing the sweep):
runSweep("guided", mode = "guided", nCores = 4, gdxFile = "data/fulldata.gdx")
runRobustness("guided", quick = TRUE)
runStatus("guided")
```

## Choosing `nCores`

- `nCores = 1` (default) runs sequentially — identical results, no parallel dependency.
- `nCores > 1` parallelises the fits via `future.apply`. On a laptop, `4`–`8` is sensible.
- For a **small** run, parallel can be *slower* than sequential because spawning fresh R
  worker processes (Windows has no `fork`) costs more than the few fits save. The payoff
  is at sweep scale (thousands of fits). So for a quick guided run, `nCores = 1` is fine.

See Tutorial 3 for every option, and Tutorial 6 for how reruns resume from cache.

## Rendering reports

Reports are a **separate, pure-consumer step** (the compute layer never renders). After a
run, from the pfm-reports root:

```bash
Rscript reports/selection/run.R          --group=guided
Rscript reports/model-selection/run.R    --group=guided
Rscript reports/results-adoption/run.R   --group=guided
Rscript reports/results-stringency/run.R --group=guided
Rscript reports/publication/run.R        --group=guided
Rscript reports/robustness/run.R         --group=guided
Rscript reports/subnational/run.R        --group=guided
```

HTML lands in `pfm-reports/output/`. To have `startRun` render automatically after the
compute finishes, add `--render` (it shells out to these `run.R` scripts):

```bash
Rscript start.R --group=guided --mode=guided --nCores=4 --render
```

For **what each report shows, why, and what you can conclude**, see the **pfm-reports
tutorials** (`pfm-reports/tutorials/`). Tutorial 4 here documents the artifacts the reports read.
