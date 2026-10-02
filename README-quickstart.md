## Quick start

The whole pipeline — data to model to the folder REMIND reads — runs through one
function. With no arguments it asks for each setting and shows the options:

```r
pfm::pfmRun()
```

Non-interactively:

```r
pfmRun(group = "v6", stage = "all")          # sweep + downstream + export
pfmRun(group = "v6", stage = "downstream")   # from a finished sweep
pfmRun(group = "v6", stage = "remind")       # export REMIND inputs only
pfmRun(group = "v6", stage = "all", dryRun = TRUE)   # show the plan
```

Stages: `all`, `sweep`, `downstream`, `remind`, `custom`. On a cluster it submits with
`sbatch` by default; `cluster = "local"` runs in the current session.

Fits and panels are cached across Run-Groups, so a new group over an unchanged panel
reuses previous estimations. When inputs change underneath a finished group, delete
before rebuilding — `resume` only checks that a file exists, not that it is still
valid:

```r
pfmRun(group = "v6", stage = "downstream", clean = "steps")
```

See `vignette("pfm-pipeline", package = "pfm")` for the full walkthrough, including
what to clean when, and how to hand the model to REMIND.
