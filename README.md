# Political Feasibility Module (PFM) for IAMs

R package **pfm**, version **0.3.0**

   [![R build status](https://github.com/pik-piam/pfm/workflows/check/badge.svg)](https://github.com/pik-piam/pfm/actions) [![codecov](https://codecov.io/gh/pik-piam/pfm/branch/master/graph/badge.svg)](https://app.codecov.io/gh/pik-piam/pfm) 

## Purpose and Functionality

Econometric model for the political feasibility of climate policy in integrated
    assessment models. The deployed model is the Policy Stringency Model (PSM): a
    single-stage bounded index estimated on OECD CAPMF stringency scores for the Bulk and
    Diffuse sectors, with a stochastic-frontier feasibility ceiling and an error-correction
    adjustment speed. Projects a politically feasible carbon-price bound and couples
    iteratively with REMIND. The earlier two-stage hurdle price model (adoption +
    stringency) is retained for comparison. Consumes magpie objects from mrpfm.

## Quick start

The whole pipeline — data to model to the folder REMIND reads — runs through one
function. With no arguments it asks for each setting and shows the options:

```r
pfm::pfmRun()
```

Non-interactively:

```r
pfmRun(group = "psm-country-v5", stage = "all")          # sweep + downstream + export
pfmRun(group = "psm-country-v5", stage = "downstream")   # from a finished sweep
pfmRun(group = "psm-country-v5", stage = "remind")       # export REMIND inputs only
pfmRun(group = "psm-country-v5", stage = "all", dryRun = TRUE)   # show the plan
```

Stages: `all`, `sweep`, `downstream`, `remind`, `custom`. On a cluster it submits with
`sbatch` by default; `cluster = "local"` runs in the current session.

Fits and panels are cached across Run-Groups, so a new group over an unchanged panel
reuses previous estimations. When inputs change underneath a finished group, delete
before rebuilding — `resume` only checks that a file exists, not that it is still
valid:

```r
pfmRun(group = "psm-country-v5", stage = "downstream", clean = "steps")
```

See `vignette("pfm-pipeline", package = "pfm")` for the full walkthrough, including
what to clean when, and how to hand the model to REMIND.

## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r
install.packages("pfm")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r
update.packages()
```

## Questions / Problems

In case of questions / problems please contact Renato Rodrigues <renato.rodrigues@pik-potsdam.de>.

## Citation

To cite package **pfm** in publications use:

Rodrigues R, Kriegler E (2026). "pfm: Political Feasibility Module (PFM) for IAMs." Version: 0.3.0, <https://github.com/pik-piam/pfm>.

A BibTeX entry for LaTeX users is

 ```latex
@Misc{,
  title = {pfm: Political Feasibility Module (PFM) for IAMs},
  author = {Renato Rodrigues and Elmar Kriegler},
  date = {2026-06-20},
  year = {2026},
  url = {https://github.com/pik-piam/pfm},
  note = {Version: 0.3.0},
}
```
