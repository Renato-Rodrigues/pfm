# Tutorial 3 — Options, alternatives, and model groups

Every knob you can turn when running a model group, and what each one does.

## The entry points

| Function / script | Use it for |
|---|---|
| `start.R` / `pfm::startRun()` | The launcher. Local-vs-SLURM detection, core sizing, optional report render. |
| `status.R` / `pfm::runStatus()` | Inspect a run (folder + live SLURM). |
| `pfm::runModelGroup()` | Run the steps in order, no SLURM logic (what `startRun` calls locally). |
| `pfm::runSweep()` | Just the sweep + selection. |
| `pfm::runRobustness/runTemporalSplit/runSubnational/runDifferenceFirst()` | A single post-processing step on an existing Run-Group. |

`start.R` flags map onto `startRun()` arguments; `runModelGroup`'s `...` flows to `runSweep`,
whose `...` flows to `runChannelsWorkflow`. So every option below is reachable from the CLI.

## Run-Group (`--group`)

A **named** folder under the Results Root. Multiple groups coexist — the exhaustive
deliverable and any experiment side by side.

```bash
Rscript start.R --group=exhaustive                  # the deliverable
Rscript start.R --group=no-fe54 --selectFE=H12,OECDp,Mundlak   # an experiment
```
`status.R --group=<name>` and every report's `--group=<name>` read that specific group.

## `--mode` — guided vs exhaustive

| Mode | Specs | Use |
|---|---|---|
| `guided` | ~14 curated specs | fast iteration, smoke tests, teaching. |
| `exhaustive` | ~965 specs (15 IQ × 2 AP × 8 controls × FE/transform axes + lag/FD) | the production sweep. |

```bash
Rscript start.R --group=guided --mode=guided
Rscript start.R --group=exhaustive --mode=exhaustive --nCores=128
```

## `--steps` — which stages to run

`sweep`, `robustness`, `temporal`, `subnational` (in that order; default = all four).
`sweep` must have run before the others (they read its `selected-models.yml`).

```bash
Rscript start.R --group=exhaustive --steps=sweep                 # sweep + selection only
Rscript start.R --group=exhaustive --steps=robustness,temporal   # post-processing on an existing sweep
```
`runDifferenceFirst` is not in the default chain (it's an alternative selection comparison —
see below); invoke it on its own when you want it.

## `--selectionMethod` — how the deliverable is chosen

| Method | What it does | ADR |
|---|---|---|
| `levels-first` (default) | Maximin over the levels specs, then the Projection-Sanity gate. Writes `selected-models.yml`. | 0012 |
| `difference-first` | Maximin over `hybridFD` specs → Falsification Gate (re-fit under `pureFD`: Actor Power must persist, Institutional Quality must vanish) → re-estimate the winner in levels. Writes `selected-models-difference-first.yml`. Never overwrites the levels-first deliverable. | 0014 |

```bash
Rscript start.R --group=exhaustive --selectionMethod=difference-first
# or, as a post-hoc comparison on an existing sweep (does not re-fit the sweep):
Rscript run-difference-first.R --group=exhaustive --maxtries=25
```

## `--selectFE` — restrict the deliverable's fixed-effects resolution

Limit *which* region-FE strategies are eligible for selection (selection-only — the full sweep
results still include every spec). The defensible block FEs are `H12`, `OECDp`, `Mundlak`;
excluding `noFE` (pooled) and `FE54` (inflation-prone 54-unit) is the standard choice
(`FULL_RERUN_DECISIONS.md` #17).

```bash
Rscript start.R --group=exhaustive --selectFE=H12,OECDp,Mundlak
```

## `--nCores` — parallelism (ADR 0019)

- Default (omitted): `SLURM_CPUS_PER_TASK` if set, else `detectCores() - 1`.
- `1` = sequential (identical results, no parallel dependency).
- `> 1` = parallel `future.apply` (`multisession` on Windows, `multicore` on Linux).
- On the cluster pass `--nCores=128` to use the full compute node.
- Small runs may be faster sequential (worker-spawn overhead); the payoff is at sweep scale.

## `--forceRefit` — ignore the cache

By default a re-run **resumes**: cached fits load instantly, only missing ones compute.
`--forceRefit` re-estimates every spec, overwriting stale fits. Use after you suspect a fit is
stale (e.g. an estimator change). See Tutorial 6.

```bash
Rscript start.R --group=exhaustive --forceRefit
```

## `--render` — render reports after compute

When set (and run with a reports directory — `start.R` uses the current pfm-reports dir),
`startRun` shells out to the report `run.R`s for the group after the compute finishes. Local
only; on the cluster, render after pulling results back. See the pfm-reports tutorials.

## SLURM directives (cluster only)

`--qos` (`short`/`medium`/`long`/`priority`), `--partition` (`standard`/`priority`),
`--time` (e.g. `2-00:00:00`), `--account`, `--mem`. Defaults: `short` / `standard` / `24:00:00`
/ default account / node-default memory. See Tutorial 2.

## Deeper sweep knobs (R only)

These are `runChannelsWorkflow` arguments, reachable by calling `runSweep(...)` in R (not all
are wired to CLI flags):

- `sectors` — default `c("Bulk","Diffuse")`.
- `y` — training years, default `2000:2022`.
- `family` — stringency GLM family (default `"gaussian"`, i.e. identity link on `log(1+ECP)`).
- `nearTieEps` — BIC parsimony tie-break tolerance (ADR 0012), default `0.05`.
- `sanityBatchSize` / `sanityMaxModels` / `sanityThresholds` — the Projection-Sanity gate's
  expanding-batch window and rule thresholds.
- difference-first: `requireBothSectors`, `falsificationPThreshold`, `maxFalsificationTries`,
  `iqVanishTest` (`"jointBlock"` | `"perChannel"`), `levelsFE`.

```r
library(pfm)
runSweep("exhaustive", mode = "exhaustive", nCores = 128,
         resultsDir = "results", cacheDir = "cache", gdxFile = "data/fulldata.gdx",
         selectFE = c("H12","OECDp","Mundlak"), nearTieEps = 0.10)
```

## Custom experiment groups — the pattern

```bash
# baseline deliverable
Rscript start.R --group=exhaustive --mode=exhaustive --nCores=128

# sensitivity: difference-first selection, same fits reused from cache
Rscript start.R --group=exhaustive-dif --mode=exhaustive --nCores=128 \
        --selectionMethod=difference-first

# compare in the reports
Rscript reports/selection/run.R --group=exhaustive
Rscript reports/selection/run.R --group=exhaustive-dif
```
Because the Fit Cache is shared across groups, the second run reuses the first run's fits — only
the selection differs.
