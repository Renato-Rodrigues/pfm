# Tutorial 2 — Running on the PIK cluster (SLURM)

How to run the exhaustive sweep as a SLURM job on the PIK HPC2024 cluster, and how to
confirm it's healthy. Reference: `docs/utils/HPC2024 User Guide.pdf`.

## What the cluster looks like

- Login via `hpc.pik-potsdam.de` → one of `login01-03` (64-core interactive nodes — **not** for production runs).
- Compute nodes: **128 cores** (2×64 AMD Genoa), 768 GB RAM.
- Partitions: `standard` (production), `priority`, `io`, `gpu`. QOS: `short` (24 h), `medium` (7 d), `long` (30 d), `priority`.
- **Run from `/p/tmp/$USER`** — the parallel filesystem. `$HOME` is not for heavy job I/O.
- Tools: `sbatch` (submit), `squeue` (queue), `scancel` (cancel), `sacct` (history).

The PFM sweep is a **single-node, multicore** R job (the `future` workers, ADR 0019) — no
MPI, no job array needed. One node, up to 128 cores.

## One-time setup

**1. Put the repo, cache, and gdx on `/p/tmp`** (rsync from your machine, per the guide):
```bash
rsync -avzP _code/ hpc.pik-potsdam.de:/p/tmp/$USER/elevate/_code/
```
You need `mrpfm/`, `pfm/`, `pfm-reports/`, the populated **madrat cache**, and (optionally,
for the Projection-Sanity gate) `pfm-reports/data/fulldata.gdx`.

**2. Install packages into the cluster R library** (on a login node — light, has internet):
```bash
module load R                       # check `module avail R` for the exact name
cd /p/tmp/$USER/elevate/_code
Rscript -e 'install.packages(c("future","future.apply"), repos="https://cloud.r-project.org")'
Rscript -e 'devtools::install("mrpfm", upgrade="never"); devtools::install("pfm", upgrade="never")'
```

**3. Point `config.yml` at `/p/tmp` paths** (`pfm-reports/config.yml`):
```yaml
modelDir:   "/p/tmp/<you>/elevate/cache"
resultsDir: "/p/tmp/<you>/elevate/results"
gdxPath:    "data/fulldata.gdx"
group:      "exhaustive"
```
> Keep `modelDir` and `resultsDir` on `/p/tmp`, not `$HOME` — the guide is explicit that heavy
> job I/O on `$HOME` can disrupt the cluster.

## Submit

From the **pfm-reports** working directory on a login node:

```bash
cd /p/tmp/$USER/elevate/_code/pfm-reports
Rscript start.R --group=exhaustive --mode=exhaustive --nCores=128
```

- `start.R` sees `sbatch` on PATH and no `SLURM_JOB_ID`, so it **submits** rather than running
  in place. It prints `submitted batch job <JOBID>` and records the job in the manifest.
- **Pass `--nCores=128` explicitly.** Without it, it sizes to the *login* node (64) and
  under-requests the 128-core compute node. The job sets `--cpus-per-task=<nCores>`.
- Defaults: `--qos=short` (24 h), `--partition=standard`, 1 node × 1 task × 128 cpus,
  `--chdir=/p/tmp/$USER/pfm-runs/exhaustive`. Override any with `--qos=`, `--partition=`,
  `--time=`, `--account=`, `--mem=`.
- **Don't add `--render` on the cluster** — render reports locally after pulling results back
  (rendering needs pandoc set up on the node). See the pfm-reports tutorials.

The submission is lightweight: the login node only writes the submit script — the panel build
and all fitting happen on the compute node once the job starts.

## Is it running fine?

**Right after submit — is it queued/running?**
```bash
squeue -u $USER
```
`ST`: `PD` = pending, `R` = running. If it stays `PD`, the `NODELIST(REASON)` column says why
(`Priority`/`Resources` = normal queueing; anything else needs attention).

**While running — watch live progress.** R progress messages go to **stderr**, so the live
log is the `.err` file in the job's `--chdir`:
```bash
cd /p/tmp/$USER/pfm-runs/exhaustive
tail -f pfm-exhaustive-<JOBID>.err
```
Healthy progression:
- `[runSweep:exhaustive] Building historical panel ...` then `Building scenario panel ...`
- `[fits] parallel backend: multisession x 128 worker(s)`
- `[fits]   N/3860` climbing steadily (≈965 specs × 4 sector/stages)
- `[fits] 3860 jobs: N newly fit, ~M from cache, 0 failed.`
- then `robustness` / `temporal` / `subnational` steps, then `DONE`.

A nonzero **failed** count, or no new `[fits]` lines for a long stretch, is the signal to look
at the `.err` traceback.

**Structured status (any time):**
```bash
Rscript status.R --group=exhaustive
```
Reads the run record **and** queries SLURM live: `manifest status: running`,
`SLURM live: RUNNING (via squeue)`, per-step timings, steps remaining. After completion:
`manifest status: completed`, `SLURM live: COMPLETED (via sacct)`.

**After it ends:**
```bash
sacct -j <JOBID> --format=JobID,State,Elapsed,MaxRSS
ls /p/tmp/$USER/elevate/results/exhaustive/    # sweep.rds, selected-models.yml, ..., manifest.json
```
`State = COMPLETED` + a `sweep.rds` present = success.

## If it hits the wall-time

If the 24 h `short` limit is reached mid-run, **just resubmit the identical command** — the
content-addressed Fit Cache makes it resume, recomputing only the missing fits (the `.err` will
show a high "from cache" count). So `short` is usually fine even for a cold full sweep; use
`--qos=medium --time=2-00:00:00` only if you'd rather not resubmit. See Tutorial 6.

## Cancel / re-run a single step

```bash
scancel <JOBID>                                        # cancel
Rscript start.R --group=exhaustive --steps=robustness  # re-run just one step (resumes the rest)
```

## Smoke test first (recommended)

The local path is fully validated; the live `sbatch` submission is exercised here for the
first time. A 1-step guided submit confirms the whole chain before the big run:
```bash
Rscript start.R --group=guided --mode=guided --steps=sweep --nCores=16
Rscript status.R --group=guided
```
A clean `results/guided/sweep.rds` and status means the exhaustive submit will behave the same.

## Pull results back to render

```bash
# from your local machine
rsync -avzP hpc.pik-potsdam.de:/p/tmp/$USER/elevate/results/ ./pfm-reports/results/
# then render locally (see the pfm-reports tutorials):
Rscript reports/selection/run.R --group=exhaustive
```

## Note on the R module inside the job

The generated submit script calls `Rscript` directly and SLURM inherits your submit-time
environment, so if `module load R` is active when you submit, the job finds R. If your site
needs the module loaded *inside* the job, ask to have an optional `--modules="R"` preamble added
to the sbatch script.
