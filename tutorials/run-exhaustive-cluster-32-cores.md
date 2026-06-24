# Run the exhaustive sweep on the PIK cluster — 32 cores

Complete steps to run the **exhaustive** sweep on the PIK cluster with **32 cores**.

> Note: PFM is a single-node multicore job — 32 cores will use only a quarter of a 128-core
> compute node, which is fine, just slower than `--nCores=128`.

## 1. One-time setup (on a login node)

```bash
# a) Get the repo + populated madrat cache + gdx onto the parallel filesystem
rsync -avzP _code/ hpc.pik-potsdam.de:/p/tmp/$USER/elevate/_code/

# b) Install packages into the cluster R library (login node has internet)
module load R                      # confirm exact name with: module avail R
cd /p/tmp/$USER/elevate/_code
Rscript -e 'install.packages(c("future","future.apply"), repos="https://cloud.r-project.org")'
Rscript -e 'devtools::install("mrpfm", upgrade="never"); devtools::install("pfm", upgrade="never")'
Rscript -e 'devtools::install("pfm-reports", upgrade="never")'   # pfmreports, for rendering later
```

Point `pfm-reports/config.yml` at `/p/tmp` paths (keep heavy I/O off `$HOME`):

```yaml
modelDir:   "/p/tmp/<you>/elevate/output"
resultsDir: "/p/tmp/<you>/elevate/output"
cachefolder: "data/cache"
gdxPath:    "data/fulldata.gdx"
group:      "exhaustive"
```

## 2. Smoke test first (recommended — exercises the live `sbatch` path)

```bash
cd /p/tmp/$USER/elevate/_code/pfm-reports
Rscript start.R --group=guided --mode=guided --steps=sweep --nCores=16
Rscript status.R --group=guided
```

A clean `output/guided/sweep.rds` + healthy status means the big submit will behave the same.

## 3. Submit the exhaustive sweep — 32 cores

```bash
cd /p/tmp/$USER/elevate/_code/pfm-reports
Rscript start.R --group=exhaustive --mode=exhaustive \
  --steps=sweep,robustness,temporal,subnational --nCores=32
```

- `start.R` sees `sbatch` on PATH (and no `SLURM_JOB_ID`) → it **submits** rather than running
  in place, printing `submitted batch job <JOBID>`.
- `--nCores=32` becomes `#SBATCH --cpus-per-task=32` in the generated script (sized correctly —
  without it, it would size to the *login* node's core count).
- **Pass `--steps=…` explicitly** — `start.R`'s default is `sweep` only; the four steps above
  produce every artifact the reports consume.
- Defaults applied: `--qos=short` (24 h), `--partition=standard`, 1 node × 1 task × 32 cpus,
  `--chdir=<resultsDir>/exhaustive`. Override with `--qos=`, `--time=`, `--account=`, `--mem=`.
- **Don't add `--render`** on the cluster — render locally after pulling results back
  (rendering needs pandoc).

## 4. Confirm it's healthy

```bash
squeue -u $USER                                  # PD=pending, R=running
cd /p/tmp/$USER/elevate/output/exhaustive
tail -f pfm-exhaustive-<JOBID>.err               # live progress (R logs to stderr)
Rscript status.R --group=exhaustive              # structured status + live SLURM query
```

Healthy log progression:

- `Building historical panel …` → `Building scenario panel …`
- `[fits] parallel backend: multisession x 32 worker(s)`
- `[fits]   N/3860` climbing steadily (~965 specs × 4 sector/stages)
- `[fits] 3860 jobs: … 0 failed.` → `robustness` → `temporal` → `subnational` → `DONE`

A nonzero **failed** count, or `[fits]` stalling for a long stretch, is your cue to read the
`.err` traceback.

## 5. After it finishes

```bash
sacct -j <JOBID> --format=JobID,State,Elapsed,MaxRSS    # want State=COMPLETED
ls /p/tmp/$USER/elevate/output/exhaustive/             # sweep.rds, selected-models.yml, …, manifest.json
```

Then pull results back and render locally:

```bash
# on your local machine, from _code/pfm-reports
rsync -avzP hpc.pik-potsdam.de:/p/tmp/$USER/elevate/output/ ./output/
Rscript inst/render.R --group=exhaustive --outputDir=output
```

## Two notes specific to 32 cores

- **Wall time:** 32 cores is ~4× slower per fit-throughput than 128. A cold full sweep may
  approach the 24 h `short` limit. If it hits the wall, **just resubmit the identical command**
  — the content-addressed Fit Cache resumes and recomputes only the missing fits. Or pre-empt
  it with `--qos=medium --time=2-00:00:00`.
- If your site requires `module load R` *inside* the job (not just at submit time), the
  generated sbatch script currently relies on the submit-time environment being inherited; an
  optional `--modules` preamble can be added to load it inside the job.
