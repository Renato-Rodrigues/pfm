# Tutorial 7 — Troubleshooting

The failure modes you'll actually hit, and the fix.

## "could not find function" / wrong behaviour after editing pfm

The running session uses the **installed** package, not the source files. After any change to
`mrpfm`/`pfm` R code:
```r
devtools::install("pfm", upgrade = "never", quick = TRUE)   # then restart R / re-run
```

## Panel build fails / "Manual download of SSP data required" / non-conformable

The compute layer runs offline from the madrat cache, but only if the needed caches exist and
`forcecache = TRUE`. `runSweep`/`runModelGroup` set `forcecache` for you. If you call lower-level
functions directly, set it yourself:
```r
madrat::setConfig(forcecache = TRUE)
```
If a specific `calcOutput`/`readSource` still tries to recompute, the matching cache file is
missing or its args-hash changed — check `MRPFM_EXTERNAL_CACHE_DEPS.md` and confirm the file is
in `cachefolder`. (A request for *all five SSPs* triggers an SSP download; the PFM panel only
needs SSP2 — if you see this, you're on stale code, reinstall.)

## "No selected-models.yml in ..." when running a post-processing step

The post-processing steps read the sweep's deliverable. Run the sweep first:
```bash
Rscript start.R --group=<g> --steps=sweep
```

## Sweep is slow / parallel is slower than sequential

On a small run, spawning fresh R workers (no `fork` on Windows) costs more than the few fits
save. Use `--nCores=1` for guided/small runs; reserve high core counts for the exhaustive sweep.
On the cluster, pass `--nCores=128` to use the full node (the default would size to the login
node's 64).

## SLURM job stuck in PD (pending)

`squeue -u $USER` → `NODELIST(REASON)`. `Priority`/`Resources` is normal queueing. Anything
else (e.g. `QOSMaxCpuPerJobLimit`) means your request exceeds the QOS — lower `--nCores` or pick
a different `--qos`. The `short` QOS allows up to 2048 CPU/job, so 128 is fine.

## SLURM job ran but produced nothing / no R output

The generated submit script calls `Rscript` directly. If `module load R` wasn't active at
submit time, the job can't find R. Re-`module load R` and resubmit, or ask for an optional
`--modules=` preamble to be added to the sbatch script. Check the `.err` file in the job's
`--chdir` (`/p/tmp/$USER/pfm-runs/<group>/`) — R progress goes to **stderr**.

## Job hit the wall-time and was killed

Not a problem — **resubmit the same command**. The content-addressed Fit Cache resumes,
recomputing only the missing fits (Tutorial 6). Or submit with `--qos=medium --time=2-00:00:00`.

## Out-of-memory (OOM)

The panel is tiny; OOM is unlikely on a 768 GB node. If it happens with very high `--nCores`
(each PSOCK worker copies the panel), lower `--nCores` or request `--mem` explicitly.

## status.R shows `running` but the job is gone

A crash leaves `manifest status="running"` (it never reached the `completed`/`failed` write).
`status.R` cross-checks SLURM: if it reports `SLURM live: FAILED/COMPLETED` while the manifest
says `running`, the job died mid-step. The completed steps' `.rds` files are still valid — fix
the cause and resume.

## A handful of fits report "failed" in the sweep summary

`[fits] ... N failed` means those spec×sector×stage fits errored (e.g. separation a given
transform couldn't handle). The sweep continues; failed fits become gate-failing rows and rank
last. Inspect the `.err`/console for the per-spec `FAILED ...` messages to see which and why —
a few failures in a large sweep are normal and don't block selection.

## Reports can't find the artifacts

Reports read `results/<group>/`. Confirm the group ran (`status.R --group=<g>`) and that the
report's `--group=` matches. Direct "Knit" in RStudio uses the Rmd `params:` defaults
(`results/exhaustive/...`); pass `--group` via `run.R` for any other group.

## Where to look first

1. `Rscript status.R --group=<g>` — the run record + live SLURM state.
2. The `.err` file in the job `--chdir` (cluster) or the console (local) — progress + tracebacks.
3. `manifest.json` in the Run-Group — per-step status and metrics.
