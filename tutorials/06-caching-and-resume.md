# Tutorial 6 — Caching and resume

How the Fit Cache works, why a run resumes for free, and how to run offline. (ADR 0009/0019.)

## Two distinct caches

1. **madrat data cache** — the `cachefolder` (config `cachefolder`, default `../data/cache`):
   the outputs of mrpfm's `calcOutput()`/`readSource()` (CarbonPrice, EDGAR, PE/FE, V-Dem, …).
   The compute layer runs with `forcecache = TRUE`, so it loads these by filename (type +
   args-hash) **without** the raw sources present. This is what lets the whole pipeline run
   offline.
2. **Fit Cache** — the `modelDir` (config `modelDir`, default `../output`): the model store
   (ADR 0009) holding `models/` + `panels/` + `index.json`; every fitted model keyed by a hash
   of its formula + training data.

These are **distinct** directories with distinct config keys — don't point one at the other.
On the cluster keep both on `/p/tmp`.

## Content-addressing → resume for free

Each fit's id is a hash of (formula + training data). On entry, `estimate*Model()` checks
`{modelDir}/models/{id}.rds`; if present, it **loads** instead of re-estimating. So:

- **Re-running the same group resumes automatically** — completed fits load instantly, only
  missing ones compute. No checkpoint file; the cache *is* the checkpoint.
- This is why a cluster job that hits its wall-time can just be **resubmitted** — it picks up
  where it stopped (Tutorial 2).
- The `[fits] N jobs: X newly fit, ~Y from cache` line tells you the split.

```bash
# first run computes everything; a re-run recomputes only what's missing:
Rscript start.R --group=exhaustive --mode=exhaustive --nCores=128
Rscript start.R --group=exhaustive --mode=exhaustive --nCores=128   # resumes
```

## Parallel-safe writing (ADR 0019)

Under parallel `future` workers, each worker writes only its own unique `{id}.rds`
(`savePFMModel(updateIndex = FALSE)`) — race-free, because filenames are unique. The **master**
rebuilds `index.json` **once** after the sweep. This avoids both the index write-race and the
O(n²) per-save churn.

A worker killed mid-write can leave a truncated `.rds`; the cache check treats an unreadable
fit as a **miss** and refits it, so a crash never poisons a resume.

## Forcing a clean rebuild

```bash
Rscript start.R --group=exhaustive --forceRefit      # ignore cache, re-estimate every spec
```
Use after an estimator change that should invalidate existing fits but doesn't change the hash
(rare). Normally let resume do its job — it's faster and safe.

## Repairing the index

If a run is interrupted, the `.rds` fits survive but `index.json` may be stale. Rebuild it from
the fits on disk:
```r
pfm::rebuildPFMModelIndex("cache")   # or your modelDir
```
The sweep does this automatically at the end of `runFitGrid`; you only call it by hand to repair
after an abnormal exit. Unreadable fits are skipped with a warning.

## The shared training panel

`runSweep` stores the panel once (content-addressed, `panels/panel_<hash>.rds`) via
`saveTrainingPanel`, and each saved fit references it by hash instead of embedding its own copy
— keeping the store slim (ADR 0009).

## Which cache files do I need to run offline?

`MRPFM_EXTERNAL_CACHE_DEPS.md` lists the external (Tier-1) madrat caches the panel build needs
(EDGAR, PE, FE, Ember, GDP/Population, V-Dem, …). Place them in the `cachefolder` (default
`../data/cache`); with those present and `forcecache = TRUE` (the compute layer sets this),
`runSweep` rebuilds the panel and runs without any raw source folders. The Fit Cache (`modelDir`,
default `../output`) is created/extended by the run itself.
