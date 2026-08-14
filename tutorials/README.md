# pfm — Tutorials

How to **run** the PFM compute layer and **understand** what it estimates. `pfm` holds all the
execution code (panel build, sweep, selection, post-processing, projections), so these guides
live with it.

> Rendering and **interpreting reports** is the job of the **pfm-reports** repo (a pure
> consumer). For what each report shows and what you can conclude, see `pfm-reports/tutorials/`.

## What pfm does

`pfm` is the **compute layer**. From one R session you run a whole **model group**:

```r
library(pfm)
runModelGroup("exhaustive", mode = "exhaustive", nCores = 128,
              resultsDir = "../output", modelDir = "../output",
              cachefolder = "../data/cache", gdxFile = "../data/fulldata.gdx")
```

This builds the panel (from the madrat cache), runs the sweep + selection, and the robustness /
temporal / subnational steps — writing curated artifacts into a **Run-Group** (`results/<group>/`).
The `start.R` launcher (in `pfm/inst/`, copied to the pfm-reports working dir) wraps this with
local-vs-SLURM detection and core sizing. `pfm` does **no rendering**.

## Tutorials

**Running**
1. [01-running-locally.md](01-running-locally.md) — install, configure, run a model group on your machine.
2. [02-running-on-the-cluster.md](02-running-on-the-cluster.md) — submit as a SLURM job on the PIK cluster and confirm it's healthy.
3. [03-options-and-model-groups.md](03-options-and-model-groups.md) — every option and alternative: modes, selection methods, steps, FE restriction, cores, resume, custom groups.
4. [04-outputs-and-run-record.md](04-outputs-and-run-record.md) — the Run-Group layout and the manifest run-record (the contract the reports read).

**Understanding**
5. [05-understanding-the-model.md](05-understanding-the-model.md) — the two-stage hurdle model, channels, theory tiers, the Maximin rule, and the gates.
6. [06-caching-and-resume.md](06-caching-and-resume.md) — the Fit Cache, content-addressing, resume after a crash, running offline.
7. [07-troubleshooting.md](07-troubleshooting.md) — the execution failure modes and how to fix them.

**Reference (deep dives)**
8. [08-statistics-and-diagnostics.md](08-statistics-and-diagnostics.md) — interpreting coefficients, AIC/BIC/AICc/HQIC, pseudo-R², VIF, separation, convergence, overfitting.
9. [09-projections.md](09-projections.md) — generating adoption & stringency projections from a REMIND GDX.

Read 01 → 07 in order the first time; 08–09 are reference deep-dives.

## Where the authoritative detail lives

- **`CONTEXT.md`** (repo root) — the glossary of domain terms.
- **`docs/adr/`** — architecture decisions: `0018` (compute/report layering + Run-Group), `0019` (parallel sweep + cache concurrency), `0020` (run entry point + SLURM + run record), and the model-design ADRs `0004`–`0017`.
- **`docs/reference/HPC2024 User Guide.pdf`** — the PIK cluster reference.
- **`pfm-reports/tutorials/`** — how to render and **interpret** the results.
