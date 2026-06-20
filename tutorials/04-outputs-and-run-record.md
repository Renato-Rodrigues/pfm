# Tutorial 4 — Outputs and the run record

What a run produces. This is the **compute-output contract** the report layer reads. (ADR 0018/0020.)
For *what each report shows and how to read it*, see the **pfm-reports tutorials**.

## The Run-Group folder

Every run writes to `results/<group>/`:

| File | Written by | Contents |
|---|---|---|
| `sweep.rds` | `runSweep` | Every fit's metrics (`results`), per-term estimates (`coefficients`), Stage-0 `screens`, `maximin` rankings, `selected` deliverable, `sanity` trace, `bestPerSector`, `fitSummary`. |
| `selected-models.yml` | `runSweep` | The chosen shared spec per stage (both sectors), report-ready. **The deliverable.** |
| `channels-<mode>.yml` | `runSweep` | The full sweep config that was fit. |
| `selected-models-difference-first.yml` | `runSweep`/`runDifferenceFirst` (difference-first) | The difference-first deliverable. |
| `difference-first.rds` | `runDifferenceFirst` | Falsification-gate diagnostics + the levels-first gate comparison. |
| `robustness.rds` | `runRobustness` | Robustness Ladder, parsimony frontier, control spec-curve, LORO. |
| `temporal-split.rds` | `runTemporalSplit` | Out-of-time train/test diagnostics. |
| `subnational.rds` | `runSubnational` | National vs full-coverage price comparison + sensitivity. |
| `manifest.json` | every step | The **run record** (below). |

These are the only files the report layer consumes. Anything a report needs that isn't here is a
sign a step hasn't run (or you're pointing at the wrong group).

## The run record (`manifest.json`)

The authoritative record of a run (ADR 0020). It carries:

- **Provenance** — pfm/mrpfm versions, training-panel hash, training years, scenario/gdx, mode, selection method.
- **`run` block** — `status` (`submitted`/`running`/`completed`/`failed`), start/end time, wall-clock seconds, host, cluster (`local`/`slurm`), SLURM job id, nCores.
- **`steps`** — per step: `{started, ended, seconds, status, metrics}`. (sweep: `nJobs`/`nNew`/`nFailed`/backend; robustness: ladder/frontier/curve/loco counts; temporal/subnational: key counts.)

Inspect it:
```bash
Rscript status.R --group=<name>      # human summary + live SLURM state
```
```r
jsonlite::fromJSON("results/<group>/manifest.json")   # raw
pfm::runStatus("<group>", resultsDir = "results")     # parsed list
```

A crashed run leaves `status="running"` with only the completed steps recorded — itself the
failure signal (Tutorial 7).

## Reading these into the reports

The report layer is a pure consumer: each report reads one or more of the files above for a
chosen `--group`. To produce reports from a finished Run-Group, and to understand **what each
report shows, why, and what you can conclude**, see:

- `pfm-reports/tutorials/` — the report-oriented guides.

A quick reminder of the render step (from the pfm-reports working directory):
```bash
Rscript reports/selection/run.R   --group=<name>
Rscript reports/robustness/run.R  --group=<name>
# ... etc; or run.R via start.R --render
```
