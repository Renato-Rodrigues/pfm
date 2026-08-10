# PSM selection-uncertainty bootstrap under Tournament v2 (2026-07-17).
# The fixture pool is all-Blue (helper-psm.R), so the runs here pass
# tierGate = "Blue" like psmTestSweep; the Green-gate default emptying the
# fixture pool is asserted explicitly at the end.

test_that("PSM selection bootstrap runs, caches, extends, and tracks the deployed spec", {
  resultsDir <- withr::local_tempdir()
  modelDir <- withr::local_tempdir()
  sw <- psmTestSweep("psm-boot", resultsDir, modelDir)
  deployed <- sw$selected[["PolicyStringency"]]
  expect_true(nzchar(deployed))

  # No panelData: exercises the offline path (manifest panel_hash -> loadTrainingPanel).
  res <- suppressMessages(suppressWarnings(runPSMSelectionBootstrap(
    group = "psm-boot", resultsDir = resultsDir, modelDir = modelDir,
    nResamples = 5L, topK = 5L, tierGate = "Blue", verbose = FALSE
  )))
  expect_true(file.exists(file.path(resultsDir, "psm-boot", "selection-bootstrap.rds")))
  expect_identical(res$stage, "PolicyStringency")
  expect_identical(res$deployed, deployed)
  expect_identical(res$nEffective, nrow(res$perResample))
  expect_lte(nrow(res$perResample), 5L)
  expect_true(all(c("resample", "winner", "winnerConditional", "nGatePass",
                    "deployedGatePass", "deployedRank") %in% names(res$perResample)))
  # No sanity walk in the fixture sweep -> no rejected specs -> conditional == unconditional.
  expect_length(res$sanityRejected, 0)
  expect_identical(res$perResample$winnerConditional, res$perResample$winner)
  expect_true(res$deployedGatePassShare >= 0 && res$deployedGatePassShare <= 1)
  expect_true(is.na(res$deployedWinShare) || (res$deployedWinShare >= 0 && res$deployedWinShare <= 1))
  # Winner frequencies are proper shares over effective resamples.
  if (!is.null(res$specFreq)) expect_lte(sum(res$specFreq), 1 + 1e-9)
  # v2 knobs are recorded for provenance.
  expect_identical(res$knobs$rankBy, "worseDeltaR2")
  expect_identical(res$knobs$tierGate, "Blue")
  # Per-spec resample caches were written.
  expect_gt(length(list.files(file.path(modelDir, "boot-cache"), pattern = "^psmboot_")), 0)
  # Step recorded in the manifest.
  mf <- jsonlite::fromJSON(file.path(resultsDir, "psm-boot", "manifest.json"))
  expect_true("selection-bootstrap" %in% names(mf$steps))
  expect_identical(mf$steps[["selection-bootstrap"]]$status, "completed")

  # Extension: a larger nResamples reuses the cached rows and appends the rest;
  # the deterministic draws make resamples 1-5 identical across runs.
  res2 <- suppressMessages(suppressWarnings(runPSMSelectionBootstrap(
    group = "psm-boot", resultsDir = resultsDir, modelDir = modelDir,
    nResamples = 7L, topK = 5L, tierGate = "Blue", verbose = FALSE
  )))
  expect_lte(nrow(res2$perResample), 7L)
  expect_gt(nrow(res2$perResample), nrow(res$perResample))
  shared <- merge(res$perResample, res2$perResample, by = "resample")
  expect_identical(shared$winner.x, shared$winner.y)

  # Under the Green deployment gate (ADR 0039 default) the mostly-Blue fixture
  # pool empties in most resamples (the full-sample pool is all-Blue, but a
  # resampled draw can promote an AP-x-IQ interaction to significance, so an
  # occasional Green resample is legitimate). Assert structural consistency:
  # a resample has a winner exactly when some spec passed the gate.
  resG <- suppressMessages(suppressWarnings(runPSMSelectionBootstrap(
    group = "psm-boot", resultsDir = resultsDir, modelDir = modelDir,
    nResamples = 5L, topK = 5L, tierGate = "Green", verbose = FALSE
  )))
  expect_gt(resG$gateEmptyShare, 0)
  expect_identical(is.na(resG$perResample$winner), resG$perResample$nGatePass == 0L)
  expect_true(all(resG$perResample$deployedRank[resG$perResample$deployedGatePass] >= 1))
})

test_that("PSM selection bootstrap skips cleanly without a sweep artifact", {
  resultsDir <- withr::local_tempdir()
  modelDir <- withr::local_tempdir()
  dir.create(file.path(resultsDir, "psm-empty"))
  res <- suppressMessages(runPSMSelectionBootstrap(
    group = "psm-empty", resultsDir = resultsDir, modelDir = modelDir,
    nResamples = 3L, verbose = FALSE
  ))
  expect_null(res)
  expect_false(file.exists(file.path(resultsDir, "psm-empty", "selection-bootstrap.rds")))
  mf <- jsonlite::fromJSON(file.path(resultsDir, "psm-empty", "manifest.json"))
  expect_identical(mf$steps[["selection-bootstrap"]]$status, "skipped")
})
