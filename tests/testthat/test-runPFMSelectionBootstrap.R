# PFM selection-uncertainty bootstrap under Tournament v2 (2026-07-17).
# The fixture pool is all-Blue (helper-pfm.R), so the runs here pass
# tierGate = "Blue" like pfmTestSweep; the Green-gate default emptying the
# fixture pool is asserted explicitly at the end.

test_that("PFM selection bootstrap runs, caches, extends, and tracks the deployed spec", {
  resultsDir <- withr::local_tempdir()
  modelDir <- withr::local_tempdir()
  sw <- pfmTestSweep("psm-boot", resultsDir, modelDir)
  deployed <- sw$selected[["PolicyStringency"]]
  expect_true(nzchar(deployed))

  # No panelData: exercises the offline path (manifest panel_hash -> loadTrainingPanel).
  res <- suppressMessages(suppressWarnings(runPFMSelectionBootstrap(
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
  expect_gt(length(list.files(file.path(modelDir, "boot-cache"), pattern = "^pfmboot_")), 0)
  # Step recorded in the manifest.
  mf <- jsonlite::fromJSON(file.path(resultsDir, "psm-boot", "manifest.json"))
  expect_true("selection-bootstrap" %in% names(mf$steps))
  expect_identical(mf$steps[["selection-bootstrap"]]$status, "completed")

  # Extension: a larger nResamples reuses the cached rows and appends the rest;
  # the deterministic draws make resamples 1-5 identical across runs.
  res2 <- suppressMessages(suppressWarnings(runPFMSelectionBootstrap(
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
  resG <- suppressMessages(suppressWarnings(runPFMSelectionBootstrap(
    group = "psm-boot", resultsDir = resultsDir, modelDir = modelDir,
    nResamples = 5L, topK = 5L, tierGate = "Green", verbose = FALSE
  )))
  expect_gt(resG$gateEmptyShare, 0)
  expect_identical(is.na(resG$perResample$winner), resG$perResample$nGatePass == 0L)
  expect_true(all(resG$perResample$deployedRank[resG$perResample$deployedGatePass] >= 1))
})

test_that("the bootstrap cache keeps actor-power transform twins apart (PITFALLS 29)", {
  # v6 (2026-10-05): the linear / satAP / satInn / satInc twins of one spec differ ONLY in
  # apTransform. The key ignored it, so the twins shared one cache file, the first twin's rows
  # served all four under its own name, and the deployed X-1791 satAP showed 0% wins.
  base <- list(actorPowerDrivers = c("Innovator Power", "Incumbent Power", "Incumbent Power pc"),
               actorPowerIndex = c("Innovator Power", "Incumbent Power", "Incumbent Power pc"),
               instQualityDrivers = "Government Effectiveness (WGI)", controlDrivers = "GDP per Capita (Q-centred)",
               regionMappingFixedEffects = "regionmapping_EU_OECDp.csv", logisticTimeTrend = TRUE)
  tf <- c("linear", "saturating", "saturating-innovator", "saturating-incumbent")
  keys <- vapply(tf, function(t) pfm:::.pfmBootCacheKey(c(base, list(apTransform = t)), "Bulk", "h", 1L), "")
  expect_length(unique(keys), 4)
  # a spec without the field is the linear one
  expect_identical(pfm:::.pfmBootCacheKey(base, "Bulk", "h", 1L), keys[["linear"]])
})

test_that("a cache file holding another spec's rows is refit, never relabelled", {
  resultsDir <- withr::local_tempdir()
  modelDir <- withr::local_tempdir()
  pfmTestSweep("psm-boot-poison", resultsDir, modelDir)
  run <- function() suppressMessages(suppressWarnings(runPFMSelectionBootstrap(
    group = "psm-boot-poison", resultsDir = resultsDir, modelDir = modelDir,
    nResamples = 4L, topK = 5L, tierGate = "Blue", verbose = FALSE)))
  res <- run()
  files <- list.files(file.path(modelDir, "boot-cache"), pattern = "^pfmboot_", full.names = TRUE)
  expect_gt(length(files), 0)
  # Poison every cache file: same key, rows labelled as some other model.
  for (f in files) { d <- readRDS(f); d$model <- "X-9999 POISON"; saveRDS(d, f) }
  res2 <- run()
  expect_false(any(res2$perResample$winner %in% "X-9999 POISON"))
  expect_identical(res2$perResample$winner, res$perResample$winner)
  # ... and the refit rewrote the files under their own spec names
  expect_false(any(vapply(files, function(f) any(readRDS(f)$model == "X-9999 POISON"), logical(1))))
})

test_that("PFM selection bootstrap skips cleanly without a sweep artifact", {
  resultsDir <- withr::local_tempdir()
  modelDir <- withr::local_tempdir()
  dir.create(file.path(resultsDir, "psm-empty"))
  res <- suppressMessages(runPFMSelectionBootstrap(
    group = "psm-empty", resultsDir = resultsDir, modelDir = modelDir,
    nResamples = 3L, verbose = FALSE
  ))
  expect_null(res)
  expect_false(file.exists(file.path(resultsDir, "psm-empty", "selection-bootstrap.rds")))
  mf <- jsonlite::fromJSON(file.path(resultsDir, "psm-empty", "manifest.json"))
  expect_identical(mf$steps[["selection-bootstrap"]]$status, "skipped")
})
