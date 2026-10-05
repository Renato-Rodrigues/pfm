# nolint start
# The whole bootstrap pool gets a sanity verdict (pfm-sanity-pool), so the bootstrap's
# conditional winners exclude every spec the gates reject - not only those the deployment
# walk happened to reach. On v6 (2026-10-05), 43% of conditional wins went to specs with a
# linear innovator term, which the actor-power gate rejects but the walk never evaluated.

test_that("the sanity walk can evaluate every candidate instead of stopping at the first pass", {
  # B extrapolates actor power far beyond support (severe); A and C are clean.
  local_mocked_bindings(
    projectPFMSpecScenario = function(cfg, sector, ...) {
      data.frame(region = "R1", year = c(2030, 2050, 2070), index = 5, outOfCoverage = FALSE,
                 driverOutOfSupport = 0, driverAPExcess = if (cfg$name == "B") 5 else 0)
    },
    computePolicyStringencySanity = function(proj, ...) {
      list(summary = list(nSevere = 0L, nWarning = 0L), flags = data.frame())
    },
    .package = "pfm")
  specs <- list(A = list(name = "A"), B = list(name = "B"), C = list(name = "C"))
  walk <- function(stop) pfm:::.pfmSanitySelect(
    passModels = c("A", "B", "C"), specByName = specs, sectors = "Bulk", panelData = NULL,
    scenarioData = NULL, modelDir = NULL, batchSize = 3, maxModels = 3, thresholds = list(),
    regionBlocks = NULL, histIndexBySector = list(Bulk = NULL), ceilingFallGate = NA_real_,
    apExtrapolationGate = 0.275, stopAtFirstPass = stop)
  first <- walk(TRUE)
  expect_identical(first$chosen, "A")
  expect_identical(first$trace$model, "A")                 # the deployment walk: unchanged
  all3 <- walk(FALSE)
  expect_identical(all3$chosen, "A")                       # still the first that passed
  expect_identical(all3$trace$model, c("A", "B", "C"))     # ... but every model has a verdict
  expect_identical(all3$trace$pass, c(TRUE, FALSE, TRUE))
  expect_match(paste(all3$flags$B$rule, collapse = ","), "actorPowerExtrapolation")
})

test_that("the bootstrap excludes pool specs that sanity-pool.rds rejects, and reports unscreened ones", {
  resultsDir <- withr::local_tempdir(); modelDir <- withr::local_tempdir()
  pfmTestSweep("psm-pool", resultsDir, modelDir)
  run <- function() suppressMessages(suppressWarnings(runPFMSelectionBootstrap(
    group = "psm-pool", resultsDir = resultsDir, modelDir = modelDir,
    nResamples = 4L, topK = 5L, tierGate = "Blue", verbose = FALSE)))
  res <- run()
  # The fixture sweep has no scenario panel, so no walk: every pool spec is unscreened.
  expect_setequal(res$sanityUnwalked, res$pool)
  top <- names(res$specFreq)[1]
  # A pool verdict rejecting the most frequent winner, and passing the rest.
  saveRDS(list(verdicts = data.frame(model = res$pool, pass = res$pool != top, stringsAsFactors = FALSE)),
          file.path(resultsDir, "psm-pool", "sanity-pool.rds"))
  res2 <- run()
  expect_true(top %in% res2$sanityRejected)
  expect_false(any(res2$perResample$winnerConditional %in% top))
  expect_length(res2$sanityUnwalked, 0)
  # unconditional winners do not move
  expect_identical(res2$perResample$winner, res$perResample$winner)
})

test_that("pfm-sanity-pool is a registered step, before the bootstrap, with its own artifact", {
  expect_identical(pfmStepArtifacts("pfm-sanity-pool")[[1]], "sanity-pool.rds")
  for (f in list(startRun, runModelGroup, pfmRun)) {
    body <- paste(deparse(f), collapse = "\n")
    expect_match(body, "pfm-sanity-pool", fixed = TRUE)
  }
  # downstream stage and runModelGroup's execution: sanity-pool precedes the bootstrap
  rmg <- paste(deparse(runModelGroup), collapse = "\n")
  expect_lt(regexpr("doStep(\"pfm-sanity-pool\")", rmg, fixed = TRUE),
            regexpr("doStep(\"pfm-selection-bootstrap\")", rmg, fixed = TRUE))
  # the scenario inputs reach it through runModelGroup's dots filter
  expect_true(all(c("referenceGdxFile", "referenceGdxRegionMappingFile", "gdxRegionMappingFile",
                    "outputRegionMappingFile") %in% names(formals(runPFMSanityPool))))
})

test_that("pfm-sanity-pool skips cleanly without a sweep", {
  resultsDir <- withr::local_tempdir(); modelDir <- withr::local_tempdir()
  dir.create(file.path(resultsDir, "psm-nopool"))
  expect_null(suppressMessages(runPFMSanityPool("psm-nopool", resultsDir = resultsDir, modelDir = modelDir)))
  mf <- jsonlite::fromJSON(file.path(resultsDir, "psm-nopool", "manifest.json"))
  expect_identical(mf$steps[["sanity-pool"]]$status, "skipped")
})
# nolint end
