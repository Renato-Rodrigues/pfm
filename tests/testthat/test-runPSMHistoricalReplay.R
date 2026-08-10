# Historical-replay gate (TODO 2.3): does the coupling reproduce the past at least
# as well as the uncoupled dynamics? Deliberately weak to pass, decisive to fail —
# the ceiling can only bind downward, so a coupling that fires spuriously shows up
# immediately as a worse RMSE.

replayGroup <- function(dir, withFrontier = TRUE) {
  gd <- file.path(dir, "grp")
  dir.create(gd, recursive = TRUE, showWarnings = FALSE)
  spec <- list(
    name = "T-0001 test spec",
    actorPowerDrivers = "Actor Power Index", actorPowerIndex = "Actor Power Index",
    instQualityDrivers = "Rule of Law (VDem)", controlDrivers = NULL,
    regionMappingFixedEffects = NULL, panelTransform = "levels",
    logisticTimeTrend = FALSE, estimator = "satP", indexMax = 10
  )
  yaml::write_yaml(lapply(c("Bulk", "Diffuse"), function(s) {
    e <- spec
    e$model_type <- paste0("PolicyStringency: ", s)
    e
  }), file.path(gd, "selected-models-psm.yml"))
  if (withFrontier) {
    ct <- data.frame(term = "(Intercept)", estimate = 0.8, stringsAsFactors = FALSE)
    saveRDS(list(bySector = list(Bulk = list(coefTable = ct),
                                 Diffuse = list(coefTable = ct))),
            file.path(gd, "frontier.rds"))
  }
  gd
}

# The shared fixture is Bulk-only; give Diffuse the same series so both sectors run.
replayPanel <- function() {
  m <- makePSMagpie()
  d <- m[, , "Policy Stringency|Bulk"]
  magclass::getNames(d) <- "Policy Stringency|Diffuse"
  a <- m[, , "Actor Power Index|Bulk"]
  magclass::getNames(a) <- "Actor Power Index|Diffuse"
  magclass::mbind(m, d, a)
}

test_that("the replay scores the coupling against the ECM and persistence", {
  dir <- withr::local_tempdir()
  gd <- replayGroup(dir)
  res <- runPSMHistoricalReplay("grp", resultsDir = dir, modelDir = NULL,
                                panelData = replayPanel(), verbose = FALSE)
  expect_true(file.exists(file.path(gd, "historical-replay.rds")))
  expect_setequal(names(res$bySector), c("Bulk", "Diffuse"))
  m <- res$bySector$Bulk$metrics
  expect_true(all(c("n", "rmseCoupled", "rmseEcm", "rmsePersistence",
                    "skillVsEcm", "skillVsPersistence", "pass") %in% names(m)))
  expect_gt(m$n, 0)
  expect_true(all(vapply(list(m$rmseCoupled, m$rmseEcm, m$rmsePersistence),
                         is.finite, logical(1))))
  expect_true(m$ceilingAvailable)
})

test_that("without a frontier the coupled and uncoupled paths are identical", {
  dir <- withr::local_tempdir()
  replayGroup(dir, withFrontier = FALSE)
  res <- runPSMHistoricalReplay("grp", resultsDir = dir, modelDir = NULL,
                                panelData = replayPanel(), verbose = FALSE)
  m <- res$bySector$Bulk$metrics
  # No ceiling => nothing can bind => the gate degenerates to an identity check,
  # which is REPORTED (ceilingAvailable = FALSE) rather than silently "passed".
  expect_false(m$ceilingAvailable)
  expect_equal(m$rmseCoupled, m$rmseEcm, tolerance = 1e-10)
  expect_true(m$pass)
  expect_equal(m$skillVsEcm, 0, tolerance = 1e-10)
})

test_that("a binding ceiling can only hurt the replay, and the gate catches it", {
  dir <- withr::local_tempdir()
  gd <- replayGroup(dir)
  # A ceiling far below the data: the coupling is forced to under-predict, so the
  # gate must FAIL. This is the failure mode the gate exists to catch.
  ct <- data.frame(term = "(Intercept)", estimate = -3, stringsAsFactors = FALSE)
  saveRDS(list(bySector = list(Bulk = list(coefTable = ct),
                               Diffuse = list(coefTable = ct))),
          file.path(gd, "frontier.rds"))
  res <- runPSMHistoricalReplay("grp", resultsDir = dir, modelDir = NULL,
                                panelData = replayPanel(), verbose = FALSE)
  m <- res$bySector$Bulk$metrics
  expect_gt(m$ceilingBindShare, 0)
  expect_gt(m$rmseCoupled, m$rmseEcm)
  expect_false(m$pass)
  expect_false(res$pass)
  expect_lt(m$skillVsEcm, 0)
})

test_that("the gate tolerance is respected and the step record is written", {
  dir <- withr::local_tempdir()
  gd <- replayGroup(dir)
  ct <- data.frame(term = "(Intercept)", estimate = -3, stringsAsFactors = FALSE)
  saveRDS(list(bySector = list(Bulk = list(coefTable = ct),
                               Diffuse = list(coefTable = ct))),
          file.path(gd, "frontier.rds"))
  strict <- runPSMHistoricalReplay("grp", resultsDir = dir, modelDir = NULL,
                                   panelData = replayPanel(), verbose = FALSE)
  file.remove(file.path(gd, "historical-replay.rds"))
  loose <- runPSMHistoricalReplay("grp", resultsDir = dir, modelDir = NULL,
                                  panelData = replayPanel(), tolerance = 10,
                                  verbose = FALSE)
  expect_false(strict$pass)
  expect_true(loose$pass)
  expect_true(file.exists(file.path(gd, "manifest.json")))
})

test_that("a group without a deployed spec skips cleanly", {
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "grp"), recursive = TRUE, showWarnings = FALSE)
  expect_null(runPSMHistoricalReplay("grp", resultsDir = dir, modelDir = NULL,
                                     panelData = replayPanel(), verbose = FALSE))
})

test_that("psm-replay is a recognised pipeline step", {
  # It is in the ALLOWED set (allSteps) and has an artifact mapping, not in the
  # short default `steps` argument.
  body <- paste(deparse(runModelGroup), collapse = " ")
  expect_true(grepl('"psm-replay"', body, fixed = TRUE))
  expect_true(grepl("historical-replay.rds", body, fixed = TRUE))
  expect_true(grepl('"psm-replay"', paste(deparse(startRun), collapse = " "),
                    fixed = TRUE))
})
