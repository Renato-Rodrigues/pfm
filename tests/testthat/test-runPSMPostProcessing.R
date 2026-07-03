# Fixtures (makePSMSweepMagpie, makePSMSweepScenarioMagpie, psmTestSpecs,
# psmTestSweep) live in helper-psm.R.

test_that("runPSMProjection fans the deployed spec out over the scenario registry", {
  resultsDir <- withr::local_tempdir()
  modelDir <- withr::local_tempdir()
  scen <- makePSMSweepScenarioMagpie()
  psmTestSweep("psm-post", resultsDir, modelDir, scenarioData = scen)

  registry <- list(
    npi = list(id = "npi", name = "National Policies", prebuilt = scen, gating = FALSE),
    amb = list(id = "amb", name = "Climate Ambition", prebuilt = scen, gating = TRUE)
  )
  res <- suppressMessages(suppressWarnings(runPSMProjection(
    group = "psm-post", resultsDir = resultsDir, modelDir = modelDir,
    panelData = makePSMSweepMagpie(), scenarios = registry, verbose = FALSE
  )))
  expect_setequal(names(res), c("npi", "amb"))

  groupDir <- file.path(resultsDir, "psm-post")
  expect_true(file.exists(file.path(groupDir, "projections", "npi.rds")))
  expect_true(file.exists(file.path(groupDir, "projections", "amb.rds")))
  # gating scenario also lands on the canonical single-projection path
  expect_true(file.exists(file.path(groupDir, "projection.rds")))
  gp <- readRDS(file.path(groupDir, "projection.rds"))
  expect_true(all(gp$scenario == "amb"))
  expect_identical(unique(gp$scenarioName), "Climate Ambition")

  # canonical output + Implementability Factor columns, bounded on both scales
  expect_true(all(c("region", "year", "sector", "eta", "index", "indexLo", "indexHi",
                    "outOfCoverage", "implementability") %in% names(gp)))
  expect_setequal(unique(gp$sector), c("Bulk", "Diffuse"))
  ok <- is.finite(gp$index)
  expect_true(any(ok))
  expect_true(all(gp$index[ok] >= 0 & gp$index[ok] <= 10))
  okI <- is.finite(gp$implementability)
  expect_true(all(gp$implementability[okI] >= 0 & gp$implementability[okI] <= 1))
  expect_equal(gp$implementability[okI], gp$index[okI] / 10, tolerance = 1e-12)
  # every training region is in coverage in this fixture
  expect_false(any(gp$outOfCoverage))
  # CI brackets the point projection where present
  okCI <- ok & is.finite(gp$indexLo) & is.finite(gp$indexHi)
  expect_true(any(okCI))
  expect_true(all(gp$indexLo[okCI] <= gp$index[okCI] + 1e-12))

  # manifest records the completed projection step with the scenario set
  man <- jsonlite::fromJSON(file.path(groupDir, "manifest.json"), simplifyVector = FALSE)
  expect_identical(man$steps$projection$status, "completed")
  expect_identical(man$steps$projection$metrics$gatingScenario, "amb")
})

test_that("runPSMProjection is skipped gracefully without any scenario source", {
  resultsDir <- withr::local_tempdir()
  modelDir <- withr::local_tempdir()
  psmTestSweep("psm-noscen", resultsDir, modelDir)
  res <- suppressMessages(runPSMProjection(
    group = "psm-noscen", resultsDir = resultsDir, modelDir = modelDir,
    panelData = makePSMSweepMagpie(), verbose = FALSE
  ))
  expect_null(res)
  man <- jsonlite::fromJSON(file.path(resultsDir, "psm-noscen", "manifest.json"),
                            simplifyVector = FALSE)
  expect_identical(man$steps$projection$status, "skipped")
})

test_that("runPSMEstimatorAgreement writes the estimator-invariance artifact", {
  resultsDir <- withr::local_tempdir()
  modelDir <- withr::local_tempdir()
  psmTestSweep("psm-agree", resultsDir, modelDir)

  res <- suppressMessages(suppressWarnings(runPSMEstimatorAgreement(
    group = "psm-agree", resultsDir = resultsDir, modelDir = modelDir,
    panelData = makePSMSweepMagpie(), verbose = FALSE
  )))
  expect_true(res$spec %in% c("psmA", "psmB"))
  expect_setequal(names(res$bySector), c("Bulk", "Diffuse"))
  bulk <- res$bySector$Bulk
  expect_true(all(c("satP", "fractional", "levels") %in% names(bulk$fits)))
  expect_true(all(bulk$agreement$signsAgree[bulk$agreement$term %in% psmTheoryTerms]))

  groupDir <- file.path(resultsDir, "psm-agree")
  expect_true(file.exists(file.path(groupDir, "estimator-agreement.rds")))
  man <- jsonlite::fromJSON(file.path(groupDir, "manifest.json"), simplifyVector = FALSE)
  expect_identical(man$steps[["estimator-agreement"]]$status, "completed")
})
