# Fixtures (makePSMagpie, makePSMScenarioMagpie, psmFitAndLoad) live in helper-psm.R.

test_that("predictPolicyStringency projects a loaded satP model bounded by construction", {
  tmp <- withr::local_tempdir()
  psModel <- psmFitAndLoad(makePSMagpie(), tmp)
  expect_s3_class(psModel, "PFMModel")
  expect_identical(psModel$stage, "policyStringency")

  proj <- predictPolicyStringency(psModel, makePSMScenarioMagpie())
  # horizon: strictly after the last training year (2019)
  expect_true(all(proj$year > 2019))
  expect_true(all(proj$year <= 2030))
  # bounded by construction — no clamp exists on this path
  ok <- is.finite(proj$index)
  expect_true(any(ok))
  expect_true(all(proj$index[ok] >= 0 & proj$index[ok] <= 10))
  # delta-method interval brackets the point projection
  okCI <- ok & is.finite(proj$indexLo) & is.finite(proj$indexHi)
  expect_true(any(okCI))
  expect_true(all(proj$indexLo[okCI] <= proj$index[okCI] + 1e-12))
  expect_true(all(proj$indexHi[okCI] >= proj$index[okCI] - 1e-12))
  # out-of-coverage flag: R99 never entered the estimation sample
  expect_true(all(proj$outOfCoverage[proj$region == "R99"]))
  expect_false(any(proj$outOfCoverage[proj$region == "R1"]))
  # out-of-coverage regions still receive driver-based predictions (full map)
  expect_true(any(is.finite(proj$index[proj$region == "R99"])))
})

test_that("computeImplementabilityFactor rescales the index to a 0-1 multiplier", {
  tmp <- withr::local_tempdir()
  psModel <- psmFitAndLoad(makePSMagpie(), tmp)
  proj <- predictPolicyStringency(psModel, makePSMScenarioMagpie())
  imp <- computeImplementabilityFactor(proj)
  ok <- is.finite(imp$implementability)
  expect_true(all(imp$implementability[ok] >= 0 & imp$implementability[ok] <= 1))
  expect_equal(imp$implementability[ok], imp$index[ok] / 10, tolerance = 1e-12)
  expect_true(all(c("implementabilityLo", "implementabilityHi") %in% names(imp)))
})

test_that("dynamic (lagged) projection recurses from the stored seed and stays bounded", {
  tmp <- withr::local_tempdir()
  psModel <- psmFitAndLoad(makePSMagpie(), tmp, includeLaggedPS = TRUE)
  expect_true("lagged_ecp" %in% all.vars(psModel$formula))
  proj <- suppressMessages(predictPolicyStringency(psModel, makePSMScenarioMagpie()))
  ok <- is.finite(proj$index)
  expect_true(any(ok))
  expect_true(all(proj$index[ok] >= 0 & proj$index[ok] <= 10))
  # CI is not propagated through the recursion
  expect_true(all(is.na(proj$indexLo)))
})

test_that("stage and input guards fire", {
  tmp <- withr::local_tempdir()
  fitList <- psmFit(makePSMagpie())
  expect_error(predictPolicyStringency(fitList, makePSMScenarioMagpie()))
  psModel <- psmFitAndLoad(makePSMagpie(), tmp)
  expect_error(computeImplementabilityFactor(data.frame(a = 1)), "index")
})
