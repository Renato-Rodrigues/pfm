# The shape of the saturating actor-power transform (design note 0005 §7a decision 6):
# xBar = apSatScale * training median. 1 is the deployed form and must leave every result and every
# cache key exactly as before.

.satPanel <- function() {
  m <- makePFMagpie()
  set.seed(5)
  inn <- magclass::setNames(m[, , "Rule of Law (VDem)"], "Innovator Power|Bulk")
  inn[, , ] <- stats::runif(length(inn), 0.03, 0.45)
  magclass::mbind(m, inn)
}
.satPrep <- function(m, ...) preparePanelData(
  data = m, sector = "Bulk", actorPowerDrivers = "Innovator Power", actorPowerIndex = "Innovator Power",
  instQualityDrivers = "Rule of Law (VDem)", controlDrivers = NULL, regionMappingFixedEffects = NULL,
  outcomeVar = "Policy Stringency", apTransform = "saturating", ...)

test_that("the half-saturation point is apSatScale times the median, and 1 is the default", {
  m <- .satPanel()
  d0 <- .satPrep(m); d1 <- .satPrep(m, apSatScale = 1); d2 <- .satPrep(m, apSatScale = 2)
  expect_identical(d0, d1)
  s1 <- attr(d1, "driverScaling")$Innovator.Power[["sat"]]
  s2 <- attr(d2, "driverScaling")$Innovator.Power[["sat"]]
  expect_equal(s2, 2 * s1)
  expect_false(isTRUE(all.equal(d1$Innovator.Power, d2$Innovator.Power)))
  expect_error(.satPrep(m, apSatScale = 0), "apSatScale")
  expect_error(.satPrep(m, apSatScale = c(1, 2)), "apSatScale")
})

test_that("apply mode reproduces the fitted shape from driverScaling, whatever apSatScale says", {
  m <- .satPanel()
  d2 <- .satPrep(m, apSatScale = 2)
  a <- .satPrep(m, driverScaling = attr(d2, "driverScaling"))
  b <- .satPrep(m, driverScaling = attr(d2, "driverScaling"), apSatScale = 0.5)
  expect_equal(a$Innovator.Power, d2$Innovator.Power)
  expect_equal(b$Innovator.Power, d2$Innovator.Power)
})

test_that("the fit cache key changes only when apSatScale is not 1", {
  m <- .satPanel(); md <- withr::local_tempdir()
  fit <- function(...) estimatePolicyStringencyModel(
    data = m, sector = "Bulk", estimator = "satP", actorPowerDrivers = "Innovator Power",
    actorPowerIndex = "Innovator Power", instQualityDrivers = "Rule of Law (VDem)", controlDrivers = NULL,
    regionMappingFixedEffects = NULL, logisticTimeTrend = FALSE, apTransform = "saturating",
    modelDir = md, updateIndex = FALSE, verbose = FALSE, ...)
  n <- function() length(list.files(file.path(md, "models"), pattern = "\\.rds$"))
  fit(); expect_equal(n(), 1)
  fit(apSatScale = 1); expect_equal(n(), 1)
  f2 <- fit(apSatScale = 2); expect_equal(n(), 2)
  expect_equal(f2$driverScaling$Innovator.Power[["sat"]],
               2 * fit()$driverScaling$Innovator.Power[["sat"]])
})

test_that("the bootstrap cache key changes only when apSatScale is not 1", {
  cfg <- list(actorPowerIndex = "Innovator Power", apTransform = "saturating")
  k0 <- pfm:::.pfmBootCacheKey(cfg, "Bulk", "h", 1)
  expect_identical(pfm:::.pfmBootCacheKey(c(cfg, apSatScale = 1), "Bulk", "h", 1), k0)
  expect_false(identical(pfm:::.pfmBootCacheKey(c(cfg, apSatScale = 2), "Bulk", "h", 1), k0))
})

test_that("every hand-listed fit call forwards apSatScale", {
  for (f in list(projectPFMSpecScenario = pfm:::projectPFMSpecScenario,
                 projectFeasiblePath = pfm::projectFeasiblePath, runPFMDonorAssumptions = pfm::runPFMDonorAssumptions,
                 iterativePFM = pfm::iterativePFM)) {
    expect_true(grepl("apSatScale", paste(deparse(f), collapse = " "), fixed = TRUE))
  }
})
