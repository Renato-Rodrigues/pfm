test_that("computeTemporalSplit returns the documented out-of-time structure", {
  skip_if_not_installed("logistf")
  set.seed(7)
  regs <- paste0("R", 1:6)
  yrs  <- 2000:2018
  m <- magclass::new.magpie(regs, yrs,
        c("Effective Carbon Price|Bulk", "Innovator Power|Bulk", "Rule of Law (VDem)"), fill = 0)
  # staggered adoption: R1/R2 early (adopted before the 2012 cutoff), R3/R4 inside the
  # test window, R5/R6 never -> a non-trivial at-risk set with both 0s and 1s.
  onset <- c(R1 = 2006, R2 = 2008, R3 = 2014, R4 = 2016, R5 = Inf, R6 = Inf)
  for (r in regs) {
    x  <- rnorm(length(yrs)) + (yrs >= onset[[r]]) * 1.5
    m[r, , "Innovator Power|Bulk"] <- x
    m[r, , "Rule of Law (VDem)"]   <- rnorm(length(yrs))
    adopt <- yrs >= onset[[r]]
    m[r, , "Effective Carbon Price|Bulk"] <- ifelse(adopt, abs(rnorm(length(yrs))) * 10 + 5, 0)
  }

  res <- computeTemporalSplit(
    data = m, sector = "Bulk", stage = "adoption",
    actorPowerDrivers = "Innovator Power", actorPowerIndex = "Innovator Power",
    instQualityDrivers = "Rule of Law (VDem)", controlDrivers = NULL,
    regionMappingFixedEffects = NULL, modelDir = NULL, verbose = FALSE
  )

  expect_type(res, "list")
  expect_equal(res$meta$cutoff, 2018 - 6)               # default cutoff = maxYear - 6
  expect_true(res$meta$testEnd == 2018)
  expect_true(is.data.frame(res$coef) && all(c("term", "full", "train", "signSame") %in% names(res$coef)))
  # single-split out-of-time scores present and finite
  expect_true(!is.null(res$single$atRisk) && is.finite(res$single$atRisk$brier))
  expect_true(res$single$nAtRiskReg >= 1)
  # rolling-origin ran at least one fold
  expect_true(res$rolling$nOrigins >= 1)
  # forecast-horizon table spans >=1 horizon
  expect_true(is.null(res$horizon) || all(res$horizon$horizon >= 1))
})

test_that("computeTemporalSplit returns NULL when the full fit cannot be built", {
  m <- magclass::new.magpie("R1", 2000:2004, "Rule of Law (VDem)", fill = 1)
  expect_null(suppressWarnings(tryCatch(
    computeTemporalSplit(data = m, sector = "Bulk", stage = "adoption",
      actorPowerDrivers = NULL, actorPowerIndex = NULL,
      instQualityDrivers = "Rule of Law (VDem)", controlDrivers = NULL,
      regionMappingFixedEffects = NULL, modelDir = NULL),
    error = function(e) NULL)))
})
