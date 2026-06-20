apP <- c("Innovator.Power", "Incumbent.Power")
iqP <- c("Rule.of.Law..VDem.", "Government.Effectiveness..WGI.")

test_that("predicate passes when AP persists (correct sign+sig) and IQ vanishes", {
  nm  <- c("(Intercept)", "Innovator.Power", "Incumbent.Power", "Rule.of.Law..VDem.",
           "Innovator.Power_x_Rule.of.Law..VDem.")
  est <- c(0.1,  1.2, -0.9, 0.3, 0.05)
  p   <- c(0.2,  0.01, 0.02, 0.60, 0.80)
  pr <- pfm:::.falsifyPredicate(nm, est, p, apP, iqP)
  expect_equal(pr$nAPsigCorrect, 2); expect_equal(pr$nAPsigWrong, 0)
  expect_equal(pr$nIQsig, 0)
  expect_true(pr$apPass); expect_true(pr$iqPass); expect_true(pr$sectorPass)
})

test_that("AP with the wrong sign (significant) fails the AP side", {
  nm  <- c("Innovator.Power", "Incumbent.Power", "Rule.of.Law..VDem.")
  est <- c(1.0, 0.8, 0.1)          # Incumbent positive = wrong sign
  p   <- c(0.01, 0.01, 0.9)
  pr <- pfm:::.falsifyPredicate(nm, est, p, apP, iqP)
  expect_equal(pr$nAPsigWrong, 1)
  expect_false(pr$apPass); expect_false(pr$sectorPass)
})

test_that("a surviving IQ channel fails the IQ-vanishes side", {
  nm  <- c("Innovator.Power", "Rule.of.Law..VDem.")
  est <- c(1.0, 0.7)
  p   <- c(0.01, 0.001)            # IQ still significant
  pr <- pfm:::.falsifyPredicate(nm, est, p, apP, iqP)
  expect_equal(pr$nIQsig, 1)
  expect_true(pr$apPass); expect_false(pr$iqPass); expect_false(pr$sectorPass)
})

test_that("the interaction is counted but not gated", {
  nm  <- c("Innovator.Power", "Incumbent.Power", "Innovator.Power_x_Rule.of.Law..VDem.")
  est <- c(1.0, -1.0, 2.0)
  p   <- c(0.01, 0.01, 0.001)      # interaction significant, AP fine, no IQ main
  pr <- pfm:::.falsifyPredicate(nm, est, p, apP, iqP)
  expect_equal(pr$nIntSig, 1)
  expect_true(pr$sectorPass)       # interaction does not block the gate
})

test_that("computeFalsificationGate returns the documented structure", {
  skip_if_not_installed("logistf")
  m <- magclass::new.magpie(paste0("R", 1:4), 2000:2006,
        c("Effective Carbon Price|Bulk", "Innovator Power|Bulk", "Rule of Law (VDem)"), fill = 0)
  cfg <- list(actorPowerDrivers = "Innovator Power", actorPowerIndex = "Innovator Power",
              instQualityDrivers = "Rule of Law (VDem)", controlDrivers = NULL,
              regionMappingFixedEffects = NULL)
  res <- suppressWarnings(computeFalsificationGate(cfg, data = m, stage = "Adoption",
            sectors = "Bulk", modelDir = NULL))
  expect_type(res, "list")
  expect_true(is.logical(res$pass) && length(res$pass) == 1)
  expect_true(all(c("sector","estimable","apPass","iqPass","sectorPass") %in% names(res$detail)))
  expect_type(res$reason, "character")
})
