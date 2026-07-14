# Fixtures (makePSMagpie, psmFit, psmTheoryTerms) live in helper-psm.R.
theoryTerms <- psmTheoryTerms

test_that("satP engine fits, recovers theory signs, and carries the PSM metadata", {
  fit <- psmFit(makePSMagpie(), estimator = "satP")
  expect_identical(fit$estimator, "satP")
  expect_identical(fit$squeeze$type, "smithson-verkuilen")
  expect_true(is.finite(fit$squeeze$n) && fit$squeeze$n > 0)
  expect_equal(unname(fit$boundaryShares), c(0, 0))
  expect_true(all(fit$coeftest[theoryTerms, 1] > 0))
  expect_true(all(fit$coeftest[theoryTerms, 4] < 0.05))
  # full likelihood: AIC/BIC defined (the selection engine requirement)
  expect_true(is.finite(stats::AIC(fit$model)))
  expect_true(is.finite(stats::BIC(fit$model)))
  # natural-scale AMEs present with delta-method SEs
  expect_s3_class(fit$ameIndex, "data.frame")
  expect_true(all(theoryTerms %in% fit$ameIndex$term))
  expect_true(all(is.finite(fit$ameIndex$se)))
  expect_true(all(fit$ameIndex$ame[fit$ameIndex$term %in% theoryTerms] > 0))
  # outcomeNatural is the untransformed 0-10 outcome
  expect_true(all(fit$outcomeNatural >= 0 & fit$outcomeNatural <= 10))
})

test_that("fractional logit fits with boundaries kept and has no likelihood (never selection)", {
  fit <- psmFit(makePSMagpie(withBoundaries = TRUE), estimator = "fractional")
  expect_identical(fit$family, "quasibinomial(logit)")
  expect_gt(fit$boundaryShares[["atZero"]], 0)
  expect_gt(fit$boundaryShares[["atMax"]], 0)
  expect_true(all(fit$coeftest[theoryTerms, 1] > 0))
  # quasi-likelihood: AIC undefined
  expect_true(is.na(suppressWarnings(stats::AIC(fit$model))))
})

test_that("levels benchmark fits and its AME equals the coefficient", {
  fit <- psmFit(makePSMagpie(), estimator = "levels")
  ame <- stats::setNames(fit$ameIndex$ame, fit$ameIndex$term)
  expect_equal(unname(ame[theoryTerms]),
               unname(fit$coeftest[theoryTerms, 1]), tolerance = 1e-10)
})

test_that("beta regression fits with clustered SEs where available", {
  skip_if_not_installed("betareg")
  fit <- psmFit(makePSMagpie(withBoundaries = TRUE), estimator = "beta")
  expect_identical(fit$family, "beta(logit)")
  expect_true(all(fit$coeftest[theoryTerms, 1] > 0))
  expect_s3_class(fit$ameIndex, "data.frame")
})

test_that("outcome outside [0, indexMax] is a hard error", {
  m <- makePSMagpie()
  m["R1", 2005, "Policy Stringency|Bulk"] <- 12
  expect_error(psmFit(m, estimator = "satP"), "outside")
})

test_that("zero-stringency observations are kept (no hurdle subset)", {
  m <- makePSMagpie(withBoundaries = TRUE)
  fit <- psmFit(m, estimator = "satP")
  expect_gt(fit$boundaryShares[["atZero"]], 0)
  # the zero rows are in the estimation data (transformed, not dropped)
  expect_equal(length(fit$outcomeNatural), nrow(fit$data))
})

test_that("computeEstimatorAgreement compares the suite on signs and natural-scale AMEs", {
  m <- makePSMagpie()
  agr <- suppressWarnings(computeEstimatorAgreement(
    m, sector = "Bulk",
    actorPowerDrivers = "Actor Power Index", actorPowerIndex = "Actor Power Index",
    instQualityDrivers = "Rule of Law (VDem)", controlDrivers = NULL,
    regionMappingFixedEffects = NULL, verbose = FALSE
  ))
  fitted_est <- names(agr$fits)
  expect_true(all(c("satP", "fractional", "levels") %in% fitted_est))
  expect_true(all(c("estimator", "term", "estimate", "ameIndex", "ameSE") %in% colnames(agr$table)))
  # theory terms: all fitted estimators agree on the (positive) sign
  agrTheory <- agr$agreement[agr$agreement$term %in% theoryTerms, ]
  expect_true(all(agrTheory$signsAgree))
  expect_true(all(grepl("^\\++$", agrTheory$signs)))
  # natural-scale fit metrics are finite and on the 0-10 scale
  expect_true(all(is.finite(agr$fitStats$rmseNatural)))
  expect_true(all(agr$fitStats$rmseNatural < 10))
  expect_true(all(is.finite(agr$fitStats$corNatural)))
  # AIC is NA for the fractional row (transparency, never comparison)
  expect_true(is.na(agr$fitStats$aic[agr$fitStats$estimator == "fractional"]))
  expect_true(is.finite(agr$fitStats$aic[agr$fitStats$estimator == "satP"]))
})

test_that("year-FE and TWFE specification rungs fit and keep dummies out of the tables (ADR 0038)", {
  m <- makePSMagpie()
  agr <- suppressWarnings(computeEstimatorAgreement(
    m, sector = "Bulk", estimators = c("satP", "satP-yearFE", "levels-twfe"),
    actorPowerDrivers = "Actor Power Index", actorPowerIndex = "Actor Power Index",
    instQualityDrivers = "Rule of Law (VDem)", controlDrivers = NULL,
    regionMappingFixedEffects = NULL, timeTrend = TRUE, verbose = FALSE
  ))
  expect_true(all(c("satP-yearFE", "levels-twfe") %in% names(agr$fits)))
  # year dummies replace the trend in the year-FE rung ...
  yf <- rownames(agr$fits[["satP-yearFE"]]$coeftest)
  expect_true(any(grepl("^yearFE", yf)))
  expect_false(any(grepl("timeTrend", yf)))
  # ... and the TWFE rung carries unit FE + year FE
  tw <- rownames(agr$fits[["levels-twfe"]]$coeftest)
  expect_true(any(grepl("^regionFE", tw)))
  expect_true(any(grepl("^yearFE", tw)))
  # neither dummy family leaks into the comparison/agreement tables
  expect_false(any(grepl("^regionFE|^yearFE", agr$table$term)))
  expect_false(any(grepl("^regionFE|^yearFE", agr$agreement$term)))
  # theory terms survive within-year identification in this DGP (sign agreement)
  agrTheory <- agr$agreement[agr$agreement$term %in% theoryTerms, ]
  expect_true(all(agrTheory$signsAgree))
})

test_that("the 'unit' FE sentinel makes every row-unit its own FE block", {
  m <- makePSMagpie()
  fit <- suppressWarnings(estimatePolicyStringencyModel(
    m, sector = "Bulk", estimator = "levels",
    actorPowerDrivers = "Actor Power Index", actorPowerIndex = "Actor Power Index",
    instQualityDrivers = "Rule of Law (VDem)", controlDrivers = NULL,
    regionMappingFixedEffects = "unit", modelDir = NULL, verbose = FALSE
  ))
  expect_true("regionFE" %in% colnames(fit$data))
  expect_equal(nlevels(fit$data$regionFE), length(unique(fit$data$region)))
})

test_that("satP engine cache round-trips through the model store", {
  m <- makePSMagpie()
  tmp <- withr::local_tempdir()
  fit1 <- suppressMessages(psmFit(m, estimator = "satP", modelDir = tmp))
  expect_true(length(list.files(file.path(tmp, "models"))) > 0)
  msgs <- capture_messages(
    fit2 <- psmFit(m, estimator = "satP", modelDir = tmp, verbose = TRUE)
  )
  expect_true(any(grepl("cache hit", msgs)))
  expect_equal(unname(fit2$coeftest[theoryTerms, 1]),
               unname(fit1$coeftest[theoryTerms, 1]), tolerance = 1e-10)
})
