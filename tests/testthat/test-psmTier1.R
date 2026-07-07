# Tier-1 methodological directions (2026-07-07; docs/psm-theoretical-directions.md):
# stochastic feasibility frontier, shift-share IV, instrument-level ratchet hazard.

# ── #1: stochastic feasibility frontier ──────────────────────────────────────────

test_that("the frontier rung fits and decomposes into ceiling + political slack", {
  skip_if_not_installed("frontier")
  fit <- psmFit(makePSMagpie(), estimator = "frontier")
  expect_identical(fit$family, "normal-halfnormal frontier (satP)")
  expect_true(all(psmTheoryTerms %in% rownames(fit$coeftest)))
  expect_true(all(c("sigmaSq", "gamma") %in% rownames(fit$coeftest)))
  expect_gte(fit$frontierGamma, 0)
  expect_lte(fit$frontierGamma, 1)
  expect_true(is.finite(fit$frontierLR))

  fr <- fit$frontier
  expect_s3_class(fr, "data.frame")
  expect_true(all(c("region", "year", "observedIndex", "frontierIndex", "expectedIndex",
                    "slackIndex", "efficiencyRatio") %in% colnames(fr)))
  # the frontier is a ceiling: slack is one-sided, the ratio lives in [0, 1]
  expect_true(all(fr$slackIndex >= -1e-8))
  expect_true(all(fr$frontierIndex >= fr$expectedIndex - 1e-8))
  expect_true(all(fr$efficiencyRatio >= 0 & fr$efficiencyRatio <= 1))
  expect_true(all(is.finite(fr$frontierIndex)))
})

test_that("runPSMFrontier writes the frontier artifact for the deployed spec", {
  skip_if_not_installed("frontier")
  resultsDir <- withr::local_tempdir()
  modelDir <- withr::local_tempdir()
  psmTestSweep("psm-fr", resultsDir, modelDir)
  res <- suppressMessages(suppressWarnings(runPSMFrontier(
    group = "psm-fr", resultsDir = resultsDir, modelDir = modelDir,
    panelData = makePSMSweepMagpie(), verbose = FALSE
  )))
  expect_true(file.exists(file.path(resultsDir, "psm-fr", "frontier.rds")))
  expect_true(length(res$bySector) >= 1)
  for (sec in names(res$bySector)) {
    e <- res$bySector[[sec]]
    expect_true(e$gamma >= 0 && e$gamma <= 1)
    expect_s3_class(e$scores, "data.frame")
    expect_gt(nrow(e$scores), 0)
  }
  mf <- jsonlite::fromJSON(file.path(resultsDir, "psm-fr", "manifest.json"))
  expect_true("frontier" %in% names(mf$steps))
})

# ── #3: shift-share IV ───────────────────────────────────────────────────────────

test_that("satP-iv instruments Incumbent Power with the shift-share and recovers the sign", {
  skip_if_not_installed("AER")
  m <- makePSMIVMagpie()
  # timeTrend = FALSE: the fixture's incumbency decline is driven by the global
  # VRE diffusion itself, so a free linear trend would absorb the identifying
  # variation of the shift (a real design consideration, documented here).
  fit <- suppressWarnings(estimatePolicyStringencyModel(
    m, sector = "Bulk", estimator = "satP-iv",
    actorPowerDrivers = "Incumbent Power", actorPowerIndex = "Incumbent Power",
    instQualityDrivers = "Rule of Law (VDem)", controlDrivers = NULL,
    regionMappingFixedEffects = NULL, timeTrend = FALSE,
    modelDir = NULL, verbose = FALSE
  ))
  expect_identical(fit$family, "2SLS shift-share (satP)")
  expect_true("Incumbent.Power" %in% rownames(fit$coeftest))
  expect_lt(fit$coeftest["Incumbent.Power", 1], 0)  # true beta < 0 in the DGP
  expect_s3_class(fit$ivDiagnostics, "data.frame")
  expect_true(any(grepl("Weak instruments", rownames(fit$ivDiagnostics))))
  # strong first stage by construction
  expect_gt(fit$ivDiagnostics[grep("Weak instruments", rownames(fit$ivDiagnostics))[1],
                              "statistic"], 10)
  expect_match(fit$instrument, "leave-one-out")
})

test_that("satP-iv guards its preconditions", {
  expect_error(
    psmFit(makePSMagpie(), estimator = "satP-iv"),
    "instruments Incumbent Power"
  )
  m <- makePSMIVMagpie()
  df <- data.frame(x = 1)
  expect_error(
    estimatePolicyStringencyModel(
      df, sector = "Bulk", estimator = "satP-iv", prepared = TRUE,
      actorPowerDrivers = "Incumbent Power", actorPowerIndex = "Incumbent Power",
      instQualityDrivers = "Rule of Law (VDem)", regionMappingFixedEffects = NULL,
      modelDir = NULL, verbose = FALSE
    ),
    "cannot run on a prepared"
  )
})

# ── #2: instrument-level ratchet hazard ─────────────────────────────────────────

test_that("estimatePolicyRatchetModel fits a cloglog hazard with the PSM drivers", {
  fit <- suppressWarnings(estimatePolicyRatchetModel(
    makePSMEventMagpie(), sector = "Bulk",
    actorPowerDrivers = "Actor Power Index", actorPowerIndex = "Actor Power Index",
    instQualityDrivers = "Rule of Law (VDem)", controlDrivers = NULL,
    regionMappingFixedEffects = NULL, verbose = FALSE
  ))
  expect_identical(fit$family, "binomial(cloglog)")
  expect_true(all(psmTheoryTerms %in% rownames(fit$coeftest)))
  # strong positive DGP effects
  expect_gt(fit$coeftest["Actor.Power.Index", 1], 0)
  expect_gt(fit$coeftest["Rule.of.Law..VDem.", 1], 0)
  expect_gt(fit$eventRate, 0.05)
  expect_lt(fit$eventRate, 0.95)
  expect_s3_class(fit$hazardRatios, "data.frame")
  expect_true(all(fit$hazardRatios$hazardRatio > 0))
  expect_true(isTRUE(fit$converged))
})

test_that("the ratchet model validates its outcome and aliases sector names", {
  m <- makePSMEventMagpie()
  # counts instead of 0/1 must be rejected
  m2 <- m
  m2[, , "Ratchet Event"] <- m2[, , "Ratchet Event"] * 2.5
  expect_error(suppressWarnings(estimatePolicyRatchetModel(
    m2, sector = "Bulk",
    actorPowerDrivers = "Actor Power Index", actorPowerIndex = "Actor Power Index",
    instQualityDrivers = "Rule of Law (VDem)", regionMappingFixedEffects = NULL,
    verbose = FALSE
  )), "not 0/1")
  # missing outcome is an informative error
  m3 <- m[, , c("Actor Power Index|Bulk", "Rule of Law (VDem)")]
  expect_error(suppressWarnings(estimatePolicyRatchetModel(
    m3, sector = "Bulk",
    actorPowerDrivers = "Actor Power Index", actorPowerIndex = "Actor Power Index",
    instQualityDrivers = "Rule of Law (VDem)", regionMappingFixedEffects = NULL,
    verbose = FALSE
  )), "calcPolicyRatchetEvents")
})
