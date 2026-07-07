# Tests for the 2026-07-06 PSM improvement batch (R1-R12, except R6; see
# docs/psm-nature-readiness-assessment.md Sections 10-11).

# ── R3: driver-support ranges + guard ────────────────────────────────────────────

test_that("driver ranges are stored on persisted fits and the guard clamps + audits", {
  tmp <- withr::local_tempdir()
  mdl <- psmFitAndLoad(makePSMagpie(), tmp)
  rng <- mdl$transforms$driverRanges
  expect_true(all(c("Actor.Power.Index", "Rule.of.Law..VDem.") %in% names(rng)))
  expect_true(all(vapply(rng, function(r) r[["min"]] < r[["max"]], logical(1))))

  # guard: a value far outside support is clamped and audited; interactions recomputed
  df <- data.frame(
    Actor.Power.Index = c(0, 50), Rule.of.Law..VDem. = c(0.5, 0.5),
    Actor.Power.Index_x_Rule.of.Law..VDem. = c(0, 25)
  )
  g <- .psmDriverGuard(df, list(Actor.Power.Index = c(min = -1, max = 1),
                                Rule.of.Law..VDem. = c(min = 0, max = 1)))
  expect_equal(g$df$Actor.Power.Index[2], 1)
  expect_equal(g$df$Actor.Power.Index_x_Rule.of.Law..VDem.[2], 0.5) # 1 * 0.5, recomputed
  expect_equal(g$outOfSupport, c(0, 0.5))
})

test_that("predictPolicyStringency guards out-of-support drivers by default", {
  tmp <- withr::local_tempdir()
  mdl <- psmFitAndLoad(makePSMagpie(), tmp)
  scen <- makePSMScenarioMagpie()
  scen["R1", , "Actor Power Index|Bulk"] <- 50   # far outside training support

  pG <- suppressMessages(predictPolicyStringency(mdl, scen))
  pN <- suppressMessages(predictPolicyStringency(mdl, scen, driverGuard = "none"))
  expect_true("driverOutOfSupport" %in% names(pG))
  r1G <- pG[pG$region == "R1", ]
  r1N <- pN[pN$region == "R1", ]
  expect_true(all(r1G$driverOutOfSupport > 0))
  expect_false(isTRUE(all.equal(r1G$index, r1N$index)))  # the guard changed the projection
  expect_true(all(is.finite(r1G$index)))
  # audit is emitted even unguarded
  expect_true(all(r1N$driverOutOfSupport > 0))
})

test_that(".regionFESpread is the sd of FE coefficients including the reference zero", {
  beta <- c(`(Intercept)` = 1, regionFEA = 0.5, regionFEB = -0.5, x = 2)
  expect_equal(.regionFESpread(beta), stats::sd(c(0, 0.5, -0.5)))
  expect_equal(.regionFESpread(c(x = 1)), 0)
})

# ── R5: error-correction form ────────────────────────────────────────────────────

test_that("form = 'ecm' fits the error-correction model with speed and long-run effects", {
  fit <- psmFit(makePSMagpie(), form = "ecm")
  expect_identical(fit$form, "ecm")
  expect_true("lagged_ecp" %in% rownames(fit$coeftest))
  # iid-driver DGP: y* is nearly serially uncorrelated -> phi ~ -1, speed ~ 1
  expect_true(is.finite(fit$adjustmentSpeed))
  expect_gt(fit$adjustmentSpeed, 0.5)
  expect_s3_class(fit$longRun, "data.frame")
  expect_true(all(psmTheoryTerms %in% fit$longRun$term))
  expect_null(fit$ameIndex)
  # response is a Delta: it must take both signs
  expect_gt(sum(fit$data$ecp > 0, na.rm = TRUE), 0)
  expect_gt(sum(fit$data$ecp < 0, na.rm = TRUE), 0)
})

test_that("ecm persistence seeds the lag recursion with LEVELS and projection is bounded", {
  tmp <- withr::local_tempdir()
  fit <- psmFit(makePSMagpie(), modelDir = tmp, form = "ecm")
  files <- list.files(file.path(tmp, "models"), pattern = "\\.rds$")
  expect_length(files, 1)
  mdl <- loadPFMModel(sub("\\.rds$", "", files[[1]]), tmp)
  # seed = last transformed LEVEL per region (Delta + lag), not the Delta response
  expected <- tapply(fit$data$ecp + fit$data$lagged_ecp, fit$data$region,
                     function(v) v[length(v)])
  expect_equal(as.numeric(mdl$applyState$seed_prices[names(expected)]),
               as.numeric(expected), tolerance = 1e-8)
  expect_identical(mdl$transforms$prepSpec$form, "ecm")

  proj <- suppressMessages(predictPolicyStringency(mdl, makePSMScenarioMagpie()))
  expect_gt(nrow(proj), 0)
  expect_true(all(is.finite(proj$index)))
  expect_true(all(proj$index >= 0 & proj$index <= 10))
})

test_that("ecm and static satP fits do not collide in the cache", {
  tmp <- withr::local_tempdir()
  psmFit(makePSMagpie(), modelDir = tmp)                 # static
  psmFit(makePSMagpie(), modelDir = tmp, form = "ecm")   # ecm
  files <- list.files(file.path(tmp, "models"), pattern = "\\.rds$")
  expect_length(files, 2)
})

test_that("form = 'ecm' rejects non-satP estimators", {
  expect_error(psmFit(makePSMagpie(), estimator = "levels", form = "ecm"), "satP engine only")
})

# ── R4b: partial-pooling rung ────────────────────────────────────────────────────

test_that("satP-re fits random region intercepts and reports fixed effects", {
  skip_if_not_installed("lme4")
  m <- makePSMagpie()
  df <- preparePanelData(
    data = m, sector = "Bulk",
    actorPowerDrivers = "Actor Power Index", actorPowerIndex = "Actor Power Index",
    instQualityDrivers = "Rule of Law (VDem)", controlDrivers = NULL,
    regionMappingFixedEffects = NULL, outcomeVar = "Policy Stringency"
  )
  df <- df[is.finite(df$ecp), , drop = FALSE]
  df$regionFE <- factor(rep_len(c("G1", "G2", "G3"), nrow(df)))
  n <- sum(is.finite(df$ecp))
  p <- pmin(pmax(df$ecp / 10, 0), 1)
  df$ecp <- stats::qlogis((p * (n - 1) + 0.5) / n)
  fit <- suppressWarnings(estimatePolicyStringencyModel(
    data = df, sector = "Bulk", estimator = "satP-re", prepared = TRUE,
    actorPowerDrivers = "Actor Power Index", actorPowerIndex = "Actor Power Index",
    instQualityDrivers = "Rule of Law (VDem)", controlDrivers = NULL,
    regionMappingFixedEffects = "regionmappingH12.csv",  # formula-side only (prepared)
    modelDir = NULL, verbose = FALSE
  ))
  expect_identical(fit$family, "gaussian (satP+RE)")
  expect_true(all(psmTheoryTerms %in% rownames(fit$coeftest)))
  expect_false("regionFEG2" %in% rownames(fit$coeftest))  # FE dummies replaced by RE
  expect_null(fit$ameIndex)
  expect_true(is.matrix(fit$vcov) || inherits(fit$vcov, "Matrix"))
})

# ── R2: marginal effect over support ─────────────────────────────────────────────

test_that("computeMarginalEffectSupport traces the AP effect over the moderator support", {
  fit <- psmFit(makePSMagpie())
  mes <- computeMarginalEffectSupport(fit)
  expect_s3_class(mes, "data.frame")
  expect_true(all(c("apTerm", "moderator", "m", "me", "se", "lo", "hi", "inSupport") %in%
                    colnames(mes)))
  expect_true(any(mes$inSupport))
  expect_true(any(!mes$inSupport))  # padded grid extends past the support
  expect_true(all(is.finite(mes$me)))
  expect_true(all(mes$se >= 0))
})

# ── R1: PSM tier attribution counts interactions toward parent groups ────────────

test_that("PSM tier attribution credits significant interactions to their groups", {
  # Strong-interaction DGP so the interaction term is reliably significant.
  set.seed(77)
  regions <- paste0("R", 1:12)
  years <- 2000:2019
  nR <- length(regions); nY <- length(years)
  m <- magclass::new.magpie(regions, years,
                            c("Policy Stringency|Bulk", "Actor Power Index|Bulk",
                              "Rule of Law (VDem)"), fill = NA)
  ap <- matrix(runif(nR * nY, -0.8, 0.1), nR, nY)
  iq <- matrix(runif(nR * nY), nR, nY)
  y <- matrix(NA_real_, nR, nY)
  for (t in 2:nY) {
    eta <- -0.5 + 0.1 * ap[, t - 1] + 0.1 * iq[, t - 1] +
      3.0 * ap[, t - 1] * iq[, t - 1] + rnorm(nR, sd = 0.2)
    y[, t] <- 10 * plogis(eta)
  }
  m[, , "Actor Power Index|Bulk"] <- as.vector(ap)
  m[, , "Rule of Law (VDem)"] <- as.vector(iq)
  m[, , "Policy Stringency|Bulk"] <- as.vector(y)

  fit <- psmFit(m)
  cfg <- list(name = "t", actorPowerDrivers = "Actor Power Index",
              actorPowerIndex = "Actor Power Index",
              instQualityDrivers = "Rule of Law (VDem)", controlDrivers = NULL,
              panelTransform = "levels")
  mPSM <- pfm:::.channelFitMetrics(fit, cfg, "Bulk", "PolicyStringency")
  mPrice <- pfm:::.channelFitMetrics(fit, cfg, "Bulk", "Stringency")
  expect_gt(mPSM$sigInteractions, 0)  # DGP guarantees a significant interaction
  # PSM attribution: interaction credits BOTH parent groups beyond mains-only
  expect_gte(mPSM$sigActorPower, mPrice$sigActorPower + 1L)
  expect_gte(mPSM$sigInstQual, mPrice$sigInstQual + 1L)
})

# ── R7 + R9: grid extensions ─────────────────────────────────────────────────────

test_that("exhaustive grid gains capture-channel and EU-diffusion specs, names appended", {
  specs <- pfm:::channelSpecs("exhaustive")
  nms <- vapply(specs, `[[`, character(1), "name")
  # every levels spec is twinned with a saturating-price variant downstream (ADR
  # 0028), so the appended blocks appear twice: base + "| satP" twin. psmSpecs
  # drops the twins for the PSM sweep.
  isTwin <- grepl("\\| satP", nms)
  expect_equal(sum(grepl(" cap:", nms) & !isTwin), 36)   # 2 AP x 3 combos x 2 ctl x 3 FE
  expect_equal(sum(grepl(" dif:EU ", nms) & !isTwin), 12) # 2 AP x 2 ctl x 3 FE
  capSpec <- specs[[which(grepl(" cap:", nms))[1]]]
  expect_true("Control of Corruption (WGI)" %in% unlist(capSpec$instQualityDrivers))
  # Numbering stability: existing spec names (incl. the deployed X-0257) unchanged.
  expect_true("X-0257 WGIge|noRoL|VerAcc compAP lev ctl:ctlNone fe:H12" %in% nms)
})

test_that("EU Membership derives a lagged accession dummy in preparePanelData", {
  set.seed(5)
  regions <- c("DEU", "POL", "R1")
  years <- 2000:2010
  m <- magclass::new.magpie(regions, years,
                            c("Policy Stringency|Bulk", "Actor Power Index|Bulk",
                              "Rule of Law (VDem)"), fill = 0.5)
  df <- preparePanelData(
    data = m, sector = "Bulk",
    actorPowerDrivers = "Actor Power Index", actorPowerIndex = "Actor Power Index",
    instQualityDrivers = "Rule of Law (VDem)", controlDrivers = "EU Membership",
    regionMappingFixedEffects = NULL, outcomeVar = "Policy Stringency"
  )
  expect_true("EU.Membership" %in% colnames(df))
  euDEU <- df$EU.Membership[df$region == "DEU" & df$year == 2010]
  euPOL04 <- df$EU.Membership[df$region == "POL" & df$year == 2004] # member at 2003? no (lag)
  euPOL10 <- df$EU.Membership[df$region == "POL" & df$year == 2010]
  euR1 <- df$EU.Membership[df$region == "R1" & df$year == 2010]
  expect_gt(euDEU, euR1)          # member > never-member (standardized scale)
  expect_equal(euPOL10, euDEU)    # Poland a member by 2009 (lagged year)
  expect_equal(euPOL04, euR1)     # accession 2004, lagged year 2003 -> not yet
})

# ── R10: pseudo-out-of-sample validation step ────────────────────────────────────

test_that("runPSMTemporalValidation scores the deployed spec against held-out years", {
  resultsDir <- withr::local_tempdir()
  modelDir <- withr::local_tempdir()
  psmTestSweep("psm-tv", resultsDir, modelDir)
  res <- suppressMessages(suppressWarnings(runPSMTemporalValidation(
    group = "psm-tv", resultsDir = resultsDir, modelDir = modelDir,
    panelData = makePSMSweepMagpie(), trainEnd = 2015, verbose = FALSE
  )))
  expect_true(file.exists(file.path(resultsDir, "psm-tv", "temporal-validation.rds")))
  expect_identical(res$trainEnd, 2015)
  expect_true(length(res$bySector) >= 1)
  for (sec in names(res$bySector)) {
    mt <- res$bySector[[sec]]$metrics
    expect_gt(mt$n, 0)
    expect_true(is.finite(mt$rmse))
    expect_true(is.finite(mt$skillVsPersistence))
    rows <- res$bySector[[sec]]$rows
    expect_true(all(rows$year > 2015))
    expect_true(all(is.finite(rows$predicted)))
  }
  # step recorded in the manifest
  mf <- jsonlite::fromJSON(file.path(resultsDir, "psm-tv", "manifest.json"))
  expect_true("temporal-validation" %in% names(mf$steps))
})

# ── R12: agreement fitStats n matches the estimation rows ────────────────────────

test_that("agreement fitStats$n reports nobs, matching the sweep's nObs", {
  ea <- suppressWarnings(computeEstimatorAgreement(
    data = makePSMagpie(), sector = "Bulk", estimators = c("satP", "levels"),
    actorPowerDrivers = "Actor Power Index", actorPowerIndex = "Actor Power Index",
    instQualityDrivers = "Rule of Law (VDem)", controlDrivers = NULL,
    regionMappingFixedEffects = NULL, verbose = FALSE
  ))
  expect_equal(ea$fitStats$n[ea$fitStats$estimator == "satP"],
               as.integer(stats::nobs(ea$fits$satP$model)))
  # R2 hook: the meSupport exhibit rides on the artifact
  expect_s3_class(ea$meSupport, "data.frame")
})
