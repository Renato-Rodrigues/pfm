# nolint start
test_that("computeLORO returns coefficient-stability + pooled OOS on a small panel", {
  skip_if_not_installed("logistf")
  set.seed(42)
  regs <- paste0("R", 1:6)
  yrs  <- 2000:2014
  # "Innovator Power" is a known index, so it is resolved to "<name>|<sector>"
  # exactly as in production; the IQ driver is looked up verbatim.
  vars <- c("Effective Carbon Price|Bulk", "Innovator Power|Bulk", "Rule of Law (VDem)")
  m <- magclass::new.magpie(regs, yrs, vars, fill = 0)
  for (r in regs) {
    x  <- rnorm(length(yrs))
    iq <- rnorm(length(yrs))
    m[r, , "Innovator Power|Bulk"] <- x
    m[r, , "Rule of Law (VDem)"]   <- iq
    adopt <- plogis(1.3 * x + 0.5 * iq) > runif(length(yrs))
    m[r, , "Effective Carbon Price|Bulk"] <- ifelse(adopt, abs(rnorm(length(yrs))) * 10 + 1, 0)
  }

  res <- computeLORO(
    data = m, sector = "Bulk", stage = "adoption",
    actorPowerDrivers = "Innovator Power", actorPowerIndex = "Innovator Power",
    instQualityDrivers = "Rule of Law (VDem)", controlDrivers = NULL,
    regionMappingFixedEffects = NULL, modelDir = NULL, verbose = FALSE
  )

  expect_type(res, "list")
  # (A) coefficient stability: one row per theory coefficient, finite summaries
  expect_true(is.data.frame(res$coef) && nrow(res$coef) >= 1)
  expect_true(all(c("term", "group", "full", "looMean", "looSD",
                    "signStable", "mostInfluential", "maxAbsDelta") %in% names(res$coef)))
  expect_true(all(is.finite(res$coef$looSD)))
  expect_true(all(res$coef$signStable >= 0 & res$coef$signStable <= 1))
  expect_true(all(res$coef$mostInfluential %in% regs))

  # tier-stability counts add up to the number of completed folds
  expect_equal(sum(res$tier$counts), sum(res$tier$perRegion != ""))

  # (B) pooled OOS: every region predictable (no FE blocks), finite Brier in [0,1]
  expect_equal(res$oos$nExcludedSingletonBlock, 0L)
  expect_gt(res$oos$n, 0)
  expect_true(is.finite(res$oos$brier) && res$oos$brier >= 0 && res$oos$brier <= 1)
  expect_true(is.finite(res$oos$brierBase))
  expect_true(is.data.frame(res$preds) && all(res$preds$region %in% regs))
})

test_that("computeLORO returns NULL when the full-sample fit cannot be built", {
  m <- magclass::new.magpie("R1", 2000:2002, "Rule of Law (VDem)", fill = 1)
  expect_null(suppressWarnings(tryCatch(
    computeLORO(data = m, sector = "Bulk", stage = "adoption",
                actorPowerDrivers = NULL, actorPowerIndex = NULL,
                instQualityDrivers = "Rule of Law (VDem)", controlDrivers = NULL,
                regionMappingFixedEffects = NULL, modelDir = NULL),
    error = function(e) NULL)))
})
# nolint end
