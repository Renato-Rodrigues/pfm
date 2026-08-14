test_that("every estimator-accepted spec field is forwarded, apTransform above all", {
  # The defect this guards (found 2026-08-15 in Run-Group v1): the post-selection
  # steps hand-listed the fields they forwarded and omitted apTransform, so the
  # estimator fell back to its "linear" default. estimator-agreement.rds, iv.rds and
  # influence.rds therefore described a DIFFERENT specification from the deployed
  # one, while carrying the deployed spec's satAP name.
  cfg <- list(
    name = "X-0370 ... fe:OECDp satAP",
    model_type = "PolicyStringency: Bulk",
    actorPowerDrivers = c("Innovator Power", "Incumbent Power"),
    actorPowerIndex = c("Innovator Power", "Incumbent Power"),
    instQualityDrivers = c("Government Effectiveness (WGI)"),
    controlDrivers = c("Population (log)"),
    regionMappingFixedEffects = "regionmapping_EU_OECDp.csv",
    logisticTimeTrend = TRUE,
    interactRegionFE = FALSE,
    useMundlak = FALSE,
    gdpGovInteraction = FALSE,
    includeLaggedPS = FALSE,
    apTransform = "saturating",
    estimator = "satP",
    indexMax = 10,
    # fields the estimator does not accept must be dropped, not passed through
    ridgeInteractions = FALSE,
    nickellCorrection = FALSE)

  args <- .psmSpecArgs(cfg)

  expect_identical(args$apTransform, "saturating")
  expect_identical(args$regionMappingFixedEffects, "regionmapping_EU_OECDp.csv")
  expect_true(args$logisticTimeTrend)
  # Set by each caller itself, never taken from the spec.
  expect_false(any(c("estimator", "indexMax") %in% names(args)))
  # Not estimator arguments; passing them would error.
  expect_false(any(c("name", "model_type", "ridgeInteractions", "nickellCorrection")
                   %in% names(args)))
  # Everything returned must be callable.
  expect_true(all(names(args) %in% names(formals(estimatePolicyStringencyModel))))

  # Caller overrides win (the IV rung instruments incumbency alone).
  ivArgs <- .psmSpecArgs(cfg, exclude = c("actorPowerDrivers", "actorPowerIndex"))
  expect_false(any(c("actorPowerDrivers", "actorPowerIndex") %in% names(ivArgs)))
  expect_identical(ivArgs$apTransform, "saturating")

  # An absent flag falls through to the estimator's default (all FALSE/linear).
  cfg$apTransform <- NULL
  expect_false("apTransform" %in% names(.psmSpecArgs(cfg)))

  # But an absent driver block or FE mapping means NONE, not the estimator's
  # default — which is "regionmappingH12.csv" and full default driver blocks.
  # Letting those apply would hand a spec fixed effects it never asked for.
  bare <- .psmSpecArgs(list(name = "x"))
  for (k in c("actorPowerDrivers", "actorPowerIndex", "instQualityDrivers",
              "controlDrivers", "regionMappingFixedEffects")) {
    expect_true(k %in% names(bare), info = k)
    expect_null(bare[[k]], info = k)
  }
})

test_that("the post-selection steps forward the spec rather than hand-listing it", {
  # A structural guard: if someone reinstates a literal argument list, the omission
  # that caused this defect becomes possible again without any test failing.
  for (fn in list(runPSMEstimatorAgreement, runPSMInfluence, runPSMIV)) {
    body <- paste(deparse(fn), collapse = " ")
    expect_true(grepl(".psmSpecArgs", body, fixed = TRUE))
  }
})
