# The v6 anchor artifact (design note 0005 C6 / F6): runPFMAnchor writes phi-anchor.rds, a lean
# design that reproduces eta without the fitted model's data, pfmAnchorFor reads it back, the step is
# registered in every step list, and the export / preflight insist on it once the step completed.

.anchorFixtureDesign <- function() {
  m <- makePFMagpie()
  fit <- estimatePolicyStringencyModel(
    data = m, sector = "Bulk", estimator = "satP", actorPowerDrivers = "Actor Power Index",
    actorPowerIndex = "Actor Power Index", instQualityDrivers = "Rule of Law (VDem)", controlDrivers = NULL,
    regionMappingFixedEffects = NULL, logisticTimeTrend = TRUE, modelDir = NULL, updateIndex = FALSE, verbose = FALSE)
  b <- stats::coef(fit$model); b <- b[is.finite(b)]
  cfg <- list(actorPowerDrivers = "Actor Power Index", actorPowerIndex = "Actor Power Index",
              instQualityDrivers = "Rule of Law (VDem)", controlDrivers = NULL, regionMappingFixedEffects = NULL)
  list(data = m, design = list(
    cfg = cfg, sector = "Bulk", fit = fit, beta = b, indexMax = 10,
    lastHist = max(fit$data$year), ranges = pfm:::.driverSupportRanges(fit$data, fit$driverScaling),
    nSqueeze = sum(is.finite(fit$data$ecp)),
    trend = fit$trendParams %||% c(midpoint = 2010, steepness = 0.2)))
}

test_that("the lean design reproduces eta exactly and carries none of the fit's data", {
  fx <- .anchorFixtureDesign()
  lean <- pfm:::.pfmLeanDesign(fx$design)
  full <- pfm:::.pfmFrontierEta(fx$design, fx$data)
  thin <- pfm:::.pfmFrontierEta(lean, fx$data)
  expect_identical(thin, full)
  expect_null(lean$fit$data)
  expect_identical(environment(lean$fit$formula), baseenv())
  expect_lt(as.numeric(utils::object.size(lean)), as.numeric(utils::object.size(fx$design)) / 2)
})

test_that("pfmAnchorFor rebuilds the computeAnchorGap shape for a resolution, and refuses others", {
  art <- list(format = "pfm-anchor/1", group = "g", spec = c(Bulk = "S"), rule = "anchor-year",
              anchorYear = 2023, t0 = 2025, ssp = "SSP2",
              country = data.frame(sector = "Bulk", region = "AAA", q = 1),
              weights = c(AAA = 1), designs = list(Bulk = list()),
              byResolution = list(EU21 = list(mapping = "m.csv", region = data.frame(sector = "Bulk", region = "R", u = 0),
                                              regionWeights = c(R = 1))))
  a <- pfmAnchorFor(art, "EU21")
  expect_setequal(names(a), c("group", "rule", "anchorYear", "t0", "mapping", "ssp", "country", "region",
                              "weights", "regionWeights", "spec", "designs"))
  expect_identical(a$mapping, "m.csv")
  expect_error(pfmAnchorFor(art, "H12"), "no resolution 'H12'")
  expect_error(pfmAnchorFor(list(format = "other")), "not a pfm anchor")
  f <- withr::local_tempfile(fileext = ".rds"); saveRDS(art, f)
  expect_identical(pfmAnchorFor(f, "EU21"), a)
})

test_that("pfm-anchor is registered after the donor step and before the projection, in every list", {
  expect_identical(pfmStepArtifacts("pfm-anchor")[[1]], "phi-anchor.rds")
  for (fn in list(pfm::pfmRun, pfm:::runModelGroup, pfm::startRun)) {
    src <- paste(deparse(fn), collapse = " ")
    expect_match(src, "\"pfm-anchor\"", fixed = TRUE)
    expect_lt(regexpr("\"pfm-donor\", \"pfm-anchor\"", src, fixed = TRUE), Inf)
    expect_true(grepl("\"pfm-donor\", \"pfm-anchor\", \"pfm-projection\"", src, fixed = TRUE))
  }
})

test_that("runPFMAnchor skips cleanly, and records it, when the prerequisites are missing", {
  rd <- withr::local_tempdir(); dir.create(file.path(rd, "g"))
  writeLines("{}", file.path(rd, "g", "manifest.json"))
  expect_null(suppressMessages(runPFMAnchor("g", resultsDir = rd, modelDir = rd, verbose = FALSE)))
  m <- jsonlite::read_json(file.path(rd, "g", "manifest.json"))
  expect_identical(m$steps[["pfm-anchor"]]$status, "skipped")
  expect_false(file.exists(file.path(rd, "g", "phi-anchor.rds")))
})

test_that("the preflight wants phi-anchor.rds in an export whose manifest records a completed pfm-anchor", {
  gd <- withr::local_tempdir()
  for (f in c("frontier.rds", "temporal-validation.rds", "donor-assignment-band-Bulk.rds",
              "donor-assignment-band-Diffuse.rds", "selected-models-pfm.yml", "panel_h.rds")) writeLines("x", file.path(gd, f))
  jsonlite::write_json(list(panel_hash = "h", steps = list(`pfm-anchor` = list(status = "skipped"))),
                       file.path(gd, "manifest.json"), auto_unbox = TRUE)
  expect_length(pfm:::.pfmGroupExportMissing(gd), 0)
  jsonlite::write_json(list(panel_hash = "h", steps = list(`pfm-anchor` = list(status = "completed"))),
                       file.path(gd, "manifest.json"), auto_unbox = TRUE)
  expect_identical(pfm:::.pfmGroupExportMissing(gd), "phi-anchor.rds")
  writeLines("x", file.path(gd, "phi-anchor.rds"))
  expect_length(pfm:::.pfmGroupExportMissing(gd), 0)
})
