# Fixtures (makePSMSweepMagpie, makePSMSweepScenarioMagpie) live in helper-psm.R.

# ── computePolicyStringencySanity: bounded-index rules ──────────────────────────

sanityProj <- function(index, regions = "R1", years = 2020:2025) {
  expand.grid(region = regions, year = years, stringsAsFactors = FALSE) |>
    (\(d) {
      d <- d[order(d$region, d$year), ]
      d$sector <- "Bulk"
      d$index <- index
      d$eta <- NA_real_
      d
    })()
}

test_that("a calm bounded projection raises no flags", {
  proj <- sanityProj(rep(5, 6))
  sn <- computePolicyStringencySanity(proj)
  expect_equal(sn$summary$nSevere, 0)
  expect_equal(sn$summary$nWarning, 0)
})

test_that("price-explosion style rules have no analogue: index near the ceiling is not severe", {
  proj <- sanityProj(rep(9.9, 6))
  sn <- computePolicyStringencySanity(proj)
  expect_equal(sn$summary$nSevere, 0)
  expect_true("index_saturation" %in% sn$flags$rule)
  expect_true(all(sn$flags$severity[sn$flags$rule == "index_saturation"] == "warning"))
})

test_that("saturation escalates to severe only before saturationSevereBefore", {
  proj <- sanityProj(rep(9.9, 6))
  sn <- computePolicyStringencySanity(proj, thresholds = list(saturationSevereBefore = 2030))
  expect_gt(sn$summary$nSevere, 0)
  expect_true(all(sn$flags$severity[sn$flags$rule == "index_saturation" &
                                      sn$flags$year < 2030] == "severe"))
})

test_that("a dead region block is severe", {
  proj <- rbind(sanityProj(rep(0.1, 6), regions = "R1"),
                sanityProj(rep(5, 6), regions = "R2"))
  sn <- computePolicyStringencySanity(proj)
  dead <- sn$flags[sn$flags$rule == "dead_region_block", ]
  expect_equal(nrow(dead), 1)
  expect_identical(dead$region, "R1")
  expect_identical(dead$severity, "severe")
})

test_that("regionBlocks group the dead rule: one live member saves the block", {
  proj <- rbind(sanityProj(rep(0.1, 6), regions = "R1"),
                sanityProj(rep(5, 6), regions = "R2"))
  blocks <- data.frame(region = c("R1", "R2"), block = "B1", stringsAsFactors = FALSE)
  sn <- computePolicyStringencySanity(proj, regionBlocks = blocks)
  expect_false("dead_region_block" %in% sn$flags$rule)
})

test_that("seam jumps and single-year spikes are warnings", {
  proj <- sanityProj(c(8, 8, 8, 2, 2, 2))
  hist <- data.frame(region = "R1", year = 2015:2019, index = 3, stringsAsFactors = FALSE)
  sn <- computePolicyStringencySanity(proj, histIndex = hist)
  expect_true("seam_jump" %in% sn$flags$rule)   # 3 -> 8 at the seam
  expect_true("index_spike" %in% sn$flags$rule) # 8 -> 2 mid-horizon
  expect_equal(sn$summary$nSevere, 0)
})

test_that("seam rule tolerates a projected region absent from histIndex (out-of-coverage)", {
  # The PSM projects all regions incl. out-of-coverage ones (USA/Brazil), which have
  # no historical CAPMF value. lastVal is an atomic named vector, so an unguarded
  # `[[` on the missing region name threw "subscript out of bounds" (cluster crash,
  # 2026-07-06). The seam rule must simply skip such regions.
  proj <- rbind(sanityProj(rep(8, 6), regions = "R1"),
                sanityProj(rep(6, 6), regions = "USA"))  # USA = out of coverage
  hist <- data.frame(region = "R1", year = 2015:2019, index = 3, stringsAsFactors = FALSE)
  expect_no_error(sn <- computePolicyStringencySanity(proj, histIndex = hist))
  expect_true("seam_jump" %in% sn$flags$rule)               # R1: 3 -> 8 seam still fires
  expect_false("USA" %in% sn$flags$region[sn$flags$rule == "seam_jump"])  # USA skipped
})

test_that("high NA share is a coverage warning", {
  proj <- sanityProj(c(5, NA, NA, NA, 5, 5))
  sn <- computePolicyStringencySanity(proj)
  expect_true("missing_share" %in% sn$flags$rule)
  expect_equal(sn$summary$nSevere, 0)
})

# ── psmSpecs: spec-grid adaptation ──────────────────────────────────────────────

test_that("psmSpecs drops PSM-meaningless axes and maps the dynamics rung", {
  specs <- list(
    list(name = "levels", instQualityDrivers = "Rule of Law (VDem)"),
    list(name = "fd", panelTransform = "hybridFD"),
    list(name = "twin | satP", stringencyOnly = TRUE, priceLink = "saturating"),
    list(name = "ridge", ridgeInteractions = TRUE),
    list(name = "lagged", includeLaggedECP = TRUE)
  )
  out <- suppressMessages(psmSpecs(specs, verbose = FALSE))
  expect_setequal(vapply(out, `[[`, character(1), "name"), c("levels", "lagged"))
  lagged <- out[[which(vapply(out, `[[`, character(1), "name") == "lagged")]]
  expect_true(lagged$includeLaggedPS)
  expect_false(lagged$includeLaggedECP)
  expect_false(lagged$includeLagged)
})

# ── projectPSMSpecScenario + runPSMSweep integration ───────────────────────────
# psmTestSpecs lives in helper-psm.R.

test_that("projectPSMSpecScenario projects a spec bounded and clamp-free", {
  m <- makePSMSweepMagpie()
  scen <- makePSMSweepScenarioMagpie()
  cfg <- suppressMessages(psmSpecs(psmTestSpecs, verbose = FALSE))[[1]]
  proj <- suppressWarnings(
    projectPSMSpecScenario(cfg, "Bulk", histData = m, scenarioData = scen, modelDir = NULL)
  )
  expect_true(all(c("region", "year", "sector", "eta", "index") %in% names(proj)))
  expect_true(all(proj$year > 2019))
  ok <- is.finite(proj$index)
  expect_true(any(ok))
  expect_true(all(proj$index[ok] >= 0 & proj$index[ok] <= 10))
})

test_that("runPSMSweep runs end-to-end and writes the Run-Group artifacts", {
  m <- makePSMSweepMagpie()
  scen <- makePSMSweepScenarioMagpie()
  resultsDir <- withr::local_tempdir()
  modelDir <- withr::local_tempdir()
  res <- suppressMessages(suppressWarnings(runPSMSweep(
    group = "psm-test", mode = "guided",
    resultsDir = resultsDir, modelDir = modelDir,
    panelData = m, scenarioData = scen, specs = psmTestSpecs,
    sectors = c("Bulk", "Diffuse"), selectFE = NULL, tierGate = "Blue",
    verbose = FALSE
  )))
  # fits: 2 specs x 2 sectors x 1 stage
  expect_equal(nrow(res$results), 4)
  expect_true(all(res$results$stage == "PolicyStringency"))
  expect_true(all(res$results$converged))
  expect_true(all(is.finite(res$results$deltaR2Theory)))
  # selection: maximin + bounded-index sanity gate
  mm <- res$maximin$PolicyStringency
  expect_s3_class(mm, "data.frame")
  expect_true(res$selected$PolicyStringency %in% c("psmA", "psmB"))
  expect_false(is.null(res$sanity$PolicyStringency$trace))
  # artifacts
  groupDir <- file.path(resultsDir, "psm-test")
  expect_true(file.exists(file.path(groupDir, "sweep.rds")))
  expect_true(file.exists(file.path(groupDir, "selected-models-psm.yml")))
  expect_true(file.exists(file.path(groupDir, "manifest.json")))
  # the selected config re-loads as PSM entries
  sel <- yaml::read_yaml(file.path(groupDir, "selected-models-psm.yml"))
  expect_equal(length(sel), 2)
  expect_true(all(vapply(sel, `[[`, character(1), "estimator") == "satP"))
  expect_true(all(grepl("^PolicyStringency: ", vapply(sel, `[[`, character(1), "model_type"))))
})

test_that("Tournament v2: the Green deployment gate selects nothing from an all-Blue pool (ADR 0039)", {
  # The fixture DGP has no interaction effect, so every spec is Blue. Under the
  # v2 default (tierGate = "Green") selection must come up EMPTY - loudly, with
  # the tier gate in the failure tally - rather than deploying a Blue spec.
  resultsDir <- withr::local_tempdir()
  res <- suppressMessages(suppressWarnings(runPSMSweep(
    group = "psm-green-gate", mode = "guided",
    resultsDir = resultsDir, modelDir = withr::local_tempdir(),
    panelData = makePSMSweepMagpie(), specs = psmTestSpecs,
    sectors = c("Bulk", "Diffuse"), selectFE = NULL, verbose = FALSE
  )))
  expect_length(res$selected, 0)
  mm <- res$maximin$PolicyStringency
  expect_true(all(!mm$gatePass))
  expect_true(any(grepl("tier below Green", mm$gateFailReason)))
  # the documented variants still record the Blue winner + sharing-cost exhibit
  v <- res$selectionVariants
  expect_false(is.null(v))
  expect_true(is.na(v$winners$Green))
  expect_true(v$winners$Blue %in% c("psmA", "psmB"))
  expect_equal(nrow(v$perSector), 2)
  expect_true(file.exists(file.path(resultsDir, "psm-green-gate", "selection-variants.rds")))
})

test_that("the scenario-responsiveness gate rejects scenario-blind specs (ADR 0039)", {
  # An IDENTICAL reference panel makes every spec's |gating - reference| delta zero
  # -> all candidates severe-flagged scenarioBlind -> forced least-flagged fallback.
  scen <- makePSMSweepScenarioMagpie()
  resultsDir <- withr::local_tempdir()
  res <- psmTestSweep("psm-blind", resultsDir, withr::local_tempdir(),
                      scenarioData = scen, referenceScenarioData = scen,
                      deltaWindow = range(magclass::getYears(scen, as.integer = TRUE)))
  sel <- res$sanity$PolicyStringency
  expect_true(isTRUE(sel$forced))
  fl <- do.call(rbind, sel$flags)
  expect_true(any(fl$rule == "scenarioBlind"))

  # A genuinely DIFFERENT reference panel clears the gate.
  scen2 <- scen
  scen2[, , "Actor Power Index|Bulk"] <- scen2[, , "Actor Power Index|Bulk"] - 0.4
  scen2[, , "Actor Power Index|Diffuse"] <- scen2[, , "Actor Power Index|Diffuse"] - 0.4
  res2 <- psmTestSweep("psm-responsive", withr::local_tempdir(), withr::local_tempdir(),
                       scenarioData = scen, referenceScenarioData = scen2,
                       deltaWindow = range(magclass::getYears(scen, as.integer = TRUE)))
  sel2 <- res2$sanity$PolicyStringency
  expect_false(isTRUE(sel2$forced))
  expect_true(res2$selected$PolicyStringency %in% c("psmA", "psmB"))
})

test_that("runPSMSweep refuses a panel without the PSM outcomes", {
  m <- makePSMSweepMagpie()
  m <- m[, , setdiff(magclass::getNames(m), "Policy Stringency|Diffuse")]
  expect_error(
    suppressMessages(runPSMSweep(
      group = "psm-bad", resultsDir = withr::local_tempdir(), modelDir = NULL,
      panelData = m, specs = psmTestSpecs, selectFE = NULL, verbose = FALSE
    )),
    "includePolicyStringency"
  )
})
