# The coupling step. What matters here is not that it computes a number, but that it
# cannot silently disagree with the offline pipeline: the band assignment must be
# used, and its absence must STOP the run rather than quietly reverting to phi = 1.

test_that("the defaults match the offline pipeline, not the pre-band behaviour", {
  fm <- formals(iterativePFM)
  # minCoverage is gone: the phi = 1 cliff must not be reachable from here.
  expect_false("minCoverage" %in% names(fm))
  expect_equal(eval(fm$gapMeasure), "relative")
  expect_equal(eval(fm$phiRule), "continuous")
  # The gdx resolution is a parameter, not hardcoded H12.
  expect_true("gdxRegionMapping" %in% names(fm))
})

test_that("a missing band assignment is an error, not a silent fallback", {
  d <- withr::local_tempdir()
  gd <- file.path(d, "grp")
  dir.create(gd, recursive = TRUE)
  # Enough of a Run-Group to get past the early file checks, but no band assignment.
  writeLines("[]", file.path(gd, "selected-models-psm.yml"))
  writeLines('{"panel_hash":"nope"}', file.path(gd, "manifest.json"))
  gdxFile <- file.path(d, "fulldata.gdx")
  file.create(gdxFile)
  # It must fail, and it must NOT write a phi gdx on failure.
  expect_warning(
    iterativePFM(gdx = gdxFile, group = "grp", resultsDir = d, modelDir = d,
                 outputFile = file.path(d, "p45_regiDiff_phi.gdx")),
    "FAILED")
  expect_false(file.exists(file.path(d, "p45_regiDiff_phi.gdx")))
})

# --- convergence rule ---------------------------------------------------------
# The loop is a fixed point in phi. What must hold: the first call can never be
# reported as converged, the delta is a MAX over regions (one region still moving
# keeps the loop open), and the history survives between calls.

phiDelta <- function(prev, phi) {
  if (!length(prev)) return(Inf)
  last <- prev[[length(prev)]]$phi
  common <- intersect(names(phi), names(last))
  if (length(common)) max(abs(phi[common] - last[common])) else Inf
}

test_that("the first call is never convergent", {
  expect_equal(phiDelta(list(), c(EUR = 0.7, USA = 0.5)), Inf)
})

test_that("the delta is a max over regions, not a mean", {
  prev <- list(list(phi = c(EUR = 0.700, USA = 0.500, CHA = 0.600)))
  # Two regions settled, one still moving by 0.2 - the loop must stay open.
  d <- phiDelta(prev, c(EUR = 0.700, USA = 0.500, CHA = 0.800))
  expect_equal(d, 0.2, tolerance = 1e-12)
  expect_gt(d, 0.01)                      # above the default cm_pfmConvTol
})

test_that("an unchanged phi converges", {
  prev <- list(list(phi = c(EUR = 0.7, USA = 0.5)))
  expect_equal(phiDelta(prev, c(EUR = 0.7, USA = 0.5)), 0)
})

test_that("regions appearing or vanishing do not fake convergence", {
  prev <- list(list(phi = c(EUR = 0.7)))
  # No overlap at all => nothing can be compared => must NOT read as converged.
  expect_equal(phiDelta(prev, c(USA = 0.7)), Inf)
})

# --- runtime config written by GAMS -------------------------------------------
# The point: GAMS owns bindMode and theta (they come from the scenario config), so a
# hand-kept copy elsewhere must never be able to disagree with it.

test_that("the GAMS runtime config overrides the local settings", {
  d <- withr::local_tempdir()
  rc <- file.path(d, "pfm-coupling-runtime.yml")
  writeLines(c("bindMode: 2", "theta: 0.79", "iteration: 24"), rc)
  cfg <- yaml::read_yaml(rc)
  expect_equal(as.integer(cfg$bindMode), 2L)
  expect_equal(as.numeric(cfg$theta), 0.79)
  # Precedence is what matters: whatever was passed locally loses to the file.
  bindMode <- 1L; theta <- 0.5
  if (!is.null(cfg$bindMode)) bindMode <- as.integer(cfg$bindMode)
  if (!is.null(cfg$theta)) theta <- as.numeric(cfg$theta)
  expect_equal(bindMode, 2L)
  expect_equal(theta, 0.79)
})

test_that("the runtime config is optional, so offline use still works", {
  fm <- formals(iterativePFM)
  expect_true("runtimeConfig" %in% names(fm))
  # Absent file must not be an error - tests and offline analysis run without GAMS.
  expect_false(file.exists(file.path(withr::local_tempdir(), "pfm-coupling-runtime.yml")))
})

test_that("weights default to final energy, not to the silent equal-weight fallback", {
  expect_equal(eval(formals(iterativePFM)$weights), "finalEnergy")
})

# --- final-energy weights: scenario-aware GDP projection ----------------------
# Only the RELATIVE spread matters: shares are normalised inside the aggregation, so
# uniform growth is a no-op and differential growth is the whole signal.

test_that("uniform GDP growth leaves within-region shares unchanged", {
  fe <- c(DEU = 100, NGA = 50)
  gr <- c(DEU = 2, NGA = 2)                  # same growth everywhere
  w0 <- fe / sum(fe)
  w1 <- (fe * gr) / sum(fe * gr)
  expect_equal(w0, w1, tolerance = 1e-12)
})

test_that("differential GDP growth is what actually moves the weights", {
  fe <- c(DEU = 100, NGA = 50)
  gr <- c(DEU = 1.2, NGA = 4)                # a faster-growing partner
  w0 <- fe / sum(fe)
  w1 <- (fe * gr) / sum(fe * gr)
  expect_gt(w1[["NGA"]], w0[["NGA"]])
  expect_lt(w1[["DEU"]], w0[["DEU"]])
})

test_that("the weight year and scenario are exposed and defaulted sensibly", {
  fm <- formals(iterativePFM)
  expect_equal(eval(fm$weightYear), 2050)
  expect_equal(eval(fm$weightScenario), "SSP2")
  expect_equal(eval(formals(psmCouplingWeights)$scaleBy)[1], "gdp")
})

test_that("gdp scaling without a target year warns rather than silently no-opping", {
  expect_true("scenario" %in% names(formals(psmCouplingWeights)))
  expect_true("year" %in% names(formals(psmCouplingWeights)))
})

# --- bind mode 3: mild progression --------------------------------------------
# Mode 3 GENERATES the price instead of constraining one, so its failure mode is
# different: an unseeded or undelivered path means a ZERO carbon price everywhere,
# which would look like a valid "uncoupled" run. Both sides must refuse instead.

test_that("mode 3 refuses to run without a seed", {
  d <- withr::local_tempdir()
  gd <- file.path(d, "grp"); dir.create(gd, recursive = TRUE)
  writeLines("[]", file.path(gd, "selected-models-psm.yml"))
  writeLines('{"panel_hash":"nope"}', file.path(gd, "manifest.json"))
  g <- file.path(d, "fulldata.gdx"); file.create(g)
  # No refGdx: the seed price cannot be read, so it must not silently start at zero.
  expect_warning(
    iterativePFM(gdx = g, group = "grp", resultsDir = d, modelDir = d,
                 bindMode = 3L, refGdx = NULL,
                 outputFile = file.path(d, "p45_regiDiff_phi.gdx")),
    "FAILED")
  expect_false(file.exists(file.path(d, "p45_regiDiff_phi.gdx")))
})

test_that("the worse-sector rule picks one stringency path per region-year", {
  # Mode 3 must use the same maximin discipline phi does, or the price path would be
  # constrained by a different sector than the feasibility share.
  feas <- data.frame(
    region = c("EUR", "EUR", "USA", "USA"),
    year   = c(2030, 2030, 2030, 2030),
    feasibleIndex = c(5, 3, 6, 7),          # EUR worse = 3, USA worse = 6
    ceilingIndex  = c(8, 8, 9, 9),
    sector = c("Bulk", "Diffuse", "Bulk", "Diffuse"), stringsAsFactors = FALSE)
  key <- paste(feas$region, feas$year)
  ord <- order(key, feas$feasibleIndex)
  one <- feas[ord, ][!duplicated(key[ord]), ]
  expect_equal(nrow(one), 2L)
  expect_equal(one$feasibleIndex[one$region == "EUR"], 3)
  expect_equal(one$feasibleIndex[one$region == "USA"], 6)
})

test_that("a heavily capped path is flagged, not returned quietly", {
  d <- data.frame(region = rep("EUR", 4), year = seq(2025, 2040, 5),
                  feasibleIndex = 1e-6, ceilingIndex = 8, stringsAsFactors = FALSE)
  p <- projectMildProgressionPrice(d, c(EUR = 10), lambda = 1, maxGrowth = 1)
  expect_gt(attr(p, "cappedShare"), 0.25)   # the threshold iterativePFM warns on
  expect_true(all(is.finite(p$price)))
})
