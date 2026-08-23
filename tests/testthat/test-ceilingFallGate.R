# nolint start
# The ceilingCollapse gate (TODO item 11). .psmCeilingTrajectory itself needs a
# frontier fit and a scenario panel, so it is mocked here: what these tests lock
# in is the GATE's contract - off by default, severe when tripped, and recorded
# for every model it evaluates whether or not it trips.

makeProj <- function(years = seq(2025, 2100, 5), regions = c("AAA", "BBB")) {
  d <- expand.grid(region = regions, year = years, stringsAsFactors = FALSE)
  d$index <- 4 + 0.01 * (d$year - 2025)      # gentle rise: no other rule fires
  d$outOfCoverage <- FALSE
  d$driverOutOfSupport <- 0
  d[order(d$region, d$year), ]
}

walk <- function(ratio, gate) {
  testthat::local_mocked_bindings(
    projectPSMSpecScenario = function(...) makeProj(),
    .psmCeilingTrajectory = function(...) list(ratio = ratio, ceil0 = 8, ceil1 = 8 * ratio,
                                               year0 = 2025, year1 = 2100)
  )
  pfm:::.psmSanitySelect(
    passModels = "SPEC-A", specByName = list(`SPEC-A` = list(name = "SPEC-A")),
    sectors = "Bulk", panelData = NULL, scenarioData = NULL, modelDir = NULL,
    batchSize = 5, maxModels = 5, thresholds = list(), regionBlocks = NULL,
    histIndexBySector = list(Bulk = NULL), indexMax = 10,
    ceilingFallGate = gate
  )
}

test_that("the gate is off by default and the spec passes", {
  res <- walk(ratio = 0.40, gate = NA_real_)      # a collapsing ceiling ...
  expect_identical(res$chosen, "SPEC-A")
  expect_false(isTRUE(res$forced))                # ... is accepted when the gate is off
  expect_null(res$flags[["SPEC-A"]]$rule)
})

test_that("a collapsing ceiling is a severe failure when the gate is on", {
  res <- walk(ratio = 0.40, gate = 0.90)
  expect_true(isTRUE(res$forced))                 # nothing passed; walker was forced
  fl <- res$flags[["SPEC-A"]]
  expect_true("ceilingCollapse" %in% fl$rule)
  expect_identical(fl$severity[fl$rule == "ceilingCollapse"], "severe")
  expect_equal(fl$value[fl$rule == "ceilingCollapse"], 0.40)
})

test_that("a stable ceiling clears the same gate", {
  res <- walk(ratio = 0.98, gate = 0.90)
  expect_identical(res$chosen, "SPEC-A")
  expect_false(isTRUE(res$forced))
  expect_false("ceilingCollapse" %in% res$flags[["SPEC-A"]]$rule)
})

test_that("the ratio is recorded for every evaluated model, passing or not", {
  expect_equal(walk(0.98, 0.90)$ceiling[["SPEC-A"]][["Bulk"]], 0.98)
  expect_equal(walk(0.40, 0.90)$ceiling[["SPEC-A"]][["Bulk"]], 0.40)
  # ... and not computed at all when the gate is off, so the default costs nothing
  expect_length(walk(0.40, NA_real_)$ceiling, 0)
})
test_that("the production default is on at 0.90 (ADR 0043)", {
  # Guards against an accidental flip back: the gate being on by default IS the
  # decision, and the test fixtures deliberately override it to NA (helper-psm.R).
  expect_equal(formals(runPSMSweep)$ceilingFallGate, 0.90)
  expect_true(is.na(formals(psmTestSweep)$ceilingFallGate))
})
# nolint end
