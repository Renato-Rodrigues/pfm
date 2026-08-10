# Feasible-path recursion (ADR 0040/0041) and delivery to IAM regions.
#
# The two defects these tests pin were live in the paper's figure script and are
# the reason the function exists (docs/psm-ceiling-feedback-diagnosis.md):
#   (1) lambda is a gap-closure rate on the LOGIT scale, so the recursion must run
#       there and be transformed once at the end;
#   (2) the attractor is the ECM equilibrium, not the SFA frontier.

specFP <- function() list(
  actorPowerDrivers = "Actor Power Index",
  actorPowerIndex = "Actor Power Index",
  instQualityDrivers = "Rule of Law (VDem)",
  controlDrivers = NULL,
  regionMappingFixedEffects = NULL,
  panelTransform = "levels",
  logisticTimeTrend = FALSE
)

fpFixture <- function() {
  list(spec = specFP(), hist = makePSMagpie(), scen = makePSMScenarioMagpie())
}

test_that("the path runs on the transformed scale and converges to the ECM equilibrium", {
  f <- fpFixture()
  p <- projectFeasiblePath(f$spec, "Bulk", f$hist, f$scen,
                           modelDir = NULL, verbose = FALSE)
  expect_true(all(c("region", "year", "feasibleIndex", "equilibriumIndex",
                    "ceilingIndex", "gapIndex", "efficiencyRatio") %in% names(p)))
  expect_true(all(p$feasibleIndex >= 0 & p$feasibleIndex <= 10, na.rm = TRUE))
  lam <- attr(p, "lambda")
  expect_gt(lam, 0)
  expect_lt(lam, 1)

  # Reproduce one region's recursion by hand ON THE LOGIT SCALE. This is the test
  # that fails if anyone "simplifies" the code back to natural-scale gaps.
  r <- p$region[1]
  d <- p[p$region == r, ]
  d <- d[order(d$year), ]
  eta <- function(v) stats::qlogis(pmin(pmax(v / 10, 1e-9), 1 - 1e-9))
  seed <- eta(d$seedIndex[1])
  yrs <- c(attr(p, "seedYear"), d$year)
  prev <- seed
  man <- numeric(nrow(d))
  for (i in seq_len(nrow(d))) {
    dt <- yrs[i + 1] - yrs[i]
    lamEff <- 1 - (1 - lam)^dt
    prev <- prev + lamEff * (eta(d$equilibriumIndex[i]) - prev)
    man[i] <- 10 * stats::plogis(prev)
  }
  expect_equal(d$feasibleIndex, man, tolerance = 1e-6)
})

test_that("every step moves TOWARD the current equilibrium and never overshoots it", {
  f <- fpFixture()
  p <- projectFeasiblePath(f$spec, "Bulk", f$hist, f$scen, modelDir = NULL)
  p <- p[is.finite(p$feasibleIndex), ]
  for (d in split(p, p$region)) {
    d <- d[order(d$year), ]
    prev <- c(d$seedIndex[1], utils::head(d$feasibleIndex, -1))
    step <- d$feasibleIndex - prev
    want <- d$equilibriumIndex - prev
    moving <- abs(want) > 1e-9
    # Same direction as the target ...
    expect_true(all(sign(step[moving]) == sign(want[moving])))
    # ... and never past it (lambda < 1 => a strict partial adjustment).
    expect_true(all(abs(step[moving]) <= abs(want[moving]) + 1e-8))
  }
})

test_that("lambda can be imposed, and a faster lambda closes the gap faster", {
  f <- fpFixture()
  slow <- projectFeasiblePath(f$spec, "Bulk", f$hist, f$scen, lambda = 0.02, modelDir = NULL)
  fast <- projectFeasiblePath(f$spec, "Bulk", f$hist, f$scen, lambda = 0.30, modelDir = NULL)
  expect_equal(attr(slow, "lambda"), 0.02)
  expect_equal(attr(fast, "lambda"), 0.30)
  # The equilibrium is a property of the FIT and must not move with imposed lambda.
  expect_equal(slow$equilibriumIndex, fast$equilibriumIndex, tolerance = 1e-10)
  gapSlow <- mean(abs(slow$feasibleIndex - slow$equilibriumIndex), na.rm = TRUE)
  gapFast <- mean(abs(fast$feasibleIndex - fast$equilibriumIndex), na.rm = TRUE)
  expect_lt(gapFast, gapSlow)
  expect_error(projectFeasiblePath(f$spec, "Bulk", f$hist, f$scen, lambda = 1.5,
                                   modelDir = NULL), "lambda must lie")
})

test_that("the frontier is a BOUND, never the attractor", {
  f <- fpFixture()
  free <- projectFeasiblePath(f$spec, "Bulk", f$hist, f$scen, modelDir = NULL)
  # A deliberately low ceiling: intercept only, well below the equilibrium.
  low <- c("(Intercept)" = -2)
  bounded <- projectFeasiblePath(f$spec, "Bulk", f$hist, f$scen,
                                 frontierBeta = low, modelDir = NULL)
  expect_true(all(is.na(free$ceilingIndex)))
  expect_true(all(bounded$feasibleIndex <= bounded$ceilingIndex + 1e-8, na.rm = TRUE))
  expect_gt(attr(bounded, "ceilingBindShare"), 0)
  # Binding lowers the path but never raises it: the ceiling caps, never pulls up.
  m <- merge(free[, c("region", "year", "feasibleIndex")],
             bounded[, c("region", "year", "feasibleIndex")],
             by = c("region", "year"), suffixes = c(".free", ".bound"))
  m <- m[is.finite(m$feasibleIndex.free) & is.finite(m$feasibleIndex.bound), ]
  expect_gt(nrow(m), 0)
  expect_true(all(m$feasibleIndex.bound <= m$feasibleIndex.free + 1e-8))
})

test_that("frozen-gap holds the polity at its seed position and needs a ceiling", {
  f <- fpFixture()
  expect_error(
    projectFeasiblePath(f$spec, "Bulk", f$hist, f$scen, rule = "frozen-gap",
                        modelDir = NULL),
    "frontierBeta"
  )
  fb <- c("(Intercept)" = 0.5)   # constant ceiling => frozen gap must be flat
  fg <- projectFeasiblePath(f$spec, "Bulk", f$hist, f$scen, rule = "frozen-gap",
                            frontierBeta = fb, modelDir = NULL)
  expect_equal(fg$rule[1], "frozen-gap")
  seeded <- fg[is.finite(fg$feasibleIndex), ]
  byr <- split(seeded, seeded$region)
  flat <- vapply(byr, function(d) diff(range(d$feasibleIndex)), 0)
  expect_true(all(flat < 1e-8))
  # ratio and absolute agree only when the ceiling is constant; both stay bounded.
  fgA <- projectFeasiblePath(f$spec, "Bulk", f$hist, f$scen, rule = "frozen-gap",
                             gapMeasure = "absolute", frontierBeta = fb, modelDir = NULL)
  expect_equal(fg$feasibleIndex, fgA$feasibleIndex, tolerance = 1e-6)
})

test_that("ratchet enforces a non-decreasing path and is OFF by default", {
  f <- fpFixture()
  p <- projectFeasiblePath(f$spec, "Bulk", f$hist, f$scen, modelDir = NULL)
  rat <- projectFeasiblePath(f$spec, "Bulk", f$hist, f$scen, ratchet = TRUE,
                             modelDir = NULL)
  dec <- function(x) {
    unlist(lapply(split(x, x$region), function(d) {
      diff(d$feasibleIndex[order(d$year)]) < -1e-8
    }))
  }
  expect_false(any(dec(rat), na.rm = TRUE))
  expect_true(is.finite(attr(p, "nonMonotoneShare")))
})

test_that("out-of-coverage regions are flagged, and the guard audit rides along", {
  f <- fpFixture()
  p <- projectFeasiblePath(f$spec, "Bulk", f$hist, f$scen, modelDir = NULL)
  expect_true("R99" %in% p$region)                 # absent from the training sample
  expect_true(all(p$outOfCoverage[p$region == "R99"]))
  expect_false(any(p$outOfCoverage[p$region == "R1"]))
  expect_true(all(c("driverOutOfSupport", "driverOutOfSample") %in% names(p)))
})

test_that("a country with no observed seed yields NA, never a fabricated path", {
  f <- fpFixture()
  # R99 appears only in the scenario panel, so it has no seed-year observation.
  sp <- projectFeasiblePath(f$spec, "Bulk", f$hist, f$scen, modelDir = NULL)
  fg <- projectFeasiblePath(f$spec, "Bulk", f$hist, f$scen, rule = "frozen-gap",
                            frontierBeta = c("(Intercept)" = 0.5), modelDir = NULL)
  for (p in list(sp, fg)) {
    expect_true(all(is.na(p$feasibleIndex[p$region == "R99"])))
    expect_true(all(is.finite(p$feasibleIndex[p$region == "R1"])))
  }
})

# ── delivery to IAM regions (item 1.4) ────────────────────────────────────────

mapFP <- function() {
  data.frame(
    CountryCode = c(paste0("R", 1:12), "R99"),
    RegionCode = c(rep("REG_A", 6), rep("REG_B", 6), "REG_B"),
    stringsAsFactors = FALSE
  )
}

test_that("aggregation weights the OUTPUT and warns when weights are missing", {
  f <- fpFixture()
  p <- projectFeasiblePath(f$spec, "Bulk", f$hist, f$scen,
                           frontierBeta = c("(Intercept)" = 0.8), modelDir = NULL)
  expect_warning(a <- aggregateFeasibilityToRegions(p, mapFP(), verbose = FALSE),
                 "EQUAL")
  expect_setequal(unique(a$region), c("REG_A", "REG_B"))
  expect_equal(attr(a, "weightSource"), "equal")

  # A weighted mean must move toward the heavily-weighted country.
  w <- stats::setNames(c(rep(1, 5), 1000, rep(1, 7)), c(paste0("R", 1:12), "R99"))
  b <- aggregateFeasibilityToRegions(p, mapFP(), weights = w)
  y <- min(a$year)
  heavy <- p$feasibleIndex[p$region == "R6" & p$year == y]
  expect_lt(abs(b$feasibleIndex[b$region == "REG_A" & b$year == y] - heavy),
            abs(a$feasibleIndex[a$region == "REG_A" & a$year == y] - heavy))
})

test_that("aggregation excludes out-of-coverage countries and reports the share", {
  f <- fpFixture()
  p <- projectFeasiblePath(f$spec, "Bulk", f$hist, f$scen,
                           frontierBeta = c("(Intercept)" = 0.8), modelDir = NULL)
  a <- suppressWarnings(aggregateFeasibilityToRegions(p, mapFP()))
  y <- min(a$year)
  ra <- a[a$region == "REG_A" & a$year == y, ]
  rb <- a[a$region == "REG_B" & a$year == y, ]
  expect_equal(ra$inCoverageShare, 1)               # R1-R6 all in coverage
  expect_lt(rb$inCoverageShare, 1)                  # R99 is not
  expect_equal(rb$nCountries - rb$nCountriesInCoverage, 1)
  # The reported ceiling describes only the estimated part of the region.
  inCov <- p[p$year == y & p$region %in% paste0("R", 7:12), "ceilingIndex"]
  expect_equal(rb$ceilingIndex, mean(inCov), tolerance = 1e-8)
})

test_that("a thinly-covered region loses its ceiling and becomes uncoupled", {
  f <- fpFixture()
  p <- projectFeasiblePath(f$spec, "Bulk", f$hist, f$scen,
                           frontierBeta = c("(Intercept)" = 0.8), modelDir = NULL)
  m <- data.frame(CountryCode = c(paste0("R", 1:12), "R99"),
                  RegionCode = c(rep("REG_A", 12), "SOLO"), stringsAsFactors = FALSE)
  a <- suppressWarnings(aggregateFeasibilityToRegions(p, m, minCoverage = 0.5))
  solo <- a[a$region == "SOLO", ]
  expect_true(all(!solo$ceilingValid))
  expect_true(all(is.na(solo$ceilingIndex)))
  expect_true(all(is.na(solo$tier)))
  expect_true(all(solo$phi == 1))                   # uncoupled: speeds only
})

test_that("tiers rank on the gap and phi spans 1 down to 1 - theta", {
  f <- fpFixture()
  p <- projectFeasiblePath(f$spec, "Bulk", f$hist, f$scen,
                           frontierBeta = c("(Intercept)" = 0.8), modelDir = NULL)
  m <- data.frame(CountryCode = paste0("R", 1:12),
                  RegionCode = paste0("REG_", 1:12), stringsAsFactors = FALSE)
  a <- suppressWarnings(aggregateFeasibilityToRegions(p, m, theta = 0.6, nTiers = 4))
  ty <- attr(a, "tierYear")
  z <- a[a$year == ty & a$ceilingValid, ]
  expect_setequal(sort(unique(z$tier)), 1:4)
  # Bigger gap => worse tier => smaller share.
  expect_gt(stats::cor(z$gapIndex, z$tier, method = "spearman"), 0.9)
  expect_equal(max(z$phi), 1)
  expect_equal(min(z$phi), 1 - 0.6, tolerance = 1e-12)
  # Tiers are assigned once and held fixed.
  byReg <- tapply(a$tier, a$region, function(v) length(unique(v[!is.na(v)])))
  expect_true(all(byReg <= 1))
})

test_that("theta = 0 is the exact uncoupled null", {
  f <- fpFixture()
  p <- projectFeasiblePath(f$spec, "Bulk", f$hist, f$scen,
                           frontierBeta = c("(Intercept)" = 0.8), modelDir = NULL)
  a <- suppressWarnings(aggregateFeasibilityToRegions(p, mapFP(), theta = 0))
  expect_true(all(a$phi == 1))
  expect_error(
    suppressWarnings(aggregateFeasibilityToRegions(p, mapFP(), theta = 1)),
    "theta must be"
  )
})
