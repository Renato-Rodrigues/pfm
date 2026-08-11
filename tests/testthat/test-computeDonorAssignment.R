# Donor assignment for countries without CAPMF coverage.
#
# The properties that matter: only the RELATIVE gap is donated (never the
# ceiling), distance is measured in the model's own coefficient metric, and a
# recipient unlike anything in the estimation sample is FLAGGED rather than
# silently matched.

donorFit <- function() {
  m <- makePSMagpie()
  estimatePolicyStringencyModel(
    data = m, sector = "Bulk", estimator = "satP",
    actorPowerDrivers = "Actor Power Index", actorPowerIndex = "Actor Power Index",
    instQualityDrivers = "Rule of Law (VDem)", controlDrivers = NULL,
    regionMappingFixedEffects = NULL, logisticTimeTrend = FALSE,
    modelDir = NULL, updateIndex = FALSE, verbose = FALSE)
}

# A design containing the covered regions plus extra "uncovered" ones.
donorDesign <- function(fit, extra = c("X1", "X2")) {
  d <- fit$data[fit$data$year == max(fit$data$year), , drop = FALSE]
  add <- d[rep(1, length(extra)), , drop = FALSE]
  add$region <- extra
  # Push the extras away from the covered cloud on the highest-weighted driver.
  add$Rule.of.Law..VDem. <- add$Rule.of.Law..VDem. + c(0.05, 12)[seq_along(extra)]
  rbind(d, add)
}

test_that("only uncovered regions become recipients, and each keeps its own ceiling", {
  fit <- donorFit()
  sc <- data.frame(region = unique(as.character(fit$data$region)),
                   year = max(fit$data$year), efficiencyRatio = 0.6,
                   stringsAsFactors = FALSE)
  d <- computeDonorAssignment(fit, sc, donorDesign(fit), k = 2, sector = "Bulk")
  expect_setequal(d$region, c("X1", "X2"))
  # The output carries a relative gap only - no ceiling or index column is emitted,
  # because the recipient's ceiling comes from its own drivers.
  expect_true("efficiencyRatio" %in% names(d))
  expect_false(any(c("ceilingIndex", "frontierIndex", "index") %in% names(d)))
  expect_true(all(d$efficiencyRatio > 0 & d$efficiencyRatio <= 1, na.rm = TRUE))
})

test_that("distance uses the model's coefficient weights, not raw driver distance", {
  fit <- donorFit()
  sc <- data.frame(region = unique(as.character(fit$data$region)),
                   year = max(fit$data$year), efficiencyRatio = 0.6,
                   stringsAsFactors = FALSE)
  d <- computeDonorAssignment(fit, sc, donorDesign(fit), k = 2, sector = "Bulk")
  w <- attr(d, "weights")
  expect_true(all(w >= 0))
  expect_equal(sum(w), 1, tolerance = 1e-8)
  # Interactions and fixed effects must not enter the metric.
  expect_false(any(grepl("_x_|regionFE|Intercept|TimeTrend", names(w))))
})

test_that("a recipient unlike anything covered is flagged, not silently matched", {
  fit <- donorFit()
  sc <- data.frame(region = unique(as.character(fit$data$region)),
                   year = max(fit$data$year), efficiencyRatio = 0.6,
                   stringsAsFactors = FALSE)
  d <- computeDonorAssignment(fit, sc, donorDesign(fit), k = 2, sector = "Bulk")
  near <- d[d$region == "X1", ]
  far <- d[d$region == "X2", ]
  expect_lt(near$distance, far$distance)
  expect_equal(far$donorQuality, "none")
  expect_true(near$donorQuality %in% c("close", "far"))
})

test_that("maxDistance refuses a match outright rather than inventing one", {
  fit <- donorFit()
  sc <- data.frame(region = unique(as.character(fit$data$region)),
                   year = max(fit$data$year), efficiencyRatio = 0.6,
                   stringsAsFactors = FALSE)
  # bandPercentile = NULL disables the fallback, so "no donor" stays NA.
  d <- computeDonorAssignment(fit, sc, donorDesign(fit), k = 2,
                              maxDistance = 0.001, bandPercentile = NULL,
                              sector = "Bulk")
  expect_true(all(is.na(d$efficiencyRatio)))
  expect_true(all(is.na(d$donors)))
  expect_true(all(d$donorQuality == "none"))
  expect_true(all(d$nDonors == 0))
})

# --- the band rule ------------------------------------------------------------
# The point of the band is that nobody is left to a silent phi = 1. What must hold:
# every recipient gets a value, the value's ORIGIN is recorded, and the two
# unmatched branches are told apart by the model's own capability ordering.

test_that("every recipient gets a value, and its basis is always recorded", {
  fit <- donorFit()
  regs <- unique(as.character(fit$data$region))
  sc <- data.frame(region = regs, year = max(fit$data$year),
                   efficiencyRatio = seq(0.3, 0.9, length.out = length(regs)),
                   stringsAsFactors = FALSE)
  d <- computeDonorAssignment(fit, sc, donorDesign(fit), k = 2, sector = "Bulk")
  expect_true(all(is.finite(d$efficiencyRatio)))
  expect_true(all(d$basis %in% c("donor", "lowBand", "median")))
  expect_false(any(is.na(d$basis)))
  # A matched recipient is never overwritten by the band.
  expect_true(all(d$basis[d$donorQuality %in% c("close", "far")] == "donor"))
})

test_that("unmatched recipients split on the covered capability range, not on distance", {
  fit <- donorFit()
  regs <- unique(as.character(fit$data$region))
  sc <- data.frame(region = regs, year = max(fit$data$year),
                   efficiencyRatio = seq(0.3, 0.9, length.out = length(regs)),
                   stringsAsFactors = FALSE)
  d <- computeDonorAssignment(fit, sc, donorDesign(fit), k = 2, sector = "Bulk")
  rng <- attr(d, "coveredIndexRange")
  unm <- d[d$donorQuality == "none", ]
  if (nrow(unm)) {
    # Below the covered range => lowBand; inside or above => median. Never mixed.
    expect_equal(unm$basis, ifelse(unm$linearIndex < rng[1], "lowBand", "median"))
  }
})

test_that("the band values are the covered quantiles, and low < median", {
  fit <- donorFit()
  regs <- unique(as.character(fit$data$region))
  sc <- data.frame(region = regs, year = max(fit$data$year),
                   efficiencyRatio = seq(0.3, 0.9, length.out = length(regs)),
                   stringsAsFactors = FALSE)
  d <- computeDonorAssignment(fit, sc, donorDesign(fit), k = 2,
                              bandPercentile = 0.25, sector = "Bulk")
  bv <- attr(d, "bandValues")
  expect_equal(unname(bv[["lowBand"]]),
               unname(stats::quantile(sc$efficiencyRatio, 0.25)), tolerance = 1e-8)
  expect_equal(unname(bv[["median"]]), stats::median(sc$efficiencyRatio),
               tolerance = 1e-8)
  # The structural case must be the more generous of the two, or the rule would be
  # punishing countries for being unusual rather than for being weak.
  expect_lt(bv[["lowBand"]], bv[["median"]])
})

test_that("a lower percentile only ever moves the low band down", {
  fit <- donorFit()
  regs <- unique(as.character(fit$data$region))
  sc <- data.frame(region = regs, year = max(fit$data$year),
                   efficiencyRatio = seq(0.3, 0.9, length.out = length(regs)),
                   stringsAsFactors = FALSE)
  d25 <- computeDonorAssignment(fit, sc, donorDesign(fit), k = 2,
                                bandPercentile = 0.25, sector = "Bulk")
  d10 <- computeDonorAssignment(fit, sc, donorDesign(fit), k = 2,
                                bandPercentile = 0.10, sector = "Bulk")
  expect_lte(attr(d10, "bandValues")[["lowBand"]],
             attr(d25, "bandValues")[["lowBand"]])
  # The sensitivity must not disturb the median branch or the matches.
  expect_equal(attr(d10, "bandValues")[["median"]],
               attr(d25, "bandValues")[["median"]])
  expect_equal(d10$basis, d25$basis)
})

test_that("an invalid bandPercentile fails loudly", {
  fit <- donorFit()
  sc <- data.frame(region = unique(as.character(fit$data$region)),
                   year = max(fit$data$year), efficiencyRatio = 0.6,
                   stringsAsFactors = FALSE)
  expect_error(computeDonorAssignment(fit, sc, donorDesign(fit),
                                      bandPercentile = 1.5), "bandPercentile")
  expect_error(computeDonorAssignment(fit, sc, donorDesign(fit),
                                      bandPercentile = c(0.1, 0.2)), "bandPercentile")
})

test_that("the inherited ratio is a weighted blend of the donors, bounded by them", {
  fit <- donorFit()
  regs <- unique(as.character(fit$data$region))
  # Donors differ, so the blend must land inside their range.
  sc <- data.frame(region = regs, year = max(fit$data$year),
                   efficiencyRatio = seq(0.3, 0.9, length.out = length(regs)),
                   stringsAsFactors = FALSE)
  d <- computeDonorAssignment(fit, sc, donorDesign(fit, extra = "X1"), k = 3,
                              sector = "Bulk")
  expect_gte(d$efficiencyRatio, min(sc$efficiencyRatio))
  expect_lte(d$efficiencyRatio, max(sc$efficiencyRatio))
  expect_equal(d$nDonors, 3L)
  expect_equal(length(strsplit(d$donors, ",")[[1]]), 3L)
})

test_that("bad inputs fail loudly", {
  fit <- donorFit()
  sc <- data.frame(region = unique(as.character(fit$data$region)),
                   year = max(fit$data$year), efficiencyRatio = 0.6,
                   stringsAsFactors = FALSE)
  expect_error(computeDonorAssignment(list(), sc, donorDesign(fit)),
               "must be a fitted PSM model")
  expect_error(computeDonorAssignment(fit, sc[, c("region", "year")], donorDesign(fit)),
               "efficiencyRatio")
})

test_that("basisOverride forces a country onto a named branch", {
  fit <- donorFit()
  regs <- unique(as.character(fit$data$region))
  sc <- data.frame(region = regs, year = max(fit$data$year),
                   efficiencyRatio = seq(0.3, 0.9, length.out = length(regs)),
                   stringsAsFactors = FALSE)
  d0 <- computeDonorAssignment(fit, sc, donorDesign(fit), k = 2, sector = "Bulk")
  d1 <- computeDonorAssignment(fit, sc, donorDesign(fit), k = 2,
                               basisOverride = c(X1 = "median"), sector = "Bulk")
  bv <- attr(d1, "bandValues")
  expect_equal(d1$basis[d1$region == "X1"], "median")
  expect_equal(d1$efficiencyRatio[d1$region == "X1"], unname(bv[["median"]]))
  # Only the named country moves; everyone else is untouched.
  other <- setdiff(d0$region, "X1")
  expect_equal(d1$efficiencyRatio[match(other, d1$region)],
               d0$efficiencyRatio[match(other, d0$region)])
})

test_that("a malformed basisOverride fails loudly", {
  fit <- donorFit()
  sc <- data.frame(region = unique(as.character(fit$data$region)),
                   year = max(fit$data$year), efficiencyRatio = 0.6,
                   stringsAsFactors = FALSE)
  expect_error(computeDonorAssignment(fit, sc, donorDesign(fit),
                                      basisOverride = "median"), "basisOverride")
  expect_error(computeDonorAssignment(fit, sc, donorDesign(fit),
                                      basisOverride = c(X1 = "nonsense")), "basisOverride")
})
