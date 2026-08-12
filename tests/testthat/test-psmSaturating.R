# Saturating actor-power transform (ADR 0040).
#
# The actor-power drivers are energy-system shares whose historical range covers a
# small part of the range any transition scenario visits, so a model linear in the
# share extrapolates its slopes - including the negative AP x IQ interaction
# slopes - far outside the identified region. That was the diagnosed cause of the
# declining projections and the perverse ceiling feedback
# (docs/psm-ceiling-feedback-diagnosis.md). These tests pin the transform, its
# freeze discipline, the widened guard that goes with it, and the sweep axis.

# Split actor-power fixture: STRICTLY POSITIVE shares (Innovator / Incumbent
# Power), unlike the composite Actor Power Index fixture in helper-psm.R, which is
# a difference and takes negative values.
makePSMSplitMagpie <- function(scenario = FALSE) {
  set.seed(77)
  regions <- if (scenario) c(paste0("R", 1:10), "R99") else paste0("R", 1:10)
  years <- if (scenario) 2019:2040 else 2000:2019
  nR <- length(regions)
  nY <- length(years)
  # The scenario panel carries drivers only: an all-NA outcome column would make
  # preparePanelData drop every row (it removes NA-outcome rows).
  vars <- c(if (!scenario) "Policy Stringency|Bulk",
            "Innovator Power|Bulk", "Incumbent Power|Bulk", "Rule of Law (VDem)")
  m <- magclass::new.magpie(regions, years, vars, fill = NA)
  # Innovator power grows over time (the transition); scenario years push it well
  # beyond the historical range, which is the whole point.
  base <- seq(0.02, if (scenario) 0.75 else 0.30, length.out = nY)
  inn <- matrix(rep(base, each = nR), nR, nY) * matrix(runif(nR * nY, 0.7, 1.3), nR, nY)
  inc <- matrix(runif(nR * nY, 0.1, 0.6), nR, nY)
  iq <- matrix(runif(nR * nY), nR, nY)
  m[, , "Innovator Power|Bulk"] <- as.vector(inn)
  m[, , "Incumbent Power|Bulk"] <- as.vector(inc)
  m[, , "Rule of Law (VDem)"] <- as.vector(iq)
  if (!scenario) {
    y <- matrix(NA_real_, nR, nY)
    for (t in 2:nY) {
      eta <- -0.5 + 1.5 * inn[, t - 1] - 1.0 * inc[, t - 1] + 1.2 * iq[, t - 1] +
        stats::rnorm(nR, sd = 0.2)
      y[, t] <- 10 * stats::plogis(eta)
    }
    m[, , "Policy Stringency|Bulk"] <- as.vector(y)
  }
  m
}

prepArgs <- function(...) {
  c(list(sector = "Bulk",
         actorPowerDrivers = c("Innovator Power", "Incumbent Power"),
         actorPowerIndex = c("Innovator Power", "Incumbent Power"),
         instQualityDrivers = "Rule of Law (VDem)",
         controlDrivers = NULL,
         regionMappingFixedEffects = NULL,
         outcomeVar = "Policy Stringency"), list(...))
}

test_that("apTransform = 'linear' is the default and stores no saturation parameter", {
  m <- makePSMSplitMagpie()
  dfL <- do.call(preparePanelData, prepArgs(data = m))
  sc <- attr(dfL, "driverScaling")
  expect_true(all(c("Innovator.Power", "Incumbent.Power") %in% names(sc)))
  expect_false(any(vapply(sc, function(z) "sat" %in% names(z) && is.finite(z[["sat"]]),
                          logical(1))))
})

test_that("saturating transform uses the training median and is applied before scaling", {
  m <- makePSMSplitMagpie()
  raw <- do.call(preparePanelData, prepArgs(data = m))          # linear reference
  sat <- do.call(preparePanelData, prepArgs(data = m, apTransform = "saturating"))
  scL <- attr(raw, "driverScaling")
  scS <- attr(sat, "driverScaling")

  # The stored `sat` is the training median on the NATURAL scale: de-standardize
  # the linear column to recover it.
  natural <- raw$Innovator.Power * scL$Innovator.Power[["sd"]] + scL$Innovator.Power[["mean"]]
  expect_equal(unname(scS$Innovator.Power[["sat"]]), stats::median(natural, na.rm = TRUE),
               tolerance = 1e-8)

  # And the transformed column is exactly (x/(x+med) - mean)/sd of the mapped values.
  mapped <- natural / (natural + scS$Innovator.Power[["sat"]])
  expect_equal(sat$Innovator.Power,
               (mapped - scS$Innovator.Power[["mean"]]) / scS$Innovator.Power[["sd"]],
               tolerance = 1e-8)

  # Strictly increasing => the rank order of countries is untouched.
  expect_equal(order(sat$Innovator.Power), order(raw$Innovator.Power))
})

test_that("IQ and control columns are NOT transformed", {
  m <- makePSMSplitMagpie()
  raw <- do.call(preparePanelData, prepArgs(data = m))
  sat <- do.call(preparePanelData, prepArgs(data = m, apTransform = "saturating"))
  expect_equal(sat$Rule.of.Law..VDem., raw$Rule.of.Law..VDem., tolerance = 1e-10)
  expect_false("sat" %in% names(attr(sat, "driverScaling")$Rule.of.Law..VDem.))
})

test_that("the saturation parameter is FROZEN: apply mode reuses it, never re-derives", {
  mHist <- makePSMSplitMagpie()
  mScen <- makePSMSplitMagpie(scenario = TRUE)
  fitDf <- do.call(preparePanelData, prepArgs(data = mHist, apTransform = "saturating"))
  scaling <- attr(fitDf, "driverScaling")

  applyDf <- do.call(preparePanelData, prepArgs(data = mScen, driverScaling = scaling))
  # apTransform is NOT passed in apply mode - the transform must ride along inside
  # driverScaling, which is what lets every existing caller work unchanged.
  expect_equal(unname(attr(applyDf, "driverScaling")$Innovator.Power[["sat"]]),
               unname(scaling$Innovator.Power[["sat"]]))

  # Recomputing the median from the (much higher) scenario data would give a
  # different, larger parameter - assert we did not do that.
  scenNatural <- magclass::as.data.frame(mScen[, , "Innovator Power|Bulk"])$Value
  expect_gt(stats::median(scenNatural, na.rm = TRUE), scaling$Innovator.Power[["sat"]])
})

test_that("negative-valued actor power (the composite index) is skipped with a warning", {
  m <- makePSMagpie()   # composite Actor Power Index in [-0.8, 0.1]
  expect_warning(
    df <- preparePanelData(data = m, sector = "Bulk",
                           actorPowerDrivers = "Actor Power Index",
                           actorPowerIndex = "Actor Power Index",
                           instQualityDrivers = "Rule of Law (VDem)",
                           controlDrivers = NULL, regionMappingFixedEffects = NULL,
                           outcomeVar = "Policy Stringency",
                           apTransform = "saturating"),
    "saturating"
  )
  expect_true(is.na(attr(df, "driverScaling")$Actor.Power.Index[["sat"]]))
})

test_that("the guard widens to the physical share domain for saturating columns only", {
  m <- makePSMSplitMagpie()
  dfL <- do.call(preparePanelData, prepArgs(data = m))
  dfS <- do.call(preparePanelData, prepArgs(data = m, apTransform = "saturating"))

  rL <- pfm:::.driverSupportRanges(dfL, attr(dfL, "driverScaling"))
  rS <- pfm:::.driverSupportRanges(dfS, attr(dfS, "driverScaling"))

  # Linear: guard range == empirical range.
  expect_equal(rL$Innovator.Power, attr(rL, "empirical")$Innovator.Power)
  # Saturating: strictly wider than the empirical range, and finite.
  expect_gt(rS$Innovator.Power[["max"]], attr(rS, "empirical")$Innovator.Power[["max"]])
  expect_lt(rS$Innovator.Power[["min"]], attr(rS, "empirical")$Innovator.Power[["min"]])
  expect_true(all(is.finite(rS$Innovator.Power)))
  # IQ columns keep the empirical range under both.
  expect_equal(rS$Rule.of.Law..VDem., attr(rS, "empirical")$Rule.of.Law..VDem.)
})

test_that("the guard reports out-of-SAMPLE separately so the wider range hides nothing", {
  mHist <- makePSMSplitMagpie()
  mScen <- makePSMSplitMagpie(scenario = TRUE)
  dfS <- do.call(preparePanelData, prepArgs(data = mHist, apTransform = "saturating"))
  scaling <- attr(dfS, "driverScaling")
  scen <- do.call(preparePanelData, prepArgs(data = mScen, driverScaling = scaling))

  g <- pfm:::.psmDriverGuard(scen, pfm:::.driverSupportRanges(dfS, scaling))
  expect_true(all(c("outOfSupport", "outOfSample") %in% names(g)))
  # The scenario runs far past the historical share range, so the out-of-sample
  # audit must fire even though the widened guard clamps little or nothing.
  expect_gt(mean(g$outOfSample, na.rm = TRUE), 0)
  expect_gte(mean(g$outOfSample, na.rm = TRUE), mean(g$outOfSupport, na.rm = TRUE))
})

test_that("psmSpecs appends saturating twins for split-AP specs only", {
  specs <- list(
    list(name = "X-0001 splitAP", panelTransform = "levels",
         actorPowerDrivers = c("Innovator Power", "Incumbent Power"),
         actorPowerIndex = c("Innovator Power", "Incumbent Power"),
         instQualityDrivers = "Rule of Law (VDem)"),
    list(name = "X-0002 compAP", panelTransform = "levels",
         actorPowerDrivers = "Actor Power Index",
         actorPowerIndex = "Actor Power Index",
         instQualityDrivers = "Rule of Law (VDem)")
  )
  out <- psmSpecs(specs, verbose = FALSE)
  expect_equal(length(out), 3L)                       # 2 originals + 1 split twin
  tw <- Filter(function(x) identical(x$apTransform, "saturating"), out)
  expect_equal(length(tw), 1L)
  expect_match(tw[[1]]$name, "satAP$")
  expect_true(any(grepl("Innovator", unlist(tw[[1]]$actorPowerIndex))))
  # Originals keep their names (X-numbers stable across sweeps) and stay linear.
  expect_equal(out[[1]]$name, "X-0001 splitAP")
  expect_equal(out[[1]]$apTransform, "linear")
})

test_that("linear and saturating fits do not collide in the fit cache", {
  skip_on_cran()
  m <- makePSMSplitMagpie()
  dir <- withr::local_tempdir()
  common <- list(data = m, sector = "Bulk", estimator = "satP",
                 actorPowerDrivers = c("Innovator Power", "Incumbent Power"),
                 actorPowerIndex = c("Innovator Power", "Incumbent Power"),
                 instQualityDrivers = "Rule of Law (VDem)", controlDrivers = NULL,
                 regionMappingFixedEffects = NULL, modelDir = dir,
                 updateIndex = FALSE, verbose = FALSE)
  fL <- do.call(estimatePolicyStringencyModel, c(common, list(apTransform = "linear")))
  fS <- do.call(estimatePolicyStringencyModel, c(common, list(apTransform = "saturating")))
  expect_false(isTRUE(all.equal(unname(stats::coef(fL$model)),
                                unname(stats::coef(fS$model)))))
  # Re-fitting the linear spec must return the LINEAR fit, not the cached satAP one.
  fL2 <- do.call(estimatePolicyStringencyModel, c(common, list(apTransform = "linear")))
  expect_equal(unname(stats::coef(fL2$model)), unname(stats::coef(fL$model)),
               tolerance = 1e-8)
})

# --- country exclusion (docs/psm-pecoal-estonia-issue.md) ----------------------
# Estonia is dropped because its upstream PE|Coal is negative, which the clamp turns
# into a coal share of 0.0 - a "clean" reading for a very carbon-intensive system.
# The exclusion must be a visible option, never a silent hard-coded filter.

test_that("excludeCountries defaults to EST and is reversible", {
  expect_equal(eval(formals(preparePanelData)$excludeCountries), "EST")
  withr::with_options(list(pfm.excludeCountries = character(0)), {
    expect_length(eval(formals(preparePanelData)$excludeCountries), 0)
  })
})

test_that("an excluded country is removed and the removal is announced", {
  m <- makePSMagpie()
  regs <- magclass::getItems(m, dim = 1)
  skip_if(length(regs) < 2)
  target <- regs[1]
  args <- list(data = m, sector = "Bulk",
               actorPowerDrivers = "Actor Power Index",
               actorPowerIndex = "Actor Power Index",
               instQualityDrivers = "Rule of Law (VDem)", controlDrivers = NULL,
               regionMappingFixedEffects = NULL, outcomeVar = "Policy Stringency")
  keptAll <- do.call(preparePanelData, c(args, list(excludeCountries = character(0))))
  expect_true(target %in% as.character(keptAll$region))
  expect_message(
    dropped <- do.call(preparePanelData, c(args, list(excludeCountries = target))),
    "excluding")
  expect_false(target %in% as.character(dropped$region))
  expect_lt(nrow(dropped), nrow(keptAll))
})
