# Design note 0009 section 7.1: the lower bound on the fossil terms, frozen in the driver scaling and
# held in the projections, and the sweep axis that adds or substitutes the bounded specs.

fosMagpie <- function(years = 2000:2015, vals = NULL) {
  set.seed(5)
  regions <- paste0("R", 1:10)
  vars <- c("Policy Stringency|Bulk", "Incumbent Power|Bulk", "Rule of Law (VDem)")
  m <- magclass::new.magpie(regions, years, vars, fill = NA)
  n <- length(regions) * length(years)
  m[, , "Incumbent Power|Bulk"] <- if (is.null(vals)) stats::runif(n, 0.05, 0.9) else vals
  m[, , "Rule of Law (VDem)"] <- stats::runif(n)
  m[, , "Policy Stringency|Bulk"] <- 10 * stats::runif(n, 0.05, 0.95)
  m
}
fosPrep <- function(m, ...) {
  preparePanelData(m, sector = "Bulk", actorPowerDrivers = "Incumbent Power", actorPowerIndex = "Incumbent Power",
                   instQualityDrivers = "Rule of Law (VDem)", controlDrivers = NULL, regionMappingFixedEffects = NULL,
                   outcomeVar = "Policy Stringency", excludeCountries = character(0), ...)
}

test_that("the bound is the training quantile, frozen in the scaling, and held in apply mode", {
  m <- fosMagpie()
  d0 <- fosPrep(m)
  d <- fosPrep(m, fossilFloor = 0.1)
  sc <- attr(d, "driverScaling")[["Incumbent.Power"]]
  raw <- as.vector(m[, 2001:2015, "Incumbent Power|Bulk"])   # the training rows (lag 1)
  expect_true("floor" %in% names(sc))
  expect_equal(unname(sc[["floor"]]), unname(stats::quantile(raw, 0.1)), tolerance = 0.02)
  expect_false("floor" %in% names(attr(d0, "driverScaling")[["Incumbent.Power"]]))   # no bound unless asked
  expect_equal(min(d$Incumbent.Power, na.rm = TRUE), (sc[["floor"]] - sc[["mean"]]) / sc[["sd"]])
  # the interaction is formed from the bounded term
  expect_equal(d$Incumbent.Power_x_Rule.of.Law..VDem., d$Incumbent.Power * d$Rule.of.Law..VDem.)
  # apply mode: a scenario far below the bound is held at it
  scen <- fosMagpie(years = 2020:2030, vals = 0.001)
  ds <- preparePanelData(scen, sector = "Bulk", actorPowerDrivers = "Incumbent Power", actorPowerIndex = "Incumbent Power",
                         instQualityDrivers = "Rule of Law (VDem)", controlDrivers = NULL, regionMappingFixedEffects = NULL,
                         outcomeVar = "Policy Stringency", excludeCountries = character(0),
                         driverScaling = attr(d, "driverScaling"))
  expect_true(all(abs(ds$Incumbent.Power - (sc[["floor"]] - sc[["mean"]]) / sc[["sd"]]) < 1e-12, na.rm = TRUE) && any(is.finite(ds$Incumbent.Power)))
  expect_error(fosPrep(m, fossilFloor = 1.5), "fossilFloor")
})

test_that("the sweep axis substitutes the bounded specs, or adds them as twins with \"none\"", {
  sp <- function(name, api) list(name = name, model_type = "PolicyStringency", actorPowerIndex = api,
                                 instQualityDrivers = "Government Effectiveness (WGI)", controlDrivers = character(0),
                                 panelTransform = "levels")
  specs <- list(sp("S1", c("Innovator Power", "Incumbent Power")), sp("S2", "Innovator Power"))
  rep <- suppressMessages(pfmSpecs(specs, verbose = FALSE, apTransforms = "linear", fossilFloors = "0.05"))
  expect_identical(vapply(rep, `[[`, "", "name"), c("S2", "S1 fos5"))
  expect_equal(rep[[2]]$fossilFloor, 0.05)
  tw <- suppressMessages(pfmSpecs(specs, verbose = FALSE, apTransforms = "linear", fossilFloors = c("none", "0.05")))
  expect_identical(vapply(tw, `[[`, "", "name"), c("S1", "S2", "S1 fos5"))
  expect_identical(vapply(suppressMessages(pfmSpecs(specs, verbose = FALSE, apTransforms = "linear")), `[[`, "", "name"), c("S1", "S2"))
  expect_error(pfmSpecs(specs, verbose = FALSE, fossilFloors = "1.2"), "fossilFloors")
})

test_that("the sweep options carry the axis", {
  o <- pfm:::.pfmSweepOptionsNormalise(list(fossilFloors = list(0.05)))
  expect_identical(o$fossilFloors, "0.05")
  expect_true("fossilFloors" %in% pfm:::.pfmSweepOptionKeys)
  expect_match(pfm:::.pfmSweepOptionsLabel(o), "fossil terms bounded below")
})
