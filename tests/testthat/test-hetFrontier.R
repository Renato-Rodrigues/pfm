# Design note 0009 section 8.2: the frontier with a heteroscedastic slack, its plumbing (spec, sweep options, panel
# key) and the scenario path of the fossil rents.

hetMagpie <- function(w = 0.6, seed = 7) {
  set.seed(seed)
  regions <- paste0("R", 1:30); years <- 2000:2015
  vars <- c("Policy Stringency|Diffuse", "Incumbent Power|Diffuse", "Rule of Law (VDem)", "Fossil Rents")
  m <- magclass::new.magpie(regions, years, vars, fill = NA)
  nR <- length(regions); nY <- length(years)
  fos <- matrix(stats::runif(nR * nY, 0.1, 0.9), nR, nY); rol <- matrix(stats::runif(nR * nY), nR, nY)
  rents <- matrix(rep(stats::rexp(nR, 1), nY), nR, nY)          # a country property, constant over time
  y <- matrix(NA_real_, nR, nY)
  for (t in 2:nY) {
    z <- (rents[, t - 1] - 1) / 1
    su <- exp(-1 + w * z); u <- abs(stats::rnorm(nR, 0, su)); v <- stats::rnorm(nR, 0, 0.1)
    y[, t] <- 10 * stats::plogis(0.5 - 1.2 * fos[, t - 1] + 1.0 * rol[, t - 1] + v - u)
  }
  m[, , "Incumbent Power|Diffuse"] <- as.vector(fos); m[, , "Rule of Law (VDem)"] <- as.vector(rol)
  m[, , "Fossil Rents"] <- as.vector(rents); m[, , "Policy Stringency|Diffuse"] <- as.vector(y)
  m
}
hetFit <- function(m, slack) {
  suppressWarnings(suppressMessages(estimatePolicyStringencyModel(
    data = m, sector = "Diffuse", estimator = "frontier", actorPowerDrivers = "Incumbent Power",
    actorPowerIndex = "Incumbent Power", instQualityDrivers = "Rule of Law (VDem)", controlDrivers = NULL,
    regionMappingFixedEffects = NULL, logisticTimeTrend = FALSE, timeTrend = FALSE, modelDir = NULL,
    updateIndex = FALSE, verbose = FALSE, outcomeVar = "Policy Stringency", slackDrivers = slack)))
}

test_that("the slack drivers of a sector are found in every form a spec can carry them", {
  f <- pfm:::.pfmSlackDriversFor
  expect_identical(f(NULL, "Diffuse"), character(0))
  expect_identical(f(list(Diffuse = "Fossil Rents"), "Diffuse"), "Fossil Rents")
  expect_identical(f(list(Diffuse = "Fossil Rents"), "Bulk"), character(0))
  expect_identical(f(unlist(list(Diffuse = "Fossil Rents")), "Bulk"), character(0))   # flattened by a reader
  expect_identical(f(unlist(list(Diffuse = c("A", "B"))), "Diffuse"), c("A", "B"))
  expect_identical(f("Fossil Rents", "Bulk"), "Fossil Rents")                          # unnamed: every sector
})

test_that("the heteroscedastic frontier recovers a slack that widens with rents, and reads like an sfa fit", {
  m <- hetMagpie(w = 0.6)
  hom <- hetFit(m, NULL); het <- hetFit(m, list(Diffuse = "Fossil Rents"))
  expect_s3_class(het$model, "pfmHetFrontier")
  b <- stats::coef(het$model)
  expect_true(all(c("sigmaSq", "gamma", "slack_(Intercept)", "slack_Fossil.Rents") %in% names(b)))
  expect_gt(b[["slack_Fossil.Rents"]], 0.3)
  expect_gt(het$coeftest["slack_Fossil.Rents", 3], 3)                                     # z
  expect_gt(as.numeric(stats::logLik(het$model)), as.numeric(stats::logLik(hom$model)))
  expect_true(het$converged); expect_identical(het$vcovCheck$status, "ok")
  # the ceiling is read by name: the slack terms are not part of it
  expect_false(any(startsWith(names(pfm:::.pfmCeilingBeta(b)), "slack_")))
  expect_false(any(names(pfm:::.pfmCeilingBeta(b)) %in% c("sigmaSq", "gamma")))
  # the frontier scores exist, and the share used stays a share
  expect_true(is.data.frame(het$frontier)); expect_true(all(het$frontier$efficiencyRatio >= 0, na.rm = TRUE))
  # the rents are scaled and frozen with the drivers, and do not enter the ceiling's formula
  expect_true("Fossil.Rents" %in% names(het$driverScaling))
  expect_false("Fossil.Rents" %in% all.vars(het$formula))
  # without the variable in a sector's list the fit is the homoscedastic frontier
  bulkLike <- hetFit(m, list(Bulk = "Fossil Rents"))
  expect_false(inherits(bulkLike$model, "pfmHetFrontier"))
  expect_equal(as.numeric(stats::logLik(bulkLike$model)), as.numeric(stats::logLik(hom$model)))
})

test_that("with no rents effect in the data the slack weight is near zero", {
  het <- hetFit(hetMagpie(w = 0, seed = 3), list(Diffuse = "Fossil Rents"))
  expect_lt(abs(het$coeftest["slack_Fossil.Rents", 3]), 3)
})

test_that("the sweep carries the slack drivers on every spec and in its options", {
  sp <- list(list(name = "S1", model_type = "PolicyStringency", actorPowerIndex = "Incumbent Power",
                  instQualityDrivers = "Rule of Law (VDem)", controlDrivers = character(0), panelTransform = "levels"))
  out <- suppressMessages(pfmSpecs(sp, verbose = FALSE, apTransforms = "linear", slackDrivers = list(Diffuse = "Fossil Rents")))
  expect_identical(out[[1]]$slackDrivers, list(Diffuse = "Fossil Rents"))
  expect_error(pfmSpecs(sp, verbose = FALSE, slackDrivers = list(Power = "x")), "named by sector")
  o <- pfm:::.pfmSweepOptionsNormalise(list(slackDrivers = list(Diffuse = list("Fossil Rents"))))
  expect_identical(o$slackDrivers, list(Diffuse = "Fossil Rents"))
  expect_true("slackDrivers" %in% pfm:::.pfmSweepOptionKeys)
  expect_match(pfm:::.pfmSweepOptionsLabel(o), "frontier slack size on Diffuse: Fossil Rents")
  expect_null(pfm:::.pfmSweepOptionsNormalise(list(slackDrivers = list()))$slackDrivers)
})

test_that("the panel key fossilRents is off by default and read from the definition", {
  expect_false(pfm:::.pfmPanelDefNormalise(NULL)$fossilRents)
  expect_true(pfm:::.pfmPanelDefNormalise(list(fossilRents = TRUE))$fossilRents)
  expect_match(pfm:::.pfmPanelDefLabel(pfm:::.pfmPanelDefNormalise(list(fossilRents = TRUE))), "fossil rents")
})

test_that("the scenario rents follow history, then the REMIND region's rents relative to 2025", {
  hp <- magclass::new.magpie(c("AAA", "BBB", "CCC"), 2019:2023, "Fossil Rents", fill = NA)
  hp["AAA", , ] <- log1p(20); hp["BBB", , ] <- log1p(2); hp["CCC", , ] <- NA
  rr <- rbind(MEA = c(`2025` = 10, `2050` = 5, `2100` = 0.5), EUR = c(`2025` = 0.001, `2050` = 0.5, `2100` = 0))
  local_mocked_bindings(.pfmREMINDFossilRents = function(gdxFile, price = "regional") rr,
                        pfmGetMapping = function(...) data.frame(CountryCode = c("AAA", "BBB", "CCC"), RegionCode = c("MEA", "EUR", "MEA")))
  s <- pfm:::.pfmScenarioFossilRents(hp, "x.gdx", "map.csv", c(2020, 2025, 2050, 2100), c("AAA", "BBB", "CCC"))
  v <- function(c, y) as.numeric(s[c, y, 1])
  expect_equal(v("AAA", 2020), log1p(20))
  expect_equal(v("AAA", 2025), log1p(20))                    # ratio 1 at t0
  expect_equal(v("AAA", 2050), log1p(20 * 0.5))
  expect_equal(v("AAA", 2100), log1p(20 * 0.05))
  expect_equal(v("BBB", 2050), log1p(2 * 3))                 # a tiny base: the ratio is bounded at 3
  expect_true(is.na(v("CCC", 2050)))                         # no history, no value
})

test_that("the valuation of REMIND's rents reaches the reader, regional by default", {
  hp <- magclass::new.magpie("AAA", 2023, "Fossil Rents", fill = log1p(10))
  seen <- character(0)
  local_mocked_bindings(.pfmREMINDFossilRents = function(gdxFile, price = "regional") {
    seen <<- c(seen, price); rbind(MEA = c(`2025` = 10, `2050` = if (price == "world") 2 else 5)) },
    pfmGetMapping = function(...) data.frame(CountryCode = "AAA", RegionCode = "MEA"))
  a <- pfm:::.pfmScenarioFossilRents(hp, "x.gdx", "map.csv", 2050, "AAA")
  b <- pfm:::.pfmScenarioFossilRents(hp, "x.gdx", "map.csv", 2050, "AAA", price = "world")
  expect_identical(seen, c("regional", "world"))
  expect_equal(as.numeric(a["AAA", 2050, 1]), log1p(10 * 0.5))
  expect_equal(as.numeric(b["AAA", 2050, 1]), log1p(10 * 0.2))
})
