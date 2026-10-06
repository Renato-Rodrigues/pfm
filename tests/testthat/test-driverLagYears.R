# PITFALLS 31: the driver lag counts YEARS, not rows of the year axis. On a scenario panel in
# 5-year steps, lag = 1 must read the driver one year back (interpolated), not five.

# the prepared drivers are standardised; read them back on the original scale
.rol <- function(d) { s <- attr(d, "driverScaling")[["Rule.of.Law..VDem."]]
  d$Rule.of.Law..VDem. * s[["sd"]] + s[["mean"]] }
.lagPrep <- function(m) preparePanelData(
  data = m, sector = "Bulk", actorPowerDrivers = "Actor Power Index", actorPowerIndex = "Actor Power Index",
  instQualityDrivers = "Rule of Law (VDem)", controlDrivers = NULL, regionMappingFixedEffects = NULL,
  outcomeVar = "Policy Stringency")

test_that(".pfmLagLookup finds the exact year, interpolates a missing one, and is NULL outside", {
  yrs <- c(2010, 2015, 2020, 2030)
  expect_equal(pfm:::.pfmLagLookup(yrs, 2015), c(i0 = 2, i1 = 2, w = 0))
  expect_equal(pfm:::.pfmLagLookup(yrs, 2019), c(i0 = 2, i1 = 3, w = 0.8))
  expect_equal(pfm:::.pfmLagLookup(yrs, 2029), c(i0 = 3, i1 = 4, w = 0.9))
  expect_null(pfm:::.pfmLagLookup(yrs, 2009))
  expect_null(pfm:::.pfmLagLookup(yrs, 2031))
  expect_equal(pfm:::.pfmLagLookup(c(2020, 2010, 2015), 2019), c(i0 = 3, i1 = 1, w = 0.8))
})

test_that("on an annual panel the driver at t is the value at t-1 (the fit is unchanged)", {
  m <- makePFMagpie()
  d <- .lagPrep(m)
  expect_equal(.rol(d)[d$region == "R3" & d$year == 2010], as.numeric(m["R3", 2009, "Rule of Law (VDem)"]))
  expect_true(all(is.na(d$Rule.of.Law..VDem.[d$year == 2000])))
})

test_that("on a 5-year panel lag = 1 reads the interpolated year t-1, not t-5", {
  m <- makePFMagpie()[, c(2000, 2005, 2010, 2015), ]
  d <- .lagPrep(m)
  x <- .rol(d)[d$region == "R3" & d$year == 2010]
  v05 <- as.numeric(m["R3", 2005, "Rule of Law (VDem)"]); v10 <- as.numeric(m["R3", 2010, "Rule of Law (VDem)"])
  expect_equal(x, 0.2 * v05 + 0.8 * v10)
  expect_false(isTRUE(all.equal(x, v05)))
  # a linear driver path gives the same lagged value on the 5-year and the annual panel
  lin <- makePFMagpie(); for (y in 2000:2019) lin[, y, "Rule of Law (VDem)"] <- (y - 2000) / 20
  a <- .lagPrep(lin); f <- .lagPrep(lin[, c(2000, 2005, 2010, 2015), ])
  expect_equal(.rol(f)[f$year == 2015], .rol(a)[a$year == 2015])
})
