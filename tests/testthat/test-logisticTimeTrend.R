# nolint start
# The trend column is STANDARDIZED from 2026-08-22, so the shape assertions below
# run on the de-scaled curve (std * sd + mean, using the stored driverScaling).
descale <- function(df) {
  sc <- attr(df, "driverScaling")[["logisticTimeTrend"]]
  testthat::expect_false(is.null(sc))
  v <- df$logisticTimeTrend * sc[["sd"]] + sc[["mean"]]
  stats::setNames(v, df$year)
}

test_that("logisticTimeTrend is a bounded, saturating, single common curve", {
  years <- c(1990, 2000, 2010, 2022, 2030, 2060, 2100)
  m <- magclass::new.magpie(c("AAA", "BBB"), years,
                            c("Effective Carbon Price|Bulk", "Rule of Law (VDem)"),
                            fill = 1)
  df <- preparePanelData(
    m, sector = "Bulk",
    actorPowerDrivers = NULL, actorPowerIndex = NULL,
    instQualityDrivers = "Rule of Law (VDem)", controlDrivers = NULL,
    regionMappingFixedEffects = NULL
  )
  keep <- df$region == "AAA"
  tr <- descale(df)[keep]
  # Bounded in [0, 1] for ALL years (no runaway extrapolation)
  expect_true(all(tr >= 0 & tr <= 1))
  # Flat toe before the historical window
  expect_lt(tr["1990"], 0.06)
  # Meaningful rise through history (not stuck in the toe)
  expect_gt(tr["2022"] - tr["2000"], 0.2)
  # Default midpoint 2010 => ~0.5
  expect_equal(unname(tr["2010"]), 0.5, tolerance = 0.02)
  # Saturating within the projection horizon
  expect_gt(tr["2060"], 0.88)
  expect_gt(tr["2100"], 0.98)
  # strictly increasing
  expect_true(all(diff(tr) > 0))
  # One common curve: identical for every region
  expect_equal(unname(descale(df)[df$region == "BBB"]), unname(tr))
})

test_that("the curve is nearly exhausted by the last data year", {
  # The point of the 2026-08-22 re-parameterization: with the inflection INSIDE
  # the estimation window there is little left for the projection to extrapolate,
  # so freezing vs not freezing the trend stops being load-bearing. Under the old
  # (2030, 0.08) defaults this margin was 0.65; a regression to anything like that
  # re-opens the 4.1-vs-10.0 ceiling swing documented in analysis/.
  years <- c(2000, 2022, 2100)
  m <- magclass::new.magpie("AAA", years, "Rule of Law (VDem)", fill = 1)
  df <- preparePanelData(m, sector = "Bulk", actorPowerDrivers = NULL,
    actorPowerIndex = NULL, instQualityDrivers = "Rule of Law (VDem)",
    controlDrivers = NULL, regionMappingFixedEffects = NULL)
  tr <- descale(df)
  expect_gt(tr["2022"], 0.85)
  expect_lt(tr["2100"] - tr["2022"], 0.15)
})

test_that("logisticTimeTrend midpoint/steepness are tunable and consistent", {
  years <- c(2000, 2030, 2060)
  m <- magclass::new.magpie("AAA", years, "Rule of Law (VDem)", fill = 1)
  base <- function(mid, steep) {
    df <- preparePanelData(m, sector = "Bulk", actorPowerDrivers = NULL,
      actorPowerIndex = NULL, instQualityDrivers = "Rule of Law (VDem)",
      controlDrivers = NULL, regionMappingFixedEffects = NULL,
      trendMidpoint = mid, trendSteepness = steep)
    descale(df)
  }
  expect_equal(unname(base(2030, 0.08)["2030"]), 0.5, tolerance = 1e-9)
  # later midpoint => lower value at a fixed year
  expect_lt(base(2045, 0.08)["2030"], base(2030, 0.08)["2030"])
})

test_that("the trend is standardized alongside the drivers", {
  years <- 2000:2022
  m <- magclass::new.magpie(c("AAA", "BBB"), years,
                            c("Effective Carbon Price|Bulk", "Rule of Law (VDem)"),
                            fill = 1)
  m[, , "Rule of Law (VDem)"] <- stats::runif(length(years) * 2)
  df <- preparePanelData(m, sector = "Bulk", actorPowerDrivers = NULL,
    actorPowerIndex = NULL, instQualityDrivers = "Rule of Law (VDem)",
    controlDrivers = NULL, regionMappingFixedEffects = NULL)
  # It now carries a scaling entry, and the column is centred/scaled like a driver
  sc <- attr(df, "driverScaling")[["logisticTimeTrend"]]
  expect_true(all(c("mean", "sd") %in% names(sc)))
  expect_equal(mean(df$logisticTimeTrend, na.rm = TRUE), 0, tolerance = 1e-8)
  expect_equal(stats::sd(df$logisticTimeTrend, na.rm = TRUE), 1, tolerance = 1e-8)
  # ... and is no longer confined to [0, 1], which is the whole point
  expect_lt(min(df$logisticTimeTrend), 0)
})

test_that("apply mode refuses a driverScaling that predates a scaled column", {
  years <- 2000:2010
  m <- magclass::new.magpie("AAA", years,
                            c("Effective Carbon Price|Bulk", "Rule of Law (VDem)"),
                            fill = 1)
  df <- preparePanelData(m, sector = "Bulk", actorPowerDrivers = NULL,
    actorPowerIndex = NULL, instQualityDrivers = "Rule of Law (VDem)",
    controlDrivers = NULL, regionMappingFixedEffects = NULL)
  legacy <- attr(df, "driverScaling")
  legacy[["logisticTimeTrend"]] <- NULL   # an artifact written before the change
  expect_error(
    preparePanelData(m, sector = "Bulk", actorPowerDrivers = NULL,
      actorPowerIndex = NULL, instQualityDrivers = "Rule of Law (VDem)",
      controlDrivers = NULL, regionMappingFixedEffects = NULL,
      driverScaling = legacy),
    "logisticTimeTrend"
  )
})
# nolint end
