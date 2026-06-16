test_that("logisticTimeTrend is a bounded, saturating, single common curve", {
  years <- c(1990, 2000, 2022, 2030, 2060, 2100)
  m <- magclass::new.magpie(c("AAA", "BBB"), years,
                            c("Effective Carbon Price|Bulk", "Rule of Law (VDem)"),
                            fill = 1)
  df <- preparePanelData(
    m, sector = "Bulk",
    actorPowerDrivers = NULL, actorPowerIndex = NULL,
    instQualityDrivers = "Rule of Law (VDem)", controlDrivers = NULL,
    regionMappingFixedEffects = NULL
  )
  tr <- df$logisticTimeTrend[df$region == "AAA"]
  names(tr) <- df$year[df$region == "AAA"]
  # Bounded in [0, 1] for ALL years (no runaway extrapolation)
  expect_true(all(tr >= 0 & tr <= 1))
  # Flat toe before the historical window
  expect_lt(tr["1990"], 0.06)
  # Meaningful rise through history (not stuck in the toe)
  expect_gt(tr["2022"] - tr["2000"], 0.2)
  # Midpoint 2030 => ~0.5
  expect_equal(unname(tr["2030"]), 0.5, tolerance = 0.02)
  # Saturating within the projection horizon
  expect_gt(tr["2060"], 0.88)
  expect_gt(tr["2100"], 0.98)
  # strictly increasing
  expect_true(all(diff(tr) > 0))
})

test_that("logisticTimeTrend midpoint/steepness are tunable and consistent", {
  years <- c(2000, 2030, 2060)
  m <- magclass::new.magpie("AAA", years, "Rule of Law (VDem)", fill = 1)
  base <- function(mid, steep) {
    df <- preparePanelData(m, sector = "Bulk", actorPowerDrivers = NULL,
      actorPowerIndex = NULL, instQualityDrivers = "Rule of Law (VDem)",
      controlDrivers = NULL, regionMappingFixedEffects = NULL,
      trendMidpoint = mid, trendSteepness = steep)
    stats::setNames(df$logisticTimeTrend, df$year)
  }
  expect_equal(unname(base(2030, 0.08)["2030"]), 0.5, tolerance = 1e-9)
  # later midpoint => lower value at a fixed year
  expect_lt(base(2045, 0.08)["2030"], base(2030, 0.08)["2030"])
})
