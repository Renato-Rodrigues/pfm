makeScalingMagpie <- function() {
  set.seed(3)
  regions <- paste0("R", 1:10)
  years <- 2000:2015
  vars <- c("Effective Carbon Price|Bulk", "Actor Power Index|Bulk",
            "Rule of Law (VDem)")
  m <- magclass::new.magpie(regions, years, vars, fill = NA)
  for (v in vars) m[, , v] <- runif(length(regions) * length(years))
  # Actor Power Index can be negative in reality; shift to a clearly-negative range
  m[, , "Actor Power Index|Bulk"] <- m[, , "Actor Power Index|Bulk"] * 0.5 - 0.8
  m[, , "Effective Carbon Price|Bulk"] <- runif(length(regions) * length(years)) * 50
  m
}

prep <- function(m, scaling = NULL) {
  preparePanelData(
    m, sector = "Bulk",
    actorPowerDrivers = "Actor Power Index", actorPowerIndex = "Actor Power Index",
    instQualityDrivers = "Rule of Law (VDem)", controlDrivers = NULL,
    regionMappingFixedEffects = NULL, driverScaling = scaling
  )
}

test_that("driver columns are standardized and the scaling is returned (negatives OK)", {
  df <- prep(makeScalingMagpie())
  sc <- attr(df, "driverScaling")
  expect_false(is.null(sc))
  expect_true(all(c("Actor.Power.Index", "Rule.of.Law..VDem.") %in% names(sc)))
  # over the complete-case rows, standardized columns have mean ~0, sd ~1
  cc <- stats::complete.cases(df$Actor.Power.Index, df$Rule.of.Law..VDem.)
  expect_equal(mean(df$Actor.Power.Index[cc]), 0, tolerance = 1e-8)
  expect_equal(sd(df$Actor.Power.Index[cc]), 1, tolerance = 1e-6)
  # the stored mean for AP is negative, proving negative-valued drivers are fine
  expect_lt(sc[["Actor.Power.Index"]][["mean"]], 0)
  # interaction is the product of the standardized factors
  expect_equal(df$Actor.Power.Index_x_Rule.of.Law..VDem.,
               df$Actor.Power.Index * df$Rule.of.Law..VDem.)
})

test_that("apply mode reuses supplied scaling (fit/predict consistency)", {
  m <- makeScalingMagpie()
  fitDf <- prep(m)
  sc <- attr(fitDf, "driverScaling")
  # apply mode on the SAME data with the stored scaling reproduces fit mode exactly
  applyDf <- prep(m, scaling = sc)
  expect_equal(applyDf$Actor.Power.Index, fitDf$Actor.Power.Index)
  expect_identical(attr(applyDf, "driverScaling")[["Actor.Power.Index"]],
                   sc[["Actor.Power.Index"]])
  # a uniform +0.1 raw shift, standardized with the FROZEN sd, shifts the
  # standardized column by exactly 0.1/sd (not re-centred on the shifted data)
  m2 <- m + 0.1
  shiftDf <- prep(m2, scaling = sc)
  cc <- stats::complete.cases(shiftDf$Actor.Power.Index, fitDf$Actor.Power.Index)
  delta <- (shiftDf$Actor.Power.Index[cc] - fitDf$Actor.Power.Index[cc])
  expect_equal(delta, rep(0.1 / sc[["Actor.Power.Index"]][["sd"]], sum(cc)),
               tolerance = 1e-8)
})

test_that("standardization is fit-neutral (identical fitted values vs raw)", {
  m <- makeScalingMagpie()
  df <- prep(m)
  df <- df[is.finite(df$ecp) & is.finite(df$Actor.Power.Index), ]
  # raw versions of the same rows
  sc <- attr(prep(m), "driverScaling")
  rawAP  <- df$Actor.Power.Index * sc[["Actor.Power.Index"]][["sd"]] + sc[["Actor.Power.Index"]][["mean"]]
  rawIQ  <- df$Rule.of.Law..VDem. * sc[["Rule.of.Law..VDem."]][["sd"]] + sc[["Rule.of.Law..VDem."]][["mean"]]
  fitStd <- lm(ecp ~ Actor.Power.Index + Rule.of.Law..VDem. +
                 Actor.Power.Index_x_Rule.of.Law..VDem., data = df)
  fitRaw <- lm(df$ecp ~ rawAP + rawIQ + I(rawAP * rawIQ))
  expect_equal(unname(fitted(fitStd)), unname(fitted(fitRaw)), tolerance = 1e-8)
})
