makeOutcomeMagpie <- function() {
  set.seed(7)
  regions <- paste0("R", 1:8)
  years <- 2000:2012
  vars <- c("Effective Carbon Price|Bulk", "Policy Stringency|Bulk",
            "Actor Power Index|Bulk", "Rule of Law (VDem)")
  m <- magclass::new.magpie(regions, years, vars, fill = NA)
  n <- length(regions) * length(years)
  for (v in vars) m[, , v] <- runif(n)
  m[, , "Effective Carbon Price|Bulk"] <- runif(n) * 50
  m[, , "Policy Stringency|Bulk"] <- runif(n) * 10
  m
}

prepOutcome <- function(m, ...) {
  preparePanelData(
    m, sector = "Bulk",
    actorPowerDrivers = "Actor Power Index", actorPowerIndex = "Actor Power Index",
    instQualityDrivers = "Rule of Law (VDem)", controlDrivers = NULL,
    regionMappingFixedEffects = NULL, ...
  )
}

test_that("default outcome remains the Effective Carbon Price", {
  m <- makeOutcomeMagpie()
  df <- prepOutcome(m)
  expect_equal(df$ecp[df$region == "R1" & df$year == 2001],
               as.numeric(m["R1", 2001, "Effective Carbon Price|Bulk"]))
  expect_equal(df$lagged_ecp[df$region == "R1" & df$year == 2001],
               as.numeric(m["R1", 2000, "Effective Carbon Price|Bulk"]))
})

test_that("outcomeVar switches the dependent variable to Policy Stringency (column still named ecp)", {
  m <- makeOutcomeMagpie()
  df <- prepOutcome(m, outcomeVar = "Policy Stringency")
  expect_equal(df$ecp[df$region == "R1" & df$year == 2001],
               as.numeric(m["R1", 2001, "Policy Stringency|Bulk"]))
  expect_equal(df$lagged_ecp[df$region == "R1" & df$year == 2001],
               as.numeric(m["R1", 2000, "Policy Stringency|Bulk"]))
  # predictors are untouched by the outcome switch
  dfEcp <- prepOutcome(m)
  expect_equal(df$Actor.Power.Index, dfEcp$Actor.Power.Index)
})

test_that("NA rows are dropped for the selected outcome only", {
  m <- makeOutcomeMagpie()
  m["R2", 2005, "Policy Stringency|Bulk"] <- NA
  df <- prepOutcome(m, outcomeVar = "Policy Stringency")
  expect_false(any(df$region == "R2" & df$year == 2005))
  dfEcp <- prepOutcome(m)
  expect_true(any(dfEcp$region == "R2" & dfEcp$year == 2005))
})
