makePooledTestMagpie <- function() {
  regions <- c("AAA", "BBB")
  years <- 2000:2005
  vars <- c(
    "Effective Carbon Price|Bulk", "Effective Carbon Price|Diffuse",
    "Actor Power Index|Bulk", "Actor Power Index|Diffuse",
    "Rule of Law (VDem)", "GDP per Capita"
  )
  m <- magclass::new.magpie(regions, years, vars, fill = 0)
  set.seed(21)
  for (v in vars) m[, , v] <- runif(length(regions) * length(years))
  m[, , "Effective Carbon Price|Bulk"] <- 5
  m[, , "Effective Carbon Price|Diffuse"] <- 8
  m
}

test_that("pooled panel stacks sectors with a dummy and sector-specific outcomes", {
  m <- makePooledTestMagpie()
  pooled <- preparePooledPanelData(
    m,
    sectors = c("Bulk", "Diffuse"),
    actorPowerDrivers = "Actor Power Index",
    actorPowerIndex = "Actor Power Index",
    instQualityDrivers = "Rule of Law (VDem)",
    controlDrivers = "GDP per Capita",
    regionMappingFixedEffects = NULL
  )
  expect_true(all(c("sector", "sectorDiffuse") %in% colnames(pooled)))
  expect_equal(sort(unique(pooled$sector)), c("Bulk", "Diffuse"))
  expect_equal(sum(pooled$sectorDiffuse), sum(pooled$sector == "Diffuse"))
  # Twice the rows of a single-sector panel
  single <- preparePanelData(
    m, sector = "Bulk",
    actorPowerDrivers = "Actor Power Index", actorPowerIndex = "Actor Power Index",
    instQualityDrivers = "Rule of Law (VDem)", controlDrivers = "GDP per Capita",
    regionMappingFixedEffects = NULL
  )
  expect_equal(nrow(pooled), 2 * nrow(single))
  # Sector-specific dependent variable preserved through stacking
  expect_true(all(pooled$ecp[pooled$sector == "Bulk"] == 5))
  expect_true(all(pooled$ecp[pooled$sector == "Diffuse"] == 8))
  # Same region appears in both sectors (basis for region-clustered SEs)
  expect_true(all(table(pooled$region, pooled$sector) > 0))
})

test_that("input validation", {
  m <- makePooledTestMagpie()
  expect_error(preparePooledPanelData(m, sectors = "Bulk"), "at least two")
  expect_error(
    preparePooledPanelData(m, sectors = c("Bulk", "Diffuse"), referenceSector = "X"),
    "referenceSector"
  )
})
