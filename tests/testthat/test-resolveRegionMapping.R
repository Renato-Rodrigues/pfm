test_that("resolveRegionMapping passes concrete mapping names through unchanged", {
  expect_identical(resolveRegionMapping("regionmappingH12.csv"), "regionmappingH12.csv")
  expect_identical(resolveRegionMapping("regionmapping_54.csv"), "regionmapping_54.csv")
})

test_that("resolveRegionMapping resolves the country sentinel to a generated identity mapping", {
  mrLocalEnv()

  fileName <- resolveRegionMapping("country")
  expect_identical(fileName, "regionmapping_country.csv")

  target <- file.path(madrat::getConfig("mappingfolder"), "regional", fileName)
  expect_true(file.exists(target))
  m <- utils::read.csv2(target)
  expect_identical(m$RegionCode, m$CountryCode)
})

test_that(".scopeRegionmapping scopes and restores the global madrat config", {
  mrLocalEnv()

  fileName <- resolveRegionMapping("country") # ensure the mapping exists
  old <- madrat::getConfig("regionmapping")

  restore <- pfm:::.scopeRegionmapping(fileName)
  expect_identical(madrat::getConfig("regionmapping"), fileName)

  restore()
  expect_identical(madrat::getConfig("regionmapping"), old)
})

test_that(".scopeRegionmapping is a no-op when the config already matches", {
  mrLocalEnv()

  fileName <- resolveRegionMapping("country")
  suppressMessages(madrat::setConfig(regionmapping = fileName))

  restore <- pfm:::.scopeRegionmapping(fileName)
  restore()
  expect_identical(madrat::getConfig("regionmapping"), fileName)
})
