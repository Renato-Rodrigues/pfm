# The panel definition (years, smoothing, IEA edition) and the institution-projection rules.

test_that("the legacy definition is the v5 panel and the option overrides it", {
  withr::local_options(pfm.panel = NULL)
  d <- pfmPanelDef()
  expect_identical(d, list(firstYear = 2000L, lastYear = 2022L, movingAverage = 5L, ieaVersion = "default"))
  expect_identical(pfm:::.pfmPanelYears(d), 2000:2022)
  expect_identical(pfm:::.pfmPanelMA(d), 5L)

  withr::local_options(pfm.panel = list(years = c(2000, 2023), movingAverage = 1, ieaVersion = "latest"))
  d <- pfmPanelDef()
  expect_identical(pfm:::.pfmPanelYears(), 2000:2023)
  expect_null(pfm:::.pfmPanelMA())
  expect_identical(d$ieaVersion, "latest")
  expect_match(pfm:::.pfmPanelDefLabel(d), "annual values")
})

test_that("an invalid definition is refused", {
  expect_error(pfm:::.pfmPanelDefNormalise(list(ieaVersion = "2026")), "ieaVersion")
  expect_error(pfm:::.pfmPanelDefNormalise(list(movingAverage = 0)), "movingAverage")
  expect_error(pfm:::.pfmPanelDefNormalise(list(years = c(2023, 2000), firstYear = 2023, lastYear = 2000)), NA)
  expect_error(pfm:::.pfmPanelDefNormalise(list(firstYear = 2023, lastYear = 2000)), "lastYear")
})

test_that("a group's definition: manifest > legacy for a swept group > config > legacy", {
  gd <- withr::local_tempdir()
  cfgPanel <- list(firstYear = 2000L, lastYear = 2023L, movingAverage = 1L, ieaVersion = "latest")
  # new group: the config
  d <- pfm:::.pfmPanelDefForGroup(gd, cfgPanel)
  expect_identical(attr(d, "source"), "config")
  expect_identical(d$lastYear, 2023L)
  # no config: legacy
  expect_identical(pfm:::.pfmPanelDefForGroup(gd, NULL)$lastYear, 2022L)
  # swept before the record existed (v5): legacy, whatever the config says
  jsonlite::write_json(list(group = "v5", panel_hash = "abc"), file.path(gd, "manifest.json"), auto_unbox = TRUE)
  d <- pfm:::.pfmPanelDefForGroup(gd, cfgPanel)
  expect_match(attr(d, "source"), "^legacy")
  expect_identical(d$movingAverage, 5L)
  # recorded by the sweep: the record, whatever the config says
  pfm:::.writeRunGroupManifest(gd, group = "v6", mode = NULL, panelDef = cfgPanel)
  d <- pfm:::.pfmPanelDefForGroup(gd, list(movingAverage = 3L))
  expect_identical(attr(d, "source"), "manifest")
  expect_identical(d$movingAverage, 1L)
  expect_identical(d$ieaVersion, "latest")
})

test_that("config.yml's panel block resolves with the group's override", {
  dir <- withr::local_tempdir()
  cfg <- file.path(dir, "config.yml")
  yaml::write_yaml(list(panel = list(years = c(2000L, 2023L), movingAverage = 5L, ieaVersion = "latest",
                                     groups = list(`v6-annual` = list(movingAverage = 1L)))), cfg)
  rc <- pfmResolveConfig(cfg, group = "v6", verbose = FALSE)
  expect_identical(rc$panel$movingAverage, 5L)
  expect_identical(rc$panel$lastYear, 2023L)
  rc <- pfmResolveConfig(cfg, group = "v6-annual", verbose = FALSE)
  expect_identical(rc$panel$movingAverage, 1L)
  expect_identical(rc$panel$ieaVersion, "latest")
  yaml::write_yaml(list(group = "v5"), cfg)
  expect_null(pfmResolveConfig(cfg, verbose = FALSE)$panel)
})

test_that("the convergence rule is the projection every group up to v5 used", {
  a <- pfmInstitutionProjection("convergence", "SSP4")
  expect_identical(a[c("mode", "percentile", "midpointYear", "convergenceYear", "shape", "keepIfAboveTarget")],
                   list(mode = "global_percentile", percentile = 75, midpointYear = 2080,
                        convergenceYear = 2150, shape = "logistic", keepIfAboveTarget = TRUE))
  skip_if_not_installed("mrpfm")
  x <- magclass::new.magpie(c("AAA", "BBB", "CCC", "DDD"), 2000:2022, "v", fill = 0)
  x[, , ] <- outer(c(0.2, 0.4, 0.6, 0.9), seq(0, 0.05, length.out = 23), "+")
  y <- c(2000:2022, seq(2025, 2150, 5))
  old <- mrpfm::toolProjectScenario(x, y, shape = "logistic", midpointYear = 2080, convergenceYear = 2150)
  expect_identical(pfm:::.pfmProjectInstitutions(x, y, "convergence", "SSP2"), old)
})

test_that("storyline is the default and, for SSP2, the convergence rule", {
  expect_identical(pfmInstitutionProjection()[c("percentile", "midpointYear", "convergenceYear")],
                   pfmInstitutionProjection("convergence")[c("percentile", "midpointYear", "convergenceYear")])
  expect_identical(formals(panelDataScenario)$institutions, "storyline")
})

test_that("storyline and hold rules", {
  s <- pfmInstitutionStorylines()
  expect_identical(s$ssp, paste0("SSP", 1:5))
  ssp2 <- pfmInstitutionProjection("storyline", "SSP2")
  expect_identical(ssp2[c("percentile", "midpointYear", "convergenceYear")],
                   pfmInstitutionProjection("convergence")[c("percentile", "midpointYear", "convergenceYear")])
  expect_identical(pfmInstitutionProjection("storyline", "SSP1")$percentile, 90)
  expect_identical(pfmInstitutionProjection("hold")$mode, "constant")
  expect_error(pfmInstitutionProjection("storyline", "SSP6"), "unknown ssp")
  skip_if_not_installed("mrpfm")
  x <- magclass::new.magpie(c("AAA", "BBB"), 2000:2022, "v", fill = 0.3)
  y <- c(2000:2022, 2050, 2100)
  h <- pfm:::.pfmProjectInstitutions(x, y, "hold", "SSP2")
  expect_equal(as.numeric(h[, 2100, ]), c(0.3, 0.3))
})

test_that("the scenario registry carries ssp and institutions, defaulting to SSP2/storyline", {
  entries <- list(
    list(id = "a", gdx = "a.gdx", gating = TRUE),
    list(id = "b", gdx = "b.gdx", ssp = "SSP3", institutions = "hold")
  )
  reg <- parseScenarioRegistry(list(scenarios = entries), requireExists = FALSE)
  expect_identical(reg$scenarios$a$ssp, "SSP2")
  expect_identical(reg$scenarios$a$institutions, "storyline")
  expect_identical(reg$scenarios$b$ssp, "SSP3")
  expect_identical(reg$scenarios$b$institutions, "hold")
})
