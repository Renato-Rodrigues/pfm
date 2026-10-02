# Design note 0005 D7 (options 1C, 2C, 3A): the actor-power forms selection chooses among, the
# composite specs dropped, the actor-power extrapolation gate, and the per-group sweep options.

apSpec <- function(name, api) {
  list(name = name, model_type = "PolicyStringency", actorPowerIndex = api,
       instQualityDrivers = "Government Effectiveness (WGI)", controlDrivers = character(0),
       panelTransform = "levels")
}
apSpecs <- list(
  apSpec("S1", c("Innovator Power", "Incumbent Power", "Incumbent Power pc")),
  apSpec("C1", "Actor Power Index")
)

test_that("the default grid is the v5 grid: linear originals plus one satAP twin per split spec", {
  out <- suppressMessages(pfmSpecs(apSpecs, verbose = FALSE))
  nm <- vapply(out, `[[`, "", "name")
  expect_identical(nm, c("S1", "C1", "S1 satAP"))
  expect_identical(vapply(out, `[[`, "", "apTransform"), c("linear", "linear", "saturating"))
})

test_that("v6: four forms per split spec, composite specs dropped", {
  out <- suppressMessages(pfmSpecs(apSpecs, verbose = FALSE,
                                   apTransforms = c("linear", "saturating", "saturating-innovator",
                                                    "saturating-incumbent"),
                                   dropCompositeAP = TRUE))
  nm <- vapply(out, `[[`, "", "name")
  expect_identical(nm, c("S1", "S1 satAP", "S1 satInn", "S1 satInc"))
  expect_identical(vapply(out, `[[`, "", "apTransform"),
                   c("linear", "saturating", "saturating-innovator", "saturating-incumbent"))
  # without "linear" the split originals leave the grid
  out <- suppressMessages(pfmSpecs(apSpecs, verbose = FALSE, apTransforms = "saturating-innovator"))
  expect_identical(vapply(out, `[[`, "", "name"), c("C1", "S1 satInn"))
  expect_error(pfmSpecs(apSpecs, verbose = FALSE, apTransforms = "cubic"))
})

test_that("each form saturates exactly its group of columns", {
  expect_false(pfm:::.apSaturates("linear", "Innovator.Power"))
  expect_true(pfm:::.apSaturates("saturating", "Incumbent.Power.pc"))
  expect_true(pfm:::.apSaturates("saturating-innovator", "Innovator.Power"))
  expect_false(pfm:::.apSaturates("saturating-innovator", "Incumbent.Power"))
  expect_true(pfm:::.apSaturates("saturating-incumbent", "Incumbent.Power.pc"))
  expect_false(pfm:::.apSaturates("saturating-incumbent", "Innovator.Power"))
})

test_that("the guard measures how far a LINEAR actor-power driver extrapolates; saturating ones pass", {
  train <- data.frame(Innovator.Power = c(-1, 0, 1), Incumbent.Power = c(-1, 0, 1),
                      Government.Effectiveness = c(-1, 0, 1))
  scaling <- list(Innovator.Power = c(mean = 0, sd = 1, sat = NA_real_),
                  Incumbent.Power = c(mean = 0, sd = 1, sat = 0.5),
                  Government.Effectiveness = c(mean = 0, sd = 1))
  ranges <- pfm:::.driverSupportRanges(train, scaling)
  expect_setequal(attr(ranges, "apCols"), c("Innovator.Power", "Incumbent.Power"))
  scen <- data.frame(Innovator.Power = c(0.5, 3, -2.5), Incumbent.Power = c(0, 0, 0),
                     Government.Effectiveness = c(5, 0, 0))
  g <- pfm:::.pfmDriverGuard(scen, ranges)
  expect_equal(g$apExcess, c(0, 2, 1.5))   # GovEff out of range is not actor power
})

test_that("sweep options: config > nothing for a new group; manifest wins; a v5-like group keeps the v5 grid", {
  dir <- withr::local_tempdir()
  cfg <- file.path(dir, "config.yml")
  yaml::write_yaml(list(sweep = list(apTransforms = c("linear", "saturating-innovator"),
                                     dropCompositeAP = TRUE, apExtrapolationGate = 0.275,
                                     apExtrapolationWindow = c(2025L, 2100L),
                                     groups = list(`v6-lin` = list(apTransforms = "linear")))), cfg)
  rc <- pfmResolveConfig(cfg, group = "v6", verbose = FALSE)
  expect_identical(rc$sweep$apTransforms, c("linear", "saturating-innovator"))
  expect_identical(rc$sweep$apExtrapolationWindow, c(2025, 2100))
  expect_identical(pfmResolveConfig(cfg, group = "v6-lin", verbose = FALSE)$sweep$apTransforms, "linear")

  gd <- file.path(dir, "out", "v6")
  dir.create(gd, recursive = TRUE)
  o <- pfm:::.pfmSweepOptionsForGroup(gd, rc$sweep)
  expect_identical(attr(o, "source"), "config")
  expect_true(o$dropCompositeAP)
  # what the sweep records is what a later call reads back, Inf included
  pfm:::.writeRunGroupManifest(gd, group = "v6", mode = NULL,
                               sweepOptions = list(apTransforms = list("linear", "saturating"),
                                                   dropCompositeAP = FALSE, apExtrapolationGate = Inf,
                                                   apExtrapolationSd = 1,
                                                   apExtrapolationWindow = list(2025, 2100)))
  o <- pfm:::.pfmSweepOptionsForGroup(gd, rc$sweep)
  expect_identical(attr(o, "source"), "manifest")
  expect_identical(o$apTransforms, c("linear", "saturating"))
  expect_identical(o$apExtrapolationGate, Inf)
  # swept before the record: nothing, i.e. runPFMSweep's defaults
  gd5 <- file.path(dir, "out", "v5")
  dir.create(gd5)
  jsonlite::write_json(list(group = "v5", panel_hash = "abc"), file.path(gd5, "manifest.json"), auto_unbox = TRUE)
  expect_length(pfm:::.pfmSweepOptionsForGroup(gd5, rc$sweep), 0)
  expect_error(pfm:::.pfmSweepOptionsNormalise(list(apTransform = "linear")), "unknown key")
})

test_that("the job script writes vector arguments as R vectors", {
  expect_identical(pfm:::.rlit(c(2025, 2100)), "c(2025, 2100)")
  expect_identical(pfm:::.rlit(c("linear", "saturating")), "c(\"linear\", \"saturating\")")
  expect_identical(pfm:::.rlit(Inf), "Inf")
})
