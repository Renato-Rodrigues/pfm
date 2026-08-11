# The REMIND include for cm_taxCO2_regiDiff = 11 (political-feasibility
# differentiation). The properties that matter: the file must be TOTAL (a region
# can never be silently dropped), defaults must be the UNCOUPLED value so a
# partial file can only weaken the constraint, and the emitted GAMS must be plain
# assignments -- declarations are illegal inside the runtime if-block that
# includes it.

rdFix <- function() {
  data.frame(region = rep(c("EUR", "JPN", "USA"), each = 2),
             sector = rep(c("Bulk", "Diffuse"), 3),
             phi = c(0.7, 0.9, 0.2, 0.5, 1, 1),
             stringsAsFactors = FALSE)
}

test_that("the include is total: absent regions are written as uncoupled", {
  d <- withr::local_tempdir()
  f <- file.path(d, "p45_regiDiff_feasibility.inc")
  regs <- c("EUR", "JPN", "USA", "CHA", "SSA")
  out <- exportFeasibilityRegiDiff(rdFix(), f, regions = regs)
  expect_setequal(out$region, regs)
  expect_equal(out$phi[out$region == "CHA"], 1)   # absent from feasibility
  expect_equal(out$phi[out$region == "SSA"], 1)
  txt <- readLines(f)
  for (r in regs) expect_true(any(grepl(r, txt, fixed = TRUE)))
})

test_that("sectorRule = 'min' takes the worse sector's share", {
  d <- withr::local_tempdir()
  out <- exportFeasibilityRegiDiff(rdFix(), file.path(d, "x.inc"), sectorRule = "min")
  expect_equal(out$phi[out$region == "EUR"], 0.7)
  expect_equal(out$phi[out$region == "JPN"], 0.2)
  outB <- exportFeasibilityRegiDiff(rdFix(), file.path(d, "y.inc"), sectorRule = "Bulk")
  expect_equal(outB$phi[outB$region == "JPN"], 0.2)
  outD <- exportFeasibilityRegiDiff(rdFix(), file.path(d, "z.inc"), sectorRule = "Diffuse")
  expect_equal(outD$phi[outD$region == "JPN"], 0.5)
})

test_that("emitted GAMS is plain assignments only - no declarations", {
  d <- withr::local_tempdir()
  f <- file.path(d, "x.inc")
  exportFeasibilityRegiDiff(rdFix(), f, lambda = 0.05)
  txt <- readLines(f)
  isComment <- startsWith(trimws(txt), "***")
  isBlank <- !nzchar(trimws(txt))
  code <- txt[!isComment & !isBlank]
  expect_gt(length(code), 0)
  # A `parameter`/`set`/`scalar` declaration inside the runtime if-block that
  # includes this file would be a GAMS compile error.
  expect_false(any(grepl("^(parameter|set|scalar)", trimws(code), ignore.case = TRUE)))
  expect_true(all(grepl("^p45_regiDiff_(phi|lambda)", trimws(code))))
  expect_true(all(endsWith(trimws(code), ";")))
  expect_true(any(grepl("0.050000", code, fixed = TRUE)))
})

test_that("phi is clamped to [0,1] and lambda kept below 1", {
  d <- withr::local_tempdir()
  bad <- data.frame(region = c("EUR", "JPN"), phi = c(-0.5, 2),
                    stringsAsFactors = FALSE)
  out <- exportFeasibilityRegiDiff(bad, file.path(d, "x.inc"), lambda = 5,
                                   sectorRule = "none")
  expect_equal(out$phi, c(0, 1))
  expect_true(all(out$lambda < 1))
})

test_that("time-varying phi warns rather than silently averaging", {
  d <- withr::local_tempdir()
  tv <- data.frame(region = c("EUR", "EUR"), year = c(2030, 2050),
                   phi = c(0.5, 0.9), stringsAsFactors = FALSE)
  expect_warning(exportFeasibilityRegiDiff(tv, file.path(d, "x.inc"),
                                           sectorRule = "none"),
                 "varies over time")
})

test_that("bad inputs fail loudly", {
  d <- withr::local_tempdir()
  expect_error(exportFeasibilityRegiDiff(rdFix(), file.path(d, "x.csv")),
               "must end in .inc")
  expect_error(exportFeasibilityRegiDiff(rdFix()[, "region", drop = FALSE],
                                         file.path(d, "x.inc")),
               "needs 'region' and 'phi'")
  expect_error(exportFeasibilityRegiDiff(rdFix()[, c("region", "phi")],
                                         file.path(d, "x.inc"), sectorRule = "Bulk"),
               "needs a 'sector' column")
})

test_that("iterativePFM degrades safely: no gdx written when the step fails", {
  d <- withr::local_tempdir()
  withr::local_dir(d)
  # No REMIND gdx present -> the coupling must warn and write nothing, so REMIND
  # retains the previous iteration's phi rather than reverting to uncoupled.
  expect_warning(ok <- iterativePFM(gdx = "definitely-missing.gdx",
                                    outputFile = "p45_regiDiff_phi.gdx",
                                    verbose = FALSE),
                 "FAILED")
  expect_false(ok)
  expect_false(file.exists("p45_regiDiff_phi.gdx"))
})
