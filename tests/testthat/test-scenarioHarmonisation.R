# Decision 2B of 2026-10-06 (PITFALLS 30): every series the scenario panel shares with history is
# harmonised to the panel's last year, the institution-rule series included, fading to zero by 2040.

.harmFixture <- function() {
  yrs <- c(2020, 2025, 2030, 2035, 2040, 2050)
  arr <- array(0.5, dim = c(2, length(yrs), 2),
               dimnames = list(c("AAA", "BBB"), paste0("y", yrs), c("Rule of Law (VDem)", "GDP")))
  out <- magclass::as.magpie(arr, spatial = 1)
  hist <- magclass::as.magpie(array(c(0.7, 0.4, 0.5, 0.5), dim = c(2, 1, 2),
                                    dimnames = list(c("AAA", "BBB"), "y2023", c("Rule of Law (VDem)", "GDP"))),
                              spatial = 1)
  hist["BBB", , "GDP"] <- NA
  list(out = out, hist = hist)
}

test_that("the offset to history is applied in full up to the stitch year and fades to zero by 2040", {
  f <- .harmFixture()
  atStitch <- magclass::time_interpolate(f$out, 2023, extrapolation_type = "linear")
  h <- pfm:::.pfmHarmoniseScenario(f$out, f$hist, atStitch, c("Rule of Law (VDem)", "GDP"), 2023, 2040)
  v <- "Rule of Law (VDem)"
  expect_equal(as.numeric(h["AAA", 2020, v]), 0.7)
  expect_equal(as.numeric(h["AAA", 2025, v]), 0.5 + 0.2 * 15 / 17)
  expect_equal(as.numeric(h["AAA", 2035, v]), 0.5 + 0.2 * 5 / 17)
  expect_equal(as.numeric(h["AAA", 2040, v]), 0.5)
  expect_equal(as.numeric(h["BBB", 2030, v]), 0.5 - 0.1 * 10 / 17)
  # a country without a history value keeps its projection rather than turning NA
  expect_equal(as.numeric(h["BBB", , "GDP"]), rep(0.5, 6))
})

test_that("panelDataScenario no longer exempts the institution-rule series from harmonisation", {
  src <- paste(deparse(body(pfm::panelDataScenario)), collapse = "\n")
  expect_false(grepl("constants <-", src, fixed = TRUE))
  expect_true(grepl(".pfmHarmoniseScenario(", src, fixed = TRUE))
  # the state-capacity PCA reads its inputs back from the harmonised panel
  expect_true(grepl("computeVDemStateCapacityPC(out[, , magclass::getNames(scNorm)]", gsub("\\s+", "", src), fixed = TRUE) ||
                grepl("out[,,magclass::getNames(scNorm)]", gsub("\\s+", "", src), fixed = TRUE))
})
