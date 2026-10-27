# The gap-closure institution rule (design note 0008) on the panel scale, and the democracy weight's level.
panel <- function(vals, years, name) {
  m <- matrix(vals, ncol = length(years), byrow = TRUE)
  magclass::new.magpie(paste0("C", seq_len(nrow(m))), years, name, fill = as.vector(m))
}

test_that("gap closure starts from the panel's last value and closes a fixed share of the gap each year", {
  hist <- panel(c(rep(0.2, 10), rep(0.6, 10), rep(1, 10)), 2014:2023, "Rule of Law (VDem)")
  out <- panel(rep(0, 3 * 5), c(2020, 2025, 2030, 2050, 2100), "Rule of Law (VDem)")
  res <- pfm:::.pfmGapClosurePanel(out, hist, "Rule of Law (VDem)", character(0), 2023, 2040)
  lam <- pfmInstitutionGapClosureRates()$rate[1]
  expect_equal(as.numeric(res["C1", 2050, 1]), 1 - 0.8 * exp(-lam * 27), tolerance = 1e-9)
  expect_equal(as.numeric(res["C2", 2100, 1]), 1 - 0.4 * exp(-lam * 77), tolerance = 1e-9)
  expect_equal(as.numeric(res["C3", 2100, 1]), 1)                        # at the top: stays
  expect_equal(as.numeric(res[, 2020, 1]), c(0, 0, 0))                    # before the panel's last year: untouched
  expect_lt(abs(as.numeric(res["C1", 2025, 1]) - 0.2), 0.02)             # no step: two years of the rule
})

test_that("each series takes its own rate: rule of law, then every other series", {
  expect_equal(pfm:::.pfmGapClosureRate("Rule of Law (VDem)"), 0.0096)
  expect_equal(pfm:::.pfmGapClosureRate("Vertical Accountability (VDem)"), 0.0056)
  expect_equal(pfm:::.pfmGapClosureRate("Policy Implementation (VDem)"), 0.0056)
})

test_that("the weight moves smoothly to the higher of its last value and its ten-year mean", {
  hist <- panel(c(seq(0.9, 0.45, length.out = 10), seq(0.3, 0.75, length.out = 10)), 2014:2023, "Freedom of Expression (VDem)")
  out <- panel(rep(0, 2 * 5), c(2025, 2030, 2040, 2050, 2100), "Freedom of Expression (VDem)")
  res <- pfm:::.pfmGapClosurePanel(out, hist, character(0), "Freedom of Expression (VDem)", 2023, 2040)
  m1 <- mean(seq(0.9, 0.45, length.out = 10))
  expect_equal(as.numeric(res["C1", 2040, 1]), m1); expect_equal(as.numeric(res["C1", 2100, 1]), m1)
  expect_gt(as.numeric(res["C1", 2025, 1]), 0.45); expect_lt(as.numeric(res["C1", 2025, 1]), 0.47)   # starts at 0.45, gently
  expect_equal(as.numeric(res["C2", 2100, 1]), 0.75)                                               # rising: keeps its last value
})

test_that("the gap-closure path is the closed form", {
  expect_equal(pfm:::.pfmGapClosurePath(0.5, 2100, 2025, 0.01, top = 0.9), 0.9 - 0.4 * exp(-0.75))
})

test_that("the smooth harmonisation fades without a kink and matches the linear fade at its ends", {
  x <- magclass::new.magpie("C1", 2023:2040, "v", fill = 0)
  h <- magclass::new.magpie("C1", 2023, "v", fill = 1)
  s <- as.numeric(pfm:::.pfmHarmoniseScenario(x, h, x, "v", 2023, 2040, shape = "smooth"))
  l <- as.numeric(pfm:::.pfmHarmoniseScenario(x, h, x, "v", 2023, 2040))
  expect_equal(c(s[1], s[18]), c(1, 0)); expect_equal(c(l[1], l[18]), c(1, 0))
  expect_lt(abs(s[2] - s[1]), abs(l[2] - l[1]))
  expect_lt(abs(s[18] - s[17]), abs(l[18] - l[17]))
})
