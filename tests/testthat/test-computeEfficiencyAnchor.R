# The efficiency anchor: where a swept theta gets its stated provenance.
#
# theta is DECLARED and swept, never estimated (MODEL.md 5.3). The anchor solves
# 1 - theta * median(u) = median(E) and exists so a swept value can be re-derived rather
# than trusted. It has already moved twice - once when the frontier was refitted on the
# deployed satAP spec, once when ADR 0042 made Diffuse rather than Bulk the sector setting
# the economy-wide floor - which is exactly why it is computed and stored, not hardcoded.

mkAgg <- function(E, sector = NULL, year = 2025) {
  d <- data.frame(region = names(E), year = year, efficiencyRatio = unname(E),
                  ceilingValid = TRUE, stringsAsFactors = FALSE)
  if (!is.null(sector)) d$sector <- sector
  d
}

test_that("the anchor solves 1 - theta*medianU = medianE", {
  E <- c(A = 0.9, B = 0.7, C = 0.5, D = 0.3)     # g = .1 .3 .5 .7, u = 0 1/3 2/3 1
  a <- computeEfficiencyAnchor(mkAgg(E))
  expect_equal(a$medianE, 0.6)
  expect_equal(a$medianU, 0.5)
  expect_equal(a$theta, (1 - 0.6) / 0.5)          # 0.8
  expect_true(a$inRange)
  # the defining identity, restated
  expect_equal(1 - a$theta * a$medianU, a$medianE, tolerance = 1e-12)
})

test_that("theta is driven by WHERE the median sits in the gap range, not its width", {
  # u is a normalised position, so a pure rescaling of the gaps leaves theta alone ...
  sym   <- computeEfficiencyAnchor(mkAgg(c(A = 0.8,  B = 0.6, C = 0.4)))
  tight <- computeEfficiencyAnchor(mkAgg(c(A = 0.65, B = 0.6, C = 0.55)))
  expect_equal(sym$medianU, tight$medianU)
  # ... while one extreme outlier compresses everyone else's u and RAISES theta, with
  # median E untouched. This is the mechanism behind Bulk's out-of-range anchor on v1
  # (RUS sits at a relative gap of 0.83) and the reason resolution matters: the same
  # countries aggregated into 21 regions give a different min/max, hence a different u.
  skewed <- computeEfficiencyAnchor(mkAgg(c(A = 0.8, B = 0.6, C = 0.05)))
  expect_equal(skewed$medianE, sym$medianE)
  expect_gt(skewed$theta, sym$theta)
})

test_that("a compressed gap distribution reports theta OUT of range rather than clipping", {
  # Bulk does this on v1 (theta = 1.19). The rule cannot reproduce median efficiency at any
  # admissible theta - a result to report, not a ceiling to raise.
  # Median gap sits near the MINIMUM while one unit carries a huge gap: median u is then
  # small relative to 1 - median E, and no admissible theta closes the difference.
  E <- c(A = 0.90, B = 0.85, C = 0.80, D = 0.75, E = 0.10)
  a <- computeEfficiencyAnchor(mkAgg(E))
  expect_gt(a$theta, 1)
  expect_false(a$inRange)
  expect_true(is.finite(a$theta))     # reported, not NA'd away
})

test_that("one row per sector, and each sector normalises on its OWN spread", {
  a <- rbind(mkAgg(c(A = 0.9, B = 0.7, C = 0.5, D = 0.3), sector = "Bulk"),
             mkAgg(c(A = 0.8, B = 0.75, C = 0.7, D = 0.65), sector = "Diffuse"))
  r <- computeEfficiencyAnchor(a)
  expect_setequal(r$sector, c("Bulk", "Diffuse"))
  expect_identical(nrow(r), 2L)
  expect_false(isTRUE(all.equal(r$theta[1], r$theta[2])))
})

test_that("invalid ceilings and duplicate region rows do not skew the median", {
  d <- mkAgg(c(A = 0.9, B = 0.7, C = 0.5, D = 0.3))
  d$ceilingValid[d$region == "D"] <- FALSE        # excluded
  dup <- rbind(d, d[d$region == "A", ])           # would double-weight A
  expect_equal(computeEfficiencyAnchor(dup)$n, 3L)
  expect_equal(computeEfficiencyAnchor(dup)$medianE, computeEfficiencyAnchor(d)$medianE)
})

test_that("a degenerate spread yields NA rather than a divide-by-zero theta", {
  a <- computeEfficiencyAnchor(mkAgg(c(A = 0.6, B = 0.6, C = 0.6)))
  expect_true(is.na(a$theta))
  expect_true(is.na(a$inRange))
})

test_that("the tier year comes from the attribute, not the earliest year", {
  d <- rbind(mkAgg(c(A = 0.9, B = 0.5), year = 2025),
             mkAgg(c(A = 0.2, B = 0.1), year = 2050))
  attr(d, "tierYear") <- 2050
  expect_identical(computeEfficiencyAnchor(d)$tierYear, 2050)
  expect_identical(computeEfficiencyAnchor(d, tierYear = 2025)$tierYear, 2025)
})

test_that("v1's country-level anchor reproduces the documented 0.74 / 1.19", {
  # Pins the number MODEL.md 5.3 quotes, against the artifact it was read from. If the
  # frontier is refitted this test fails, which is the point - the anchor moved silently
  # once already when the spec was corrected.
  root <- tryCatch(rprojroot::find_root("DESCRIPTION"), error = function(e) NA_character_)
  skip_if(is.na(root), "package root not found")
  fr <- file.path(root, "..", "output", "v1", "frontier.rds")
  skip_if_not(file.exists(fr), "output/v1/frontier.rds not available")
  f <- readRDS(fr)
  got <- vapply(c("Diffuse", "Bulk"), function(sec) {
    q <- as.data.frame(f$bySector[[sec]]$scores)
    q <- q[q$year == 2022, c("region", "efficiencyRatio")]
    q$year <- 2022; q$ceilingValid <- TRUE
    computeEfficiencyAnchor(q, resolution = "country")$theta
  }, numeric(1))
  expect_equal(unname(got[["Diffuse"]]), 0.7435, tolerance = 1e-3)
  expect_equal(unname(got[["Bulk"]]),    1.1895, tolerance = 1e-3)
})
