makeProj <- function(price = 100, prob = 0.5, regions = c("AAA", "BBB"),
                     years = 2025:2035) {
  g <- expand.grid(region = regions, year = years, stringsAsFactors = FALSE)
  g$sector <- "Bulk"
  g$prob <- prob
  g$price <- price
  g$expectedPrice <- g$prob * g$price
  g
}

test_that("clean projections pass all rules", {
  s <- computeProjectionSanity(makeProj(), stage = "hurdle")
  expect_true(s$summary$pass)
  expect_equal(nrow(s$flags), 0)
})

test_that("price explosion and invalid prices are severe", {
  p <- makeProj()
  p$price[p$region == "AAA" & p$year == 2030] <- 2500
  s <- computeProjectionSanity(p, stage = "stringency")
  expect_false(s$summary$pass)
  expect_true("price-explosion" %in% s$flags$rule)

  p2 <- makeProj()
  p2$price[3] <- -5
  s2 <- computeProjectionSanity(p2, stage = "stringency")
  expect_true("price-invalid" %in% s2$flags$rule)
  expect_false(s2$summary$pass)
})

test_that("missing prices are a coverage warning, not severe", {
  p <- makeProj()
  p$price[p$region == "BBB"] <- NA # 50% missing
  s <- computeProjectionSanity(p, stage = "stringency")
  expect_true("price-missing" %in% s$flags$rule)
  expect_identical(s$flags$severity[s$flags$rule == "price-missing"], "warning")
  expect_true(s$summary$pass)
})

test_that("all-region saturation is a warning by default (ambitious-scenario expected)", {
  p <- makeProj(prob = 0.5, years = 2025:2055)
  p$prob[p$year >= 2035] <- 0.995
  s <- computeProjectionSanity(p, stage = "adoption")
  expect_identical(s$flags$severity[s$flags$rule == "prob-saturation"], "warning")
  expect_true(s$summary$pass) # does NOT disqualify
})

test_that("saturation can be escalated to severe only if implausibly early", {
  p <- makeProj(prob = 0.5, years = 2025:2055)
  p$prob[p$year >= 2028] <- 0.995
  s <- computeProjectionSanity(p, stage = "adoption",
                               thresholds = list(saturationSevereBefore = 2030))
  expect_identical(s$flags$severity[s$flags$rule == "prob-saturation"], "severe")
  expect_false(s$summary$pass)
})

test_that("saturation warns (not severe); dead regions are severe; blocks respected", {
  p <- makeProj(prob = 0.5)
  p$prob[p$year >= 2033] <- 0.995
  s <- computeProjectionSanity(p, stage = "adoption")
  expect_true("prob-saturation" %in% s$flags$rule)
  expect_identical(s$flags$severity[s$flags$rule == "prob-saturation"], "warning")
  expect_true(s$summary$pass) # saturation no longer disqualifies (ambitious-scenario expected)

  p2 <- makeProj(prob = 0.5)
  p2$prob[p2$region == "BBB"] <- 0.005
  s2 <- computeProjectionSanity(p2, stage = "adoption")
  expect_true("prob-dead" %in% s2$flags$rule)
  expect_false(s2$summary$pass) # a never-adopting block IS severe

  # With a block mapping that pools BBB with the healthy AAA, no dead flag
  blocks <- data.frame(region = c("AAA", "BBB"), block = "BLK")
  s3 <- computeProjectionSanity(p2, stage = "adoption", regionBlocks = blocks)
  expect_false("prob-dead" %in% s3$flags$rule)
})

test_that("spikes and seam jumps are warnings, not disqualifying", {
  p <- makeProj(price = 50)
  p$price[p$region == "AAA" & p$year == 2030] <- 150 # 3x neighbours
  s <- computeProjectionSanity(p, stage = "stringency")
  expect_true("price-spike" %in% s$flags$rule)
  expect_true(s$summary$pass) # warnings only

  hist <- data.frame(region = "AAA", year = 2022, price = 10)
  s2 <- computeProjectionSanity(makeProj(price = 60), stage = "stringency",
                                histPrices = hist)
  expect_true("seam-jump-price" %in% s2$flags$rule)
  expect_true(s2$summary$pass)

  hp <- data.frame(region = "AAA", prob = 0.1)
  s3 <- computeProjectionSanity(makeProj(prob = 0.6), stage = "adoption",
                                histProbs = hp)
  expect_true("seam-jump-prob" %in% s3$flags$rule)
  expect_true(s3$summary$pass)
})

test_that("stage scoping: adoption ignores price rules and vice versa", {
  p <- makeProj()
  p$price <- 5000 # would be severe for stringency
  s <- computeProjectionSanity(p, stage = "adoption")
  expect_true(s$summary$pass)
  p2 <- makeProj()
  p2$prob <- 0.999 # would be severe for adoption
  s2 <- computeProjectionSanity(p2, stage = "stringency")
  expect_true(s2$summary$pass)
})

test_that("seam diagnostics flag misaligned variables", {
  h <- magclass::new.magpie(c("AAA", "BBB"), 2000:2022, c("X", "Y"), fill = NA)
  s <- magclass::new.magpie(c("AAA", "BBB"), 2025:2040, c("X", "Y"), fill = NA)
  set.seed(5)
  for (r in c("AAA", "BBB")) {
    # X: smooth trend, scenario continues exactly where history ends
    h[r, , "X"] <- seq(0.3, 0.52, length.out = 23) + rnorm(23, 0, 0.02)
    h[r, , "Y"] <- rnorm(23, 0.5, 0.1)
    xLast <- as.numeric(h[r, 2022, "X"])
    s[r, , "X"] <- xLast + seq(0, 0.015, length.out = 16)  # continuous
    s[r, , "Y"] <- rnorm(16, 1.5, 0.1)                     # ~10-sd jump
  }
  d <- computeSeamDiagnostics(h, s)
  expect_true(all(d$flagged[d$variable == "Y"]))
  expect_false(any(d$flagged[d$variable == "X"]))
  expect_identical(d$variable[1], "Y") # sorted by |normJump|
})

test_that("overlapping panels anchor the seam on the last common year", {
  # Scenario republishes 2015-2025 (overlapping history 2000-2022) then extends.
  h <- magclass::new.magpie(c("AAA", "BBB"), 2000:2022, c("X", "Y"), fill = NA)
  s <- magclass::new.magpie(c("AAA", "BBB"), seq(2015, 2035, 5), c("X", "Y"), fill = NA)
  set.seed(9)
  for (r in c("AAA", "BBB")) {
    h[r, , "X"] <- rnorm(23, 0.5, 0.1)
    h[r, , "Y"] <- rnorm(23, 0.5, 0.1)
    # X agrees with history at the common years; Y is offset by ~10 sd at 2020.
    s[r, , "X"] <- as.numeric(h[r, c(2015, 2020), "X"])[2] + rnorm(5, 0, 0.01)
    s[r, , "Y"] <- 1.5 + rnorm(5, 0, 0.01)
  }
  d <- computeSeamDiagnostics(h, s)
  # Anchor must be the last common year (2020), not min-scenario (2015).
  expect_true(all(d$histYear == 2020 & d$scenYear == 2020))
  expect_false(any(d$flagged[d$variable == "X"]))
  expect_true(all(d$flagged[d$variable == "Y"]))
})
