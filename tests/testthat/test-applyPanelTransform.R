makeFDTestPanel <- function() {
  set.seed(42)
  regions <- c("AAA", "BBB", "CCC")
  years <- 2000:2010
  df <- expand.grid(region = regions, year = years, stringsAsFactors = FALSE)
  df <- df[order(df$region, df$year), ]
  df$timeTrend <- df$year - 1999L
  df$logisticTimeTrend <- 1 / (1 + exp(-0.2 * (df$year - 2030)))
  df$Innovator.Power <- ave(runif(nrow(df)), df$region, FUN = cumsum)
  df$Incumbent.Power <- ave(runif(nrow(df)), df$region, FUN = cumsum)
  df$State.Capacity.PC1..VDem. <- rep(c(0.3, 0.5, 0.8), each = length(years))
  df$GDP.per.Capita <- ave(runif(nrow(df), 1, 2), df$region, FUN = cumsum)
  # AAA adopts 2005 onward; BBB adopts 2003 onward; CCC never
  df$ecp <- 0
  aaa <- df$region == "AAA" & df$year >= 2005
  bbb <- df$region == "BBB" & df$year >= 2003
  df$ecp[aaa] <- 10 + (df$year[aaa] - 2005) * 2
  df$ecp[bbb] <- 5 + (df$year[bbb] - 2003) * 1.5
  df$adoption <- as.integer(df$ecp > 0)
  df$Innovator.Power_x_State.Capacity.PC1..VDem. <-
    df$Innovator.Power * df$State.Capacity.PC1..VDem.
  df
}

test_that("levels transform is a no-op", {
  df <- makeFDTestPanel()
  lv <- applyPanelTransform(df, "levels", "adoption")
  expect_identical(nrow(lv), nrow(df))
  expect_identical(attr(lv, "panelTransform"), "levels")
})

test_that("hybridFD adoption builds the hazard (onset) sample", {
  df <- makeFDTestPanel()
  ad <- applyPanelTransform(df, "hybridFD", "adoption",
    actorPowerDrivers = c("Innovator Power", "Incumbent Power"),
    actorPowerIndex = c("Innovator Power", "Incumbent Power"),
    instQualityDrivers = "State Capacity PC1 (VDem)",
    controlDrivers = "GDP per Capita", verbose = FALSE
  )
  # At-risk set: AAA 2001-2005 (5), BBB 2001-2003 (3), CCC 2001-2010 (10)
  expect_equal(nrow(ad), 18)
  expect_equal(sum(ad$adoption), 2) # onsets: AAA 2005, BBB 2003
  expect_true(all(ad$adoption[ad$region == "CCC"] == 0))
  expect_false(any(ad$year == 2000)) # first panel year has no previous year
  # AP differenced (per-region cumsum of runif => diffs in (0,1))
  expect_true(all(ad$Innovator.Power > 0 & ad$Innovator.Power < 1))
  # IQ stays in levels under hybridFD
  expect_true(all(ad$State.Capacity.PC1..VDem. %in% c(0.3, 0.5, 0.8)))
  # Interaction recomputed = deltaAP x IQ(level)
  expect_equal(
    ad$Innovator.Power_x_State.Capacity.PC1..VDem.,
    ad$Innovator.Power * ad$State.Capacity.PC1..VDem.
  )
  # Controls stay in levels under hybridFD
  expect_gt(max(ad$GDP.per.Capita), 2)
  expect_identical(attr(ad, "panelTransform"), "hybridFD")
})

test_that("pureFD differences institutional quality and controls too", {
  df <- makeFDTestPanel()
  adP <- applyPanelTransform(df, "pureFD", "adoption",
    actorPowerDrivers = c("Innovator Power", "Incumbent Power"),
    actorPowerIndex = c("Innovator Power", "Incumbent Power"),
    instQualityDrivers = "State Capacity PC1 (VDem)",
    controlDrivers = "GDP per Capita", verbose = FALSE
  )
  # IQ is constant within region => differences ~ 0 (the channel is extinguished)
  expect_true(all(abs(adP$State.Capacity.PC1..VDem.) < 1e-12))
  expect_true(all(adP$GDP.per.Capita > 0 & adP$GDP.per.Capita < 2 + 1e-9))
})

test_that("hybridFD stringency computes within-spell delta log(1+ECP)", {
  df <- makeFDTestPanel()
  st <- applyPanelTransform(df, "hybridFD", "stringency",
    actorPowerDrivers = c("Innovator Power", "Incumbent Power"),
    actorPowerIndex = c("Innovator Power", "Incumbent Power"),
    instQualityDrivers = "State Capacity PC1 (VDem)",
    controlDrivers = "GDP per Capita",
    logTransform = TRUE, verbose = FALSE
  )
  # Spell pairs: AAA 2006-2010 (5), BBB 2004-2010 (7)
  expect_equal(nrow(st), 12)
  expect_equal(
    st$ecp[st$region == "AAA" & st$year == 2006],
    log1p(12) - log1p(10)
  )
  expect_true(all(is.finite(st$ecp)))
})

test_that("stringency without log transform differences raw ECP", {
  df <- makeFDTestPanel()
  st2 <- applyPanelTransform(df, "hybridFD", "stringency",
    actorPowerDrivers = "Innovator Power",
    actorPowerIndex = "Innovator Power",
    instQualityDrivers = "State Capacity PC1 (VDem)",
    controlDrivers = "GDP per Capita",
    logTransform = FALSE, verbose = FALSE
  )
  expect_equal(st2$ecp[st2$region == "AAA" & st2$year == 2006], 2) # AAA grows 2/yr
})

test_that("Mundlak group-mean columns are rejected under FD", {
  df <- makeFDTestPanel()
  df$Innovator.Power_grp_mean <- 1
  expect_error(
    applyPanelTransform(df, "hybridFD", "adoption",
      actorPowerDrivers = "Innovator Power", verbose = FALSE
    ),
    "Mundlak"
  )
})
