makeScreenTestPanel <- function(n = 400) {
  set.seed(7)
  base <- rnorm(n)
  df <- data.frame(
    region = rep(sprintf("R%02d", 1:20), each = n / 20),
    year = rep(2000:2019, times = n / 20)
  )
  # govEff candidates: PC1 informative, PC2 noise
  df$State.Capacity.PC1..VDem. <- 0.8 * base + 0.6 * rnorm(n)
  df$State.Capacity.PC2..VDem. <- rnorm(n)
  # rule of law correlated with horacc at ~0.95 (the forbidden pair)
  df$Rule.of.Law..VDem. <- 0.7 * base + 0.7 * rnorm(n)
  df$Horizontal.Accountability..VDem. <-
    0.97 * df$Rule.of.Law..VDem. + 0.18 * rnorm(n)
  # veracc carries independent signal; diagacc is noise
  signal2 <- rnorm(n)
  df$Vertical.Accountability..VDem. <- 0.8 * signal2 + 0.6 * rnorm(n)
  df$Diagonal.Accountability..VDem. <- rnorm(n)
  # outcome loads on base (through PC1/RoL) and on veracc's signal
  df$ecp <- 1.5 * base + 1.2 * signal2 + rnorm(n)
  df$adoption <- as.integer(df$ecp > median(df$ecp))
  df
}

test_that("forbidden pairs are detected above the threshold", {
  df <- makeScreenTestPanel()
  scr <- computeChannelScreen(df, depVar = "ecp",
    channelCandidates = list(
      govEff = c("State Capacity PC1 (VDem)", "State Capacity PC2 (VDem)"),
      ruleOfLaw = "Rule of Law (VDem)",
      accountability = c(
        "Vertical Accountability (VDem)",
        "Horizontal Accountability (VDem)",
        "Diagonal Accountability (VDem)"
      )
    ),
    corThreshold = 0.8
  )
  fp <- scr$forbiddenPairs
  expect_true(nrow(fp) >= 1)
  expect_true(any(
    grepl("Rule of Law", fp$var1) & grepl("Horizontal", fp$var2) |
      grepl("Rule of Law", fp$var2) & grepl("Horizontal", fp$var1)
  ))
})

test_that("accountability ranking prefers the candidate with unique signal", {
  df <- makeScreenTestPanel()
  scr <- computeChannelScreen(df, depVar = "ecp")
  # WGI Government Effectiveness is absent from this panel: dropped with a message
  expect_false(any(grepl("WGI", scr$slotRankings$candidate)))
  # Vertical accountability carries signal orthogonal to govEff => best in slot
  expect_identical(unname(scr$bestPerSlot["accountability"]),
                   "Vertical Accountability (VDem)")
  # Partial-correlation rows control for each available govEff candidate
  ctl <- unique(scr$slotRankings$controllingFor)
  expect_true("State Capacity PC1 (VDem)" %in% ctl)
  expect_true("(none)" %in% ctl)
})

test_that("binary depVar (adoption) and missing depVar are handled", {
  df <- makeScreenTestPanel()
  scr <- computeChannelScreen(df, depVar = "adoption")
  expect_true(all(is.finite(scr$slotRankings$partialR)))
  expect_error(computeChannelScreen(df, depVar = "nope"), "not found")
})
