# nolint start
makeScoreInput <- function() {
  data.frame(
    model = rep(c("D4", "IQ-01", "FE-05", "BadVIF"), each = 2),
    sector = rep(c("Bulk", "Diffuse"), times = 4),
    sigActorPower   = c(1, 1,  1, 1,  0, 0,  1, 1),
    sigInstQual     = c(1, 1,  1, 0,  0, 0,  1, 1),
    sigInteractions = c(0, 1,  1, 0,  0, 0,  1, 1),
    deltaR2Theory   = c(0.186, 0.339,  0.020, 0.150,  0.001, 0.002,  0.40, 0.40),
    maxVIF          = c(2.4, 2.4,  3.1, 3.0,  1.5, 1.5,  12, 2),
    converged       = TRUE,
    stringsAsFactors = FALSE
  )
}

test_that("computeTheoryTier matches the report definition", {
  expect_identical(computeTheoryTier(1, 1, 1), "Green")
  expect_identical(computeTheoryTier(1, 0, 0), "Blue")
  expect_identical(computeTheoryTier(0, 0, 0), "Yellow")
  expect_identical(computeTheoryTier(NA, NA, NA), "Yellow")
  expect_identical(computeTheoryTier(c(1, 0), c(1, 0), c(1, 0)), c("Green", "Yellow"))
})

test_that("maximin ranks by worse-sector tier, then mean deltaR2", {
  out <- computeMaximinScore(makeScoreInput())
  # D4: Bulk Blue (no sig interaction) / Diffuse Green => minTier Blue
  # IQ-01: Bulk Green / Diffuse Blue => minTier Blue
  # FE-05: Yellow/Yellow; BadVIF: Green/Green but VIF gate fails
  expect_identical(out$minTier[out$model == "D4"], "Blue")
  expect_identical(out$minTier[out$model == "IQ-01"], "Blue")
  expect_identical(out$minTier[out$model == "FE-05"], "Yellow")
  # BadVIF fails gates and is ranked last despite Green/Green
  expect_false(out$gatePass[out$model == "BadVIF"])
  expect_equal(out$rank[out$model == "BadVIF"], 4)
  expect_match(out$gateFailReason[out$model == "BadVIF"], "VIF")
  # Among the two Blue specs, D4 wins on mean deltaR2 (0.2625 vs 0.085)
  expect_lt(out$rank[out$model == "D4"], out$rank[out$model == "IQ-01"])
  # Worse-sector deltaR2 reported
  expect_equal(out$minDeltaR2[out$model == "D4"], 0.186)
})

test_that("inflated deltaR2(theory) fails the fit-reliability gate", {
  df <- makeScoreInput()
  # Make a Green/Green spec with an impossible (>1) incremental pseudo-R2 in one sector
  df$deltaR2Theory[df$model == "BadVIF"] <- c(3.45, 0.40)   # also has VIF issue, but test the inflation path
  df$maxVIF[df$model == "BadVIF"] <- 2                      # clear the VIF gate to isolate the inflation gate
  out <- computeMaximinScore(df)                             # deltaR2Max defaults to 1
  expect_false(out$gatePass[out$model == "BadVIF"])
  expect_match(out$gateFailReason[out$model == "BadVIF"], "inflated")
  # A valid Green/Green spec (deltaR2 <= 1) still passes and outranks the inflated one
  expect_true(out$rank[out$model == "BadVIF"] > min(out$rank))
  # Disabling the gate (deltaR2Max = Inf) lets the inflated spec pass again
  out2 <- computeMaximinScore(df, deltaR2Max = Inf)
  expect_true(out2$gatePass[out2$model == "BadVIF"])
})

test_that("optional pseudoR2Range gate rejects degenerate fits when enabled", {
  df <- makeScoreInput()
  df$pseudoR2 <- 0.3
  df$pseudoR2[df$model == "FE-05"] <- c(-2.5, 0.1)   # degenerate negative pseudo-R2
  on_gate  <- computeMaximinScore(df, pseudoR2Range = c(0, 1))
  off_gate <- computeMaximinScore(df)
  expect_match(on_gate$gateFailReason[on_gate$model == "FE-05"], "pseudoR2")
  # off by default: pseudoR2 not consulted
  expect_false(grepl("pseudoR2", off_gate$gateFailReason[off_gate$model == "FE-05"]))
})

test_that("missing sectors, non-convergence and lagged terms fail gates", {
  df <- makeScoreInput()
  df <- df[!(df$model == "D4" & df$sector == "Diffuse"), ]
  df$converged[df$model == "IQ-01" & df$sector == "Bulk"] <- FALSE
  df$usesLagged <- df$model == "FE-05"
  out <- computeMaximinScore(df)
  expect_match(out$gateFailReason[out$model == "D4"], "missing sector")
  expect_match(out$gateFailReason[out$model == "IQ-01"], "not converged")
  expect_match(out$gateFailReason[out$model == "FE-05"], "lagged")
})
# nolint end
