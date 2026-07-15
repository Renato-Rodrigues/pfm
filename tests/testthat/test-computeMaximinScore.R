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

test_that("inference-fragility preference demotes marginal theory terms within a near-tie band (ADR 0037)", {
  # Two Green/Green specs in a near-tie: "Squeaker" leads on meanDeltaR2 by < eps but its
  # weakest significant theory term is a p=.049 squeaker (|t| ~ 1.97); "Comfort" carries
  # comfortable margins. With inferenceTGate the comfortable spec wins the band; without,
  # the base deltaR2 ordering holds.
  df <- data.frame(
    model = rep(c("Squeaker", "Comfort"), each = 2),
    sector = rep(c("Bulk", "Diffuse"), times = 2),
    sigActorPower = 1, sigInstQual = 1, sigInteractions = 1,
    deltaR2Theory = c(0.310, 0.310, 0.300, 0.300),
    maxVIF = 2, converged = TRUE,
    # Squeaker carries the better BIC, so absent the demotion it wins the band on parsimony
    bic = c(90, 90, 100, 100),
    minSigTheoryT = c(1.97, 2.8, 3.1, 2.9),
    stringsAsFactors = FALSE
  )
  on_pref <- computeMaximinScore(df, nearTieEps = 0.025, inferenceTGate = 2.33)
  expect_lt(on_pref$rank[on_pref$model == "Comfort"], on_pref$rank[on_pref$model == "Squeaker"])
  # both still pass the hard gates - it is a preference, never a gate
  expect_true(all(on_pref$gatePass))

  off_pref <- computeMaximinScore(df, nearTieEps = 0.025, inferenceTGate = NULL)
  expect_lt(off_pref$rank[off_pref$model == "Squeaker"], off_pref$rank[off_pref$model == "Comfort"])

  # outside the near-tie band the preference cannot reorder: a clearly better spec wins anyway
  df2 <- df
  df2$deltaR2Theory[df2$model == "Squeaker"] <- 0.40
  far <- computeMaximinScore(df2, nearTieEps = 0.025, inferenceTGate = 2.33)
  expect_lt(far$rank[far$model == "Squeaker"], far$rank[far$model == "Comfort"])

  # NA minSigTheoryT (no significant theory term / column semantics) is never demoted
  df3 <- df
  df3$minSigTheoryT <- NA_real_
  na_pref <- computeMaximinScore(df3, nearTieEps = 0.025, inferenceTGate = 2.33)
  expect_lt(na_pref$rank[na_pref$model == "Squeaker"], na_pref$rank[na_pref$model == "Comfort"])
})

test_that("Tournament v2: worseDeltaR2 ranking and tier gate (ADR 0039)", {
  df <- data.frame(
    model = rep(c("BalancedBlue", "LopsidedGreen", "WeakGreen"), each = 2),
    sector = rep(c("Bulk", "Diffuse"), times = 3),
    #                Balanced (Blue)  Lopsided (Green)  Weak (Green)
    sigActorPower   = c(1, 1,          1, 1,              1, 1),
    sigInstQual     = c(1, 1,          1, 1,              1, 1),
    sigInteractions = c(0, 0,          1, 1,              1, 1),
    deltaR2Theory   = c(0.10, 0.17,    0.04, 0.16,        0.07, 0.13),
    maxVIF = 2, converged = TRUE, bic = 100,
    stringsAsFactors = FALSE
  )
  # tierMean (legacy): tier first -> LopsidedGreen (mean .100) beats WeakGreen (mean .100)?
  # Both Green; means equal -> band/BIC/name; the point: BalancedBlue ranks LAST despite
  # the best worse-sector dR2, because it is Blue.
  legacy <- computeMaximinScore(df, rankBy = "tierMean")
  expect_gt(legacy$rank[legacy$model == "BalancedBlue"],
            max(legacy$rank[legacy$model != "BalancedBlue"] * 0 + 2) - 1) # ranks 3rd
  expect_equal(legacy$rank[legacy$model == "BalancedBlue"], 3)

  # worseDeltaR2 (v2, no tier gate): BalancedBlue wins on min dR2 (.10 > .07 > .04)
  v2 <- computeMaximinScore(df, rankBy = "worseDeltaR2", nearTieEps = 0)
  expect_equal(v2$model[v2$rank == 1], "BalancedBlue")
  expect_equal(v2$model[v2$rank == 2], "WeakGreen")

  # Green tier gate: BalancedBlue fails the hard gate; WeakGreen wins
  v2g <- computeMaximinScore(df, rankBy = "worseDeltaR2", tierGate = "Green", nearTieEps = 0)
  expect_false(v2g$gatePass[v2g$model == "BalancedBlue"])
  expect_match(v2g$gateFailReason[v2g$model == "BalancedBlue"], "tier below Green")
  expect_equal(v2g$model[v2g$rank == 1], "WeakGreen")

  # Blue gate admits all three; ranking unchanged from ungated v2
  v2b <- computeMaximinScore(df, rankBy = "worseDeltaR2", tierGate = "Blue", nearTieEps = 0)
  expect_true(all(v2b$gatePass))
  expect_equal(v2b$model[v2b$rank == 1], "BalancedBlue")
})

test_that("computeSelectionVariants documents tier winners and the sharing cost", {
  df <- data.frame(
    model = rep(c("BalancedBlue", "LopsidedGreen", "WeakGreen"), each = 2),
    sector = rep(c("Bulk", "Diffuse"), times = 3),
    sigActorPower   = c(1, 1,  1, 1,  1, 1),
    sigInstQual     = c(1, 1,  1, 1,  1, 1),
    sigInteractions = c(0, 0,  1, 1,  1, 1),
    deltaR2Theory   = c(0.10, 0.17,  0.04, 0.16,  0.07, 0.13),
    maxVIF = 2, converged = TRUE, bic = 100,
    stringsAsFactors = FALSE
  )
  v <- computeSelectionVariants(df, tierGates = c("Green", "Blue"),
                                deployedGate = "Green", nearTieEps = 0)
  expect_equal(v$winners$Green, "WeakGreen")
  expect_equal(v$winners$Blue, "BalancedBlue")
  ps <- v$perSector
  # Bulk best own spec = BalancedBlue (0.10); deployed (WeakGreen) has 0.07
  expect_equal(ps$bestModel[ps$sector == "Bulk"], "BalancedBlue")
  expect_equal(ps$sharingCost[ps$sector == "Bulk"], 0.03, tolerance = 1e-9)
  # Diffuse best own = BalancedBlue (0.17); deployed 0.13 -> cost 0.04
  expect_equal(ps$sharingCost[ps$sector == "Diffuse"], 0.04, tolerance = 1e-9)
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
