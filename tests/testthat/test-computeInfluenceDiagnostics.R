# nolint start
test_that("computeInfluenceDiagnostics returns the full LOCO path for a satP fit", {
  fit <- psmFit(makePSMagpie())
  inf <- computeInfluenceDiagnostics(fit, terms = psmTheoryTerms, wcb = FALSE, verbose = FALSE)

  expect_named(inf, c("byTerm", "path", "alpha", "clusters", "nClusters"))
  expect_setequal(inf$byTerm$term, psmTheoryTerms)
  # one row per (term x cluster)
  expect_equal(nrow(inf$path), inf$nClusters * length(psmTheoryTerms))
  expect_true(all(inf$path$crossing %in% c("gain", "loss", "none", "fold-failed")))
  ok <- inf$path$crossing != "fold-failed"
  expect_true(all(is.finite(inf$path$dBeta[ok])))
  expect_true(all(is.finite(inf$path$seRatio[ok])))
  # full-sample stats present and consistent
  expect_true(all(is.finite(inf$byTerm$p)))
  expect_true(all(inf$byTerm$pMin <= inf$byTerm$pMax))
  # wcb = FALSE: no targeted bootstrap p anywhere
  expect_true(all(is.na(inf$path$pWild)))
})

test_that("a planted influential cluster is flagged pivotal with targeted WCB", {
  set.seed(7)
  G <- 10; Tn <- 20
  d <- expand.grid(region = paste0("C", sprintf("%02d", 1:G)), year = seq_len(Tn),
                   stringsAsFactors = FALSE)
  d$x <- stats::rnorm(nrow(d))
  d$ecp <- 0.5 + 0.30 * d$x + stats::rnorm(nrow(d), sd = 1.0)
  sab <- d$region == "C10"                       # C10 reverses the slope hard
  d$ecp[sab] <- 0.5 - 2.0 * d$x[sab] + stats::rnorm(sum(sab), sd = 0.3)
  fit <- list(formula = "ecp ~ x", data = d)

  inf <- computeInfluenceDiagnostics(fit, terms = "x", wcb = TRUE, wcbB = 199,
                                     verbose = FALSE)
  bt <- inf$byTerm[inf$byTerm$term == "x", ]
  # full sample: the sabotaged cluster wrecks significance ...
  expect_gte(bt$p, 0.05)
  # ... dropping C10 restores it: a "gain" crossing, C10 pivotal and most influential
  c10 <- inf$path[inf$path$term == "x" & inf$path$cluster == "C10", ]
  expect_identical(c10$crossing, "gain")
  expect_match(bt$pivotalClusters, "C10")
  expect_identical(bt$topInfluencer, "C10")
  expect_gte(bt$nPivotal, 1)
  # targeted WCB ran at the pivotal fold (and only at pivotal folds)
  expect_true(is.finite(c10$pWild))
  expect_true(all(is.na(inf$path$pWild[inf$path$crossing == "none"])))
})

test_that("a fold that empties a region-FE level still fits (empty-level gotcha)", {
  set.seed(8)
  G <- 9; Tn <- 15
  d <- expand.grid(region = paste0("C", seq_len(G)), year = seq_len(Tn),
                   stringsAsFactors = FALSE)
  d$x <- stats::rnorm(nrow(d))
  d$ecp <- 1 + 0.5 * d$x + stats::rnorm(nrow(d), sd = 0.5)
  # C9 is ALONE in its FE block: dropping it empties the level
  d$regionFE <- factor(ifelse(d$region %in% paste0("C", 1:4), "B1",
                              ifelse(d$region %in% paste0("C", 5:8), "B2", "B3")))
  fit <- list(formula = "ecp ~ x + regionFE", data = d)

  inf <- computeInfluenceDiagnostics(fit, terms = "x", wcb = FALSE, verbose = FALSE)
  c9 <- inf$path[inf$path$term == "x" & inf$path$cluster == "C9", ]
  expect_false(identical(c9$crossing, "fold-failed"))
  expect_true(is.finite(c9$estimate))
})
# nolint end
