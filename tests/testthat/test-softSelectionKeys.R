# nolint start
# ADR 0048: the within-band soft keys of the maximin order (softVifGate, inferenceTGate) are
# declared group settings - config.yml `sweep:` -> the sweep -> manifest `sweepOptions` -> the
# selection bootstrap - so a group records the rule it was selected under, and v5 keeps its own.

test_that("the soft keys parse from config: numbers, \"off\" and YAML's unquoted off", {
  n <- pfm:::.pfmSweepOptionsNormalise
  expect_identical(n(list(softVifGate = "off", inferenceTGate = "off")),
                   list(softVifGate = Inf, inferenceTGate = 0))
  # unquoted `off` reaches R as FALSE: still off, never a gate at 0 that flags every spec
  expect_identical(n(list(softVifGate = FALSE, inferenceTGate = FALSE)),
                   list(softVifGate = Inf, inferenceTGate = 0))
  expect_identical(n(list(softVifGate = 6L, inferenceTGate = "2.33")),
                   list(softVifGate = 6, inferenceTGate = 2.33))
  expect_error(n(list(softVifGate = "lots")), "one number")
  expect_error(n(list(inferenceTGate = c(1, 2))), "one number")
  expect_error(n(list(softVIFGate = 6)), "unknown key")

  dir <- withr::local_tempdir(); cfg <- file.path(dir, "config.yml")
  writeLines(c("sweep:", "  softVifGate: \"off\"", "  inferenceTGate: off"), cfg)
  rc <- pfmResolveConfig(cfg, group = "v6", verbose = FALSE)
  expect_identical(rc$sweep$softVifGate, Inf)
  expect_identical(rc$sweep$inferenceTGate, 0)
  expect_match(pfm:::.pfmSweepOptionsLabel(rc$sweep), "soft keys: VIF off, |t| off", fixed = TRUE)
  expect_match(pfm:::.pfmSweepOptionsLabel(list()), "VIF > 6, |t| < 2.33", fixed = TRUE)
})

test_that("the manifest records the soft keys, \"off\" for a disabled one, and reads them back", {
  gd <- withr::local_tempdir()
  pfm:::.writeRunGroupManifest(gd, group = "v6", mode = NULL,
                               sweepOptions = list(apTransforms = list("linear"), dropCompositeAP = TRUE,
                                                   apExtrapolationGate = 0.275, apExtrapolationSd = 1,
                                                   apExtrapolationWindow = list(2025, 2100),
                                                   softVifGate = Inf, inferenceTGate = 0))
  raw <- jsonlite::fromJSON(file.path(gd, "manifest.json"))$sweepOptions
  expect_identical(raw$softVifGate, "off")       # JSON has no Inf
  expect_identical(raw$inferenceTGate, "off")
  o <- pfm:::.pfmSweepOptionsForGroup(gd)
  expect_identical(o$softVifGate, Inf)
  expect_identical(o$inferenceTGate, 0)

  # ... and a numeric key survives as a number
  pfm:::.writeRunGroupManifest(gd, group = "v6", mode = NULL,
                               sweepOptions = list(softVifGate = 6, inferenceTGate = 2.33))
  o <- pfm:::.pfmSweepOptionsForGroup(gd)
  expect_identical(c(o$softVifGate, o$inferenceTGate), c(6, 2.33))
})

test_that("a key the record lacks comes from config; the record's own keys do not move; v5 stays", {
  dir <- withr::local_tempdir()
  configSweep <- list(apTransforms = "linear", softVifGate = Inf, inferenceTGate = 0)

  # v6 / v6-annual today: swept with a record of the actor-power keys only, before ADR 0048
  gd <- file.path(dir, "v6"); dir.create(gd)
  pfm:::.writeRunGroupManifest(gd, group = "v6", mode = NULL,
                               sweepOptions = list(apTransforms = list("linear", "saturating"),
                                                   dropCompositeAP = TRUE, apExtrapolationGate = 0.275,
                                                   apExtrapolationSd = 1,
                                                   apExtrapolationWindow = list(2025, 2100)))
  o <- pfm:::.pfmSweepOptionsForGroup(gd, configSweep)
  expect_identical(o$softVifGate, Inf)                          # filled from config
  expect_identical(o$inferenceTGate, 0)
  expect_identical(o$apTransforms, c("linear", "saturating"))   # the record wins where it speaks
  expect_match(attr(o, "source"), "manifest + config (softVifGate, inferenceTGate)", fixed = TRUE)

  # once the re-sweep records them, config can no longer move them
  pfm:::.writeRunGroupManifest(gd, group = "v6", mode = NULL,
                               sweepOptions = c(unclass(o)[c("apTransforms", "dropCompositeAP")],
                                                list(softVifGate = Inf, inferenceTGate = 0)))
  o2 <- pfm:::.pfmSweepOptionsForGroup(gd, list(softVifGate = 6, inferenceTGate = 2.33))
  expect_identical(c(o2$softVifGate, o2$inferenceTGate), c(Inf, 0))
  expect_identical(attr(o2, "source"), "manifest")

  # v5: swept before any record - its defaults (6 / 2.33), whatever config declares
  gd5 <- file.path(dir, "v5"); dir.create(gd5)
  jsonlite::write_json(list(group = "v5", panel_hash = "abc"), file.path(gd5, "manifest.json"), auto_unbox = TRUE)
  expect_length(pfm:::.pfmSweepOptionsForGroup(gd5, configSweep), 0)
})

test_that("with both keys off, the band is ordered by trend reliance, not by the VIF / |t| flags", {
  # Two theory-equivalent Green specs. A: VIF 8 and a significant term at |t| 2.0 (both flags),
  # low trend share. B: clean on both flags, high trend share. The v6 situation in miniature:
  # X-1791 satInc (A-like) against X-1950 (B-like).
  row <- function(model, sector, vif, t, trend, dr2) data.frame(
    model = model, sector = sector, sigActorPower = 1L, sigInstQual = 1L, sigInteractions = 1L,
    deltaR2Theory = dr2, maxVIF = vif, converged = TRUE, bic = 100, trendShare = trend,
    minSigTheoryT = t, nControl = 1L, sigControl = 1L, stringsAsFactors = FALSE)
  df <- rbind(row("A", "Bulk", 8, 2.0, 0.70, 0.128), row("A", "Diffuse", 7.8, 2.2, 0.58, 0.172),
              row("B", "Bulk", 3.8, 2.4, 0.85, 0.113), row("B", "Diffuse", 3.7, 2.6, 0.62, 0.164))
  # runPFMSweep's own settings: computeMaximinScore's default trend gate is 0.5, the sweep's 0.9
  score <- function(...) computeMaximinScore(df, rankBy = "worseDeltaR2", tierGate = "Green",
                                             nearTieEps = 0.025, trendDominanceGate = 0.9, ...)$model[1]
  expect_identical(score(softVifGate = 6, inferenceTGate = 2.33), "B")   # old rule: the flags decide
  expect_identical(score(softVifGate = Inf, inferenceTGate = 0), "A")    # ADR 0048: trend reliance
  # the hard VIF gate still applies with the soft key off
  df$maxVIF[df$model == "A" & df$sector == "Bulk"] <- 12
  expect_identical(score(softVifGate = Inf, inferenceTGate = 0), "B")
})

test_that("the keys reach both the sweep and the bootstrap, and the bootstrap reads the record", {
  # pfmRun forwards the group's sweep options; runModelGroup filters dots by each step's formals,
  # so a key missing from a formals list would be dropped without a word.
  for (f in list(runPFMSweep, runPFMSelectionBootstrap)) {
    expect_true(all(c("softVifGate", "inferenceTGate") %in% names(formals(f))))
  }
  expect_identical(pfm:::.pfmSweepOptionKeys[6:7], c("softVifGate", "inferenceTGate"))
  expect_match(paste(deparse(pfmRun), collapse = "\n"), "sweepKeys <- .pfmSweepOptionKeys", fixed = TRUE)
  boot <- paste(deparse(runPFMSelectionBootstrap), collapse = "\n")
  expect_match(boot, "missing(softVifGate) && !is.null(rec$softVifGate)", fixed = TRUE)
  expect_match(boot, "missing(inferenceTGate) && !is.null(rec$inferenceTGate)", fixed = TRUE)
  # the sweep records what it ran with
  sw <- paste(deparse(runPFMSweep), collapse = "\n")
  expect_match(sw, "softVifGate = softVifGate %||% Inf", fixed = TRUE)
  expect_match(sw, "inferenceTGate = inferenceTGate %||%", fixed = TRUE)
})
# nolint end
