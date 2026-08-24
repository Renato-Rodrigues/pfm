# nolint start
# Per-capita actor-power variants (design-notes/0001). These lock in the contract the
# sweep relies on: the shares are untouched, the per-capita variants are ADDED, and a
# specification selects between them purely by naming drivers.

apDrivers <- c("Coal primary energy share", "Oil/Gas primary energy share",
               "Fossil share in Industry", "VRE share", "Electrification",
               "Biofuel Displacement")

makeDrivers <- function(regions = c("AAA", "BBB"), years = c(2010, 2020)) {
  m <- magclass::new.magpie(regions, years, apDrivers, fill = 0.4)
  m
}

test_that("without energyPerCapita the output is exactly as before", {
  out <- actorPowerIndex(makeDrivers())
  expect_setequal(magclass::getNames(out),
                  c("Actor Power Index|Bulk", "Actor Power Index|Diffuse",
                    "Innovator Power|Bulk", "Innovator Power|Diffuse",
                    "Incumbent Power|Bulk", "Incumbent Power|Diffuse"))
  expect_false(any(grepl(" pc\\|", magclass::getNames(out))))
})

test_that("per-capita variants are share x energy-per-capita, and shares are unchanged", {
  d <- makeDrivers()
  epc <- magclass::new.magpie(c("AAA", "BBB"), c(2010, 2020), NULL, fill = 1)
  epc["AAA", , ] <- 2     # AAA uses twice the energy per person
  epc["BBB", , ] <- 0.5

  base <- actorPowerIndex(d)
  out  <- actorPowerIndex(d, energyPerCapita = epc)

  # shares survive bit-identical - the variants are additive, never a replacement
  for (v in magclass::getNames(base)) {
    expect_equal(as.numeric(out[, , v]), as.numeric(base[, , v]))
  }
  for (s in c("Bulk", "Diffuse")) {
    for (a in c("Innovator", "Incumbent")) {
      share <- as.numeric(base[, , paste0(a, " Power|", s)])
      pc    <- as.numeric(out[, , paste0(a, " Power pc|", s)])
      expect_equal(pc, share * as.numeric(epc), tolerance = 1e-12)
    }
  }
  # ... and the per-capita index ORDERS countries differently from the share, which
  # is the whole point: identical mix, different absolute weight in the polity.
  expect_equal(as.numeric(base["AAA", 2020, "Incumbent Power|Bulk"]),
               as.numeric(base["BBB", 2020, "Incumbent Power|Bulk"]))
  expect_gt(as.numeric(out["AAA", 2020, "Incumbent Power pc|Bulk"]),
            as.numeric(out["BBB", 2020, "Incumbent Power pc|Bulk"]))
})

test_that(".energyPerCapita divides raw energy by raw population and refuses nonsense", {
  iam <- magclass::new.magpie(c("AAA", "BBB"), c(2010, 2020), "petotal", fill = 100)
  pop <- magclass::new.magpie(c("AAA", "BBB"), c(2010, 2020), NULL, fill = 10)
  epc <- pfm:::.energyPerCapita(iam, pop)
  expect_equal(unique(as.numeric(epc)), 10)

  # zero population must not leak Inf into the standardisation of every other driver
  pop0 <- pop; pop0["AAA", , ] <- 0
  expect_true(all(is.na(as.numeric(pfm:::.energyPerCapita(iam, pop0)["AAA", , ]))))

  # missing petotal, or no shared support, degrades to NULL rather than erroring
  expect_null(pfm:::.energyPerCapita(magclass::new.magpie("AAA", 2010, "other"), pop))
  expect_null(pfm:::.energyPerCapita(iam, magclass::new.magpie("ZZZ", 2010, NULL, fill = 1)))
  expect_null(pfm:::.energyPerCapita(NULL, pop))
})

test_that("the sweep grid carries all four actor-power forms and can be restricted", {
  full <- pfm:::channelSpecs("exhaustive")
  nm <- vapply(full, `[[`, character(1), "name")
  for (k in c("compAP", "splitAP", "splitAPpc", "mixedAP", "bothIncAP")) {
    expect_gt(sum(grepl(paste0(" ", k, " "), nm)), 0)
  }
  # mixedAP is the design-note recommendation: innovator share, incumbent per capita
  i <- which(grepl(" mixedAP ", nm))[1]
  expect_equal(full[[i]]$actorPowerIndex, c("Innovator Power", "Incumbent Power pc"))
  # bothIncAP nests splitAP and mixedAP: it is the test of whether the share still
  # carries information once the per-capita level is present (design-notes/0001).
  j <- which(grepl(" bothIncAP ", nm))[1]
  expect_equal(full[[j]]$actorPowerIndex,
               c("Innovator Power", "Incumbent Power", "Incumbent Power pc"))

  # Numbering stability (the invariant test-psmImprovements.R pins): the per-capita
  # forms are APPENDED, so every pre-existing X-number is untouched.
  base <- pfm:::channelSpecs("exhaustive", apPcForms = character(0))
  baseNm <- vapply(base, `[[`, character(1), "name")
  expect_false(any(grepl("splitAPpc|mixedAP|bothIncAP", baseNm)))
  # Nothing in the base grid is renamed or renumbered by the addition ...
  expect_true(all(baseNm %in% nm))
  # ... and the RAW specs keep their positions, i.e. the new block is appended, not
  # interleaved. Compared on the raw specs only because the satP twins (ADR 0028) are
  # all appended after every raw spec, so the base grid is not a literal prefix of the
  # full one even though no X-number moves.
  # Compared on the X-numbered raw specs: the satP twins (ADR 0028) are all appended
  # after every raw spec, and the LAG- specs carry no X-number, so neither belongs in
  # a positional check even though no X-number moves.
  xRaw <- function(x) grep("^X-", x[!grepl(" | satP", x, fixed = TRUE)], value = TRUE)
  expect_identical(xRaw(baseNm), head(xRaw(nm), length(xRaw(baseNm))))
  # 3 forms x 15 IQ x 8 controls x 3 FE = 360 each, then each is satP-twinned
  expect_equal(length(full) - length(base), 3 * 15 * 8 * 3 * 2)

  expect_error(pfm:::channelSpecs("exhaustive", apPcForms = "nonsenseAP"), "unknown apPcForms")
})
# nolint end
