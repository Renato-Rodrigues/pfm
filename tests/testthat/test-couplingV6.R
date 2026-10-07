# The v6 coupling path (design note 0005 Phase 3; ADR 0049, 0050, 0054): typed options, the
# formulation switch, the share-path delta and damping, the time-indexed symbols, and a regression
# against the Phase 1 offline numbers on the real v6 artifacts (workstation only).

test_that("the v6 options have the headline defaults, merge in order and validate", {
  d <- pfmV6CouplingDefaults()
  expect_identical(d$holdYear, 2100); expect_identical(d$hold, "logit"); expect_identical(d$spread, "k")
  expect_identical(d$ordering, "model"); expect_identical(d$kappa, 0); expect_identical(d$strength, "common")
  o <- pfm:::.pfmV6Options(list(holdYear = 2060, kappa = 0.01), list(kappa = "0.02", ordering = "uniform"))
  expect_identical(o$holdYear, 2060); expect_identical(o$kappa, 0.02); expect_identical(o$ordering, "uniform")
  expect_error(pfm:::.pfmV6Options(list(kappa = 1)), "kappa")
  expect_error(pfm:::.pfmV6Options(list(hold = "level")))
  expect_error(pfm:::.pfmV6Options(list(strength = "regional", ordering = "permuted")), "not combined")
  y <- pfm:::.pfmV6OptionsFromYaml(list(phiHoldYear = 2060, phiOrdering = "reversed", theta = 0.5, other = 1))
  expect_identical(y, list(holdYear = 2060, ordering = "reversed"))
  expect_null(pfm:::.pfmV6OptionsFromYaml(list(theta = 0.5)))
  expect_identical(pfm:::.pfmV6Checkpoints(2100), c(2035, 2050, 2070, 2100))
  expect_identical(pfm:::.pfmV6Checkpoints(2060), c(2035, 2050, 2060))
})

test_that("auto couples a group with an anchor artifact by v6 and any other by v5", {
  gd <- withr::local_tempdir()
  expect_identical(pfm:::.pfmCouplingFormulation(gd, "auto"), "v5-tier")
  expect_error(pfm:::.pfmCouplingFormulation(gd, "v6-anchor"), "phi-anchor.rds")
  saveRDS(list(), file.path(gd, "phi-anchor.rds"))
  expect_identical(pfm:::.pfmCouplingFormulation(gd, "auto"), "v6-anchor")
  expect_identical(pfm:::.pfmCouplingFormulation(gd, "v5-tier"), "v5-tier")
})

test_that("the delivery mapping selects the anchor's resolution, and an unknown one stops", {
  art <- list(byResolution = list(EU21 = list(mapping = "regionmapping_21_EU11.csv"),
                                  H12 = list(mapping = "regionmappingH12.csv")))
  expect_identical(pfm:::.pfmAnchorResolution(art, "regionmappingH12.csv"), "H12")
  expect_identical(pfm:::.pfmAnchorResolution(art, "some/dir/regionmapping_21_EU11.csv"), "EU21")
  expect_error(pfm:::.pfmAnchorResolution(art, "regionmapping_54.csv"), "no resolution")
  expect_error(pfm:::.pfmAnchorResolution(art, data.frame()), "file name")
})

test_that("the path delta reads the checkpoint years only, from t0, and is Inf on the first call", {
  mk <- function(v) data.frame(sector = "Bulk", region = rep(c("A", "B"), each = 3),
                               year = rep(c(2020, 2035, 2050), 2), phi = v)
  a <- mk(c(0.9, 0.5, 0.6, 0.9, 0.7, 0.7)); b <- mk(c(0.1, 0.5, 0.65, 0.9, 0.7, 0.7))
  expect_identical(pfm:::.pfmPhiPathDelta(a, NULL), Inf)
  expect_equal(pfm:::.pfmPhiPathDelta(a, b, years = c(2035, 2050)), 0.05)
  expect_equal(pfm:::.pfmPhiPathDelta(a, b, years = NULL, t0 = 2025), 0.05)   # 2020 is before t0
  expect_equal(pfm:::.pfmPhiPathDelta(a, b, years = 2035), 0)
})

test_that("damping acts only when the path oscillates, and then moves it halfway", {
  mk <- function(v) data.frame(sector = "Bulk", region = c("A", "B", "C"), year = 2050, phi = v)
  p2 <- mk(c(0.5, 0.6, 0.7)); p1 <- mk(c(0.6, 0.7, 0.8)); up <- mk(c(0.7, 0.8, 0.9)); back <- mk(c(0.5, 0.62, 0.71))
  calm <- pfm:::.pfmV6Damp(up, p1, p2, 2050)
  expect_false(calm$oscillating); expect_identical(calm$alpha, 1); expect_identical(calm$shares, up)
  osc <- pfm:::.pfmV6Damp(back, p1, p2, 2050)
  expect_true(osc$oscillating); expect_identical(osc$alpha, 0.5)
  expect_equal(osc$shares$phi, (p1$phi + back$phi) / 2)
  expect_false(pfm:::.pfmV6Damp(back, p1, NULL, 2050)$oscillating)   # needs two previous calls
  # a uniform back-and-forth shift is an oscillation too (the cosine reads it, a correlation could not)
  u2 <- mk(c(0.5, 0.6, 0.7)); u1 <- mk(c(0.6, 0.7, 0.8)); u0 <- mk(c(0.52, 0.62, 0.72))
  expect_true(pfm:::.pfmV6Damp(u0, u1, u2, 2050)$oscillating)
})

test_that("iterativePFM carries the v6 switch, refuses mode 3 and the v5 override under it, and exports the paths", {
  expect_identical(formals(iterativePFM)$formulation, quote(getOption("pfm.couplingFormulation", "auto")))
  src <- paste(deparse(iterativePFM), collapse = " ")
  for (frag in c("p45_pfmPhiPath", "p45_pfmPhiMktPath", "is retired from the",
                 "phi-override.yml is a v5 instrument", "SSP mismatch", "options(pfm.panel = panelDef")) {
    expect_true(grepl(frag, src, fixed = TRUE), info = frag)
  }
})

test_that("the share-path symbols are written ttot first, with real domains", {
  skip_if_not_installed("gamstransfer")
  f <- withr::local_tempfile(fileext = ".gdx")
  fl <- data.frame(region = c("CHA", "CHA", "IND"), year = c(2025, 2050, 2050), phi = c(0.5, 0.54, 0.69))
  mk <- list(Bulk = fl, Diffuse = transform(fl, phi = phi + 0.1))
  syms <- list(pfm:::.pfmCouplingSym2d("p45_pfmPhiPath", fl, "phi"),
               pfm:::.pfmCouplingSymMkt2d("p45_pfmPhiMktPath", mk, "phi"))
  pfm:::.pfmWriteCouplingGdx(f, syms)
  back <- gamstransfer::Container$new(f)
  p <- back$getSymbols("p45_pfmPhiPath")[[1]]
  expect_identical(as.character(p$domainNames), c("ttot", "all_regi"))
  m <- back$getSymbols("p45_pfmPhiMktPath")[[1]]
  expect_identical(as.character(m$domainNames), c("ttot", "all_regi", "all_emiMkt"))
  r <- m$records
  expect_equal(r$value[r$ttot == "2050" & r$all_regi == "IND" & r$all_emiMkt == "ES"], 0.79)
  expect_setequal(unique(as.character(r$all_emiMkt)), c("ETS", "ES", "other"))
})

test_that("on the real v6 anchor, pfmV6Shares reproduces Phase 1 and its options act as declared", {
  root <- normalizePath(file.path(testthat::test_path(), "..", "..", "..", ".."), mustWork = FALSE)
  exp <- file.path(root, "output", "remind-inputs", "v6")
  scenF <- file.path(root, "output", "pfm", "v6", "phase1", "scen-PkBudg1000.rds")
  skip_if_not(file.exists(file.path(exp, "phi-anchor.rds")) && file.exists(scenF), "v6 artifacts not present (workstation only)")
  anc <- pfmAnchorFor(exp, "EU21"); scen <- readRDS(scenF)
  base <- pfmV6Shares(anc, scen, theta = 0.5)
  k <- base$strength
  expect_equal(k$k[k$sector == "Bulk" & k$year == 2050], 0.6287, tolerance = 1e-3)
  expect_equal(k$k[k$sector == "Bulk" & k$year == 2100], 0.4809, tolerance = 1e-3)
  expect_equal(min(base$floor$phi[base$floor$year == 2025]), 0.5, tolerance = 1e-9)   # phi(t0) = 1 - theta u, max u = 1
  # the same weights passed explicitly change nothing (the SSP / weight-year path)
  same <- pfmV6Shares(anc, scen, theta = 0.5, weights = anc$weights)
  expect_equal(same$shares$phi, base$shares$phi)
  # kappa closes the strength geometrically from t0
  kap <- pfmV6Shares(anc, scen, theta = 0.5, options = list(kappa = 0.02))
  r <- kap$strength$k / base$strength$k
  expect_equal(r[kap$strength$year == 2050], rep(0.98^25, 2), tolerance = 1e-9)
  # a uniform ordering gives one share per sector and year
  uni <- pfmV6Shares(anc, scen, theta = 0.5, options = list(ordering = "uniform"))
  expect_true(all(tapply(uni$shares$phi, paste(uni$shares$sector, uni$shares$year), function(x) diff(range(x))) < 1e-12))
  # the regional arm: phi(t0) as the common arm, each region on its own strength afterwards
  reg <- pfmV6Shares(anc, scen, theta = 0.5, options = list(strength = "regional"))
  m <- merge(reg$shares[reg$shares$year == 2025, c("sector", "region", "phi")],
             base$shares[base$shares$year == 2025, c("sector", "region", "phi")], by = c("sector", "region"))
  expect_equal(m$phi.x, m$phi.y)
  expect_true(all(reg$shares$phi >= 0 & reg$shares$phi <= 1))
  # hold 2060: k flat after 2060
  h60 <- pfmV6Shares(anc, scen, theta = 0.5, options = list(holdYear = 2060))$strength
  b <- h60[h60$sector == "Bulk", ]
  expect_equal(b$k[b$year == 2100], b$k[b$year == 2060])
})

test_that("a v6 call told cm_pfmPhiPath = 0 stops, and preparePFM writes the v6 options", {
  src <- paste(deparse(iterativePFM), collapse = " ")
  expect_true(grepl("rtPhiPath == 0", src, fixed = TRUE))
  pp <- normalizePath(file.path(testthat::test_path(), "..", "..", "..", "remind_pfm", "scripts", "start", "preparePFM.R"),
                      mustWork = FALSE)
  skip_if_not(file.exists(pp), "the remind_pfm fork is not beside pfm")
  txt <- paste(readLines(pp, warn = FALSE), collapse = "\n")
  for (k in c("pfmFormulation", "pfmPhiHoldYear", "pfmPhiOrdering", "pfmPhiKappa", "pfmPhiStrength", "phi-anchor.rds")) {
    expect_true(grepl(k, txt, fixed = TRUE), info = k)
  }
})

test_that("the exported path covers every GAMS period: t0 value before, last value held after", {
  d <- data.frame(region = rep(c("A", "B"), each = 3), year = rep(c(2025, 2030, 2050), 2),
                  phi = c(0.5, 0.6, 0.7, 0.8, 0.85, 0.9))
  p <- pfm:::.pfmCompletePath(d, c(1900, 2005, 2025, 2030, 2040, 2050, 2150), 2025)
  a <- p[p$region == "A", ]
  expect_identical(a$year, c(1900L, 2005L, 2025L, 2030L, 2040L, 2050L, 2150L))
  expect_equal(a$phi, c(0.5, 0.5, 0.5, 0.6, 0.6, 0.7, 0.7))
  expect_false(any(p$phi == 0))
})
