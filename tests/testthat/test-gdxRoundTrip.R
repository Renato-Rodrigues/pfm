# The PFM <-> REMIND gdx round trip.
#
# This is the interface that has never executed end to end (TODO blocker 2). GAMS
# cannot run here, so these tests exercise the half we own and pin the CONTRACT the
# GAMS side depends on: symbol names, index sets, and the guarantee that a failure
# writes nothing. A mismatch here is silent in production - Execute_Loadpoint simply
# leaves the parameter untouched, and REMIND carries on with the previous iteration's
# phi as if the coupling had succeeded.

skip_if_no_gdx <- function() {
  skip_if_not_installed("gdx")
  skip_if_not_installed("magclass")
}

# Symbols MUST be tagged with a gdxdata attribute or writeGDX dies with "argument is
# of length zero" - see .psmGdxParam(). Building them the same way production does is
# the point of these tests.
mkPhi <- function(regs = c("EUR", "USA", "CHA"), v = c(0.7, 0.45, 0.9)) {
  pfm:::.psmGdxParam(
    magclass::new.magpie(regs, NULL, "p45_regiDiff_phi", fill = v),
    "p45_regiDiff_phi")
}

test_that("phi survives a write/read cycle with its symbol name and regions intact", {
  skip_if_no_gdx()
  f <- withr::local_tempfile(fileext = ".gdx")
  phi <- mkPhi()
  gdx::writeGDX(list(p45_regiDiff_phi = phi), f)
  expect_true(file.exists(f))
  back <- gdx::readGDX(f, "p45_regiDiff_phi")
  # The symbol name is the contract: presolve.gms does
  #   Execute_Loadpoint 'p45_regiDiff_phi' p45_regiDiff_phi_aux = p45_regiDiff_phi;
  # A renamed symbol loads NOTHING and leaves the previous phi in place silently.
  # The gdx round trip REORDERS regions alphabetically, so compare BY NAME. This is
  # harmless in GAMS - Execute_Loadpoint maps by set element, not by position - but
  # any R-side consumer that indexed positionally would silently swap regions.
  expect_setequal(magclass::getItems(back, dim = 1), magclass::getItems(phi, dim = 1))
  for (r in magclass::getItems(phi, dim = 1)) {
    expect_equal(as.numeric(back[r, , ]), as.numeric(phi[r, , ]), tolerance = 1e-12,
                 info = r)
  }
})

test_that("all three symbols the coupling needs coexist in one gdx", {
  skip_if_no_gdx()
  f <- withr::local_tempfile(fileext = ".gdx")
  regs <- c("EUR", "USA")
  phi <- mkPhi(regs, c(0.7, 0.45))
  delta <- pfm:::.psmGdxParam(
    magclass::new.magpie(regs, NULL, "p45_pfmDelta", fill = 0.004), "p45_pfmDelta")
  bound <- pfm:::.psmGdxParam(
    magclass::new.magpie(regs, c(2030, 2050), "p45_pfmPriceBound",
                         fill = c(50, 60, 120, 140)), "p45_pfmPriceBound")
  gdx::writeGDX(list(p45_regiDiff_phi = phi, p45_pfmDelta = delta,
                     p45_pfmPriceBound = bound), f)
  for (nm in c("p45_regiDiff_phi", "p45_pfmDelta", "p45_pfmPriceBound")) {
    expect_true(is.null(attr(try(gdx::readGDX(f, nm), silent = TRUE), "condition")),
                info = nm)
  }
  expect_equal(dim(gdx::readGDX(f, "p45_pfmPriceBound"))[2], 2L)
})

test_that("the delta is written over regions, not GLO", {
  skip_if_no_gdx()
  # GAMS loads p45_pfmDelta into a parameter indexed on regi and sums over it. A
  # GLO-only symbol sums to nothing there, reading as delta = 0 - FALSE CONVERGENCE
  # on the first call, collapsing the whole coupling to a single PFM evaluation.
  f <- withr::local_tempfile(fileext = ".gdx")
  regs <- c("EUR", "USA")
  delta <- pfm:::.psmGdxParam(
    magclass::new.magpie(regs, NULL, "p45_pfmDelta", fill = 0.01), "p45_pfmDelta")
  gdx::writeGDX(list(p45_pfmDelta = delta), f)
  back <- gdx::readGDX(f, "p45_pfmDelta")
  expect_setequal(magclass::getItems(back, dim = 1), regs)
  expect_false("GLO" %in% magclass::getItems(back, dim = 1))
  # The GAMS side computes sum(regi, aux)/card(regi); constant fill must survive it.
  expect_equal(mean(as.numeric(back)), 0.01, tolerance = 1e-12)
})

test_that("a failed coupling writes NO gdx, so REMIND keeps the previous phi", {
  d <- withr::local_tempdir()
  outFile <- file.path(d, "p45_regiDiff_phi.gdx")
  gdxFile <- file.path(d, "fulldata.gdx")
  file.create(gdxFile)
  gd <- file.path(d, "grp"); dir.create(gd, recursive = TRUE)
  writeLines("[]", file.path(gd, "selected-models-psm.yml"))
  writeLines('{"panel_hash":"nope"}', file.path(gd, "manifest.json"))
  expect_warning(
    iterativePFM(gdx = gdxFile, group = "grp", resultsDir = d, modelDir = d,
                 outputFile = outFile), "FAILED")
  # This is the property presolve.gms relies on: "if the R side failed to produce the
  # file the previous iteration's phi is retained rather than silently reverting to 1".
  expect_false(file.exists(outFile))
})

test_that("the phi history accumulates across calls and drives the delta", {
  d <- withr::local_tempdir()
  hist <- file.path(d, "pfm-phi-history.rds")
  # Mirrors the in-function logic; the point is that state persists BETWEEN GAMS
  # iterations through a file, since each Rscript call is a fresh process.
  step <- function(phi) {
    prev <- if (file.exists(hist)) readRDS(hist) else list()
    delta <- if (length(prev)) {
      last <- prev[[length(prev)]]$phi
      cm <- intersect(names(phi), names(last))
      if (length(cm)) max(abs(phi[cm] - last[cm])) else Inf
    } else Inf
    prev[[length(prev) + 1L]] <- list(phi = phi, delta = delta)
    saveRDS(prev, hist)
    delta
  }
  expect_equal(step(c(EUR = 0.7, USA = 0.5)), Inf)
  expect_equal(step(c(EUR = 0.6, USA = 0.5)), 0.1, tolerance = 1e-12)
  expect_equal(step(c(EUR = 0.6, USA = 0.5)), 0)
  expect_length(readRDS(hist), 3L)
})

test_that("an untagged magpie is exactly what breaks writeGDX", {
  skip_if_no_gdx()
  # Regression guard for the root cause. gdx::writeGDX reads its symbol metadata from
  # attr(x, "gdxdata"), which readGDX sets but new.magpie does not. Without it the
  # writer dereferences out$type on a NULL. The error names nothing about the real
  # cause, and the failure only appears OUTSIDE devtools::load_all - i.e. exactly in
  # the bare Rscript that GAMS invokes.
  f <- withr::local_tempfile(fileext = ".gdx")
  raw <- magclass::new.magpie(c("EUR", "USA"), NULL, "probe", fill = c(1, 2))
  magclass::getSets(raw)[1] <- "all_regi"
  expect_null(attr(raw, "gdxdata"))
  expect_error(gdx::writeGDX(list(probe = raw), f), "length zero")
  # Tagged, the same object writes cleanly.
  expect_silent(gdx::writeGDX(list(probe = pfm:::.psmGdxParam(raw, "probe")), f))
  expect_true(file.exists(f))
})
