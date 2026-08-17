# The PFM <-> REMIND gdx interface.
#
# These tests pin the CONTRACT the GAMS side depends on, and the contract is not just
# "the numbers are right" - it is the index structure REMIND declares in
# 45_carbonprice/functionalForm/declarations.gms:
#
#   p45_regiDiff_phi(all_regi)                     rank 1
#   p45_pfmDelta_aux(all_regi)                     rank 1
#   p45_pfmPriceBound(ttot,all_regi)               rank 2, ttot FIRST
#   p45_pfmMPPrice(ttot,all_regi)                  rank 2, ttot FIRST
#   p45_pfmPhiMkt(all_regi,all_emiMkt)             rank 2, all_regi FIRST
#   p45_pfmLambdaMkt(all_regi,all_emiMkt)          rank 2, all_regi FIRST
#   p45_pfmPriceBoundMkt(ttot,all_regi,all_emiMkt) rank 3
#   p45_pfmMPPriceMkt(ttot,all_regi,all_emiMkt)    rank 3
#
# The rank-3 symbols are ADR 0042's market dimension, added 2026-08-17 when the markup
# became symmetric. The ADR originally rejected the extra rank as "that trap one
# dimension up"; these tests are the reason it is now safe to carry. Note the two rank-2
# families have OPPOSITE leading indices - (ttot, all_regi) versus (all_regi, all_emiMkt)
# - which is exactly the confusion the order defect was made of, so both are pinned.
#
# Getting the VALUES right and the SHAPE wrong is the dangerous case, and both variants
# have shipped. Wrong rank: Execute_Loadpoint reports "Dimensions do not match", loads
# nothing, leaves phi at 1 (uncoupled) and kills the run hours later citing an unrelated
# infeasibility. Wrong ORDER: correct rank, no error anywhere, loads as ( ALL 0.000 ).
#
# Assertions go through gamstransfer, which reads the gdx the way GAMS does - rank and
# real domain names. The version of this file that read back through magclass could not
# catch either defect, because magclass rebuilds a magpie from any rank.

skip_if_no_gt <- function() skip_if_not_installed("gamstransfer")

# What GAMS sees, straight out of the file.
symInfo <- function(f, name) {
  m <- gamstransfer::Container$new(f)
  if (!length(m$getSymbols(name))) return(NULL)
  s <- m$getSymbols(name)[[1]]
  r <- s$records
  list(dim = as.integer(s$dimension), domains = as.character(s$domainNames),
       domainType = s$domainType, n = as.integer(s$numberRecords), records = r,
       value = r$value)
}
# Value keyed by the label on each index position - the mapping GAMS will make.
cells <- function(i) {
  key <- do.call(paste, lapply(i$domains, function(d) as.character(i$records[[d]])))
  stats::setNames(i$value, key)
}

mkPhi <- function(regs = c("EUR", "USA", "CHA"), v = c(0.7, 0.45, 0.9)) {
  pfm:::.psmCouplingSym1d("p45_regiDiff_phi", stats::setNames(v, regs))
}
mkBound <- function() {
  d <- expand.grid(year = c(2030, 2050), region = c("EUR", "USA"),
                   stringsAsFactors = FALSE)
  d$priceBound <- c(50, 120, 60, 140)   # EUR: 50/120, USA: 60/140
  d
}
writeSyms <- function(f, ...) pfm:::.psmWriteCouplingGdx(f, list(...))

# --- rank ---------------------------------------------------------------------

test_that("phi is written at rank 1, as all_regi is declared", {
  skip_if_no_gt()
  f <- withr::local_tempfile(fileext = ".gdx")
  writeSyms(f, mkPhi())
  i <- symInfo(f, "p45_regiDiff_phi")
  expect_identical(i$dim, 1L)          # 2 = the magpie shape that failed to load
  expect_identical(i$domains, "all_regi")
  expect_identical(i$n, 3L)
})

test_that("the delta is rank 1, over regions, and not GLO", {
  skip_if_no_gt()
  # GAMS computes sum(regi, aux)/card(regi). A GLO-only symbol sums to nothing there,
  # reading as delta = 0 - FALSE CONVERGENCE on the first call.
  f <- withr::local_tempfile(fileext = ".gdx")
  regs <- c("EUR", "USA")
  writeSyms(f, pfm:::.psmCouplingSym1d("p45_pfmDelta",
                                       stats::setNames(rep(0.01, 2), regs)))
  i <- symInfo(f, "p45_pfmDelta")
  expect_identical(i$dim, 1L)
  expect_setequal(as.character(i$records$all_regi), regs)
  expect_false("GLO" %in% as.character(i$records$all_regi))
  expect_equal(mean(i$value), 0.01, tolerance = 1e-12)
})

test_that("the price bound is written at rank 2", {
  skip_if_no_gt()
  f <- withr::local_tempfile(fileext = ".gdx")
  writeSyms(f, pfm:::.psmCouplingSym2d("p45_pfmPriceBound", mkBound(), "priceBound"))
  i <- symInfo(f, "p45_pfmPriceBound")
  expect_identical(i$dim, 2L)          # 3 = the magpie shape
  expect_identical(i$n, 4L)
})

# --- index order, domains and labels ------------------------------------------

test_that("the 2-d symbols are indexed (ttot, all_regi), year first", {
  skip_if_no_gt()
  f <- withr::local_tempfile(fileext = ".gdx")
  writeSyms(f, pfm:::.psmCouplingSym2d("p45_pfmPriceBound", mkBound(), "priceBound"))
  expect_identical(symInfo(f, "p45_pfmPriceBound")$domains, c("ttot", "all_regi"))
})

test_that("domains are real GAMS sets, not the universe", {
  skip_if_no_gt()
  # "regular" is what makes the write-time domain check possible at all; a domain-less
  # symbol ("none"/*) accepts any record, which is how the order defect got through.
  f <- withr::local_tempfile(fileext = ".gdx")
  writeSyms(f, mkPhi(),
            pfm:::.psmCouplingSym2d("p45_pfmPriceBound", mkBound(), "priceBound"))
  expect_identical(symInfo(f, "p45_pfmPriceBound")$domainType, "regular")
  expect_identical(symInfo(f, "p45_regiDiff_phi")$domainType, "regular")
})

test_that("year labels are bare ttot elements, not magclass 'y2030'", {
  skip_if_no_gt()
  # ttot elements are 2030. "y2030" is not in the set, so every record would be
  # dropped on load and the bound would read as zero everywhere.
  f <- withr::local_tempfile(fileext = ".gdx")
  writeSyms(f, pfm:::.psmCouplingSym2d("p45_pfmPriceBound", mkBound(), "priceBound"))
  u <- unique(as.character(symInfo(f, "p45_pfmPriceBound")$records$ttot))
  expect_true(all(grepl("^[0-9]{4}$", u)))
  expect_false(any(startsWith(u, "y")))
})

test_that("ttot elements are ordered numerically, not as text", {
  skip_if_no_gt()
  # "2100" sorts before "255" as text. REMIND's ttot runs to 2150, so a text sort puts
  # the century boundary in the wrong place.
  f <- withr::local_tempfile(fileext = ".gdx")
  d <- data.frame(region = "EUR", year = c(2100, 2030, 2150, 2055),
                  priceBound = c(4, 1, 5, 2), stringsAsFactors = FALSE)
  writeSyms(f, pfm:::.psmCouplingSym2d("p45_pfmPriceBound", d, "priceBound"))
  m <- gamstransfer::Container$new(f)
  expect_identical(as.character(m$getSymbols("ttot")[[1]]$records[[1]]),
                   c("2030", "2055", "2100", "2150"))
})

# --- values -------------------------------------------------------------------

test_that("values land on the right year-region cell", {
  skip_if_no_gt()
  f <- withr::local_tempfile(fileext = ".gdx")
  d <- mkBound()
  writeSyms(f, pfm:::.psmCouplingSym2d("p45_pfmPriceBound", d, "priceBound"))
  got <- cells(symInfo(f, "p45_pfmPriceBound"))
  want <- stats::setNames(d$priceBound, paste(d$year, d$region))
  expect_equal(got[names(want)], want, tolerance = 1e-12)
})

test_that("shuffled and ragged input still lands correctly", {
  skip_if_no_gt()
  f <- withr::local_tempfile(fileext = ".gdx")
  d <- data.frame(region = c("USA", "EUR", "USA", "EUR"),
                  year = c(2050, 2030, 2030, 2050),
                  priceBound = c(4, 1, 3, 2), stringsAsFactors = FALSE)
  writeSyms(f, pfm:::.psmCouplingSym2d("p45_pfmPriceBound", d, "priceBound"))
  got <- cells(symInfo(f, "p45_pfmPriceBound"))
  expect_equal(unname(got[["2030 EUR"]]), 1)
  expect_equal(unname(got[["2050 EUR"]]), 2)
  expect_equal(unname(got[["2030 USA"]]), 3)
  expect_equal(unname(got[["2050 USA"]]), 4)
})

test_that("phi keeps its values, matched by region name", {
  skip_if_no_gt()
  f <- withr::local_tempfile(fileext = ".gdx")
  regs <- c("EUR", "USA", "CHA"); v <- c(0.7, 0.45, 0.9)
  writeSyms(f, mkPhi(regs, v))
  got <- cells(symInfo(f, "p45_regiDiff_phi"))
  expect_equal(got[regs], stats::setNames(v, regs), tolerance = 1e-12)
})

test_that("all symbols the coupling needs coexist with the right ranks", {
  skip_if_no_gt()
  f <- withr::local_tempfile(fileext = ".gdx")
  d <- mkBound(); names(d)[names(d) == "priceBound"] <- "price"
  writeSyms(f,
            mkPhi(c("EUR", "USA"), c(0.7, 0.45)),
            pfm:::.psmCouplingSym1d("p45_pfmDelta", c(EUR = 0.004, USA = 0.004)),
            pfm:::.psmCouplingSym2d("p45_pfmPriceBound", mkBound(), "priceBound"),
            pfm:::.psmCouplingSym2d("p45_pfmMPPrice", d, "price"))
  expect_identical(symInfo(f, "p45_regiDiff_phi")$dim, 1L)
  expect_identical(symInfo(f, "p45_pfmDelta")$dim, 1L)
  expect_identical(symInfo(f, "p45_pfmPriceBound")$dim, 2L)
  expect_identical(symInfo(f, "p45_pfmMPPrice")$dim, 2L)
  expect_identical(symInfo(f, "p45_pfmMPPrice")$domains, c("ttot", "all_regi"))
})

# --- what GAMS Transfer buys over the predecessor -----------------------------

test_that("GAMS Transfer refuses records that violate the declared domain", {
  skip_if_no_gt()
  # The defect that reached a cluster run, handed straight to the writer: a
  # (ttot, all_regi) parameter given region-first records. gdxrrw::wgdx.lst wrote this
  # happily and GAMS read it as all zeros WITHOUT any error.
  m <- gamstransfer::Container$new()
  sr <- m$addSet("all_regi", records = c("EUR", "USA"))
  st <- m$addSet("ttot", records = c("2030", "2050"))
  m$addParameter("p", domain = list(st, sr),
                 records = data.frame(ttot = c("EUR", "USA"),
                                      all_regi = c("2030", "2050"),
                                      value = c(1, 2), stringsAsFactors = FALSE))
  f <- withr::local_tempfile(fileext = ".gdx")
  expect_error(m$write(f), "[Dd]omain violation")
})

test_that("the post-write check reads the FILE, not the objects that made it", {
  skip_if_no_gt()
  # Cheap insurance that does not depend on the writer's own bookkeeping. Point it at a
  # file whose symbol is indexed the wrong way round and it must object.
  f <- withr::local_tempfile(fileext = ".gdx")
  m <- gamstransfer::Container$new()
  sr <- m$addSet("all_regi", records = c("EUR", "USA"))
  st <- m$addSet("ttot", records = c("2030", "2050"))
  m$addParameter("p45_pfmPriceBound", domain = list(sr, st),   # REVERSED
                 records = data.frame(all_regi = c("EUR", "USA"),
                                      ttot = c("2030", "2050"),
                                      value = c(1, 2), stringsAsFactors = FALSE))
  m$write(f)
  expect_error(
    pfm:::.psmVerifyCouplingGdx(
      f, list(pfm:::.psmCouplingSym2d("p45_pfmPriceBound", mkBound(), "priceBound"))),
    "indexed \\(all_regi, ttot\\)")
})

test_that("the post-write check catches a wrong rank", {
  skip_if_no_gt()
  f <- withr::local_tempfile(fileext = ".gdx")
  m <- gamstransfer::Container$new()
  sr <- m$addSet("all_regi", records = c("EUR", "USA"))
  st <- m$addSet("ttot", records = "2030")
  # phi at rank 2: exactly the magpie shape that shipped.
  m$addParameter("p45_regiDiff_phi", domain = list(sr, st),
                 records = data.frame(all_regi = c("EUR", "USA"),
                                      ttot = c("2030", "2030"),
                                      value = c(0.2, 0.5), stringsAsFactors = FALSE))
  m$write(f)
  expect_error(
    pfm:::.psmVerifyCouplingGdx(f, list(mkPhi(c("EUR", "USA"), c(0.2, 0.5)))),
    "rank 2 but REMIND declares rank 1")
})

# --- the market dimension (ADR 0042, symmetric markup) ------------------------

mkPhiSector <- function() {
  list(Bulk    = c(EUR = 0.80, USA = 0.60),
       Diffuse = c(EUR = 0.50, USA = 0.95))
}
mkBndSector <- function() {
  d <- function(v) data.frame(region = c("EUR", "EUR", "USA", "USA"),
                              year = c(2030, 2050, 2030, 2050),
                              priceBound = v, stringsAsFactors = FALSE)
  list(Bulk = d(c(10, 20, 30, 40)), Diffuse = d(c(11, 21, 31, 41)))
}

test_that("the sector-to-market map covers every market exactly once", {
  # If a market were missing, GAMS would leave it at the datainput default and that
  # market would silently keep the floor. If one appeared twice, the last sector written
  # would win and the fan-out would be order-dependent.
  m <- pfm:::.psmSectorMarkets()
  expect_setequal(unlist(m, use.names = FALSE), c("ETS", "ES", "other"))
  expect_identical(anyDuplicated(unlist(m, use.names = FALSE)), 0L)
  expect_identical(m$Bulk, "ETS")
  expect_true(all(c("ES", "other") %in% m$Diffuse))
})

test_that("phi per market is rank 2, indexed (all_regi, all_emiMkt)", {
  skip_if_no_gt()
  f <- withr::local_tempfile(fileext = ".gdx")
  writeSyms(f, pfm:::.psmCouplingSymMkt1d("p45_pfmPhiMkt", mkPhiSector()))
  i <- symInfo(f, "p45_pfmPhiMkt")
  expect_identical(i$dim, 2L)
  expect_identical(i$domains, c("all_regi", "all_emiMkt"))
  expect_identical(i$domainType, "regular")
  expect_identical(i$n, 6L)            # 2 regions x 3 markets
})

test_that("Diffuse fans out to BOTH ES and other, with the same value", {
  skip_if_no_gt()
  # "other = ES" is the decision recorded in .psmSectorMarkets(): REMIND's own
  # convention (47_regipol postsolve) and ADR 0042's stated mapping. Before the
  # symmetric markup, "other" silently kept the floor.
  f <- withr::local_tempfile(fileext = ".gdx")
  writeSyms(f, pfm:::.psmCouplingSymMkt1d("p45_pfmPhiMkt", mkPhiSector()))
  got <- cells(symInfo(f, "p45_pfmPhiMkt"))
  expect_equal(unname(got[["EUR ETS"]]), 0.80)
  expect_equal(unname(got[["EUR ES"]]), 0.50)
  expect_equal(unname(got[["EUR other"]]), 0.50)
  expect_equal(unname(got[["USA ETS"]]), 0.60)
  expect_equal(unname(got[["USA ES"]]), 0.95)
  expect_equal(unname(got[["USA other"]]), 0.95)
})

test_that("the per-market bound is rank 3, indexed (ttot, all_regi, all_emiMkt)", {
  skip_if_no_gt()
  f <- withr::local_tempfile(fileext = ".gdx")
  writeSyms(f, pfm:::.psmCouplingSymMkt2d("p45_pfmPriceBoundMkt", mkBndSector(),
                                          "priceBound"))
  i <- symInfo(f, "p45_pfmPriceBoundMkt")
  expect_identical(i$dim, 3L)
  expect_identical(i$domains, c("ttot", "all_regi", "all_emiMkt"))
  expect_identical(i$domainType, "regular")
  expect_identical(i$n, 12L)           # 2 years x 2 regions x 3 markets
})

test_that("rank-3 values land on the right year-region-market cell", {
  skip_if_no_gt()
  f <- withr::local_tempfile(fileext = ".gdx")
  writeSyms(f, pfm:::.psmCouplingSymMkt2d("p45_pfmPriceBoundMkt", mkBndSector(),
                                          "priceBound"))
  got <- cells(symInfo(f, "p45_pfmPriceBoundMkt"))
  expect_equal(unname(got[["2030 EUR ETS"]]), 10)   # Bulk
  expect_equal(unname(got[["2050 USA ETS"]]), 40)   # Bulk
  expect_equal(unname(got[["2030 EUR ES"]]), 11)    # Diffuse
  expect_equal(unname(got[["2050 USA other"]]), 41) # Diffuse, fanned out
})

test_that("rank-3 year labels stay bare and sort numerically", {
  skip_if_no_gt()
  f <- withr::local_tempfile(fileext = ".gdx")
  b <- list(Bulk = data.frame(region = "EUR", year = c(2100, 2030, 2150),
                              priceBound = c(2, 1, 3), stringsAsFactors = FALSE))
  writeSyms(f, pfm:::.psmCouplingSymMkt2d("p45_pfmPriceBoundMkt", b, "priceBound"))
  u <- unique(as.character(symInfo(f, "p45_pfmPriceBoundMkt")$records$ttot))
  expect_true(all(grepl("^[0-9]{4}$", u)))
  m <- gamstransfer::Container$new(f)
  expect_identical(as.character(m$getSymbols("ttot")[[1]]$records[[1]]),
                   c("2030", "2100", "2150"))
})

test_that("the post-write check catches a transposed rank-3 symbol", {
  skip_if_no_gt()
  # The order defect, one dimension up: correct rank, no GAMS error, all zeros on load.
  f <- withr::local_tempfile(fileext = ".gdx")
  m <- gamstransfer::Container$new()
  sr <- m$addSet("all_regi", records = "EUR")
  st <- m$addSet("ttot", records = "2030")
  sm <- m$addSet("all_emiMkt", records = "ETS")
  m$addParameter("p45_pfmPriceBoundMkt", domain = list(sr, st, sm),   # ttot NOT first
                 records = data.frame(all_regi = "EUR", ttot = "2030",
                                      all_emiMkt = "ETS", value = 1,
                                      stringsAsFactors = FALSE))
  m$write(f)
  expect_error(
    pfm:::.psmVerifyCouplingGdx(f, list(pfm:::.psmCouplingSymMkt2d(
      "p45_pfmPriceBoundMkt", mkBndSector(), "priceBound"))),
    "indexed \\(all_regi, ttot, all_emiMkt\\)")
})

test_that("all coupling symbols coexist at their declared ranks", {
  skip_if_no_gt()
  f <- withr::local_tempfile(fileext = ".gdx")
  d <- mkBound(); names(d)[names(d) == "priceBound"] <- "price"
  mp <- lapply(mkBndSector(), function(x) {
    names(x)[names(x) == "priceBound"] <- "price"; x })
  writeSyms(f,
            mkPhi(c("EUR", "USA"), c(0.7, 0.45)),
            pfm:::.psmCouplingSym1d("p45_pfmDelta", c(EUR = 0.004, USA = 0.004)),
            pfm:::.psmCouplingSym2d("p45_pfmPriceBound", mkBound(), "priceBound"),
            pfm:::.psmCouplingSym2d("p45_pfmMPPrice", d, "price"),
            pfm:::.psmCouplingSymMkt1d("p45_pfmPhiMkt", mkPhiSector()),
            pfm:::.psmCouplingSymMkt1d("p45_pfmLambdaMkt", mkPhiSector()),
            pfm:::.psmCouplingSymMkt2d("p45_pfmPriceBoundMkt", mkBndSector(),
                                       "priceBound"),
            pfm:::.psmCouplingSymMkt2d("p45_pfmMPPriceMkt", mp, "price"))
  expect_identical(symInfo(f, "p45_regiDiff_phi")$dim, 1L)
  expect_identical(symInfo(f, "p45_pfmPriceBound")$domains, c("ttot", "all_regi"))
  expect_identical(symInfo(f, "p45_pfmPhiMkt")$domains, c("all_regi", "all_emiMkt"))
  expect_identical(symInfo(f, "p45_pfmLambdaMkt")$domains, c("all_regi", "all_emiMkt"))
  expect_identical(symInfo(f, "p45_pfmPriceBoundMkt")$domains,
                   c("ttot", "all_regi", "all_emiMkt"))
  expect_identical(symInfo(f, "p45_pfmMPPriceMkt")$domains,
                   c("ttot", "all_regi", "all_emiMkt"))
})

# --- guards -------------------------------------------------------------------

test_that("an unnamed phi vector is refused rather than written as junk", {
  expect_error(pfm:::.psmCouplingSym1d("p45_regiDiff_phi", c(0.1, 0.2)), "fully named")
})

test_that("the market helpers refuse unnamed vectors and bad years", {
  expect_error(
    pfm:::.psmCouplingSymMkt1d("p45_pfmPhiMkt", list(Bulk = c(0.1, 0.2))),
    "fully named")
  expect_error(
    pfm:::.psmCouplingSymMkt2d("p45_pfmPriceBoundMkt",
                               list(Bulk = data.frame(region = "EUR",
                                                      year = "not-a-year",
                                                      priceBound = 1,
                                                      stringsAsFactors = FALSE)),
                               "priceBound"),
    "non-numeric years")
})

test_that("a sector with no market mapping is refused, not silently dropped", {
  # A typo'd or renamed sector must not vanish from the gdx leaving that market on the
  # floor - that is the failure the symmetric markup exists to remove.
  expect_error(
    pfm:::.psmCouplingSymMkt1d("p45_pfmPhiMkt", list(Nonsense = c(EUR = 0.5))),
    "no market maps to sector")
})

test_that("non-numeric years are refused", {
  d <- data.frame(region = "EUR", year = "not-a-year", priceBound = 1,
                  stringsAsFactors = FALSE)
  expect_error(pfm:::.psmCouplingSym2d("p45_pfmPriceBound", d, "priceBound"),
               "non-numeric years")
})
