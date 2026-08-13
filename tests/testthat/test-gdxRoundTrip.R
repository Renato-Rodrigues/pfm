# The PFM <-> REMIND gdx interface.
#
# These tests pin the CONTRACT the GAMS side depends on, and the contract is not just
# "the numbers are right" - it is the index structure REMIND declares in
# 45_carbonprice/functionalForm/declarations.gms:
#
#   p45_regiDiff_phi(all_regi)         rank 1
#   p45_pfmDelta_aux(all_regi)         rank 1
#   p45_pfmPriceBound(ttot,all_regi)   rank 2, ttot FIRST
#   p45_pfmMPPrice(ttot,all_regi)      rank 2, ttot FIRST
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

# --- guards -------------------------------------------------------------------

test_that("an unnamed phi vector is refused rather than written as junk", {
  expect_error(pfm:::.psmCouplingSym1d("p45_regiDiff_phi", c(0.1, 0.2)), "fully named")
})

test_that("non-numeric years are refused", {
  d <- data.frame(region = "EUR", year = "not-a-year", priceBound = 1,
                  stringsAsFactors = FALSE)
  expect_error(pfm:::.psmCouplingSym2d("p45_pfmPriceBound", d, "priceBound"),
               "non-numeric years")
})
