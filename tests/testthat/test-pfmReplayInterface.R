# The GAMS half of the gdx contract.
#
# test-gdxRoundTrip.R checks what R WRITES. This checks what GAMS READS, which is a
# different question and the only one that can catch a disagreement between the exporters
# and 45_carbonprice/functionalForm/declarations.gms.
#
# SKIPS without GAMS or the REMIND fork, so it is inert on a machine that has neither. A
# skip is not a pass - run inst/replay/replay-interface.R before changing the contract.

remindDir <- function() {
  cand <- c(getOption("pfm.remindDir", ""), "../remind_pfm", "../../remind_pfm",
            "../../../remind_pfm", file.path(rprojroot::find_root("DESCRIPTION"),
                                             "..", "remind_pfm"))
  cand <- cand[nzchar(cand)]
  hit <- cand[file.exists(file.path(cand, "modules", "45_carbonprice", "functionalForm",
                                    "declarations.gms"))]
  if (length(hit)) normalizePath(hit[1]) else NA_character_
}
skip_unless_gams <- function() {
  skip_if_not_installed("gamstransfer")
  if (!nzchar(Sys.which("gams"))) skip("no GAMS on PATH")
  if (is.na(remindDir())) skip("remind_pfm not found next to the package")
}

test_that("every symbol the replay loads is declared in the module", {
  # Runs WITHOUT GAMS: it only reads declarations.gms. This is the check that fires when
  # a symbol is renamed on one side and not the other - the cheapest way to notice.
  if (is.na(remindDir())) skip("remind_pfm not found next to the package")
  decl <- file.path(remindDir(), "modules", "45_carbonprice", "functionalForm",
                    "declarations.gms")
  d <- pfm:::.psmReplayDeclarations(decl)
  expect_length(d, 16L)
  expect_true(all(nzchar(d)))
  # the ADR 0042 market symbols, at the ranks REMIND declares
  expect_true(any(grepl("^p45_pfmPhiMkt\\(all_regi,all_emiMkt\\)", d)))
  expect_true(any(grepl("^p45_pfmLambdaMkt\\(all_regi,all_emiMkt\\)", d)))
  expect_true(any(grepl("^p45_pfmPriceBoundMkt\\(ttot,all_regi,all_emiMkt\\)", d)))
  expect_true(any(grepl("^p45_pfmMPPriceMkt\\(ttot,all_regi,all_emiMkt\\)", d)))
})

test_that("a renamed or missing declaration is an error, not a silent pass", {
  tmp <- withr::local_tempfile(fileext = ".gms")
  writeLines(c("parameters", "  p45_regiDiff_phi(all_regi) \"only this one\"", ";"), tmp)
  expect_error(pfm:::.psmReplayDeclarations(tmp), "not declared in declarations.gms")
})

test_that("GAMS loads every coupling symbol, rank 3 included, with the right values", {
  skip_unless_gams()
  res <- pfmReplayInterface(remindDir = remindDir(), negativeControl = FALSE, quiet = TRUE)
  skip_if(!is.null(res$skipped), res$skipped %||% "skipped")
  expect_true(res$positive$loadedCleanly)
  expect_identical(res$positive$status, 0L)
  expect_true(any(grepl("REPLAY OK", res$positive$verdict)))
})

test_that("the harness FAILS on a transposed rank-3 symbol", {
  # The load-bearing test. A transposed symbol has the right rank, raises no GAMS error at
  # all, and loads as ( ALL 0.000 ) - defect 4, one dimension up. If this ever passes
  # silently, the positive replay above proves nothing.
  skip_unless_gams()
  res <- pfmReplayInterface(remindDir = remindDir(), negativeControl = TRUE, quiet = TRUE)
  skip_if(!is.null(res$skipped), res$skipped %||% "skipped")
  expect_false(res$negative$loadedCleanly)
  expect_true(res$negative$status != 0L)
  expect_true(any(grepl("MISMATCH|ALL-ZERO", res$negative$verdict)))
  expect_true(res$ok)          # positive passed AND negative was caught
})
