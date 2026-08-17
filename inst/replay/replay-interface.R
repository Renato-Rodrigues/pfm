#!/usr/bin/env Rscript
# Standalone entry point for pfm::pfmReplayInterface().
#
# Promoted from the archived replay-coupling.R (_archive/2026-08-14/root-scripts/),
# TODO.md item 6. That script replayed a whole coupling call from a finished
# fulldata.gdx; this is its INTERFACE half - the part no R-side test can cover, because
# it is the only thing that asks GAMS itself what it sees.
#
# Run it whenever the gdx contract changes: a new symbol, a renamed one, a changed rank
# or index order in 45_carbonprice/functionalForm/declarations.gms.
#
#   Rscript inst/replay/replay-interface.R
#   Rscript inst/replay/replay-interface.R --remind ../remind_pfm --keep out/replay
#   Rscript inst/replay/replay-interface.R --no-negative-control
#
# Exit codes: 0 pass, 1 fail, 2 skipped (no GAMS or no gamstransfer). A SKIP is not a
# pass - it means nothing was verified.

args <- commandArgs(trailingOnly = TRUE)
getArg <- function(flag, default = NULL) {
  i <- match(flag, args)
  if (is.na(i) || i == length(args)) default else args[[i + 1L]]
}
hasFlag <- function(flag) flag %in% args

remindDir <- getArg("--remind", getOption("pfm.remindDir", "../remind_pfm"))
keepDir   <- getArg("--keep", NULL)
negCtl    <- !hasFlag("--no-negative-control")

# Prefer the SOURCE tree over an installed pfm when run from the package root: this
# script exists to check the source's contract against the module, and an installed copy
# is by definition older than the change being checked.
inPkgRoot <- file.exists("DESCRIPTION") &&
  any(grepl("^Package:\\s*pfm\\s*$", readLines("DESCRIPTION", warn = FALSE)))
if (inPkgRoot && requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(".", quiet = TRUE)
} else if (!requireNamespace("pfm", quietly = TRUE)) {
  stop("install pfm, or run from the package root with pkgload available")
}

res <- pfm::pfmReplayInterface(
  remindDir = remindDir,
  dir = if (is.null(keepDir)) file.path(tempdir(), "pfm-replay-interface") else keepDir,
  negativeControl = negCtl,
  quiet = FALSE)

cat("\n")
if (!is.null(res$skipped)) {
  cat("SKIPPED: ", res$skipped, "\n", sep = "")
  cat("Nothing was verified. This is NOT a pass.\n")
  quit(status = 2L)
}
cat("declarations lifted from the module: ", res$nDeclarations, "\n", sep = "")
cat("positive replay: ", if (isTRUE(res$positive$ok)) "PASS" else "FAIL", "\n", sep = "")
for (v in res$positive$verdict) cat("   ", v, "\n")
if (!is.null(res$negative)) {
  cat("negative control: ", if (isTRUE(res$negative$ok)) "PASS (defect caught)"
      else "FAIL (a transposed rank-3 symbol went UNDETECTED)", "\n", sep = "")
  for (v in res$negative$verdict) cat("   ", v, "\n")
}
cat("artefacts: ", res$dir, "\n", sep = "")
cat("\n", if (isTRUE(res$ok)) "INTERFACE OK" else "INTERFACE FAILED", "\n", sep = "")
quit(status = if (isTRUE(res$ok)) 0L else 1L)
