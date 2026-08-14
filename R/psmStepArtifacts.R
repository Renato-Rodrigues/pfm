# nolint start
#' What each Run-Group step writes
#'
#' @description
#' The single source of truth mapping a pipeline step to the artifacts it produces.
#' Two things need this map and must never disagree: \code{\link{runModelGroup}},
#' which uses it to decide what \code{resume} may skip, and \code{\link{pfmRun}},
#' which uses it to decide what \code{clean} must delete. A step whose artifact is
#' listed in one place and not the other is a step that either re-runs forever or
#' is skipped on the strength of a stale file.
#'
#' Some entries name a directory (the projection fan-out) or a glob; callers should
#' handle both. Paths are relative to the Run-Group directory.
#'
#' @param steps Character vector of step names, or \code{NULL} for the whole map.
#'
#' @return Named list of character vectors: step -> relative artifact paths.
#' @author Renato Rodrigues
#' @export
psmStepArtifacts <- function(steps = NULL) {
  m <- list(
    "sweep"                   = "sweep.rds",
    "robustness"              = "robustness.rds",
    "temporal"                = "temporal-split.rds",
    "subnational"             = "subnational.rds",
    "difference-first"        = "difference-first.rds",
    "projection"              = c("projection.rds", "projections"),
    "selection-bootstrap"     = "selection-bootstrap.rds",
    "psm-sweep"               = c("selected-models-psm.yml", "sweep.rds"),
    "psm-projection"          = c("projection.rds", "projections"),
    "psm-agreement"           = "estimator-agreement.rds",
    "psm-temporal"            = "temporal-validation.rds",
    "psm-frontier"            = "frontier.rds",
    "psm-iv"                  = "iv.rds",
    "psm-influence"           = "influence.rds",
    "psm-sector-speeds"       = "sector-speeds.rds",
    "psm-selection-bootstrap" = "selection-bootstrap.rds",
    "psm-replay"              = "historical-replay.rds",
    "psm-donor"               = c("donor-assignment-band-Bulk.rds",
                                  "donor-assignment-band-Diffuse.rds",
                                  "coverage"),
    "psm-coupling-bound"      = "coupling",
    # The REMIND export writes OUTSIDE the Run-Group, so it has no artifact here:
    # cleaning it is the caller's business, and re-running it is cheap and idempotent.
    "psm-remind-inputs"       = character(0))
  if (is.null(steps)) return(m)
  m[intersect(names(m), steps)]
}

#' Delete the artifacts a set of steps would write
#'
#' @description
#' Makes a re-run genuinely fresh. \code{resume} decides what to skip by asking
#' whether a file EXISTS, which is a good answer to "did this finish?" and a bad one
#' to "is this still valid?" — a Run-Group carried across a code change, a panel
#' rebuild or a new gdx keeps artifacts that look complete and are wrong. That is
#' exactly how a projection covering 33 countries survived alongside a model fitted
#' on 48. Deleting first turns a silent stale read into an honest recompute.
#'
#' @param group Run-Group name.
#' @param steps Steps whose artifacts should be removed.
#' @param resultsDir Results root.
#' @param dryRun Report what would be removed without removing it.
#' @param verbose Logical.
#'
#' @return Invisibly, the paths removed (or that would be).
#' @author Renato Rodrigues
#' @export
psmCleanSteps <- function(group, steps,
                          resultsDir = getOption("pfm.resultsDir", "output"),
                          dryRun = FALSE, verbose = TRUE) {
  groupDir <- file.path(resultsDir, group)
  if (!dir.exists(groupDir)) return(invisible(character(0)))
  rel <- unique(unlist(psmStepArtifacts(steps), use.names = FALSE))
  if (!length(rel)) return(invisible(character(0)))
  paths <- file.path(groupDir, rel)
  paths <- paths[file.exists(paths)]
  if (!length(paths)) {
    if (isTRUE(verbose)) message("[clean] nothing to remove in ", groupDir)
    return(invisible(character(0)))
  }
  if (isTRUE(verbose)) {
    message("[clean] ", if (dryRun) "would remove" else "removing", " ",
            length(paths), " artifact(s) from ", groupDir, ":")
    for (p in paths) message("    ", basename(p))
  }
  if (!isTRUE(dryRun)) unlink(paths, recursive = TRUE, force = TRUE)
  invisible(paths)
}
# nolint end
