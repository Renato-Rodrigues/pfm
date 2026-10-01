# nolint start
#' Historical panel for a Run-Group step, preferring the panel the group was fit on
#'
#' @description
#' Every Run-Group step needs the historical panel. Rebuilding it with
#' \code{\link{panelDataHistorical}} looks harmless but is not: the rebuild reads
#' madrat source data, and on a machine whose source folder is incomplete it
#' succeeds while returning FEWER countries. That is how \code{psm-country-v4}
#' came to report 33 in-coverage countries against a model actually fitted on 48
#' — the sweep used the real panel, the projection silently rebuilt a degraded
#' one, and nothing in between compared the two.
#'
#' The group's \code{manifest.json} already records \code{panel_hash}, and the
#' sweep already wrote \code{<resultsDir>/panels/panel_<hash>.rds}. That file is
#' the authoritative input: it is exactly what the deployed coefficients were
#' estimated on. This helper loads it, and only falls back to a rebuild when it
#' is genuinely absent — warning loudly, because a rebuild is a different dataset.
#'
#' @param groupDir Run-Group directory (holds \code{manifest.json}).
#' @param y Training-year vector, passed through to the rebuild.
#' @param outputRegionMappingFile Regional resolution for the rebuild.
#' @param verbose Logical.
#'
#' @return A magpie panel, or \code{NULL} if neither route yields one.
#' @keywords internal
.psmHistPanel <- function(groupDir, y = NULL, outputRegionMappingFile = NULL,
                          verbose = TRUE) {
  say <- function(...) if (isTRUE(verbose)) message("[panel] ", ...)

  # 1. the panel the group was actually fitted on
  hash <- NULL
  mf <- file.path(groupDir, "manifest.json")
  if (file.exists(mf)) {
    hash <- tryCatch(jsonlite::fromJSON(mf)$panel_hash, error = function(e) NULL)
  }
  if (!is.null(hash) && nzchar(hash)) {
    cand <- .psmPanelCandidates(groupDir, hash)
    for (p in cand) {
      if (file.exists(p)) {
        pan <- tryCatch(readRDS(p), error = function(e) NULL)
        if (is.list(pan) && !is.null(pan$data)) pan <- pan$data
        if (!is.null(pan)) {
          say("using the fitted panel ", basename(p), " (",
              length(magclass::getItems(pan, dim = 1)), " regions).")
          return(pan)
        }
      }
    }
    say("manifest records panel_hash ", hash,
        " but no panel_", hash, ".rds was found (looked in: ", paste(cand, collapse = ", "),
        ") — falling back to a rebuild.")
  }

  # 2. rebuild — a DIFFERENT dataset, so say so
  warning("[panel] rebuilding the historical panel from madrat rather than loading the ",
          "panel this group was fitted on. If the madrat source folder is incomplete the ",
          "rebuild silently returns fewer countries, and every coverage number computed ",
          "from it will be wrong. Check manifest.json:panel_hash and <resultsDir>/panels/.",
          call. = FALSE)
  pan <- tryCatch(
    panelDataHistorical(aggregate = TRUE, y = y,
                        outputRegionMappingFile = outputRegionMappingFile,
                        includePolicyStringency = TRUE),
    error = function(e) NULL
  )
  if (!is.null(pan)) {
    say("rebuilt panel has ", length(magclass::getItems(pan, dim = 1)), " regions.")
  }
  pan
}

#' Where the panel a Run-Group was fitted on can be, in order of preference
#'
#' The fitted panels live in the Fit Cache (\code{modelDir}), at
#' \code{<modelDir>/panels/panel_<hash>.rds}. Until 2026-10-01 the Fit Cache and the
#' Results Root were the same folder, so \code{panels/} sat beside every Run-Group, and
#' this lookup assumed it. With the Fit Cache in its own folder
#' (\code{output/pfm/fit-cache}) that assumption made every step silently rebuild the
#' panel from madrat — a different dataset. Every layout that has existed is listed, the
#' configured model store first.
#'
#' @param groupDir Run-Group directory.
#' @param hash The \code{panel_hash} from the group's \code{manifest.json}.
#' @param modelDir The Fit Cache, default the \code{pfm.modelDir} option.
#' @return Character vector of candidate paths (not checked for existence).
#' @keywords internal
.psmPanelCandidates <- function(groupDir, hash, modelDir = getOption("pfm.modelDir", NULL)) {
  f <- paste0("panel_", hash, ".rds")
  unique(c(
    if (!is.null(modelDir) && nzchar(modelDir)) file.path(modelDir, "panels", f),  # the configured Fit Cache
    file.path(dirname(groupDir), "fit-cache", "panels", f),   # output/pfm/<group> -> output/pfm/fit-cache
    file.path(dirname(groupDir), "panels", f),                # Fit Cache == Results Root (before 2026-10-01)
    file.path(groupDir, "panels", f),                         # a REMIND-input export
    file.path(groupDir, f)))                                  # flat
}
# nolint end
