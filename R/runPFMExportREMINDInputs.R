# nolint start
#' Assemble the REMIND-ready PFM input folder
#'
#' @description
#' The last step of the PFM pipeline: copy exactly what the coupling reads out of a
#' Run-Group into a self-contained folder that REMIND's \code{preparePFM.R} picks up
#' (its \code{cfg$pfm$source}, default \code{../../output/remind-inputs} from a checkout
#' in \code{models/}).
#'
#' Only the six artifacts \code{\link{iterativePFM}} actually opens are copied, plus
#' the one panel named by \code{manifest.json:panel_hash}. Copying the whole Run-Group
#' would drag \code{sweep.rds} and the projection fan-out — hundreds of MB the coupling
#' never opens — into every REMIND run folder.
#'
#' The folder is written in the nested layout (\code{<dest>/<group>/...}) so several
#' Run-Groups can sit side by side and REMIND can auto-detect a single one, or select
#' by \code{cfg$pfm$group} when there are several.
#'
#' Verification is the point of this step: it refuses to write a partial folder. A
#' half-copied \code{pfm-data} reads as "the copy worked" at REMIND submit time, which
#' is the most misleading state to leave it in.
#'
#' @param group Run-Group name.
#' @param dest Destination root. Default \code{"output/remind-inputs"} under the project
#'   root, matching the REMIND fork's default \code{cfg$pfm$source}.
#' @param resultsDir,modelDir,cachefolder Standard Run-Group locations. \code{cachefolder} is
#'   also the prepared madrat cache whose coupling files (its \code{cache-manifest.tsv}) are
#'   staged into \code{<dest>/<group>/madrat-cache/}; see \code{\link{pfmPrepareCache}}.
#' @param overwrite Overwrite an existing destination group folder.
#' @param verbose Logical.
#'
#' @return Invisibly, the destination folder path.
#' @author Renato Rodrigues
#' @export
runPFMExportREMINDInputs <- function(group,
                                     dest = "output/remind-inputs",
                                     resultsDir = getOption("pfm.resultsDir", "output"),
                                     modelDir = getOption("pfm.modelDir", "output"),
                                     cachefolder = NULL,
                                     overwrite = TRUE,
                                     verbose = TRUE) {
  groupDir <- .resolveGroupDir(group, resultsDir, modelDir, cachefolder)
  say <- function(...) if (isTRUE(verbose)) message("[PFM-REMIND:", group, "] ", ...)
  t0 <- Sys.time()

  # Exactly what iterativePFM() opens — kept in step with preparePFM.R's own list.
  need <- c(basename(.pfmSelectedModels(groupDir)), "manifest.json", "frontier.rds",
            "temporal-validation.rds",
            "donor-assignment-band-Bulk.rds", "donor-assignment-band-Diffuse.rds")
  missing <- need[!file.exists(file.path(groupDir, need))]
  if (length(missing)) {
    stop("runPFMExportREMINDInputs: the Run-Group is missing ",
         paste(missing, collapse = ", "), ".\n",
         "  The band assignments come from the pfm-donor step; without them the ",
         "coupling refuses to run rather than reverting to phi = 1.\n",
         "  Run:  pfmRun(group = \"", group, "\", steps = \"pfm-downstream\")",
         call. = FALSE)
  }

  mf <- jsonlite::read_json(file.path(groupDir, "manifest.json"))
  hash <- mf$panel_hash %||% ""
  if (!nzchar(hash)) {
    stop("runPFMExportREMINDInputs: manifest.json has no panel_hash, so the panel the ",
         "deployed spec was fitted on cannot be identified.", call. = FALSE)
  }
  panel <- paste0("panel_", hash, ".rds")
  panelCand <- .pfmPanelCandidates(groupDir, hash, modelDir = modelDir)
  panelSrc <- panelCand[file.exists(panelCand)][1]
  if (is.na(panelSrc)) {
    stop("runPFMExportREMINDInputs: panel '", panel, "' not found in any of:\n  ",
         paste(panelCand, collapse = "\n  "),
         "\n  The Run-Group and the panel store are out of sync.", call. = FALSE)
  }

  outDir <- file.path(dest, group)
  if (dir.exists(outDir) && !isTRUE(overwrite)) {
    stop("runPFMExportREMINDInputs: '", outDir, "' exists and overwrite = FALSE.",
         call. = FALSE)
  }
  dir.create(file.path(outDir, "panels"), recursive = TRUE, showWarnings = FALSE)
  # The v6 anchor artifact (pfm-anchor, 0005 C6/F6): shipped when the group has one. A group whose
  # manifest records a completed pfm-anchor step must have it (pfmPreflight checks the export).
  optional <- "phi-anchor.rds"
  optional <- optional[file.exists(file.path(groupDir, optional))]
  if (!length(optional) && identical(jsonlite::read_json(file.path(groupDir, "manifest.json"))$steps[["pfm-anchor"]]$status, "completed")) {
    stop("runPFMExportREMINDInputs: the manifest records pfm-anchor but phi-anchor.rds is missing; ",
         "re-run pfmRun(group = \"", group, "\", steps = \"pfm-anchor\").", call. = FALSE)
  }
  need <- c(need, optional)
  unlink(file.path(outDir, "phi-anchor.rds"))   # never leave a previous export's anchor behind
  ok <- all(file.copy(file.path(groupDir, need), file.path(outDir, need), overwrite = TRUE))
  ok <- ok && file.copy(panelSrc, file.path(outDir, "panels", panel), overwrite = TRUE)
  if (!ok) stop("runPFMExportREMINDInputs: one or more files failed to copy to '",
                outDir, "'.", call. = FALSE)

  # The coupling's madrat cache files, from the prepared project cache (pfmPrepareCache): the
  # scenario panel and the coupling weights read them at every coupling iteration. Staged with
  # the group so a REMIND run reads the data versions the estimation was prepared with, not
  # whatever madrat's shared cache holds on the day the run starts - which is how the v5
  # estimation and the v5 coupled runs came to read different calcFE/calcPE versions.
  staged <- character(0)
  manFile <- if (!is.null(cachefolder)) file.path(cachefolder, "cache-manifest.tsv") else NULL
  mc <- file.path(outDir, "madrat-cache")
  unlink(mc, recursive = TRUE)
  if (!is.null(manFile) && file.exists(manFile)) {
    man <- .readCacheTable(manFile)
    use <- man[grepl("scenario-panel|coupling-weights", man$builders), , drop = FALSE]
    if (!any(grepl("scenario-panel", use$builders))) {
      warning("runPFMExportREMINDInputs: the prepared cache has no scenario-panel files (no ",
              "registry gdx existed when it was prepared); the coupling computes them on a miss.",
              call. = FALSE)
    }
    dir.create(mc)
    ok <- file.copy(file.path(cachefolder, use$file), file.path(mc, use$file), copy.date = TRUE)
    if (!all(ok)) stop("runPFMExportREMINDInputs: could not stage ", paste(use$file[!ok], collapse = ", "),
                       " from ", cachefolder, call. = FALSE)
    .writeCacheTable(use, file.path(mc, "cache-manifest.tsv"),
                     header = c(paste0("from: ", normalizePath(cachefolder, winslash = "/")),
                                paste0("tag: ", attr(man, "tag") %||% "")))
    staged <- file.path("madrat-cache", c(use$file, "cache-manifest.tsv"))
    say("  staged ", nrow(use), " madrat cache files for the coupling (tag ", attr(man, "tag") %||% "?", ")")
  } else {
    warning("runPFMExportREMINDInputs: no prepared madrat cache (",
            manFile %||% "no cachefolder given", "), so nothing is staged and the coupling reads ",
            "madrat's own cache. Run pfm::pfmPrepareCache() and export again.", call. = FALSE)
  }

  # Verify what landed, not what we intended to write.
  wrote <- c(need, file.path("panels", panel), staged)
  bad <- wrote[!file.exists(file.path(outDir, wrote))]
  if (length(bad)) {
    stop("runPFMExportREMINDInputs: destination is incomplete after copying: ",
         paste(bad, collapse = ", "), call. = FALSE)
  }

  say("REMIND input folder ready: ", normalizePath(outDir, mustWork = FALSE))
  say("  ", length(wrote), " files, panel ", panel)
  if (isTRUE(verbose)) {
    message("\nPoint a REMIND run at it with, in default.cfg or the scenario config:")
    message("    cfg$pfm$source <- \"", normalizePath(dest, winslash = "/", mustWork = FALSE), "\"")
    message("    cfg$pfm$group  <- \"", group, "\"     # omit if this is the only group there")
    message("  and set cm_taxCO2_regiDiff = 11 on the scenarios that should couple.\n")
  }
  .recordStep(groupDir, group, "pfm-remind-inputs", t0,
              metrics = list(dest = outDir, files = length(wrote), panel = panel,
                             madratCache = length(staged)))
  invisible(outDir)
}
# nolint end
