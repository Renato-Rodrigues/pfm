# nolint start
#' Assemble the REMIND-ready PFM input folder
#'
#' @description
#' The last step of the PFM pipeline: copy exactly what the coupling reads out of a
#' Run-Group into a self-contained folder that REMIND's \code{preparePFM.R} picks up
#' (its \code{cfg$pfm$source}, default \code{../pfm-data}).
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
#' @param dest Destination root. Default \code{"pfm-data"} beside the working
#'   directory, matching REMIND's \code{../pfm-data} default from a run folder.
#' @param resultsDir,modelDir,cachefolder Standard Run-Group locations.
#' @param overwrite Overwrite an existing destination group folder.
#' @param verbose Logical.
#'
#' @return Invisibly, the destination folder path.
#' @author Renato Rodrigues
#' @export
runPSMExportREMINDInputs <- function(group,
                                     dest = "pfm-data",
                                     resultsDir = getOption("pfm.resultsDir", "output"),
                                     modelDir = getOption("pfm.modelDir", "output"),
                                     cachefolder = NULL,
                                     overwrite = TRUE,
                                     verbose = TRUE) {
  groupDir <- .resolveGroupDir(group, resultsDir, modelDir, cachefolder)
  say <- function(...) if (isTRUE(verbose)) message("[PSM-REMIND:", group, "] ", ...)
  t0 <- Sys.time()

  # Exactly what iterativePFM() opens — kept in step with preparePFM.R's own list.
  need <- c("selected-models-psm.yml", "manifest.json", "frontier.rds",
            "temporal-validation.rds",
            "donor-assignment-band-Bulk.rds", "donor-assignment-band-Diffuse.rds")
  missing <- need[!file.exists(file.path(groupDir, need))]
  if (length(missing)) {
    stop("runPSMExportREMINDInputs: the Run-Group is missing ",
         paste(missing, collapse = ", "), ".\n",
         "  The band assignments come from the psm-donor step; without them the ",
         "coupling refuses to run rather than reverting to phi = 1.\n",
         "  Run:  pfmRun(group = \"", group, "\", steps = \"psm-downstream\")",
         call. = FALSE)
  }

  mf <- jsonlite::read_json(file.path(groupDir, "manifest.json"))
  hash <- mf$panel_hash %||% ""
  if (!nzchar(hash)) {
    stop("runPSMExportREMINDInputs: manifest.json has no panel_hash, so the panel the ",
         "deployed spec was fitted on cannot be identified.", call. = FALSE)
  }
  panel <- paste0("panel_", hash, ".rds")
  panelCand <- c(file.path(dirname(groupDir), "panels", panel),
                 file.path(groupDir, "panels", panel),
                 file.path(groupDir, panel))
  panelSrc <- panelCand[file.exists(panelCand)][1]
  if (is.na(panelSrc)) {
    stop("runPSMExportREMINDInputs: panel '", panel, "' not found in any of:\n  ",
         paste(panelCand, collapse = "\n  "),
         "\n  The Run-Group and the panel store are out of sync.", call. = FALSE)
  }

  outDir <- file.path(dest, group)
  if (dir.exists(outDir) && !isTRUE(overwrite)) {
    stop("runPSMExportREMINDInputs: '", outDir, "' exists and overwrite = FALSE.",
         call. = FALSE)
  }
  dir.create(file.path(outDir, "panels"), recursive = TRUE, showWarnings = FALSE)
  ok <- all(file.copy(file.path(groupDir, need), file.path(outDir, need), overwrite = TRUE))
  ok <- ok && file.copy(panelSrc, file.path(outDir, "panels", panel), overwrite = TRUE)
  if (!ok) stop("runPSMExportREMINDInputs: one or more files failed to copy to '",
                outDir, "'.", call. = FALSE)

  # Verify what landed, not what we intended to write.
  wrote <- c(need, file.path("panels", panel))
  bad <- wrote[!file.exists(file.path(outDir, wrote))]
  if (length(bad)) {
    stop("runPSMExportREMINDInputs: destination is incomplete after copying: ",
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
  .recordStep(groupDir, group, "psm-remind-inputs", t0,
              metrics = list(dest = outDir, files = length(wrote), panel = panel))
  invisible(outDir)
}
# nolint end
