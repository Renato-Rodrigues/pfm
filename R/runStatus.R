# nolint start
#' Report the status of a model-group run
#'
#' @description
#' Reads a Run-Group's \code{manifest.json} (the run record, ADR 0020) and reports overall
#' status, per-step timings/metrics, artifacts present, and steps remaining. When a SLURM job id
#' is recorded and \code{squeue}/\code{sacct} are available, it additionally queries SLURM live so
#' an in-progress run can be reported as truly running / pending / completed / failed (rather than
#' a stale \code{status="running"} left by a crashed job). Works on any finished or aborted run
#' purely from the output folder.
#'
#' @param group Character. Run-Group name.
#' @param resultsDir Character. Results Root. Default \code{getOption("pfm.resultsDir")}.
#' @param verbose Logical. Print a human-readable summary. Default \code{TRUE}.
#' @return Invisibly, a list with the parsed status (\code{manifestStatus}, \code{slurm},
#'   \code{steps}, \code{remaining}, \code{artifacts}, timings, ...). \code{NULL} if no run exists.
#' @seealso \code{\link{startRun}}, ADR 0020
#' @export
#' @author Renato Rodrigues
runStatus <- function(group, resultsDir = getOption("pfm.resultsDir", "output"), verbose = TRUE) {
  if (missing(group) || is.null(group) || !nzchar(group)) stop("runStatus: 'group' is required.", call. = FALSE)
  if (is.null(resultsDir)) stop("runStatus: supply 'resultsDir' or set options(pfm.resultsDir = '...').", call. = FALSE)
  groupDir <- file.path(resultsDir, group)
  manPath <- file.path(groupDir, "manifest.json")
  if (!file.exists(manPath)) {
    if (isTRUE(verbose)) message("No run found for group '", group, "' (no manifest at ", groupDir, ").")
    return(invisible(NULL))
  }
  man <- tryCatch(jsonlite::fromJSON(manPath, simplifyVector = FALSE), error = function(e) list())
  run <- man$run %||% list()
  requested <- unlist(run$steps %||% list())
  doneSteps <- names(man$steps %||% list())
  jobId <- run$slurmJobId
  live <- NULL
  if (!is.null(jobId) && length(jobId) == 1 && !is.na(jobId) && nzchar(jobId) &&
      nzchar(Sys.which("squeue"))) {
    live <- .querySlurm(jobId)
  }
  status <- list(
    group = group, dir = groupDir,
    manifestStatus = run$status %||% "unknown",
    cluster = run$cluster %||% NA_character_, slurmJobId = jobId %||% NA_character_,
    nCores = run$nCores, host = run$host %||% NA_character_,
    startedAt = run$startedAt, endedAt = run$endedAt, seconds = run$seconds,
    steps = man$steps, requested = requested,
    remaining = setdiff(requested, doneSteps),
    artifacts = unlist(man$artifacts %||% list()), slurm = live
  )
  if (isTRUE(verbose)) .printRunStatus(status)
  invisible(status)
}

# Internal: live SLURM state for a job id — squeue first (queued/running), then sacct (finished).
#' @keywords internal
.querySlurm <- function(jobId) {
  sq <- tryCatch(suppressWarnings(system2("squeue", c("-j", jobId, "-h", "-o", "%T"),
                                          stdout = TRUE, stderr = FALSE)),
                 error = function(e) character(0))
  sq <- trimws(sq[nzchar(sq)])
  if (length(sq) >= 1) return(list(state = sq[[1]], source = "squeue"))
  if (nzchar(Sys.which("sacct"))) {
    sa <- tryCatch(suppressWarnings(system2("sacct", c("-j", jobId, "-n", "-X", "-o", "State,Elapsed"),
                                            stdout = TRUE, stderr = FALSE)),
                   error = function(e) character(0))
    sa <- trimws(sa[nzchar(sa)])
    if (length(sa) >= 1) {
      parts <- strsplit(sa[[1]], "\\s+")[[1]]
      return(list(state = parts[[1]], elapsed = if (length(parts) > 1) parts[[2]] else NA_character_,
                  source = "sacct"))
    }
  }
  list(state = "UNKNOWN", source = "none")
}

# Internal: human-readable status print.
#' @keywords internal
.printRunStatus <- function(s) {
  line <- function(...) message(...)
  line("PFM run status - group '", s$group, "'")
  line("  manifest status : ", s$manifestStatus,
       if (!is.null(s$seconds)) paste0("  (", s$seconds, "s)") else "")
  if (!is.na(s$cluster)) line("  cluster         : ", s$cluster,
       if (!is.na(s$slurmJobId)) paste0("  job ", s$slurmJobId) else "",
       if (!is.null(s$nCores)) paste0("  nCores=", s$nCores) else "")
  if (!is.null(s$slurm)) line("  SLURM live      : ", s$slurm$state, " (via ", s$slurm$source, ")")
  if (!is.null(s$startedAt)) line("  started         : ", s$startedAt)
  if (!is.null(s$endedAt))   line("  ended           : ", s$endedAt)
  line("  steps:")
  for (nm in names(s$steps)) {
    e <- s$steps[[nm]]
    if (is.list(e)) {
      mtr <- if (length(e$metrics)) paste0(" {", paste(names(e$metrics), unlist(e$metrics), sep = "=",
                                                       collapse = ", "), "}") else ""
      line("    - ", nm, ": ", e$status %||% "?", "  ", e$seconds %||% "?", "s", mtr)
    } else {
      line("    - ", nm, ": ", e)
    }
  }
  if (length(s$remaining)) line("  remaining       : ", paste(s$remaining, collapse = ", "))
  line("  artifacts       : ", paste(s$artifacts, collapse = ", "))
  invisible(NULL)
}
# nolint end
