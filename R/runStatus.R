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
  # Live progress bars (model fitting / report rendering), parsed from the run log while it is active.
  active <- (status$manifestStatus %in% c("running", "submitted")) ||
    (!is.null(live) && toupper(live$state %||% "") %in% c("RUNNING", "PENDING", "CONFIGURING", "COMPLETING"))
  status$progress <- if (active) .runProgress(groupDir) else NULL
  if (isTRUE(verbose)) .printRunStatus(status)
  invisible(status)
}

# Internal: an ASCII progress bar, e.g. "[#########---------------------]  41%".
#' @keywords internal
.progressBar <- function(frac, width = 30L) {
  frac <- max(0, min(1, if (is.finite(frac)) frac else 0))
  n <- round(frac * width)
  paste0("[", strrep("#", n), strrep("-", width - n), "] ", sprintf("%3d%%", round(100 * frac)))
}

# Internal: human-readable duration, e.g. 31715.6 -> "8h 48m 36s", 90061 -> "1d 1h 1m 1s", 45 -> "45s".
#' @keywords internal
.formatDuration <- function(secs) {
  x <- suppressWarnings(as.numeric(secs))
  if (length(x) != 1 || !is.finite(x) || x < 0) return("?")
  d <- x %/% 86400; r <- x %% 86400
  h <- r %/% 3600;  r <- r %% 3600
  m <- r %/% 60;    s <- round(r %% 60)
  parts <- character(0)
  if (d > 0) parts <- c(parts, paste0(d, "d"))
  if (h > 0) parts <- c(parts, paste0(h, "h"))
  if (m > 0) parts <- c(parts, paste0(m, "m"))
  if (s > 0 || !length(parts)) parts <- c(parts, paste0(s, "s"))
  paste(parts, collapse = " ")
}

# Internal: parse live progress from a run's newest .err log.
# - model fitting: the most recent "[fits] N/M" (sweep) or "resample r/N" (selection bootstrap) line.
# - report rendering: present only once "rendering reports via ..." appears (i.e. render was requested);
#   total = the report set listed in that marker, done = count of "[pfmreports] rendering <name>" lines.
#' @keywords internal
.runProgress <- function(groupDir) {
  errs <- list.files(groupDir, pattern = "\\.err$", full.names = TRUE)
  if (!length(errs)) return(NULL)
  errFile <- errs[which.max(file.info(errs)$mtime)]
  ln <- tryCatch(readLines(errFile, warn = FALSE), error = function(e) character(0))
  if (!length(ln)) return(NULL)
  res <- list()

  # --- model fitting bar: take whichever fit-progress line appears latest in the log ----------------
  fitIdx  <- grep("\\[fits\\][[:space:]]+[0-9]+/[0-9]+", ln)
  bootIdx <- grep("resample[[:space:]]+[0-9]+/[0-9]+", ln)
  frac2 <- function(line) {
    mm <- regmatches(line, regexpr("[0-9]+/[0-9]+", line))
    if (!length(mm)) return(NULL)
    as.integer(strsplit(mm[[1]], "/")[[1]])
  }
  lastFit  <- if (length(fitIdx))  max(fitIdx)  else 0L
  lastBoot <- if (length(bootIdx)) max(bootIdx) else 0L
  nm <- NULL; lbl <- NULL
  if (lastBoot > lastFit) {
    nm <- frac2(ln[lastBoot]); lbl <- "model bootstrap"
  } else if (lastFit > 0L) {
    nm <- frac2(ln[lastFit]);  lbl <- "model fitting  "
  }
  if (!is.null(nm) && length(nm) == 2L && nm[2] > 0L) {
    res$model <- list(label = lbl, done = nm[1], total = nm[2], frac = nm[1] / nm[2])
  }

  # --- report rendering: per-report detail, only once rendering has started (=> --render run) -------
  renStart <- grep("rendering reports via", ln)
  if (length(renStart)) {
    sline <- ln[max(renStart)]
    inParen <- regmatches(sline, regexpr("\\(([^)]*)\\)", sline))
    expected <- if (length(inParen)) trimws(strsplit(gsub("[()]", "", inParen), ",")[[1]]) else character(0)
    grab <- function(re) {
      hit <- regmatches(ln, regexpr(re, ln, perl = TRUE))
      unique(hit[nzchar(hit)])
    }
    startedNm <- grab("(?<=\\[pfmreports\\] rendering )\\S+")
    doneLines <- grep("\\[pfmreports\\] done ", ln, value = TRUE)
    doneNm  <- regmatches(doneLines, regexpr("(?<=\\[pfmreports\\] done )\\S+", doneLines, perl = TRUE))
    doneDur <- gsub("[()]", "", regmatches(doneLines, regexpr("\\(([^)]*)\\)", doneLines)))
    reports <- lapply(expected, function(nm) {
      if (nm %in% doneNm) list(name = nm, state = "done", dur = doneDur[match(nm, doneNm)])
      else if (nm %in% startedNm) list(name = nm, state = "running", dur = NA_character_)
      else list(name = nm, state = "pending", dur = NA_character_)
    })
    # completed count drives the bar; fall back to "started" count for logs without done-markers.
    done <- if (length(doneNm)) length(doneNm) else length(startedNm)
    total <- if (length(expected)) length(expected) else NA_integer_
    res$render <- list(done = done, total = total,
                       frac = if (!is.na(total) && total > 0) min(1, done / total) else 0,
                       reports = reports)
  }
  if (length(res)) res else NULL
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
       if (!is.null(s$seconds)) paste0("  (", .formatDuration(s$seconds), ")") else "")
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
      line("    - ", nm, ": ", e$status %||% "?", "  ", .formatDuration(e$seconds), mtr)
    } else {
      line("    - ", nm, ": ", e)
    }
  }
  if (!is.null(s$progress)) {
    p <- s$progress
    if (!is.null(p$model)) {
      line("  ", p$model$label, " : ", .progressBar(p$model$frac),
           "  (", p$model$done, "/", p$model$total, ")")
    }
    if (!is.null(p$render)) {
      tot <- if (is.na(p$render$total)) "?" else p$render$total
      line("  reports         : ", .progressBar(p$render$frac),
           "  (", p$render$done, "/", tot, " rendered)")
      rs <- p$render$reports
      if (length(rs)) {
        nmOf <- function(st) vapply(Filter(function(r) r$state == st, rs),
          function(r) if (identical(st, "done") && !is.na(r$dur)) paste0(r$name, " (", r$dur, ")") else r$name,
          character(1))
        dn <- nmOf("done"); rn <- nmOf("running"); pn <- nmOf("pending")
        if (length(dn)) line("      done      : ", paste(dn, collapse = ", "))
        if (length(rn)) line("      rendering : ", paste(rn, collapse = ", "))
        if (length(pn)) line("      pending   : ", paste(pn, collapse = ", "))
      }
    }
  }
  if (length(s$remaining)) line("  remaining       : ", paste(s$remaining, collapse = ", "))
  line("  artifacts       : ", paste(s$artifacts, collapse = ", "))
  invisible(NULL)
}
# nolint end
