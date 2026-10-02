# nolint start
#' Submit a coupled REMIND start group, checked and recorded
#'
#' @description
#' The submission procedure of \code{RUNNING.md} step 7 as one call (design note 0005 F3):
#' \enumerate{
#'   \item \code{\link{pfmPreflight}} for the start group (refuses on any failure);
#'   \item the project's scenario-config validator
#'     (\code{analysis/run-groups/validatePFMScenarioConfig.R}), when present;
#'   \item REMIND's own check, \code{Rscript start.R --test <config> startgroup=<group>};
#'   \item the rows and Run-Groups that will start, printed;
#'   \item unless \code{dry}: a \strong{batch manifest} written to \code{batchDir}
#'     (the commit of every repository, the installed \code{pfm}/\code{mrpfm} versions, the
#'     scenario config's md5 and the rows), then \code{Rscript start.R <config>
#'     startgroup=<group>} in the REMIND checkout, its output saved next to the manifest.
#' }
#' The manifest is the per-batch provenance that \code{analysis/coupled/runProvenance.R}
#' otherwise reconstructs after the fact.
#'
#' @param startGroup The scenario config's start group (REMIND's \code{startgroup}).
#' @param remindDir The REMIND checkout to start from. Default: the first \code{remind_pfm*}
#'   of \code{tools/repos.txt} present on this machine.
#' @param scenarioConfig The scenario config, relative to \code{remindDir} or absolute.
#'   Default \code{config/scenario_config_PFM.csv}.
#' @param config Path to the project's \code{config.yml}.
#' @param dry Logical. Run every check and print the plan, but submit nothing. Default
#'   \code{TRUE}: a submission is always an explicit \code{dry = FALSE}.
#' @param preflight Logical. Run \code{\link{pfmPreflight}}. \code{FALSE} only for a
#'   deliberate exception; the manifest records it.
#' @param batchDir Where batch manifests go. Default \code{output/remind-runs/batches} under
#'   the project root.
#' @param verbose Logical.
#' @return Invisibly, a list: \code{rows}, \code{manifest} (path, or \code{NULL} when dry),
#'   \code{submitted} (logical).
#' @seealso \code{\link{pfmPreflight}}
#' @export
#' @author Renato Rodrigues
submitPFM <- function(startGroup, remindDir = NULL, scenarioConfig = "config/scenario_config_PFM.csv",
                      config = "config.yml", dry = TRUE, preflight = TRUE, batchDir = NULL,
                      verbose = TRUE) {
  say <- function(...) if (isTRUE(verbose)) message("[submit] ", ...)
  if (missing(startGroup) || !nzchar(startGroup)) stop("submitPFM: 'startGroup' is required.", call. = FALSE)
  root <- normalizePath(dirname(config), winslash = "/", mustWork = TRUE)
  repos <- .pfmProjectRepos(root)
  remindDir <- normalizePath(remindDir %||% repos$path[grepl("^remind_pfm", repos$name)][1],
                             winslash = "/", mustWork = TRUE)
  scenAbs <- if (grepl("^([A-Za-z]:|/)", scenarioConfig)) scenarioConfig else file.path(remindDir, scenarioConfig)
  if (!file.exists(scenAbs)) stop("submitPFM: no scenario config at ", scenAbs, call. = FALSE)
  batchDir <- batchDir %||% file.path(root, "output", "remind-runs", "batches")
  say("start group ", startGroup, " from ", remindDir, " (", basename(scenAbs), ")",
      if (isTRUE(dry)) " - DRY RUN" else "")

  # 1. preflight
  pf <- NULL
  if (isTRUE(preflight)) {
    pf <- pfmPreflight(config = config, startGroup = startGroup, scenarioConfig = scenAbs,
                       remindDirs = remindDir, verbose = verbose)
  } else {
    say("WARNING: preflight skipped on request; the batch manifest records it")
  }

  # 2. the project's scenario-config validator
  validator <- file.path(root, "analysis", "run-groups", "validatePFMScenarioConfig.R")
  if (file.exists(validator)) {
    v <- .pfmRun(file.path(R.home("bin"), "Rscript"), c(shQuote(validator), shQuote(scenAbs)), wd = root)
    if (v$status != 0) {
      stop("submitPFM: validatePFMScenarioConfig.R reports errors:\n",
           paste(utils::tail(v$out, 20), collapse = "\n"), call. = FALSE)
    }
    say("ok    scenario config validated")
  }

  # 3. REMIND's own test of the start group
  rel <- .pfmRelPath(scenAbs, remindDir)
  t <- .pfmRun(file.path(R.home("bin"), "Rscript"),
               c("start.R", "--test", shQuote(rel), paste0("startgroup=", startGroup)), wd = remindDir)
  nErr <- suppressWarnings(as.integer(sub(".*?([0-9]+) errors?.*", "\\1",
                                          grep("[0-9]+ errors?", t$out, value = TRUE, perl = TRUE))))
  if (t$status != 0 || (length(nErr) && any(nErr > 0, na.rm = TRUE))) {
    stop("submitPFM: start.R --test failed for startgroup=", startGroup, ":\n",
         paste(utils::tail(t$out, 25), collapse = "\n"), call. = FALSE)
  }
  say("ok    start.R --test")

  # 4. what will start
  rows <- .pfmCoupledRows(scenAbs, startGroup, remindDir)
  say(nrow(rows), " coupled row(s):")
  for (i in seq_len(nrow(rows))) {
    say("  ", rows$title[i], "  [group ", rows$pfmGroup[i], ", ", rows$ssp[i], ", ref ", rows$ref[i], "]")
  }
  if (isTRUE(dry)) {
    say("dry run: nothing submitted. Submit with dry = FALSE.")
    return(invisible(list(rows = rows, manifest = NULL, submitted = FALSE)))
  }

  # 5. the batch manifest, then the submission
  id <- paste0(format(Sys.time(), "%Y-%m-%d_%H.%M.%S"), "_", startGroup)
  dir.create(batchDir, showWarnings = FALSE, recursive = TRUE)
  commits <- lapply(stats::setNames(repos$path, repos$name), function(p) {
    g <- function(...) suppressWarnings(system2("git", c("-C", shQuote(p), ...), stdout = TRUE, stderr = TRUE))[1]
    list(commit = g("rev-parse", "HEAD"), branch = g("rev-parse", "--abbrev-ref", "HEAD"))
  })
  man <- list(
    batch = id, startGroup = startGroup, submittedAt = as.character(Sys.time()),
    host = Sys.info()[["nodename"]], user = Sys.getenv("USER", Sys.getenv("USERNAME")),
    remindDir = remindDir, scenarioConfig = scenAbs, scenarioConfigMd5 = unname(tools::md5sum(scenAbs)),
    commits = commits,
    installed = if (!is.null(pf)) pf[pf$check == "installed", c("target", "detail")] else "preflight skipped",
    preflight = if (is.null(pf)) "skipped" else "passed",
    rows = rows
  )
  mf <- file.path(batchDir, paste0(id, ".json"))
  jsonlite::write_json(man, mf, pretty = TRUE, auto_unbox = TRUE)
  say("batch manifest: ", mf)
  s <- .pfmRun(file.path(R.home("bin"), "Rscript"), c("start.R", shQuote(rel), paste0("startgroup=", startGroup)),
               wd = remindDir)
  writeLines(s$out, sub("[.]json$", ".log", mf))
  if (s$status != 0) {
    stop("submitPFM: start.R exited ", s$status, "; see ", sub("[.]json$", ".log", mf), call. = FALSE)
  }
  say("submitted; start.R output: ", sub("[.]json$", ".log", mf))
  invisible(list(rows = rows, manifest = mf, submitted = TRUE))
}

# Run a command in `wd`, returning its output lines and exit status.
.pfmRun <- function(cmd, args, wd) {
  owd <- setwd(wd)
  on.exit(setwd(owd), add = TRUE)
  out <- suppressWarnings(system2(cmd, args, stdout = TRUE, stderr = TRUE))
  list(out = out, status = attr(out, "status") %||% 0L)
}

# `path` relative to `base` when it lies under it, else unchanged.
.pfmRelPath <- function(path, base) {
  p <- normalizePath(path, winslash = "/", mustWork = FALSE)
  b <- paste0(normalizePath(base, winslash = "/", mustWork = FALSE), "/")
  if (startsWith(p, b)) substring(p, nchar(b) + 1L) else p
}
# nolint end
