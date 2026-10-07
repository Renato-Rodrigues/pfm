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
#'   \item unless \code{compile = FALSE}: REMIND's GAMS compile of every row of the group,
#'     \code{Rscript start.R --gamscompile <config> startgroup=<group>} (compile only, with the run's
#'     inputs; the listings go to \code{output/gamscompile/}). \code{--test} checks the
#'     configuration, not the GAMS code: on 2026-10-07 a compile error in the module stopped four
#'     runs of a submitted chain;
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
#' @param compile Logical. Compile every row with GAMS before submitting (default \code{TRUE};
#'   needs REMIND's input data, i.e. the cluster).
#' @param slurmConfig The SLURM setup for rows of the start group that set none:
#'   \code{"priority"} (REMIND's choice 5, 12 tasks), \code{"standby"} (choice 1, 12 tasks), a
#'   choice number \code{"1"}-\code{"16"} of REMIND's \code{choose_slurmConfig}, or a full sbatch
#'   string. Required when any row of the group has an empty \code{slurmConfig}: \code{start.R}
#'   would otherwise ask on a terminal this function captures, and wait unseen. Rows that set
#'   their own keep it.
#' @param batchDir Where batch manifests go. Default \code{output/remind-runs/batches} under
#'   the project root.
#' @param verbose Logical.
#' @return Invisibly, a list: \code{rows}, \code{manifest} (path, or \code{NULL} when dry),
#'   \code{submitted} (logical).
#' @seealso \code{\link{pfmPreflight}}
#' @export
#' @author Renato Rodrigues
submitPFM <- function(startGroup, remindDir = NULL, scenarioConfig = "config/scenario_config_PFM.csv",
                      config = "config.yml", dry = TRUE, preflight = TRUE, compile = TRUE, slurmConfig = NULL, batchDir = NULL,
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
  slurmArg <- .pfmSlurmArg(slurmConfig, .pfmRowsWithoutSlurm(scenAbs, startGroup))
  if (length(slurmArg)) say("slurmConfig for rows without one: ", attr(slurmArg, "value"))

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
               c("start.R", "--test", shQuote(rel), paste0("startgroup=", startGroup), slurmArg), wd = remindDir)
  nErr <- suppressWarnings(as.integer(sub(".*?([0-9]+) errors?.*", "\\1",
                                          grep("[0-9]+ errors?", t$out, value = TRUE, perl = TRUE))))
  if (t$status != 0 || (length(nErr) && any(nErr > 0, na.rm = TRUE))) {
    stop("submitPFM: start.R --test failed for startgroup=", startGroup, ":\n",
         paste(utils::tail(t$out, 25), collapse = "\n"), call. = FALSE)
  }
  say("ok    start.R --test")

  # 3b. GAMS: compile every row of the group (start.R exits non-zero on any FAIL)
  if (isTRUE(compile)) {
    g <- .pfmRun(file.path(R.home("bin"), "Rscript"),
                 c("start.R", "--gamscompile", shQuote(rel), paste0("startgroup=", startGroup)), wd = remindDir)
    fails <- grep("FAIL ", g$out, value = TRUE, fixed = TRUE)
    if (g$status != 0 || length(fails)) {
      stop("submitPFM: GAMS compile failed for startgroup=", startGroup, " (listings in ",
           file.path(remindDir, "output", "gamscompile"), "):\n",
           paste(utils::tail(c(fails, g$out), 25), collapse = "\n"), call. = FALSE)
    }
    say("ok    start.R --gamscompile (", length(grep(" OK  ", g$out, fixed = TRUE)), " compiled)")
  } else {
    say("WARNING: GAMS compile skipped on request; the batch manifest records it")
  }

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
    gamsCompile = if (isTRUE(compile)) "passed" else "skipped",
    slurmConfig = if (length(slurmArg)) attr(slurmArg, "value") else "set by every row",
    rows = rows
  )
  mf <- file.path(batchDir, paste0(id, ".json"))
  jsonlite::write_json(man, mf, pretty = TRUE, auto_unbox = TRUE)
  say("batch manifest: ", mf)
  s <- .pfmRun(file.path(R.home("bin"), "Rscript"), c("start.R", shQuote(rel), paste0("startgroup=", startGroup), slurmArg),
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

# Titles of the start group's rows (coupled or not) whose slurmConfig is empty.
.pfmRowsWithoutSlurm <- function(scenarioConfig, startGroup) {
  sc <- utils::read.csv2(scenarioConfig, check.names = FALSE, stringsAsFactors = FALSE,
                         comment.char = "#", na.strings = "")
  start <- as.character(sc$start %||% rep(NA_character_, nrow(sc)))
  inGroup <- if (identical(startGroup, "*")) !is.na(start) & start != "0" else
    grepl(paste0("(^|,)", startGroup, "($|,)"), start, perl = TRUE)
  slurm <- if ("slurmConfig" %in% names(sc)) trimws(as.character(sc$slurmConfig)) else rep(NA_character_, nrow(sc))
  as.character(sc$title[inGroup & (is.na(slurm) | !nzchar(slurm))])
}

# The start.R argument for `slurmConfig`, or character(0) when every row sets its own. A missing
# choice with rows that need one is an error: start.R would prompt on a captured terminal.
.pfmSlurmArg <- function(slurmConfig, missingRows) {
  if (!length(missingRows)) return(character(0))
  if (is.null(slurmConfig) || !nzchar(slurmConfig)) {
    stop("submitPFM: ", length(missingRows), " row(s) of the start group set no slurmConfig (",
         paste(utils::head(missingRows, 4), collapse = ", "), if (length(missingRows) > 4) ", ...", ").\n",
         "  Pass slurmConfig = \"priority\" or \"standby\" (12 tasks), a REMIND choice \"1\"-\"16\", ",
         "or a full sbatch string - start.R would otherwise ask on a terminal it cannot show.", call. = FALSE)
  }
  v <- switch(slurmConfig, priority = "5", standby = "1", slurmConfig)
  if (!grepl("^([1-9]|1[0-6])$", v) && !grepl("^--", v)) {
    stop("submitPFM: slurmConfig '", slurmConfig, "' is not \"priority\", \"standby\", a choice 1-16 ",
         "or an sbatch string starting with '--'.", call. = FALSE)
  }
  structure(paste0("slurmConfig=", shQuote(v)), value = v)
}

# `path` relative to `base` when it lies under it, else unchanged.
.pfmRelPath <- function(path, base) {
  p <- normalizePath(path, winslash = "/", mustWork = FALSE)
  b <- paste0(normalizePath(base, winslash = "/", mustWork = FALSE), "/")
  if (startsWith(p, b)) substring(p, nchar(b) + 1L) else p
}
# nolint end
