# nolint start
#' Start a PFM model-group run — locally or as a SLURM job
#'
#' @description
#' The single entry point for running a model group (ADR 0020). Detects whether to submit a
#' SLURM job (PIK cluster) or run in-process, sizes parallelism to the available cores, runs
#' \code{\link{runModelGroup}}, records run statistics + status in the Run-Group manifest, and
#' optionally renders the pfm-reports outputs afterwards.
#'
#' Cluster detection (\code{cluster = "auto"}): if \code{SLURM_JOB_ID} is set we are already
#' inside an allocation and run in-process; else if \code{sbatch} is on \code{PATH} we submit a
#' one-node job; else we run locally. \code{"slurm"} / \code{"local"} force the choice.
#'
#' @param group Character. Run-Group name. Required.
#' @param steps Character subset of \code{c("sweep","robustness","temporal","subnational")}.
#' @param mode \code{"exhaustive"} (default) or \code{"guided"}.
#' @param selectionMethod \code{"levels-first"} (default) or \code{"difference-first"}.
#' @param resultsDir,cacheDir Configurable Results Root / Fit Cache (defaults from options).
#' @param gdxFile Character or NULL. Scenario gdx (forwarded to the sweep).
#' @param nCores Integer or NULL. Cores for the parallel sweep; NULL (default) uses
#'   \code{SLURM_CPUS_PER_TASK} when set, else \code{parallel::detectCores() - 1}.
#' @param cluster \code{"auto"} (default), \code{"slurm"}, or \code{"local"}.
#' @param time,qos,partition,account,mem,chdir SLURM directives (PIK defaults: 24h / short /
#'   standard / default account / node-default mem / \code{/p/tmp/$USER/pfm-runs/<group>}).
#' @param reportsDir Character or NULL. pfm-reports root, used only when \code{render = TRUE}.
#' @param render Logical. After the run, shell out to render the pfm-reports outputs for the
#'   group (requires \code{reportsDir}). Default \code{FALSE}.
#' @param forceRefit Logical. Ignore cached fits. Default \code{FALSE}.
#' @param verbose Logical. Default \code{TRUE}.
#' @param ... Forwarded to \code{\link{runSweep}} (e.g. \code{selectFE}).
#'
#' @return Invisibly: for a submission, \code{list(submitted=TRUE, jobId=, script=)}; for a
#'   local run, \code{list(group=, status=, dir=)}.
#' @seealso \code{\link{runModelGroup}}, \code{\link{runStatus}}, ADR 0020
#' @export
#' @author Renato Rodrigues
startRun <- function(group,
                     steps = c("sweep", "robustness", "temporal", "subnational"),
                     mode = c("exhaustive", "guided"),
                     selectionMethod = c("levels-first", "difference-first"),
                     resultsDir = getOption("pfm.resultsDir", NULL),
                     cacheDir = getOption("pfm.modelDir", NULL),
                     gdxFile = NULL,
                     nCores = NULL,
                     cluster = c("auto", "slurm", "local"),
                     time = "24:00:00", qos = "short", partition = "standard",
                     account = NULL, mem = NULL, chdir = NULL,
                     reportsDir = NULL, render = FALSE,
                     forceRefit = FALSE, verbose = TRUE, ...) {
  mode <- match.arg(mode)
  selectionMethod <- match.arg(selectionMethod)
  cluster <- match.arg(cluster)
  steps <- intersect(c("sweep", "robustness", "temporal", "subnational"), steps)
  if (length(steps) == 0) stop("startRun: no valid steps.", call. = FALSE)
  if (missing(group) || is.null(group) || !nzchar(group)) stop("startRun: 'group' is required.", call. = FALSE)
  if (is.null(resultsDir)) stop("startRun: supply 'resultsDir' or set options(pfm.resultsDir = '...').", call. = FALSE)
  if (is.null(nCores)) nCores <- .pfmDetectCores()
  say <- function(...) if (isTRUE(verbose)) message("[startRun:", group, "] ", ...)

  inJob <- nzchar(Sys.getenv("SLURM_JOB_ID"))
  haveSbatch <- nzchar(Sys.which("sbatch"))
  doSubmit <- switch(cluster,
    auto  = (!inJob && haveSbatch),
    slurm = { if (inJob) FALSE else if (!haveSbatch) stop("cluster='slurm' but 'sbatch' is not on PATH.", call. = FALSE) else TRUE },
    local = FALSE)

  if (doSubmit) {
    return(.submitSlurm(group = group, steps = steps, mode = mode, selectionMethod = selectionMethod,
      resultsDir = resultsDir, cacheDir = cacheDir, gdxFile = gdxFile, nCores = nCores,
      time = time, qos = qos, partition = partition, account = account, mem = mem, chdir = chdir,
      reportsDir = reportsDir, render = render, forceRefit = forceRefit, say = say, dots = list(...)))
  }

  # ── Local (in-process) run ──────────────────────────────────────────────────
  groupDir <- file.path(resultsDir, group)
  dir.create(groupDir, showWarnings = FALSE, recursive = TRUE)
  if (!is.null(cacheDir)) options(pfm.modelDir = cacheDir)
  pkgver <- function(p) tryCatch(as.character(utils::packageVersion(p)), error = function(e) NA_character_)
  t0 <- Sys.time()
  .writeRunGroupManifest(groupDir, group = group, mode = mode, run = list(
    status = "running", startedAt = as.character(t0), endedAt = NULL, seconds = NULL,
    host = Sys.info()[["nodename"]], cluster = if (inJob) "slurm" else "local",
    slurmJobId = if (inJob) Sys.getenv("SLURM_JOB_ID") else NULL,
    nCores = nCores, steps = as.list(steps),
    pfm_version = pkgver("pfm"), mrpfm_version = pkgver("mrpfm")))

  say(if (inJob) "running on SLURM node" else "running locally", " (nCores = ", nCores, "); steps: ",
      paste(steps, collapse = ", "))
  ok <- tryCatch({
    runModelGroup(group = group, steps = steps, resultsDir = resultsDir, cacheDir = cacheDir,
      gdxFile = gdxFile, mode = mode, selectionMethod = selectionMethod, nCores = nCores,
      forceRefit = forceRefit, verbose = verbose, ...)
    TRUE
  }, error = function(e) { say("RUN FAILED: ", conditionMessage(e)); FALSE })

  endedAt <- Sys.time()
  .writeRunGroupManifest(groupDir, group = group, mode = mode, run = list(
    status = if (ok) "completed" else "failed", endedAt = as.character(endedAt),
    seconds = round(as.numeric(difftime(endedAt, t0, units = "secs")), 1)))

  if (ok && isTRUE(render)) {
    if (is.null(reportsDir)) say("render = TRUE but reportsDir not supplied; skipping reports.")
    else .renderReports(reportsDir, group, steps, say)
  }
  say(if (ok) "DONE" else "FAILED", " - ", groupDir)
  invisible(list(group = group, status = if (ok) "completed" else "failed", dir = groupDir))
}

# Internal: cores for the parallel sweep — SLURM_CPUS_PER_TASK when set, else detectCores()-1.
#' @keywords internal
.pfmDetectCores <- function() {
  s <- suppressWarnings(as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", "")))
  if (length(s) == 1 && !is.na(s) && s >= 1) return(s)
  n <- tryCatch(parallel::detectCores(), error = function(e) 1L)
  if (is.na(n)) n <- 1L
  max(1L, n - 1L)
}

# Internal: serialise an R value to a literal for the generated job script.
#' @keywords internal
.rlit <- function(x) {
  if (is.null(x)) return("NULL")
  if (is.logical(x)) return(if (isTRUE(x)) "TRUE" else "FALSE")
  if (is.numeric(x)) return(as.character(x))
  if (length(x) != 1) return(paste0("c(", paste(vapply(x, .rlit, character(1)), collapse = ", "), ")"))
  paste0('"', gsub('"', '\\\\"', x), '"')
}

# Internal: write the submit script + job script and sbatch it (ADR 0020). Returns invisibly
# list(submitted=TRUE, jobId=, script=).
#' @keywords internal
.submitSlurm <- function(group, steps, mode, selectionMethod, resultsDir, cacheDir, gdxFile,
                         nCores, time, qos, partition, account, mem, chdir, reportsDir, render,
                         forceRefit, say, dots) {
  abspath <- function(p) if (is.null(p)) NULL else normalizePath(p, winslash = "/", mustWork = FALSE)
  resultsDir <- abspath(resultsDir); cacheDir <- abspath(cacheDir)
  gdxFile <- abspath(gdxFile); reportsDir <- abspath(reportsDir)
  user <- Sys.getenv("USER", Sys.getenv("USERNAME", "user"))
  if (is.null(chdir)) chdir <- file.path("/p/tmp", user, "pfm-runs", group)
  dir.create(chdir, showWarnings = FALSE, recursive = TRUE)         # must exist before sbatch
  dir.create(file.path(resultsDir, group), showWarnings = FALSE, recursive = TRUE)

  # Job R script: re-invoke startRun in local mode on the compute node.
  call <- sprintf(paste0(
    "pfm::startRun(group=%s, steps=%s, mode=%s, selectionMethod=%s, resultsDir=%s, cacheDir=%s, ",
    "gdxFile=%s, nCores=%d, cluster=\"local\", forceRefit=%s, render=%s, reportsDir=%s%s)"),
    .rlit(group), .rlit(steps), .rlit(mode), .rlit(selectionMethod), .rlit(resultsDir),
    .rlit(cacheDir), .rlit(gdxFile), nCores, .rlit(forceRefit), .rlit(render), .rlit(reportsDir),
    if (length(dots)) paste0(", ", paste(sprintf("%s=%s", names(dots),
      vapply(dots, .rlit, character(1))), collapse = ", ")) else "")
  jobR <- file.path(chdir, paste0("pfm-", group, "-job.R"))
  writeLines(c("suppressMessages(library(pfm))", call), jobR)

  sbatchLines <- c(
    "#!/bin/bash",
    sprintf("#SBATCH --job-name=pfm-%s", group),
    sprintf("#SBATCH --qos=%s", qos),
    sprintf("#SBATCH --partition=%s", partition),
    sprintf("#SBATCH --time=%s", time),
    "#SBATCH --nodes=1",
    "#SBATCH --ntasks=1",
    sprintf("#SBATCH --cpus-per-task=%d", nCores),
    if (!is.null(account)) sprintf("#SBATCH --account=%s", account),
    if (!is.null(mem)) sprintf("#SBATCH --mem=%s", mem),
    sprintf("#SBATCH --chdir=%s", chdir),
    sprintf("#SBATCH --output=pfm-%s-%%j.out", group),
    sprintf("#SBATCH --error=pfm-%s-%%j.err", group),
    "",
    sprintf("Rscript %s", shQuote(jobR))
  )
  sbatchLines <- sbatchLines[!vapply(sbatchLines, is.null, logical(1))]
  subScript <- file.path(chdir, paste0("pfm-", group, ".sub"))
  writeLines(sbatchLines, subScript)

  say("submitting SLURM job: qos=", qos, " partition=", partition, " time=", time,
      " cpus-per-task=", nCores, " chdir=", chdir)
  out <- tryCatch(system2("sbatch", shQuote(subScript), stdout = TRUE, stderr = TRUE),
                  error = function(e) paste("sbatch-error:", conditionMessage(e)))
  jobId <- sub(".*Submitted batch job ([0-9]+).*", "\\1", paste(out, collapse = " "))
  if (!grepl("^[0-9]+$", jobId)) {
    say("submission may have failed: ", paste(out, collapse = " | "))
    jobId <- NA_character_
  } else {
    say("submitted batch job ", jobId)
  }
  # Record the submission in the Run-Group manifest so runStatus can query SLURM.
  .writeRunGroupManifest(file.path(resultsDir, group), group = group, mode = mode, run = list(
    status = "submitted", submittedAt = as.character(Sys.time()),
    cluster = "slurm", slurmJobId = jobId, nCores = nCores,
    steps = as.list(steps), submitScript = subScript))
  invisible(list(submitted = TRUE, jobId = jobId, script = subScript))
}

# Internal: render the pfm-reports outputs for a group (optional shell-out, ADR 0018/0020).
# Maps the steps that ran to the reports that consume them; never makes pfm depend on
# pfm-reports (it only invokes run.R scripts in the supplied directory).
#' @keywords internal
.renderReports <- function(reportsDir, group, steps, say) {
  base <- c("selection" = "reports/selection/run.R",
            "results-adoption" = "reports/results-adoption/run.R",
            "results-stringency" = "reports/results-stringency/run.R",
            "publication" = "reports/publication/run.R")
  reps <- names(base)
  if (any(c("robustness", "temporal") %in% steps)) reps <- c(reps, "robustness")
  if ("subnational" %in% steps) reps <- c(reps, "subnational")
  extra <- c("robustness" = "reports/robustness/run.R", "subnational" = "reports/subnational/run.R")
  paths <- c(base, extra)
  oldwd <- setwd(reportsDir); on.exit(setwd(oldwd), add = TRUE)
  for (rep in reps) {
    rp <- paths[[rep]]
    if (!file.exists(rp)) { say("report '", rep, "' (", rp, ") not found; skipping."); next }
    say("rendering report: ", rep)
    st <- tryCatch(system2("Rscript", c(rp, paste0("--group=", group), paste0("--reportName=", group)),
                           stdout = TRUE, stderr = TRUE),
                   error = function(e) paste("render-error:", conditionMessage(e)))
    if (!is.null(attr(st, "status")) && attr(st, "status") != 0) {
      say("report '", rep, "' FAILED (see its output/logs).")
    }
  }
}
# nolint end
