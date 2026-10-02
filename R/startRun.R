# nolint start
#' Start a PFM model-group run — locally or as a SLURM job
#'
#' @description
#' The single entry point for running a model group (ADR 0020). Detects whether to submit a
#' SLURM job (PIK cluster) or run in-process, sizes parallelism to the available cores, runs
#' \code{\link{runModelGroup}}, and records run statistics + status in the Run-Group manifest.
#'
#' Cluster detection (\code{cluster = "auto"}): if \code{SLURM_JOB_ID} is set we are already
#' inside an allocation and run in-process; else if \code{sbatch} is on \code{PATH} we submit a
#' one-node job; else we run locally. \code{"slurm"} / \code{"local"} force the choice.
#'
#' @param group Character. Run-Group name. Required.
#' @param steps Character subset of \code{c("sweep","robustness","temporal","subnational",
#'   "difference-first","projection","selection-bootstrap","pfm-sweep","pfm-projection",
#'   "pfm-agreement")}. Default is the first four;
#'   \code{"difference-first"} is the ADR 0014 alternative-selection comparison,
#'   \code{"projection"} writes the per-scenario feasibility projections (ADR 0035; needs a
#'   scenario gdx) and \code{"selection-bootstrap"} the selection-uncertainty bootstrap — all
#'   off by default and enabled by the \code{--paper} publication workflow. The
#'   \code{pfm-*} steps are the Political Feasibility Model pipeline (ADR 0036:
#'   \code{\link{runPFMSweep}}, \code{\link{runPFMProjection}},
#'   \code{\link{runPFMEstimatorAgreement}}, \code{\link{runPFMTemporalValidation}},
#'   \code{\link{runPFMFrontier}}, \code{\link{runPFMIV}}, \code{\link{runPFMInfluence}},
#'   \code{\link{runPFMSectorSpeeds}}, \code{\link{runPFMSelectionBootstrap}}); run them
#'   in their OWN Run-Group (e.g. \code{group = "psm-exhaustive"}), never mixed into a
#'   price-model group.
#' @param mode \code{"exhaustive"} (default) or \code{"guided"}.
#' @param selectionMethod \code{"levels-first"} (default) or \code{"difference-first"}.
#' @param resultsDir,modelDir Configurable Results Root / Fit Cache (the ADR 0009 model store;
#'   defaults from options).
#' @param cachefolder Character or NULL. The \strong{madrat} data-cache folder (distinct from
#'   \code{modelDir}); when set, madrat reads inputs from there on every node.
#' @param gdxFile Character or NULL. Gating-scenario gdx (forwarded to the sweep's
#'   selection sanity gate). Set it to the gating scenario's gdx
#'   (\code{\link{scenarioGatingGdx}}) when projecting multiple scenarios.
#' @param scenarios Optional list of Policy Scenario descriptors (ADR 0035), e.g.
#'   the \code{$scenarios} of \code{\link{parseScenarioRegistry}}. Forwarded to the
#'   \code{projection} step's fan-out; \code{NULL} = legacy single scenario.
#' @param nCores Integer or NULL. Cores for the parallel sweep; NULL (default) uses
#'   \code{SLURM_CPUS_PER_TASK} when set, else \code{parallel::detectCores() - 1}.
#' @param cluster \code{"auto"} (default), \code{"slurm"}, or \code{"local"}.
#' @param time,qos,partition,account,mem,chdir SLURM directives (PIK defaults: 24h / short /
#'   standard / default account / node-default mem / \code{resultsDir/<group>}).
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
                     resultsDir = getOption("pfm.resultsDir", "output"),
                     modelDir = getOption("pfm.modelDir", "output"),
                     cachefolder = getOption("pfm.cachefolder", "data/cache"),
                     gdxFile = getOption("pfm.gdxFile", "data/fulldata.gdx"),
                     scenarios = NULL,
                     nCores = NULL,
                     cluster = c("auto", "slurm", "local"),
                     time = "24:00:00", qos = "short", partition = "standard",
                     account = NULL, mem = NULL, chdir = NULL,
                     bootstrapResamples = 200L, bootstrapDetail = "channel", bootstrapTopK = 40L,
                     forceRefit = FALSE, resume = FALSE, verbose = TRUE, ...) {
  mode <- match.arg(mode)
  selectionMethod <- match.arg(selectionMethod)
  cluster <- match.arg(cluster)
  steps <- .pfmLegacySteps(steps)
  requestedSteps <- steps
  validSteps <- c("sweep", "robustness", "temporal", "subnational", "difference-first",
                  "projection", "selection-bootstrap",
                  # ADR 0036 PFM pipeline. THIS VECTOR IS IN DEPENDENCY ORDER AND MUST STAY
                  # THAT WAY: steps are filtered with intersect(validSteps, steps), which
                  # re-sorts the caller's list into THIS order. Declaration order is therefore
                  # execution order. Until 2026-08-18 pfm-projection sat second, so it ran
                  # before pfm-frontier and before pfm-donor - silently projecting against a
                  # stale frontier whenever one happened to exist, and failing outright when
                  # one did not.
                  "pfm-sweep", "pfm-frontier", "pfm-temporal", "pfm-sector-speeds",
                  # pfm-inference and pfm-regfront were added to pfmStepArtifacts() and to
                  # pfmRun()'s diagnostics/downstream stages on 2026-09-15, but NOT to this
                  # whitelist - so startRun silently DROPPED them and `stage = "diagnostics"`
                  # produced no inference.rds. Fixed 2026-09-15; keep the three lists in step.
                  "pfm-agreement", "pfm-iv", "pfm-influence", "pfm-inference", "pfm-replay",
                  "pfm-donor", "pfm-projection", "pfm-coupling-bound",
                  "pfm-selection-bootstrap", "pfm-regfront", "pfm-remind-inputs",
                  # aliases — expanded to ordered step lists inside runModelGroup
                  "pfm-downstream", "pfm-all")
  steps <- intersect(validSteps, steps)
  droppedSteps <- setdiff(requestedSteps, validSteps)
  if (length(steps) == 0) stop("startRun: no valid steps.", call. = FALSE)
  if (missing(group) || is.null(group) || !nzchar(group)) stop("startRun: 'group' is required.", call. = FALSE)
  if (is.null(resultsDir)) stop("startRun: supply 'resultsDir' or set options(pfm.resultsDir = '...').", call. = FALSE)
  if (is.null(nCores)) nCores <- .pfmDetectCores()
  say <- function(...) if (isTRUE(verbose)) message("[startRun:", group, "] ", ...)

  # ── Diagnostics (ADR 0035): make step-filtering + scenario wiring visible in the log, so a
  # silently-dropped step (e.g. an installed build whose whitelist predates the 'projection' step)
  # or an unresolved scenario registry is obvious rather than mysterious. ──
  say("pfm version ", utils::packageVersion("pfm"),
      " | 'projection' is a recognised step: ", "projection" %in% validSteps)
  say("steps requested: ", paste(requestedSteps, collapse = ", "))
  say("steps to run:    ", paste(steps, collapse = ", "))
  if (length(droppedSteps)) say("WARNING: requested step(s) NOT recognised by this build and DROPPED: ",
      paste(droppedSteps, collapse = ", "),
      " -- reinstall pfm from current source if you expected them (e.g. 'projection').")
  if ("projection" %in% requestedSteps && !("projection" %in% steps))
    say("WARNING: 'projection' was requested but will NOT run -> no scenario projections will be written.")
  if (is.null(scenarios) || !length(scenarios)) {
    say("scenarios: none supplied (legacy single-scenario projection from gdxFile = ", gdxFile %||% "NULL", ")")
  } else {
    gating <- names(Filter(function(s) isTRUE(s$gating), scenarios))
    say("scenarios (", length(scenarios), "): ", paste(names(scenarios), collapse = ", "),
        " | gating: ", if (length(gating)) gating[[1]] else "NONE",
        " | sweep sanity-gate gdx: ", gdxFile %||% "NULL")
    for (s in scenarios) say("  - scenario '", s$id, "' gdx=", s$gdx %||% "NULL",
        " exists=", if (!is.null(s$gdx)) file.exists(s$gdx) else FALSE,
        " mapping=", s$gdxRegionMapping %||% "(default)")
  }

  inJob <- nzchar(Sys.getenv("SLURM_JOB_ID"))
  haveSbatch <- nzchar(Sys.which("sbatch"))
  doSubmit <- switch(cluster,
    auto  = (!inJob && haveSbatch),
    slurm = { if (inJob) FALSE else if (!haveSbatch) stop("cluster='slurm' but 'sbatch' is not on PATH.", call. = FALSE) else TRUE },
    local = FALSE)

  if (doSubmit) {
    return(.submitSlurm(group = group, steps = steps, mode = mode, selectionMethod = selectionMethod,
      resultsDir = resultsDir, modelDir = modelDir, cachefolder = cachefolder, gdxFile = gdxFile,
      scenarios = scenarios, nCores = nCores,
      time = time, qos = qos, partition = partition, account = account, mem = mem, chdir = chdir,
      forceRefit = forceRefit, resume = resume,
      bootstrapResamples = bootstrapResamples, bootstrapDetail = bootstrapDetail,
      bootstrapTopK = bootstrapTopK, say = say, dots = list(...)))
  }

  # ── Local (in-process) run ──────────────────────────────────────────────────
  groupDir <- file.path(resultsDir, group)
  dir.create(groupDir, showWarnings = FALSE, recursive = TRUE)
  if (!is.null(modelDir)) options(pfm.modelDir = modelDir)
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
  # Collected from the run's audit so the final line of the log can name any step that
  # produced nothing. Without this the audit lives only in the middle of the log, which is
  # exactly where it gets lost.
  auditIncomplete <- character(0)
  ok <- tryCatch({
    r <- runModelGroup(group = group, steps = steps,
      resultsDir = resultsDir, modelDir = modelDir, cachefolder = cachefolder, gdxFile = gdxFile,
      scenarios = scenarios,
      mode = mode, selectionMethod = selectionMethod, nCores = nCores, forceRefit = forceRefit,
      resume = resume, bootstrapResamples = bootstrapResamples, bootstrapDetail = bootstrapDetail,
      bootstrapTopK = bootstrapTopK, verbose = verbose, ...)
    auditIncomplete <- unique(c(attr(r, "incomplete"), attr(r, "notRefreshed")))
    TRUE
  }, error = function(e) { say("RUN FAILED: ", conditionMessage(e)); FALSE })

  endedAt <- Sys.time()
  # "completed" must not mean "every requested step ran". A step that self-skips on a missing
  # input returns quietly and the run exits 0; recording that as completed is how a job comes
  # back looking fine with an artifact nobody wrote. Third status for exactly that case.
  runStatus <- if (!ok) "failed" else if (length(auditIncomplete)) "incomplete" else "completed"
  .writeRunGroupManifest(groupDir, group = group, mode = mode, run = list(
    status = runStatus, endedAt = as.character(endedAt),
    incompleteSteps = as.list(auditIncomplete),
    seconds = round(as.numeric(difftime(endedAt, t0, units = "secs")), 1)))

  say(switch(runStatus, completed = "DONE", incomplete = "DONE WITH GAPS", failed = "FAILED"),
      " - ", groupDir)
  if (length(auditIncomplete)) {
    # Repeated as the LAST line of the log on purpose: the per-step audit is printed in the
    # middle of a long run and is easy to scroll past.
    say("!! ", length(auditIncomplete), " step(s) produced no fresh artifact: ",
        paste(auditIncomplete, collapse = ", "))
    say("!! Do not treat this Run-Group as complete. See the audit block above for the reason.")
  }
  invisible(list(group = group, status = runStatus, dir = groupDir,
                 incompleteSteps = auditIncomplete))
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
.submitSlurm <- function(group, steps, mode, selectionMethod, resultsDir, modelDir, cachefolder,
                         gdxFile, scenarios = NULL, nCores, time, qos, partition, account, mem, chdir,
                         forceRefit, resume = FALSE,
                         bootstrapResamples = 200L,
                         bootstrapDetail = "channel", bootstrapTopK = 40L, say, dots) {
  # normalizePath(mustWork = FALSE) returns a NON-EXISTENT path unchanged, so a
  # relative resultsDir stayed relative whenever the directory had not been created
  # yet — which is the normal case for a new Run-Group, since dir.create() runs below.
  # That made `--chdir` relative too, and SLURM then resolved the job-script path
  # against the directory it had just changed into: `Rscript output/<g>/job.R` run from
  # inside `output/<g>` looks for `output/<g>/output/<g>/job.R` and the job dies at
  # once with "cannot open file". Anchor to getwd() FIRST, then normalise.
  abspath <- function(p) {
    if (is.null(p) || !length(p) || !nzchar(p)) return(p)
    if (!grepl("^([A-Za-z]:|/|\\\\)", p)) p <- file.path(getwd(), p)
    normalizePath(p, winslash = "/", mustWork = FALSE)
  }
  resultsDir <- abspath(resultsDir); modelDir <- abspath(modelDir)
  cachefolder <- abspath(cachefolder); gdxFile <- abspath(gdxFile)
  user <- Sys.getenv("USER", Sys.getenv("USERNAME", "user"))
  if (is.null(chdir)) chdir <- file.path(resultsDir, group)
  dir.create(chdir, showWarnings = FALSE, recursive = TRUE)         # must exist before sbatch
  dir.create(file.path(resultsDir, group), showWarnings = FALSE, recursive = TRUE)

  # Job R script: re-invoke startRun in local mode on the compute node. The Policy Scenario
  # registry (ADR 0035) is a nested named list, so it is serialised with deparse() rather than
  # .rlit (which only handles atomic vectors); deparse backtick-quotes the hyphenated ids.
  scenLit <- if (is.null(scenarios)) "" else
    paste0(", scenarios=", paste(deparse(scenarios), collapse = " "))
  dotsLit <- if (length(dots)) paste0(", ", paste(sprintf("%s=%s", names(dots),
    vapply(dots, .rlit, character(1))), collapse = ", ")) else ""
  call <- sprintf(paste0(
    "pfm::startRun(group=%s, steps=%s, mode=%s, selectionMethod=%s, resultsDir=%s, modelDir=%s, ",
    "cachefolder=%s, gdxFile=%s, nCores=%d, cluster=\"local\", forceRefit=%s, resume=%s, ",
    "bootstrapResamples=%d, bootstrapDetail=%s, bootstrapTopK=%d%s)"),
    .rlit(group), .rlit(steps), .rlit(mode), .rlit(selectionMethod), .rlit(resultsDir),
    .rlit(modelDir), .rlit(cachefolder), .rlit(gdxFile), nCores, .rlit(forceRefit), .rlit(resume),
    as.integer(bootstrapResamples), .rlit(bootstrapDetail), as.integer(bootstrapTopK),
    paste0(scenLit, dotsLit))
  jobR <- file.path(chdir, paste0("pfm-", group, "-job.R"))
  # The madrat source folder is a session option (set by pfmRun from config.yml `madrat:
  # sourcefolder`); a fresh job process would not have it.
  sf <- getOption("pfm.sourcefolder", NULL)
  optLine <- if (!is.null(sf) && nzchar(sf)) sprintf("options(pfm.sourcefolder = %s)", .rlit(abspath(sf))) else NULL
  writeLines(c("suppressMessages(library(pfm))", optLine, call), jobR)

  # sbatch --chdir changes the working directory BEFORE the payload runs, so every
  # path in the script must be absolute or the job dies on the node with "cannot open
  # file" — a failure you only see minutes or hours later, after the queue wait.
  # Fail here, at submit time, where it is cheap and the message can say why.
  isAbs <- function(p) grepl("^([A-Za-z]:|/|\\\\)", p)
  bad <- c(chdir = chdir, jobScript = jobR, resultsDir = resultsDir,
           modelDir = modelDir)
  bad <- bad[!vapply(bad, isAbs, logical(1))]
  if (length(bad)) {
    stop(".submitSlurm: these must be absolute paths but are relative:\n",
         paste0("    ", names(bad), " = ", bad, collapse = "\n"), "\n",
         "  With --chdir set, a relative path resolves against the job's new working\n",
         "  directory, so the job would fail immediately. This usually means an OLD\n",
         "  installed build of pfm is being used: reinstall with\n",
         "    Rscript -e 'devtools::install(\"pfm\", upgrade=\"never\", quick=TRUE)'",
         call. = FALSE)
  }

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
    if (any(grepl("Invalid qos", out, ignore.case = TRUE))) {
      say("hint: SLURM rejected qos='", qos, "' for partition='", partition, "'. Your account/",
          "partition may not be associated with that QOS, or it needs a different partition. ",
          "Check: sacctmgr show assoc user=$USER format=Partition,QOS%80  (and ",
          "scontrol show partition ", partition, " | grep -i qos). Resubmit with a permitted ",
          "--qos=/--partition= (e.g. --qos=short).")
    }
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
# nolint end
