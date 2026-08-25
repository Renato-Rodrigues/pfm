# nolint start
#' The single entry point: run any part of the PFM pipeline, interactively or not
#'
#' @description
#' One console entry point for everything between raw data and the folder REMIND
#' reads. Called with no arguments in an interactive session it asks for each
#' setting, showing the available options and a sensible default you can accept with
#' Enter. Called with arguments it runs straight through, so the same function serves
#' a SLURM job and a person at a prompt.
#'
#' This exists so that nothing outside the package is needed to drive the pipeline.
#' The shell scripts it replaces re-implemented step ordering, resume logic and
#' preflight in bash — untestable, invisible to \code{R CMD check}, and free to drift
#' from the package. Anything worth doing twice belongs in a function.
#'
#' @section Stages:
#' \describe{
#'   \item{\code{sweep}}{Estimate and select: the model selection sweep, frontier,
#'     temporal validation and sector speeds. Produces the deployed spec.}
#'   \item{\code{diagnostics}}{Inference and validation of that spec: estimator
#'     agreement and wild-cluster p-values, the shift-share IV, leave-one-country-out
#'     influence, and the historical-replay gate. Needed before any number from the
#'     Run-Group is quotable, and before any coupled claim.}
#'   \item{\code{downstream}}{Everything the coupling needs from a finished sweep:
#'     donor assumptions, projection fan-out, coupling bound, selection bootstrap.}
#'   \item{\code{remind}}{Assemble the self-contained \code{pfm-data} folder REMIND's
#'     \code{preparePFM.R} consumes.}
#'   \item{\code{all}}{Every step above, in dependency order — raw data to REMIND
#'     inputs.}
#'   \item{\code{custom}}{Choose individual steps.}
#' }
#'
#' @param group Run-Group name. Interactively: offered from the existing groups under
#'   \code{resultsDir}, or type a new name to start a fresh one.
#' @param stage One of \code{"all"}, \code{"sweep"}, \code{"diagnostics"},
#'   \code{"downstream"}, \code{"remind"}, \code{"custom"}.
#' @param steps Explicit step vector. Overrides \code{stage} when given.
#' @param cluster \code{"auto"} (SLURM when \code{sbatch} is on PATH), \code{"slurm"}
#'   or \code{"local"}.
#' @param resultsDir,modelDir Run-Group locations.
#' @param remindDir Destination for the REMIND input folder.
#' @param nCores Cores. \code{NULL} lets the launcher size the job.
#' @param resume Skip steps whose artifacts already exist.
#' @param clean What to delete before running. \code{"none"} (default) keeps
#'   everything; \code{"steps"} removes the artifacts of the steps about to run;
#'   \code{"group"} removes every step artifact in the Run-Group. Use it whenever the
#'   inputs changed underneath a finished group — new code, a rebuilt panel, a
#'   different gdx — because \code{resume} only asks whether a file exists, not
#'   whether it is still valid. Never touches \code{models/} or \code{panels/}: those
#'   are content-addressed caches shared across groups, and clearing them is what
#'   turns a 20-minute re-run into a multi-hour one.
#' @param priority Logical or \code{NULL}. On a SLURM submit host, schedule on the
#'   high-priority QOS and auto-size cores/memory/walltime to the detected allowance
#'   (ADR 0031). \code{NULL} (default) asks interactively, and takes it when available
#'   otherwise; \code{FALSE} keeps the ordinary queue. Passing an explicit \code{qos}
#'   through \code{...} disables it. A large job on the default \code{short} QOS
#'   queues behind everything — this is usually the difference between starting now
#'   and starting tomorrow.
#' @param config Path to the scenario-registry YAML.
#' @param ask Force the interactive wizard on (\code{TRUE}) or off (\code{FALSE}).
#'   Default: interactive sessions ask only for what was not supplied.
#' @param dryRun Print the resolved plan and stop without running.
#' @param ... Passed through to \code{\link{startRun}} (e.g. \code{qos}, \code{time}).
#'
#' @return Invisibly, the resolved settings list.
#'
#' @examples
#' \dontrun{
#' pfmRun()                                          # ask for everything
#' pfmRun(group = "v1", stage = "downstream")
#' pfmRun(group = "v1", stage = "diagnostics", cluster = "slurm")
#' pfmRun(group = "v1", stage = "all", cluster = "slurm")
#' pfmRun(group = "v1", stage = "remind", remindDir = "../pfm-data")
#' }
#' @author Renato Rodrigues
#' @export
pfmRun <- function(group = NULL,
                   stage = NULL,
                   steps = NULL,
                   cluster = NULL,
                   resultsDir = getOption("pfm.resultsDir", "output"),
                   modelDir = getOption("pfm.modelDir", "output"),
                   remindDir = NULL,
                   nCores = NULL,
                   resume = NULL,
                   clean = NULL,
                   priority = NULL,
                   config = NULL,
                   ask = NULL,
                   dryRun = FALSE,
                   ...) {

  stageSteps <- list(
    sweep       = c("psm-sweep", "psm-frontier", "psm-temporal", "psm-sector-speeds"),
    # Inference and validation of the SELECTED spec. Not optional extras: the
    # wild-cluster p-values in `estimator-agreement.rds` are the only quotable
    # inference at this cluster count, and `psm-replay` is the gate a coupled claim
    # depends on. They ran only via `custom` until 2026-08-14, which is how
    # Run-Group v1 came to be complete-looking and unciteable.
    diagnostics = c("psm-agreement", "psm-iv", "psm-influence", "psm-replay"),
    downstream  = c("psm-donor", "psm-projection", "psm-coupling-bound",
                    "psm-selection-bootstrap"),
    remind      = "psm-remind-inputs",
    custom      = character(0))

  # "all" means every step, in dependency order — not "every stage I happened to
  # list first". Diagnostics sit after the sweep because they read the selected spec.
  stageSteps$all <- c(stageSteps$sweep, stageSteps$diagnostics,
                      stageSteps$downstream, stageSteps$remind)
  allSteps <- stageSteps$all

  # Testing isatty() would exclude piped answers, which is a legitimate way to drive
  # this. So: always ATTEMPT to prompt when arguments are missing, and let end-of-input
  # be the signal that nobody is there — readIn() returns NA, and the prompt turns that
  # into an error naming the arguments to pass instead. A SLURM job therefore fails
  # with instructions rather than hanging on a read that will never be answered.
  interactiveRun <- ask %||% (is.null(steps) && (is.null(group) || is.null(stage)))
  # ONE connection, opened lazily and reused. readLines(con = "stdin") opens a fresh
  # connection per call, so the second question read EOF no matter how many answers
  # were waiting — the wizard could ask exactly one thing and then give up.
  stdinCon <- NULL
  on.exit(if (!is.null(stdinCon)) try(close(stdinCon), silent = TRUE), add = TRUE)
  readIn <- function() {
    if (interactive()) return(trimws(readline()))
    if (is.null(stdinCon)) {
      stdinCon <<- tryCatch(file("stdin", "r"), error = function(e) NULL)
      if (is.null(stdinCon)) return(NA_character_)
    }
    ln <- tryCatch(readLines(stdinCon, n = 1L), error = function(e) character(0))
    if (!length(ln)) return(NA_character_)
    trimws(ln[[1]])
  }
  noInput <- function() {
    stop("pfmRun: needs an answer but stdin is empty (not a terminal).\n",
         "  Pass the settings instead, e.g.\n",
         "    pfmRun(group = \"<run-group>\", stage = \"downstream\", ",
         "config = \"config.yml\")\n",
         "  Stages: all | sweep | diagnostics | downstream | remind | custom",
         call. = FALSE)
  }

  # ── prompt helpers ──────────────────────────────────────────────────────────
  hr <- function(ch = "-") message(strrep(ch, 66))
  # A default must be visible and acceptable with Enter, otherwise a wizard is just
  # a slower way to retype what the function already knew.
  askText <- function(prompt, default = NULL) {
    d <- if (!is.null(default) && nzchar(as.character(default)))
      paste0(" [", default, "]") else ""
    repeat {
      cat(prompt, d, ": ", sep = "")
      v <- readIn()
      if (is.na(v)) noInput()
      if (!nzchar(v) && !is.null(default)) return(default)
      if (nzchar(v)) return(v)
      message("  (a value is required)")
    }
  }
  askChoice <- function(prompt, options, labels = NULL, default = 1L, allowFree = FALSE) {
    labels <- labels %||% options
    message("")
    message(prompt)
    for (i in seq_along(options)) {
      message(sprintf("  %2d) %-16s %s", i, options[i],
                      if (!identical(labels[i], options[i])) labels[i] else ""))
    }
    if (allowFree) message("      (or type a value not listed)")
    repeat {
      cat("  choice [", default, "]: ", sep = "")
      v <- readIn()
      if (is.na(v)) noInput()
      if (!nzchar(v)) return(options[default])
      idx <- suppressWarnings(as.integer(v))
      if (!is.na(idx) && idx >= 1 && idx <= length(options)) return(options[idx])
      if (v %in% options) return(v)
      if (allowFree) return(v)
      message("  (choose 1-", length(options), ")")
    }
  }
  askYesNo <- function(prompt, default = TRUE) {
    d <- if (default) "Y/n" else "y/N"
    cat(prompt, " [", d, "]: ", sep = "")
    v <- readIn(); if (is.na(v)) noInput(); v <- tolower(v)
    if (!nzchar(v)) return(default)
    substr(v, 1, 1) == "y"
  }
  askMulti <- function(prompt, options, default = options) {
    message(""); message(prompt)
    for (i in seq_along(options)) message(sprintf("  %2d) %s", i, options[i]))
    cat("  numbers, comma-separated, or Enter for all: ")
    v <- readIn()
    if (is.na(v)) noInput()
    if (!nzchar(v)) return(default)
    idx <- suppressWarnings(as.integer(strsplit(v, "[ ,]+")[[1]]))
    idx <- idx[!is.na(idx) & idx >= 1 & idx <= length(options)]
    if (!length(idx)) return(default)
    options[idx]
  }

  if (interactiveRun) {
    hr("=")
    message("PFM pipeline — data to model to REMIND inputs")
    hr("=")
  }

  # ── output folder ───────────────────────────────────────────────────────────
  # Resolve the config FIRST so its resultsDir can be the prompt's default. Without
  # this the prompt offered the hardcoded "output" on a project whose config says
  # "_output": pressing Enter found no Run-Groups, reported "No existing Run-Group",
  # and started a fresh one beside the real results instead of continuing them.
  rc <- pfmResolveConfig(config, verbose = FALSE)
  if (missing(resultsDir) && is.null(getOption("pfm.resultsDir")) &&
      !is.null(rc$resultsDir) && nzchar(rc$resultsDir)) {
    resultsDir <- rc$resultsDir
    if (missing(modelDir)) modelDir <- rc$modelDir %||% rc$resultsDir
  }
  if (interactiveRun && is.null(getOption("pfm.resultsDir"))) {
    resultsDir <- askText("Output folder (Run-Groups live here)", resultsDir)
  }
  if (identical(modelDir, "output") && !identical(resultsDir, "output")) {
    modelDir <- resultsDir
  }

  # ── group ───────────────────────────────────────────────────────────────────
  if (is.null(group)) {
    existing <- character(0)
    if (dir.exists(resultsDir)) {
      cand <- list.dirs(resultsDir, full.names = FALSE, recursive = FALSE)
      existing <- cand[vapply(cand, function(g) file.exists(
        file.path(resultsDir, g, "selected-models-psm.yml")), logical(1))]
      # Most-recently-touched first, so option 1 is the group you were last working
      # on. Alphabetical order puts an arbitrary group at the top and makes the
      # default a coin flip between a dozen old sweeps.
      if (length(existing) > 1) {
        mt <- vapply(existing, function(g) as.numeric(
          file.info(file.path(resultsDir, g, "manifest.json"))$mtime %||% 0), numeric(1))
        mt[!is.finite(mt)] <- 0
        existing <- existing[order(mt, decreasing = TRUE)]
      }
    }
    if (!interactiveRun) {
      stop("pfmRun: 'group' is required when there is nothing to prompt.\n",
           if (length(existing))
             paste0("  Run-Groups under '", resultsDir, "': ",
                    paste(utils::head(existing, 8), collapse = ", "), "\n") else "",
           "  e.g. pfmRun(group = \"",
           if (length(existing)) existing[1] else "psm-country-v1",
           "\", stage = \"downstream\", config = \"config.yml\")\n",
           "  Stages: all | sweep | downstream | remind | custom", call. = FALSE)
    }
    if (length(existing)) {
      group <- askChoice(
        "Which Run-Group?", c(existing, "<new>"),
        labels = c("(most recent)", rep("(has a deployed spec)", length(existing) - 1L),
                   "start a fresh group"),
        default = 1L, allowFree = TRUE)
      if (identical(group, "<new>")) group <- askText("New Run-Group name")
    } else {
      message("\nNo existing Run-Group under '", resultsDir, "'.")
      group <- askText("New Run-Group name", "psm-country-v1")
    }
  }
  groupDir <- file.path(resultsDir, group)
  hasSpec <- file.exists(file.path(groupDir, "selected-models-psm.yml"))

  # ── stage / steps ───────────────────────────────────────────────────────────
  if (is.null(steps)) {
    if (is.null(stage)) {
      if (!interactiveRun) stop("pfmRun: supply 'stage' or 'steps'.", call. = FALSE)
      opts <- c("all", "sweep", "diagnostics", "downstream", "remind", "custom")
      labs <- c("data -> model -> REMIND inputs (every step)",
                "estimate and select the model",
                "agreement/p-values, IV, influence, replay gate",
                "donor, projection, coupling bound, bootstrap",
                "assemble the REMIND pfm-data folder",
                "pick individual steps")
      # Default to the first thing this group still needs, rather than to a fixed
      # stage: on a finished sweep "all" would re-offer work already done.
      stage <- askChoice("What do you want to run?", opts, labs,
                         default = if (hasSpec) 3L else 1L)
    }
    stage <- match.arg(stage, c("all", "sweep", "diagnostics", "downstream",
                                "remind", "custom"))
    steps <- if (identical(stage, "custom")) {
      if (!interactiveRun) stop("pfmRun: stage = 'custom' needs 'steps'.", call. = FALSE)
      askMulti("Which steps?", allSteps)
    } else stageSteps[[stage]]
  }
  steps <- unique(steps)

  needsSpec <- c(stageSteps$diagnostics, stageSteps$downstream)
  if (!hasSpec && any(steps %in% needsSpec)) {
    msg <- paste0("Run-Group '", group, "' has no selected-models-psm.yml, so the ",
                  "diagnostics and downstream steps have no deployed spec to work from.")
    if (!any(steps %in% stageSteps$sweep)) {
      if (interactiveRun) {
        message("\nNOTE: ", msg)
        if (!askYesNo("Add the sweep stage first?", TRUE)) {
          message("  continuing — the downstream steps will skip.")
        } else steps <- unique(c(stageSteps$sweep, steps))
      } else warning(msg, call. = FALSE)
    }
  }

  # ── where to run ────────────────────────────────────────────────────────────
  hasSbatch <- nzchar(Sys.which("sbatch"))
  if (is.null(cluster)) {
    cluster <- if (interactiveRun) {
      askChoice("Where should it run?",
                c("auto", "slurm", "local"),
                c(paste0("detected: ", if (hasSbatch) "slurm" else "local"),
                  "submit with sbatch", "run here, in this session"),
                default = 1L)
    } else "auto"
  }
  if (identical(cluster, "auto")) cluster <- if (hasSbatch) "slurm" else "local"
  if (identical(cluster, "slurm") && !hasSbatch) {
    warning("pfmRun: cluster = 'slurm' but sbatch is not on PATH — running locally.",
            call. = FALSE)
    cluster <- "local"
  }

  # ── priority QOS (ADR 0031) ─────────────────────────────────────────────────
  # This logic used to live only in pfm-reports/start.R, so pfmRun — which calls
  # startRun directly — submitted on the default `short` QOS. A 127-core job on
  # `short` sits behind everything; the priority QOS exists precisely so a job this
  # size starts now rather than tomorrow. Detection is best-effort and never throws:
  # a hiccup falls back to the ordinary defaults rather than blocking the submission.
  prio <- NULL
  if (identical(cluster, "slurm") && is.null(list(...)$qos) && !identical(priority, FALSE)) {
    ps <- tryCatch(prioritySizing(qos = "priority",
                                  partition = list(...)$partition %||% "standard"),
                   error = function(e) NULL)
    if (!is.null(ps)) {
      wanted <- priority %||% TRUE
      if (interactiveRun && is.null(priority)) {
        wanted <- askYesNo(sprintf(
          paste0("\nPriority QOS is available (%s cores, %s, %s on partition '%s').",
                 "\n  It schedules ahead of the default 'short' queue. Use it?"),
          ps$nCores, ps$mem, ps$time, ps$partition), TRUE)
      }
      if (isTRUE(wanted)) {
        prio <- ps
        if (!isTRUE(ps$partitionOk)) {
          message("  NOTE: could not confirm a partition allowing qos='priority' — ",
                  "the submission may be rejected. Check with\n",
                  "    scontrol show partition | grep -iE 'PartitionName|AllowQos'")
        }
      }
    } else if (interactiveRun) {
      message("\n(priority QOS not detected on this host — using the default queue)")
    }
  }

  if (interactiveRun) {
    if (is.null(nCores) && is.null(prio)) {
      v <- askText("Cores (Enter lets the launcher size the job)", "auto")
      nCores <- if (identical(v, "auto")) NULL else suppressWarnings(as.integer(v))
    }
    if (is.null(clean)) {
      clean <- askChoice(
        "Existing outputs for this group?",
        c("none", "steps", "group"),
        c("keep them (resume skips finished steps)",
          "delete what these steps write, then rebuild",
          "delete ALL step artifacts in the group"),
        default = 1L)
    }
    if (is.null(resume) && identical(clean, "none")) {
      resume <- askYesNo("Skip steps whose outputs already exist?", TRUE)
    }
    if (is.null(config)) {
      dflt <- if (file.exists("config.yml")) "config.yml" else ""
      v <- askText("Scenario registry YAML (Enter for none)", dflt)
      config <- if (nzchar(v)) v else NULL
    }
    if (is.null(remindDir) && "psm-remind-inputs" %in% steps) {
      remindDir <- askText("REMIND input folder to write", "pfm-data")
    }
  }
  clean <- match.arg(clean %||% "none", c("none", "steps", "group"))
  # Cleaning and resuming are contradictory: deleting an artifact and then skipping
  # the step because the artifact is missing would leave the group worse than before.
  resume <- resume %||% TRUE
  if (!identical(clean, "none")) resume <- FALSE
  remindDir <- remindDir %||% "pfm-data"

  settings <- list(group = group, steps = steps, cluster = cluster,
                   resultsDir = resultsDir, modelDir = modelDir,
                   remindDir = remindDir, nCores = nCores, resume = resume,
                   clean = clean, config = config)

  # `rc` was resolved above (quietly, for the resultsDir default). Report it here,
  # BEFORE the plan: a projection with no scenarios does not fail — it quietly writes
  # one legacy projection from a generic gdx — so the scenario count has to be
  # something you SEE before agreeing to the run.
  if (!is.null(rc$path)) message("[config] using ", rc$path)
  if (!is.null(rc$scenarios)) {
    message("[config] scenario registry: ", length(rc$scenarios), " scenario(s) [",
            paste(names(rc$scenarios), collapse = ", "), "]")
  }
  settings$scenarios <- rc$scenarios
  needsScenarios <- any(c("psm-projection", "psm-coupling-bound") %in% steps)
  if (needsScenarios && is.null(rc$scenarios)) {
    message("\nWARNING: no scenario registry resolved, but ",
            paste(intersect(c("psm-projection", "psm-coupling-bound"), steps),
                  collapse = " and "), " need one.")
    message("  The projection would fall back to a single legacy scenario and the ",
            "coupling bound would skip.")
    message("  Pass config = \"<path to config.yml>\" with a scenarios: block.")
  }

  # ── plan ────────────────────────────────────────────────────────────────────
  hr("=")
  message("Plan")
  hr()
  message("  group       : ", group)
  message("  output      : ", normalizePath(resultsDir, winslash = "/", mustWork = FALSE))
  message("  run on      : ", cluster, if (identical(cluster, "local")) " (this session)" else " (sbatch)")
  if (!is.null(prio)) {
    message("  queue       : qos=priority partition=", prio$partition,
            "  [", prio$detail, "]")
    message("  sized       : ", prio$nCores, " cores, ", prio$mem, ", ", prio$time)
  } else if (identical(cluster, "slurm")) {
    message("  queue       : default (", list(...)$qos %||% "short", ")")
  }
  message("  cores       : ", nCores %||% "auto")
  message("  resume      : ", resume)
  message("  clean       : ", clean,
          if (identical(clean, "none")) "" else "  (resume disabled)")
  message("  registry    : ", rc$path %||% "(none)",
          if (is.null(rc$scenarios)) "  [no scenarios]"
          else paste0("  [", length(rc$scenarios), " scenario(s)]"))
  if ("psm-remind-inputs" %in% steps) message("  REMIND out  : ", remindDir)
  message("  steps       : ", paste(steps, collapse = ", "))
  hr("=")

  if (!identical(clean, "none")) {
    victims <- if (identical(clean, "group")) names(psmStepArtifacts()) else steps
    psmCleanSteps(group, victims, resultsDir = resultsDir, dryRun = TRUE)
  }

  # Validate the caller's dots against startRun BEFORE the dryRun exit, so a dry run
  # cannot report a plan that is impossible to execute. This fired for real on
  # 2026-08-25: dryRun printed a clean plan and the identical live call then died on
  # a duplicated `cachefolder`. A dry run that does not check argument assembly gives
  # false confidence, which is worse than not offering one.
  local({
    dn <- names(list(...))
    dn <- dn[nzchar(dn)]
    if (!length(dn)) return(invisible(NULL))
    known <- names(formals(startRun))
    # Args startRun accepts only through its own `...` (forwarded to the step
    # functions). Anything matching neither is almost certainly a typo and would be
    # swallowed silently — the `config` trap, which cost a full run.
    fwd <- unique(c(names(formals(runPSMSweep)), names(formals(runPostProcessing))))
    unknown <- setdiff(dn, c(known, fwd))
    if (length(unknown)) {
      warning("pfmRun: argument(s) not recognised by startRun or the step functions: ",
              paste(unknown, collapse = ", "),
              ". These are SILENTLY IGNORED, not applied. Check the spelling.",
              call. = FALSE)
    }
  })
  if (isTRUE(dryRun)) { message("dryRun = TRUE — nothing was run."); return(invisible(settings)) }
  if (interactiveRun && !askYesNo("Proceed?", TRUE)) {
    message("cancelled."); return(invisible(settings))
  }
  if (!identical(clean, "none")) {
    victims <- if (identical(clean, "group")) names(psmStepArtifacts()) else steps
    psmCleanSteps(group, victims, resultsDir = resultsDir, dryRun = FALSE)
  }

  # ── run ─────────────────────────────────────────────────────────────────────
  # The REMIND export is a plain local file copy: submitting it to SLURM would queue
  # a job to copy seven files. Split it out and run it here, after the rest.
  pipelineSteps <- setdiff(steps, "psm-remind-inputs")
  if (length(pipelineSteps)) {
    # startRun takes `scenarios`/`gdxFile`, not a config path — passing `config` would
    # land in ... and be silently ignored, leaving the projection with no registry and
    # a fallback to one legacy scenario from a generic gdx. Resolve it here instead.
    # The caller's `...` OVERRIDES these defaults rather than colliding with them.
    # Splicing `...` into the same list() literal made any of `cachefolder`,
    # `scenarios` or `outputRegionMappingFile` a duplicate name, and do.call then
    # died with "formal argument matched by multiple actual arguments" — AFTER the
    # plan had printed and AFTER dryRun had reported success, because dryRun returns
    # before this assembly. Passing a cachefolder explicitly is the natural thing to
    # do (every startRun example in the docs does), so this was easy to hit.
    # modifyList gives the caller precedence, matching the nCores/mem/time rule below.
    args <- utils::modifyList(
      list(group = group, steps = pipelineSteps, cluster = cluster,
           resultsDir = resultsDir, modelDir = modelDir, resume = resume,
           scenarios = rc$scenarios, cachefolder = rc$cachefolder,
           outputRegionMappingFile = "country"),
      list(...))
    if (!is.null(rc$gdxFile)) args$gdxFile <- rc$gdxFile
    # Each axis is set only when the caller did NOT pass it, so an explicit
    # nCores/mem/time/partition still wins over the auto-sizing.
    if (!is.null(prio)) {
      args$qos <- "priority"
      if (is.null(list(...)$partition)) args$partition <- prio$partition
      if (is.null(list(...)$mem))       args$mem       <- prio$mem
      if (is.null(list(...)$time))      args$time      <- prio$time
      if (is.null(nCores))              nCores         <- prio$nCores
    }
    if (!is.null(nCores)) args$nCores <- nCores
    do.call(startRun, args)
  }
  if ("psm-remind-inputs" %in% steps) {
    if (identical(cluster, "slurm") && length(pipelineSteps)) {
      message("\nNOTE: the pipeline was SUBMITTED, so the REMIND folder is not built yet —")
      message("      its inputs do not exist until the job finishes. When it does, run:")
      message("        pfmRun(group = \"", group, "\", stage = \"remind\", remindDir = \"",
              remindDir, "\")")
    } else {
      runPSMExportREMINDInputs(group = group, dest = remindDir,
                               resultsDir = resultsDir, modelDir = modelDir)
    }
  }
  invisible(settings)
}
# nolint end
