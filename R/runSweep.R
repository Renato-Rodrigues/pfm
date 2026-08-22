# nolint start
#' Run a model sweep into a Run-Group
#'
#' @description
#' Compute-layer entry point (ADR 0018). Builds/loads the historical panel (and, if a gdx is
#' given, the scenario panel), runs the full specification sweep + maximin / Projection-Sanity
#' selection (parallel-capable, ADR 0019), and writes the curated artifacts into
#' \code{<resultsDir>/<group>/}: \code{sweep.rds}, \code{selected-models.yml} (or
#' \code{selected-models-difference-first.yml}), the sweep config, and \code{manifest.json}.
#' Renders no reports — that is the report layer's job (pfm-reports reads this Run-Group).
#'
#' @param group Character. Run-Group name (e.g. \code{"exhaustive"}, \code{"guided"}, or a
#'   custom experiment name). Required.
#' @param mode Character. \code{"exhaustive"} or \code{"guided"}. Default \code{"exhaustive"}.
#' @param resultsDir Character. Results Root (configurable). Default
#'   \code{getOption("pfm.resultsDir")}. The Run-Group is \code{file.path(resultsDir, group)}.
#' @param modelDir Character. Fit Cache root (configurable; ADR 0009 model store — distinct
#'   from the madrat \code{cachefolder}). Default \code{getOption("pfm.modelDir")}; also set as
#'   \code{options(pfm.modelDir=)} for the run.
#' @param cachefolder Character or NULL. The \strong{madrat} data-cache folder. When non-NULL,
#'   \code{madrat::setConfig(cachefolder = ...)} so the panel/CarbonPrice load from there. NULL
#'   (default) leaves the ambient madrat config untouched (only \code{forcecache} is set).
#' @param gdxFile Character or NULL. REMIND \code{fulldata.gdx} for the scenario panel
#'   (enables the Projection Sanity gate). When NULL/missing, the gate is skipped.
#' @param panelData,scenarioData Optional pre-built panels (skip the build step).
#' @param y Integer vector of training years. Default \code{2000:2022}.
#' @param outputRegionMappingFile Character. Region mapping. Default \code{"regionmapping_54.csv"}.
#' @param movingAverage Integer. Passed to \code{panelDataHistorical}. Default \code{5}.
#' @param sectors Character vector. Default \code{c("Bulk","Diffuse")}.
#' @param nCores Integer. Cores for the fit sweep (ADR 0019). Default \code{1}.
#' @param forceRefit Logical. Ignore cached fits and re-estimate. Default \code{FALSE}.
#' @param selectionMethod Character. \code{"levels-first"} (default) or \code{"difference-first"}.
#' @param family Character. Stringency GLM family. Default \code{"gaussian"}.
#' @param verbose Logical. Default \code{TRUE}.
#' @param ... Further arguments forwarded to \code{\link{runChannelsWorkflow}} (e.g.
#'   \code{sanityThresholds}, \code{selectFE}, \code{nearTieEps}).
#'
#' @return The workflow result list (invisibly); see \code{\link{runChannelsWorkflow}}.
#' @seealso \code{\link{runChannelsWorkflow}}, \code{\link{runModelGroup}}, ADR 0018, ADR 0019
#' @export
#' @author Renato Rodrigues
runSweep <- function(group,
                     mode = c("exhaustive", "guided"),
                     resultsDir = getOption("pfm.resultsDir", "output"),
                     modelDir = getOption("pfm.modelDir", "output"),
                     cachefolder = NULL,
                     gdxFile = NULL,
                     panelData = NULL,
                     scenarioData = NULL,
                     y = 2000:2022,
                     outputRegionMappingFile = "regionmapping_54.csv",
                     movingAverage = 5,
                     sectors = c("Bulk", "Diffuse"),
                     nCores = 1L,
                     forceRefit = FALSE,
                     selectionMethod = c("levels-first", "difference-first"),
                     family = "gaussian",
                     verbose = TRUE,
                     ...) {
  mode <- match.arg(mode)
  selectionMethod <- match.arg(selectionMethod)
  if (missing(group) || is.null(group) || !nzchar(group)) {
    stop("runSweep: 'group' is required.", call. = FALSE)
  }
  if (is.null(resultsDir)) {
    stop("runSweep: supply 'resultsDir' or set options(pfm.resultsDir = '...').", call. = FALSE)
  }
  say <- function(...) if (isTRUE(verbose)) message("[runSweep:", group, "] ", ...)

  # Compute layer runs offline from the madrat cache (the panels/CarbonPrice load by
  # args-hash). forcecache makes that the default so `library(pfm); runSweep(...)` works
  # without the raw sources present; cachefolder (when given) points madrat at the data cache.
  .useMadratCache(cachefolder)
  t0 <- Sys.time()

  groupDir <- file.path(resultsDir, group)
  dir.create(groupDir, showWarnings = FALSE, recursive = TRUE)
  if (!is.null(modelDir)) {
    dir.create(modelDir, showWarnings = FALSE, recursive = TRUE)
    options(pfm.modelDir = modelDir)
  }

  # Historical panel — built the same way the report driver did (+ GDP^2 column).
  if (is.null(panelData)) {
    say("Building historical panel (", min(y), "-", max(y), ") ...")
    panelData <- panelDataHistorical(
      aggregate = TRUE, y = y,
      outputRegionMappingFile = outputRegionMappingFile, movingAverage = movingAverage
    )
  }
  if ("GDP per Capita" %in% magclass::getNames(panelData) &&
      !"GDP per Capita Sq" %in% magclass::getNames(panelData)) {
    panelData <- magclass::mbind(
      panelData,
      magclass::setNames(panelData[, , "GDP per Capita"]^2, "GDP per Capita Sq")
    )
  }
  # Store the shared Training Panel once (content-addressed) so the fits saved this run
  # reference it by hash rather than embedding their own copy (ADR 0009).
  if (!is.null(modelDir)) saveTrainingPanel(panelData, dir = modelDir)

  # Scenario panel — optional; enables the Projection Sanity selection gate.
  if (is.null(scenarioData) && !is.null(gdxFile) && file.exists(gdxFile)) {
    say("Building scenario panel from gdx ...")
    scenarioData <- tryCatch(
      panelDataScenario(gdxFile = gdxFile, aggregate = TRUE,
                        outputRegionMappingFile = outputRegionMappingFile),
      error = function(e) {
        say("scenario panel failed (", conditionMessage(e), "); Projection Sanity gate skipped.")
        NULL
      }
    )
  }

  # Sweep + selection. No reports / findings; the selected-models YAML and sweep config are
  # written into the Run-Group via configDir.
  res <- runChannelsWorkflow(
    mode = mode, panelData = panelData, scenarioData = scenarioData, sectors = sectors,
    configDir = groupDir, modelDir = modelDir, nCores = nCores, forceRefit = forceRefit,
    family = family, selectionMethod = selectionMethod,
    # A fresh sweep (runSweep only runs when NOT resuming) must REGENERATE the auto-generated spec
    # config from the current channelSpecs() — otherwise code changes to the spec grid (e.g. the
    # ADR 0028 saturating `| satP` twins) are silently ignored because the stale YAML is reused.
    overwriteConfig = TRUE,
    reportsDir = NULL, renderReports = FALSE, renderRobustness = FALSE,
    updateFindings = FALSE, saveRds = FALSE, writeSelectedConfig = TRUE,
    verbose = verbose, ...
  )

  # Canonical Run-Group artifacts.
  saveRDS(res, file.path(groupDir, "sweep.rds"))
  if (!is.null(res$selectedConfigPath) && file.exists(res$selectedConfigPath)) {
    canonical <- if (selectionMethod == "difference-first") {
      "selected-models-difference-first.yml"
    } else {
      "selected-models.yml"
    }
    file.copy(res$selectedConfigPath, file.path(groupDir, canonical), overwrite = TRUE)
  }

  # Provenance (no step) + the timed sweep step with its fit metrics (ADR 0020).
  .writeRunGroupManifest(
    groupDir, group = group, mode = mode,
    panelData = panelData, scenarioData = scenarioData, gdxFile = gdxFile,
    selectionMethod = selectionMethod, nCores = nCores
  )
  .recordStep(groupDir, group, "sweep", t0, mode = mode,
              metrics = c(res$fitSummary %||% list(),
                          list(nCores = nCores, selectionMethod = selectionMethod)))

  say("Run-Group written: ", groupDir)
  invisible(res)
}

# Internal: point madrat at the data cache (the compute layer runs offline from it). Always sets
# forcecache = TRUE; when a cachefolder is supplied, also sets it. Distinct from the Fit Cache /
# model store (options(pfm.modelDir)) — that is the `modelDir` argument, not this.
#' @keywords internal
.useMadratCache <- function(cachefolder = NULL) {
  if (!is.null(cachefolder) && nzchar(cachefolder)) {
    madrat::setConfig(cachefolder = cachefolder, forcecache = TRUE)
  } else {
    madrat::setConfig(forcecache = TRUE)
  }
  invisible(NULL)
}

# Internal: write/update the Run-Group manifest (provenance contract; ADR 0018). Each step
# (sweep, robustness, ...) records its own timestamp; package versions, panel hash, training
# years, scenario/gdx provenance, and the artifact list are refreshed each call.
#' @importFrom jsonlite fromJSON write_json
#' @keywords internal
.writeRunGroupManifest <- function(groupDir, group, mode, step = NULL,
                                   panelData = NULL, scenarioData = NULL, gdxFile = NULL,
                                   selectionMethod = NULL, nCores = NULL,
                                   stepStats = NULL, run = NULL) {
  path <- file.path(groupDir, "manifest.json")
  man <- if (file.exists(path)) {
    tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE), error = function(e) list())
  } else {
    list()
  }
  pkgver <- function(p) tryCatch(as.character(utils::packageVersion(p)), error = function(e) NA_character_)
  man$group <- group
  if (!is.null(mode) && !is.na(mode)) man$mode <- mode
  man$pfm_version <- pkgver("pfm")
  man$mrpfm_version <- pkgver("mrpfm")
  # The logistic time trend's shape changes every fitted coefficient, every
  # ceiling and the whole projection, but it is a function DEFAULT rather than a
  # spec field, so without this a Run-Group carries no record of which curve
  # produced it. The default moved on 2026-08-22 (TODO item 0b) and will move
  # again if item 13 makes it a swept dimension; two groups must never be
  # indistinguishable on it. Resolved from preparePanelData() so this tracks the
  # default automatically rather than repeating a literal.
  man$trend <- list(
    form = "logistic",
    midpoint = as.numeric(formals(preparePanelData)$trendMidpoint),
    steepness = as.numeric(formals(preparePanelData)$trendSteepness),
    scaled = TRUE   # standardized with the drivers since 2026-08-22
  )
  if (!is.null(panelData)) {
    man$panel_hash <- substr(digest::digest(panelData, algo = "sha256"), 1, 16)
    yrs <- tryCatch(magclass::getYears(panelData, as.integer = TRUE), error = function(e) NULL)
    if (!is.null(yrs)) man$training_years <- c(min(yrs), max(yrs))
  }
  if (!is.null(scenarioData)) man$scenario <- "present"
  if (!is.null(gdxFile)) {
    man$gdx <- if (file.exists(gdxFile)) {
      list(path = gdxFile, mtime = as.character(file.info(gdxFile)$mtime))
    } else "none"
  }
  if (!is.null(selectionMethod)) man$selectionMethod <- selectionMethod
  if (!is.null(nCores)) man$nCores <- nCores
  # Per-step record: rich stats {started, ended, seconds, status, metrics} when supplied
  # (ADR 0020), else a bare timestamp.
  if (!is.null(step)) {
    steps <- man$steps
    if (is.null(steps)) steps <- list()
    if (identical(stepStats, FALSE)) {
      steps[[step]] <- NULL
    } else {
      steps[[step]] <- stepStats %||% as.character(Sys.time())
    }
    man$steps <- steps
  }
  # Overall run block (written/updated by startRun): merge the supplied fields.
  if (!is.null(run)) {
    man$run <- utils::modifyList(man$run %||% list(), run)
  }
  man$updated <- as.character(Sys.time())
  man$artifacts <- as.list(list.files(groupDir, pattern = "\\.(rds|yml)$"))
  jsonlite::write_json(man, path, pretty = TRUE, auto_unbox = TRUE)
  invisible(man)
}

# Internal: format a per-step stats record and write it to the manifest (ADR 0020).
#' @keywords internal
.recordStep <- function(groupDir, group, step, t0, status = "completed",
                        metrics = list(), mode = NULL) {
  ended <- Sys.time()
  stats <- list(
    started = as.character(t0), ended = as.character(ended),
    seconds = round(as.numeric(difftime(ended, t0, units = "secs")), 1),
    status = status, metrics = metrics
  )
  .writeRunGroupManifest(groupDir, group = group, mode = mode, step = step, stepStats = stats)
}
# nolint end
