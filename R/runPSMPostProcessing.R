# nolint start
#' Persist the deployed PSM scenario projections as Run-Group artifacts
#'
#' @description
#' The PSM counterpart of \code{\link{runProjection}} (ADR 0035 + ADR 0036): fans
#' the deployed Policy Stringency spec (from the Run-Group's
#' \code{selected-models-psm.yml}) out over the Policy Scenario Registry. For each
#' scenario it builds the scenario panel from that scenario's gdx (with the gdx's
#' own region mapping), projects both sectors via
#' \code{\link{projectPSMSpecScenario}} (bounded index, delta-method interval,
#' out-of-coverage flag), derives the \strong{Implementability Factor} columns
#' (\code{\link{computeImplementabilityFactor}} — the post-processing 0-1
#' multiplier, keeping the model layer coupling-agnostic), and writes one labelled
#' artifact \code{<group>/projections/<id>.rds} (rows tagged
#' \code{scenario}/\code{scenarioName}). The \strong{gating} scenario is
#' additionally written to \code{<group>/projection.rds} so single-projection
#' consumers work unchanged.
#'
#' When \code{scenarios} is \code{NULL}, a single legacy scenario is synthesised
#' from \code{scenarioData} / a cached scenario panel / \code{gdxFile}; without any
#' usable source the step is recorded as skipped, never thrown.
#'
#' @param group Character. PSM Run-Group name (from \code{\link{runPSMSweep}}).
#' @param resultsDir,modelDir,cachefolder As in \code{\link{runProjection}}.
#' @param gdxFile Character or NULL. Legacy single-scenario gdx.
#' @param scenarios Optional list of normalised scenario descriptors
#'   (\code{\link{parseScenarioRegistry}}: \code{id}, \code{name}, \code{gdx},
#'   \code{gdxRegionMapping}, \code{gating}; a \code{prebuilt} panel is honoured).
#' @param panelData Optional pre-built historical panel (must carry the Policy
#'   Stringency outcomes).
#' @param scenarioData Optional pre-built scenario panel (legacy single-scenario).
#' @param y,outputRegionMappingFile As in \code{\link{runProjection}}.
#' @param indexMax Numeric. Index ceiling. Default \code{10}.
#' @param verbose Logical.
#'
#' @return Invisibly, a named list of per-scenario projection data.frames (keyed
#'   by scenario id), or \code{NULL} if nothing was produced.
#' @seealso \code{\link{runPSMSweep}}, \code{\link{projectPSMSpecScenario}},
#'   \code{\link{computeImplementabilityFactor}}, ADR 0035, ADR 0036
#' @export
#' @author Renato Rodrigues
runPSMProjection <- function(group, resultsDir = getOption("pfm.resultsDir", "output"),
                             modelDir = getOption("pfm.modelDir", "output"), cachefolder = NULL,
                             gdxFile = NULL, scenarios = NULL,
                             panelData = NULL, scenarioData = NULL,
                             y = 2000:2022, outputRegionMappingFile = "regionmapping_54.csv",
                             indexMax = 10, verbose = TRUE) {
  groupDir <- .resolveGroupDir(group, resultsDir, modelDir, cachefolder)
  say <- function(...) if (isTRUE(verbose)) message("[PSM-PROJ:", group, "] ", ...)
  t0 <- Sys.time()
  if (!requireNamespace("yaml", quietly = TRUE)) stop("The 'yaml' package is required.")
  sectors <- c("Bulk", "Diffuse")

  if (is.null(scenarios) || !length(scenarios)) {
    say("starting projection (legacy single-scenario): gdxFile = ", gdxFile %||% "NULL",
        ", scenarioData supplied = ", !is.null(scenarioData))
  } else {
    say("starting projection fan-out over ", length(scenarios), " scenario(s): ",
        paste(vapply(scenarios, function(s) s$id %||% "?", character(1)), collapse = ", "))
  }

  loadCachedPanel <- function(fname) {
    p <- file.path(groupDir, "data", fname)
    if (!file.exists(p)) return(NULL)
    obj <- tryCatch(readRDS(p), error = function(e) NULL)
    if (is.list(obj) && !is.null(obj$data)) obj$data else obj
  }

  # ── Shared inputs: historical panel (with the PSM outcomes) + the deployed spec ──
  panel <- panelData
  if (is.null(panel)) {
    panel <- loadCachedPanel("panelDataHistorical.rds")
    if (!is.null(panel)) say("using cached historical panel.")
  }
  if (is.null(panel)) {
    panel <- tryCatch(
      panelDataHistorical(aggregate = TRUE, y = y,
                          outputRegionMappingFile = outputRegionMappingFile,
                          includePolicyStringency = TRUE),
      error = function(e) {
        say("historical panel build FAILED: ", conditionMessage(e))
        NULL
      }
    )
  }
  # ── Resolution guard (2026-08-10) ───────────────────────────────────────────
  # The projection panel MUST be built at the same regional resolution the model
  # was estimated at. Nothing else enforces this: `outputRegionMappingFile`
  # defaults to R54, so a country-resolution Run-Group projected via a launcher
  # that does not pass the "country" sentinel silently scores R54-AGGREGATED
  # drivers on a country-trained model. That happened to psm-country-v3 (job
  # 1747324): 54 aggregate regions against 49 ISO3 training countries, and the
  # resulting fan-out reversed the sign of the scenario response. Fail loudly.
  if (!is.null(panel) && magclass::is.magpie(panel)) {
    projRegions <- magclass::getItems(panel, dim = 1)
    trainPath <- file.path(groupDir, "manifest.json")
    trainRegions <- tryCatch({
      mf <- jsonlite::read_json(trainPath)
      tp <- file.path(modelDir, "panels", paste0("panel_", mf$panel_hash, ".rds"))
      tr <- readRDS(tp)
      if (is.list(tr) && !is.null(tr$data)) tr <- tr$data
      if (magclass::is.magpie(tr)) magclass::getItems(tr, dim = 1) else unique(as.character(tr$region))
    }, error = function(e) NULL)
    if (!is.null(trainRegions) && length(trainRegions)) {
      shared <- length(intersect(projRegions, trainRegions)) / length(trainRegions)
      if (shared < 0.9) {
        stop("runPSMProjection: REGIONAL RESOLUTION MISMATCH. The deployed model was ",
             "estimated on ", length(trainRegions), " regions but the projection panel ",
             "carries ", length(projRegions), " (only ",
             round(100 * shared), "% of the training regions appear in it). Projecting ",
             "aggregated drivers onto a model trained at a finer resolution is not a ",
             "valid projection. Pass outputRegionMappingFile = \"country\" (the ",
             "Country-Identity Mapping sentinel) for a country-resolution Run-Group, ",
             "or supply `panelData`/`scenarioData` built at the model's resolution.",
             call. = FALSE)
      }
    }
  }

  psVars <- paste0("Policy Stringency|", sectors)
  if (is.null(panel) ||
        (magclass::is.magpie(panel) && !all(psVars %in% magclass::getNames(panel)))) {
    .recordStep(groupDir, group, "projection", t0, status = "failed",
                metrics = list(reason = "historical panel missing or lacks the PSM outcomes"))
    return(invisible(NULL))
  }

  norm <- function(s) {
    for (f in c("actorPowerDrivers", "actorPowerIndex", "instQualityDrivers", "controlDrivers"))
      if (!is.null(s[[f]])) s[[f]] <- unlist(s[[f]])
    s$panelTransform <- s$panelTransform %||% "levels"
    s
  }
  selPath <- file.path(groupDir, "selected-models-psm.yml")
  if (!file.exists(selPath)) {
    .recordStep(groupDir, group, "projection", t0, status = "skipped",
                metrics = list(reason = "no selected-models-psm.yml (run runPSMSweep first)"))
    return(invisible(NULL))
  }
  sel <- yaml::read_yaml(selPath)
  cfg <- list()
  for (sec in sectors) {
    hit <- Filter(function(x) identical(x$model_type, paste0("PolicyStringency: ", sec)), sel)
    cfg[[sec]] <- if (length(hit) > 0) norm(hit[[1]]) else NULL
  }
  if (all(vapply(cfg, is.null, logical(1)))) {
    stop("No 'PolicyStringency: <Sector>' entries in selected-models-psm.yml", call. = FALSE)
  }
  for (sec in sectors) if (is.null(cfg[[sec]])) cfg[[sec]] <- cfg[[setdiff(sectors, sec)]]

  # ── Resolve the scenario set (ADR 0035): explicit registry descriptors fan out;
  # else a single legacy scenario is synthesised. ──
  scen <- scenarios
  if (is.null(scen) || !length(scen)) {
    scen <- list(scenario = list(
      id = "scenario", name = "Scenario", gdx = gdxFile,
      gdxRegionMapping = "regionmappingH12.csv", gating = TRUE,
      prebuilt = scenarioData %||% loadCachedPanel("panelDataScenario.rds")
    ))
  }
  gatingId <- {
    g <- names(Filter(function(s) isTRUE(s$gating), scen))
    if (length(g)) g[[1]] else names(scen)[[1]]
  }

  buildScenarioPanel <- function(s) {
    if (!is.null(s$prebuilt)) return(s$prebuilt)
    if (is.null(s$gdx) || !nzchar(s$gdx) || !file.exists(s$gdx)) {
      say("scenario '", s$id, "': no usable gdx (", s$gdx %||% "NULL", ") -> skipped.")
      return(NULL)
    }
    say("scenario '", s$id, "': building scenario panel from gdx: ", s$gdx)
    tryCatch(panelDataScenario(gdxFile = s$gdx, aggregate = TRUE,
                               gdxRegionMappingFile = s$gdxRegionMapping %||% "regionmappingH12.csv",
                               outputRegionMappingFile = outputRegionMappingFile),
             error = function(e) {
               say("  scenario panel build FAILED: ", conditionMessage(e))
               NULL
             })
  }

  # ── Fan out: project, derive the Implementability Factor, write one artifact each ──
  projDir <- file.path(groupDir, "projections")
  dir.create(projDir, showWarnings = FALSE, recursive = TRUE)
  results <- list()
  anySource <- FALSE
  for (id in names(scen)) {
    s <- scen[[id]]
    if (!is.null(s$prebuilt) || (!is.null(s$gdx) && nzchar(s$gdx) && file.exists(s$gdx))) {
      anySource <- TRUE
    }
    sdata <- buildScenarioPanel(s)
    if (is.null(sdata)) next
    proj <- do.call(rbind, lapply(sectors, function(sec) {
      tryCatch(
        projectPSMSpecScenario(cfg[[sec]], sec, histData = panel, scenarioData = sdata,
                               modelDir = modelDir, indexMax = indexMax, verbose = FALSE),
        error = function(e) {
          say("  ", id, " | ", sec, " projection failed: ", conditionMessage(e))
          NULL
        }
      )
    }))
    if (is.null(proj) || !nrow(proj)) {
      say("scenario '", id, "': empty projection -> not written")
      next
    }
    proj <- computeImplementabilityFactor(proj, indexMax = indexMax)
    proj$scenario <- id
    proj$scenarioName <- s$name %||% id
    saveRDS(proj, file.path(projDir, paste0(gsub("[^A-Za-z0-9._-]", "_", id), ".rds")))
    if (identical(id, gatingId)) saveRDS(proj, file.path(groupDir, "projection.rds"))
    results[[id]] <- proj
    say("scenario '", id, "': saved ", nrow(proj), " rows",
        if (identical(id, gatingId)) " (+ projection.rds)" else "")
  }

  if (!length(results)) {
    .recordStep(groupDir, group, "projection", t0,
                status = if (anySource) "failed" else "skipped",
                metrics = list(reason = if (anySource) "no scenario produced a projection"
                               else "no usable scenario gdx/panel"))
    return(invisible(NULL))
  }
  combined <- do.call(rbind, results)
  .recordStep(groupDir, group, "projection", t0, metrics = list(
    scenarios = paste(names(results), collapse = "/"), gatingScenario = gatingId,
    rows = nrow(combined),
    outOfCoverageShare = round(mean(combined$outOfCoverage, na.rm = TRUE), 3)
  ))
  say("Projection fan-out complete: ", length(results), " scenario(s).")
  invisible(results)
}

#' Estimator-agreement Run-Group step for the deployed PSM spec
#'
#' @description
#' Post-selection exhibit (ADR 0036): re-estimates the Run-Group's deployed Policy
#' Stringency spec under every member of the PSM Estimator Suite per sector
#' (\code{\link{computeEstimatorAgreement}} — satP engine, fractional-logit
#' headline, beta-regression cross-check, levels benchmark) and writes
#' \code{<group>/estimator-agreement.rds}. This is the paper's
#' estimator-invariance claim: signs/AMEs of the selected spec across the suite,
#' compared only on scale-free natural-scale metrics, never on AIC/BIC.
#'
#' @inheritParams runPSMProjection
#' @param estimators Character vector of suite members to fit.
#' @return Invisibly, a list with one \code{\link{computeEstimatorAgreement}}
#'   result per sector plus the deployed spec name, or \code{NULL} when skipped.
#' @seealso \code{\link{computeEstimatorAgreement}}, ADR 0036
#' @export
#' @author Renato Rodrigues
runPSMEstimatorAgreement <- function(group, resultsDir = getOption("pfm.resultsDir", "output"),
                                     modelDir = getOption("pfm.modelDir", "output"),
                                     cachefolder = NULL, panelData = NULL,
                                     y = 2000:2022,
                                     outputRegionMappingFile = "regionmapping_54.csv",
                                     indexMax = 10,
                                     estimators = c("satP", "fractional", "beta",
                                                    "levels", "satP-re",
                                                    "satP-yearFE", "levels-twfe"),
                                     verbose = TRUE) {
  groupDir <- .resolveGroupDir(group, resultsDir, modelDir, cachefolder)
  say <- function(...) if (isTRUE(verbose)) message("[PSM-AGREE:", group, "] ", ...)
  t0 <- Sys.time()
  if (!requireNamespace("yaml", quietly = TRUE)) stop("The 'yaml' package is required.")
  sectors <- c("Bulk", "Diffuse")

  selPath <- file.path(groupDir, "selected-models-psm.yml")
  if (!file.exists(selPath)) {
    .recordStep(groupDir, group, "estimator-agreement", t0, status = "skipped",
                metrics = list(reason = "no selected-models-psm.yml (run runPSMSweep first)"))
    return(invisible(NULL))
  }
  sel <- yaml::read_yaml(selPath)

  panel <- panelData
  if (is.null(panel)) {
    p <- file.path(groupDir, "data", "panelDataHistorical.rds")
    if (file.exists(p)) panel <- tryCatch(readRDS(p), error = function(e) NULL)
    if (is.list(panel) && !is.null(panel$data)) panel <- panel$data
  }
  if (is.null(panel)) {
    panel <- tryCatch(
      panelDataHistorical(aggregate = TRUE, y = y,
                          outputRegionMappingFile = outputRegionMappingFile,
                          includePolicyStringency = TRUE),
      error = function(e) NULL
    )
  }
  if (is.null(panel)) {
    .recordStep(groupDir, group, "estimator-agreement", t0, status = "failed",
                metrics = list(reason = "historical panel unavailable"))
    return(invisible(NULL))
  }

  norm <- function(s) {
    for (f in c("actorPowerDrivers", "actorPowerIndex", "instQualityDrivers", "controlDrivers"))
      if (!is.null(s[[f]])) s[[f]] <- unlist(s[[f]])
    s
  }
  out <- list(spec = NULL, bySector = list())
  for (sec in sectors) {
    hit <- Filter(function(x) identical(x$model_type, paste0("PolicyStringency: ", sec)), sel)
    if (length(hit) == 0) next
    cfg <- norm(hit[[1]])
    out$spec <- out$spec %||% cfg$name
    say("estimator agreement for '", cfg$name, "' (", sec, ") ...")
    out$bySector[[sec]] <- tryCatch(
      computeEstimatorAgreement(
        data = panel, sector = sec, estimators = estimators, indexMax = indexMax,
        verbose = FALSE,
        actorPowerDrivers = cfg$actorPowerDrivers, actorPowerIndex = cfg$actorPowerIndex,
        instQualityDrivers = cfg$instQualityDrivers, controlDrivers = cfg$controlDrivers,
        regionMappingFixedEffects = cfg$regionMappingFixedEffects,
        logisticTimeTrend = isTRUE(cfg$logisticTimeTrend),
        interactRegionFE = isTRUE(cfg$interactRegionFE),
        useMundlak = isTRUE(cfg$useMundlak),
        gdpGovInteraction = isTRUE(cfg$gdpGovInteraction),
        includeLaggedPS = isTRUE(cfg$includeLaggedPS)
      ),
      error = function(e) {
        say("  ", sec, " failed: ", conditionMessage(e))
        NULL
      }
    )
    # Few-clusters inference upgrade (2026-07-07): wild-cluster bootstrap-t on the
    # satP engine fit — the p-values the paper must quote for the headline terms.
    if (!is.null(out$bySector[[sec]]$fits$satP)) {
      out$bySector[[sec]]$wildBootstrap <- tryCatch(
        computeWildClusterBootstrap(out$bySector[[sec]]$fits$satP),
        error = function(e) {
          say("  ", sec, " wild bootstrap failed: ", conditionMessage(e))
          NULL
        }
      )
      wb <- out$bySector[[sec]]$wildBootstrap
      if (!is.null(wb)) {
        say("  ", sec, " wild-cluster p<0.05 terms: ",
            paste(wb$term[wb$pWild < 0.05], collapse = ", "))
      }
    }
  }
  fitted <- names(Filter(Negate(is.null), out$bySector))
  if (!length(fitted)) {
    .recordStep(groupDir, group, "estimator-agreement", t0, status = "failed",
                metrics = list(reason = "no sector produced an agreement table"))
    return(invisible(NULL))
  }
  # A silently skipped suite member is a hole in the estimator-invariance exhibit
  # (the first cluster run shipped without the beta rung because betareg was not
  # installed — R11, 2026-07-06). Make it loud: a warning that reaches the SLURM
  # log AND the step metrics.
  skippedAll <- unique(unlist(lapply(out$bySector, function(e) names(e$skipped))))
  if (length(skippedAll) > 0) {
    warning("runPSMEstimatorAgreement: estimator(s) SKIPPED: ",
            paste(skippedAll, collapse = ", "),
            " - the estimator-invariance exhibit is incomplete. ",
            "Install the missing optional package(s) (betareg / lme4) and re-run.",
            call. = FALSE)
  }
  saveRDS(out, file.path(groupDir, "estimator-agreement.rds"))
  .recordStep(groupDir, group, "estimator-agreement", t0, metrics = list(
    spec = out$spec, sectors = paste(fitted, collapse = "/"),
    estimators = paste(names(out$bySector[[fitted[[1]]]]$fits), collapse = "/"),
    skipped = if (length(skippedAll)) paste(skippedAll, collapse = "/") else "none"
  ))
  say("Saved ", file.path(groupDir, "estimator-agreement.rds"))
  invisible(out)
}
# nolint end
