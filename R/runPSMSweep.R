# nolint start
#' Run a Policy Stringency Model sweep into its own Run-Group
#'
#' @description
#' The PSM counterpart of \code{\link{runSweep}} (ADR 0036). Reuses the shared
#' machinery end to end — the channel spec grid (\code{createChannelConfigs},
#' adapted by \code{\link{psmSpecs}}), \code{\link{runFitGrid}} with the single
#' stage \code{"PolicyStringency"} (satP engine fits, Fit-Cache reuse), the
#' unchanged \code{\link{computeMaximinScore}} worse-sector ranking, and — when a
#' scenario panel is available — the bounded-index Projection Sanity gate
#' (\code{\link{computePolicyStringencySanity}} via the expanding-batch walker).
#' Writes the standard Run-Group artifacts into \code{<resultsDir>/<group>/}:
#' \code{sweep.rds}, \code{selected-models-psm.yml}, and \code{manifest.json}.
#'
#' The PSM has \strong{one} stage (no adoption hurdle), its outcome axis headline
#' is the sectoral Bulk/Diffuse mapping, and estimator adjudication never touches
#' selection — the sweep runs on the satP engine only ("PSM Estimator Suite" in
#' CONTEXT.md); the estimator-agreement exhibit is computed on the selected spec
#' afterwards (\code{\link{computeEstimatorAgreement}}).
#'
#' @param group Character. Run-Group name (e.g. \code{"psm-exhaustive"}). Required.
#' @param mode Character. \code{"exhaustive"} or \code{"guided"} spec grid.
#' @param resultsDir,modelDir,cachefolder,gdxFile As in \code{\link{runSweep}}.
#' @param panelData Optional pre-built historical panel. When NULL it is built
#'   with \code{panelDataHistorical(includePolicyStringency = TRUE)}.
#' @param scenarioData Optional pre-built scenario panel (enables the sanity gate).
#' @param specs Optional list of normalised specs (bypasses
#'   \code{createChannelConfigs}; used by tests and custom experiments). They are
#'   still passed through \code{\link{psmSpecs}}.
#' @param y,outputRegionMappingFile,movingAverage,sectors,nCores,forceRefit,verbose
#'   As in \code{\link{runSweep}}.
#' @param indexMax Numeric. Index ceiling (CAPMF: 10).
#' @param selectFE Character vector or NULL. Region-FE constraint on the
#'   deliverable (model-name \code{fe:} tags), as in the price-model selection.
#' @param nearTieEps,feParsimonyWeight,dropIdleControls,softVifGate Maximin knobs,
#'   forwarded to \code{\link{computeMaximinScore}}.
#' @param trendDominanceGate Numeric or \code{NULL}. Hard trend-dominance gate
#'   forwarded to \code{\link{computeMaximinScore}} (ADR 0033). \strong{Defaults to
#'   \code{0.9} for the PSM}, relaxed from the price model's \code{0.5}: the CAPMF
#'   policy-stringency index accumulates almost monotonically over time, so the linear
#'   \code{timeTrend} is a legitimate common-shock control rather than atheoretical
#'   extrapolation. The first real-data sweep (2026-07-06) had a median \code{trendShare}
#'   of 0.64, so a \code{0.5} gate rejected the trended-but-driver-informed majority; the
#'   \code{0.9} ceiling still culls near-pure-trend degenerates (trendShare > 0.9). The
#'   \emph{soft} within-tie low-trend preference is retained and \code{trendShare} is
#'   surfaced in the report, so a trend-heavy winner stays visible. \code{NULL} disables
#'   the hard gate entirely.
#' @param deltaR2Max Numeric. Fit-reliability gate forwarded to
#'   \code{\link{computeMaximinScore}} (default \code{1} — a mathematical validity
#'   bound on the incremental McFadden pseudo-R2; a spec exceeding it is degenerate).
#' @param inferenceTGate Numeric or \code{NULL}. Inference-fragility within-band
#'   preference (ADR 0037), forwarded to \code{\link{computeMaximinScore}}. Defaults
#'   to \code{2.33} (roughly p < .02) for the PSM: among theory-equivalent specs a
#'   p=.049 squeaker on a theory term loses the near-tie to a comfortable margin.
#'   Never a hard gate — significance is not an admission criterion. \code{NULL}
#'   disables.
#' @param sanityBatchSize,sanityMaxModels,sanityThresholds Sanity-walk knobs;
#'   thresholds are the \code{\link{computePolicyStringencySanity}} overrides.
#' @param overwriteConfig Logical. Regenerate the auto-generated spec YAML.
#'
#' @return Invisibly, a list: \code{results} (per-fit metrics), \code{coefficients},
#'   \code{maximin}, \code{selected}, \code{sanity}, \code{specs},
#'   \code{selectedConfigPath}, \code{fitSummary}.
#'
#' @seealso \code{\link{runSweep}}, \code{\link{estimatePolicyStringencyModel}},
#'   \code{\link{computePolicyStringencySanity}}, ADR 0036
#' @export
#' @author Renato Rodrigues
runPSMSweep <- function(group,
                        mode = c("exhaustive", "guided"),
                        resultsDir = getOption("pfm.resultsDir", "output"),
                        modelDir = getOption("pfm.modelDir", "output"),
                        cachefolder = NULL,
                        gdxFile = NULL,
                        panelData = NULL,
                        scenarioData = NULL,
                        specs = NULL,
                        y = 2000:2022,
                        outputRegionMappingFile = "regionmapping_54.csv",
                        movingAverage = 5,
                        sectors = c("Bulk", "Diffuse"),
                        indexMax = 10,
                        nCores = 1L,
                        forceRefit = FALSE,
                        selectFE = c("H12", "OECDp", "Mundlak"),
                        nearTieEps = 0.025,
                        feParsimonyWeight = 0,
                        dropIdleControls = TRUE,
                        softVifGate = 6,
                        trendDominanceGate = 0.9,
                        deltaR2Max = 1,
                        inferenceTGate = 2.33,
                        sanityBatchSize = 5,
                        sanityMaxModels = 20,
                        sanityThresholds = list(),
                        overwriteConfig = TRUE,
                        verbose = TRUE) {
  mode <- match.arg(mode)
  if (missing(group) || is.null(group) || !nzchar(group)) {
    stop("runPSMSweep: 'group' is required.", call. = FALSE)
  }
  if (is.null(resultsDir)) {
    stop("runPSMSweep: supply 'resultsDir' or set options(pfm.resultsDir = '...').", call. = FALSE)
  }
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("runPSMSweep: the 'yaml' package is required.")
  }
  say <- function(...) if (isTRUE(verbose)) message("[runPSMSweep:", group, "] ", ...)

  .useMadratCache(cachefolder)
  t0 <- Sys.time()

  groupDir <- file.path(resultsDir, group)
  dir.create(groupDir, showWarnings = FALSE, recursive = TRUE)
  if (!is.null(modelDir)) {
    dir.create(modelDir, showWarnings = FALSE, recursive = TRUE)
    options(pfm.modelDir = modelDir)
  }

  # ── Historical panel (with the PSM outcomes) ─────────────────────────────────
  if (is.null(panelData)) {
    say("Building historical panel incl. Policy Stringency (", min(y), "-", max(y), ") ...")
    panelData <- panelDataHistorical(
      aggregate = TRUE, y = y,
      outputRegionMappingFile = outputRegionMappingFile, movingAverage = movingAverage,
      includePolicyStringency = TRUE
    )
  }
  psVars <- paste0("Policy Stringency|", sectors)
  if (magclass::is.magpie(panelData) && !all(psVars %in% magclass::getNames(panelData))) {
    stop("runPSMSweep: panelData lacks ", paste(setdiff(psVars, magclass::getNames(panelData)),
         collapse = ", "), " - build it with includePolicyStringency = TRUE.", call. = FALSE)
  }
  if (magclass::is.magpie(panelData) &&
        "GDP per Capita" %in% magclass::getNames(panelData) &&
        !"GDP per Capita Sq" %in% magclass::getNames(panelData)) {
    panelData <- magclass::mbind(
      panelData,
      magclass::setNames(panelData[, , "GDP per Capita"]^2, "GDP per Capita Sq")
    )
  }
  if (!is.null(modelDir)) saveTrainingPanel(panelData, dir = modelDir)

  # ── Scenario panel (optional; enables the bounded-index sanity gate) ─────────
  if (is.null(scenarioData) && !is.null(gdxFile) && file.exists(gdxFile)) {
    say("Building scenario panel from gdx ...")
    scenarioData <- tryCatch(
      panelDataScenario(gdxFile = gdxFile, aggregate = TRUE,
                        outputRegionMappingFile = outputRegionMappingFile),
      error = function(e) {
        say("scenario panel failed (", conditionMessage(e), "); sanity gate skipped.")
        NULL
      }
    )
  }

  # ── Specs: shared channel grid adapted for the PSM ───────────────────────────
  if (is.null(specs)) {
    configPath <- createChannelConfigs(groupDir, mode, overwrite = overwriteConfig)
    specs <- yaml::read_yaml(configPath)
  }
  specs <- psmSpecs(specs, verbose = verbose)
  say(length(specs), " PSM specs after adaptation.")

  # ── Fit grid: one stage, satP engine ─────────────────────────────────────────
  grid <- runFitGrid(
    specs = specs, sectors = sectors, stages = "PolicyStringency", panelData = panelData,
    family = "gaussian", modelDir = modelDir, nCores = nCores, forceRefit = forceRefit,
    verbose = verbose, say = say
  )
  results <- grid$results

  # ── Selection: maximin, then the bounded-index sanity gate ───────────────────
  specByName <- stats::setNames(specs, vapply(specs, `[[`, character(1), "name"))
  maximin <- list()
  selected <- list()
  sanity <- list()
  sub <- results[results$stage == "PolicyStringency", , drop = FALSE]
  if (!is.null(selectFE)) {
    fePat <- paste0("fe:(", paste(selectFE, collapse = "|"), ")")
    keep <- grepl(fePat, sub$model)
    if (any(keep)) {
      sub <- sub[keep, , drop = FALSE]
    } else {
      say("WARNING: selectFE matched no PSM specs; ignoring the FE constraint.")
    }
  }
  mmCols <- intersect(c("model", "sector", "sigActorPower", "sigInstQual",
                        "sigInteractions", "deltaR2Theory", "pseudoR2", "bic", "maxVIF",
                        "converged", "usesLagged", "nFE", "nObs", "sigControl", "nControl",
                        "trendShare", "minSigTheoryT"), colnames(sub))
  mm <- computeMaximinScore(sub[, mmCols, drop = FALSE], nearTieEps = nearTieEps,
                            feParsimonyWeight = feParsimonyWeight,
                            dropIdleControls = dropIdleControls,
                            softVifGate = softVifGate,
                            trendDominanceGate = trendDominanceGate,
                            deltaR2Max = deltaR2Max,
                            inferenceTGate = inferenceTGate)
  maximin[["PolicyStringency"]] <- mm
  pass <- mm[mm$gatePass, , drop = FALSE]
  if (nrow(pass) == 0) {
    # Self-diagnosing: tally WHY every spec failed the hard gate (reason labels,
    # sector suffix stripped) so the SLURM log states the cause without sweep.rds.
    reasons <- mm$gateFailReason[nzchar(mm$gateFailReason)]
    labels <- trimws(sub(":.*$", "", unlist(strsplit(reasons, "; "))))
    tally <- sort(table(labels), decreasing = TRUE)
    say("WARNING: no gate-passing PSM spec. Gate-failure tally: ",
        if (length(tally)) paste(sprintf("%s (x%d)", names(tally), as.integer(tally)),
                                 collapse = "; ") else "(no reasons recorded)")
  } else if (is.null(scenarioData)) {
    selected[["PolicyStringency"]] <- pass$model[1]
    say("Selected PSM spec (maximin only - no scenario, sanity gate skipped): ",
        pass$model[1], " (", pass$minTier[1], ", mean dR2 = ",
        round(pass$meanDeltaR2[1], 3), ")")
  } else {
    sel <- .psmSanitySelect(
      passModels = pass$model, specByName = specByName, sectors = sectors,
      panelData = panelData, scenarioData = scenarioData, modelDir = modelDir,
      batchSize = sanityBatchSize, maxModels = sanityMaxModels,
      thresholds = sanityThresholds, regionBlocks = .h12RegionBlocks(),
      histIndexBySector = .histIndexBySector(panelData, sectors),
      indexMax = indexMax, say = say
    )
    selected[["PolicyStringency"]] <- sel$chosen
    sanity[["PolicyStringency"]] <- sel
    say("Selected PSM spec: ", sel$chosen,
        if (isTRUE(sel$forced)) " (LEAST-FLAGGED fallback - no candidate passed sanity)"
        else " (passed bounded-index Projection Sanity)")
  }
  results$tier <- computeTheoryTier(results$sigActorPower, results$sigInstQual,
                                    results$sigInteractions)

  # ── Run-Group artifacts ───────────────────────────────────────────────────────
  selectedConfigPath <- NULL
  if (length(selected) > 0) {
    selectedConfigPath <- .writeSelectedPSMConfig(
      specs = specs, selected = selected, maximin = maximin,
      mode = mode, configDir = groupDir, sectors = sectors, indexMax = indexMax
    )
  }
  res <- list(
    results = results, coefficients = grid$coefficients,
    maximin = maximin, selected = selected, sanity = sanity,
    specs = specs, selectedConfigPath = selectedConfigPath,
    fitSummary = list(nJobs = grid$nJobs, nNew = grid$nNew, nFailed = grid$nFailed)
  )
  saveRDS(res, file.path(groupDir, "sweep.rds"))

  .writeRunGroupManifest(
    groupDir, group = group, mode = paste0("psm-", mode),
    panelData = panelData, scenarioData = scenarioData, gdxFile = gdxFile,
    selectionMethod = "psm-maximin", nCores = nCores
  )
  .recordStep(groupDir, group, "sweep-psm", t0, mode = paste0("psm-", mode),
              metrics = c(res$fitSummary, list(nCores = nCores)))

  say("PSM Run-Group written: ", groupDir)
  invisible(res)
}

#' Adapt the shared channel spec grid for the Policy Stringency Model
#'
#' @description
#' The PSM mirrors the carbon-price model's theory RHS by design (ADR 0036), so
#' it sweeps the \emph{same} channel spec grid — minus the axes that have no PSM
#' meaning:
#' \itemize{
#'   \item \strong{FD/hazard transforms dropped} — the PSM currently estimates in
#'     levels only (no adoption stage exists, and the FD-on-logit-scale variant is
#'     future work), so \code{panelTransform != "levels"} specs are removed.
#'   \item \strong{Saturating-price twins dropped} — the \code{priceLink} axis
#'     (ADR 0028) is the price model's bounded-form competition; the PSM is
#'     \emph{always} the bounded satP form, so the \code{"| satP"} twins would be
#'     duplicates.
#'   \item \strong{Ridge specs dropped} — not supported by the PSM estimator.
#'   \item \code{includeLaggedECP} (the price-lag dynamics rung) maps to
#'     \code{includeLaggedPS} (the policy-ratcheting rung).
#' }
#'
#' @param specs List of spec lists (raw YAML entries or already normalised).
#' @param verbose Logical.
#' @return The adapted, normalised spec list.
#' @export
psmSpecs <- function(specs, verbose = TRUE) {
  normalize <- function(cfg) {
    for (f in c("actorPowerDrivers", "actorPowerIndex", "instQualityDrivers", "controlDrivers")) {
      if (!is.null(cfg[[f]])) cfg[[f]] <- unlist(cfg[[f]])
    }
    for (f in c("includeLagged", "includeLaggedECP", "includeLaggedPS", "nickellCorrection",
                "interactRegionFE", "logisticTimeTrend", "useMundlak", "gdpGovInteraction",
                "ridgeInteractions", "stringencyOnly")) {
      cfg[[f]] <- isTRUE(cfg[[f]])
    }
    cfg$panelTransform <- if (is.null(cfg$panelTransform)) "levels" else cfg$panelTransform
    cfg
  }
  specs <- lapply(specs, normalize)
  n0 <- length(specs)
  keep <- vapply(specs, function(cfg) {
    identical(cfg$panelTransform, "levels") &&
      !isTRUE(cfg$stringencyOnly) &&
      !isTRUE(cfg$ridgeInteractions)
  }, logical(1))
  specs <- specs[keep]
  specs <- lapply(specs, function(cfg) {
    cfg$includeLaggedPS <- isTRUE(cfg$includeLaggedPS) || isTRUE(cfg$includeLaggedECP)
    cfg$includeLaggedECP <- FALSE
    # Adoption-only knobs have no PSM meaning; neutralised so the maximin
    # unintended-lag gate keys off nothing.
    cfg$includeLagged <- FALSE
    cfg$nickellCorrection <- FALSE
    cfg
  })
  if (isTRUE(verbose) && length(specs) < n0) {
    message("[psmSpecs] ", n0 - length(specs), " of ", n0,
            " specs dropped (FD transforms, satP twins, ridge).")
  }
  specs
}

# Internal: writes selected-models-psm.yml (one entry per sector, single stage).
#' @keywords internal
.writeSelectedPSMConfig <- function(specs, selected, maximin, mode, configDir,
                                    sectors, indexMax = 10) {
  specByName <- stats::setNames(specs, vapply(specs, `[[`, character(1), "name"))
  entries <- list()
  for (stg in names(selected)) {
    cfg <- specByName[[selected[[stg]]]]
    if (is.null(cfg)) next
    mm <- maximin[[stg]]
    mmRow <- mm[mm$model == selected[[stg]], , drop = FALSE]
    for (sec in sectors) {
      e <- cfg
      e$model_type <- paste0(stg, ": ", sec)
      e$estimator <- "satP"
      e$indexMax <- indexMax
      e$description <- paste0(
        "Maximin-selected PSM shared spec (psm-", mode, ", ", format(Sys.Date()), "). ",
        "Worse-sector tier: ", mmRow$minTier[1],
        " | mean dR2(theory): ", round(mmRow$meanDeltaR2[1], 3),
        " | ", mmRow$tierBySector[1], " | ", mmRow$deltaR2BySector[1]
      )
      entries[[length(entries) + 1L]] <- e
    }
  }
  out <- file.path(configDir, "selected-models-psm.yml")
  header <- paste0(
    "# PFM Selected Models - Policy Stringency Model (psm-", mode, ") - ",
    format(Sys.Date()), "\n",
    "# GENERATED by pfm::runPSMSweep() via the Maximin Selection Rule (ADR 0036).\n",
    "# One shared spec, applied to both sectors; satP engine, bounded index.\n\n"
  )
  writeLines(paste0(header, yaml::as.yaml(entries, indent.mapping.sequence = TRUE)), out)
  out
}
# nolint end
