# nolint start
#' @title runChannelsWorkflow
#' @description End-to-end driver for the Institutional Quality Channels model
#' selection (ADR 0004). In one call it:
#' \enumerate{
#'   \item creates the sweep YAML config if missing (\code{\link{createChannelConfigs}});
#'   \item runs the Stage-0 Channel Screen (\code{\link{computeChannelScreen}}) per
#'     sector and stage — no fits, correlation pruning + accountability ranking;
#'   \item fits every spec in the config for all sector/stage combinations
#'     (cached via the PFM model store when \code{modelDir} is set);
#'   \item scores Shared Specifications with the Maximin Selection Rule
#'     (\code{\link{computeMaximinScore}}) per stage, and reports the
#'     independently-best model per sector/stage as a secondary view;
#'   \item writes \code{selected-models-channels-<mode>.yml} with the winning
#'     shared spec per stage (both sectors), report-ready (\code{model_type} entries);
#'   \item optionally renders the pfm-reports outputs (model-selection sweep report,
#'     adoption / stringency / publication reports for the selected models);
#'   \item optionally updates \code{findings.md} with an auto-generated, marker-delimited
#'     results section.
#' }
#'
#' @param mode Character. \code{"guided"} (ADR 0004 staged algorithm config, 19 specs)
#'   or \code{"exhaustive"} (full combination suite, 187 specs).
#' @param panelData Optional \code{magpie} object or prepared panel. When \code{NULL},
#'   loaded via \code{panelDataHistorical}.
#' @param y Numeric vector of years for \code{panelDataHistorical}. Default \code{2000:2022}.
#' @param outputRegionMappingFile Character. Region mapping for data aggregation.
#'   Default \code{"regionmapping_54.csv"} (the model-selection report convention).
#' @param sectors Character vector. Default \code{c("Bulk", "Diffuse")}.
#' @param reportsDir Character or NULL. Path to the pfm-reports repository root.
#'   Required for writing configs into the reports tree, rendering reports, and
#'   updating findings.md. When NULL, configs go to \code{tempdir()} and the
#'   render/findings steps are skipped.
#' @param configDir Character or NULL. Directory for the sweep YAML and the selected-models
#'   YAML. When NULL, derived from \code{reportsDir} (else \code{tempdir()}). \code{runSweep}
#'   sets this to the Run-Group directory so the selected spec lands beside the other
#'   artifacts (ADR 0018).
#' @param modelDir Character or NULL. PFM model store for fit caching.
#' @param nCores Integer. Number of cores for the fit sweep. \code{1} (default) runs
#'   sequentially; \code{> 1} parallelises the fits via \pkg{future.apply}
#'   (\code{multisession} on Windows, \code{multicore} on Unix), falling back to sequential
#'   with a warning if \pkg{future.apply} is unavailable. See \code{\link{runFitGrid}} / ADR 0019.
#' @param forceRefit Logical. Ignore any cached fits and re-estimate every spec. Default
#'   \code{FALSE} (resume: cached fits are loaded, only missing ones recomputed).
#' @param family Character. Stringency GLM family for levels specs. Default \code{"Gamma"}.
#' @param scenarioData Optional \code{magpie}. Scenario panel (e.g.
#'   \code{panelDataScenario} output). When supplied, the Projection Sanity gate
#'   runs after maximin: candidates are evaluated in expanding batches
#'   (\code{sanityBatchSize} at a time, up to \code{sanityMaxModels}) and the
#'   selected spec is the best maximin-ranked model that passes all severe rules;
#'   if none passes, the least-flagged is chosen and marked. When \code{NULL},
#'   selection falls back to pure maximin with a loud message.
#' @param sanityBatchSize Integer. Batch size for the expanding sanity window.
#'   Default \code{5}.
#' @param sanityMaxModels Integer. Maximum candidates evaluated per stage.
#'   Default \code{20}.
#' @param sanityThresholds Named list. Overrides for
#'   \code{\link{computeProjectionSanity}} thresholds.
#' @param saveRds Logical. Save the workflow results to
#'   \code{<reportsDir>/output/channels_workflow_<mode>.rds} before rendering
#'   (the selection report consumes this file). Default \code{TRUE}.
#' @param selectModels Logical. Run maximin selection. Default \code{TRUE}.
#' @param selectionMethod Character. \code{"levels-first"} (default — the current
#'   behaviour: maximin over levels specs, then the Projection Sanity gate) or
#'   \code{"difference-first"} (Dynamic Identification First, ADR 0014: maximin over
#'   the \code{hybridFD} specs, then \code{\link{computeFalsificationGate}}, then the
#'   levels re-estimate + Projection Sanity, via \code{\link{selectDifferenceFirst}}).
#'   Both methods are supported; difference-first writes a separate
#'   \code{selected-models-channels-<mode>-difference-first.yml} and never overwrites
#'   the levels-first deliverable.
#' @param requireBothSectors Logical. Difference-first only: the Falsification Gate
#'   must pass in both sectors (\code{TRUE}, default) or any sector (\code{FALSE}).
#' @param falsificationPThreshold Numeric. Difference-first only: significance
#'   threshold for the Falsification Gate. Default \code{0.05}.
#' @param maxFalsificationTries Integer. Difference-first only: maximum number of
#'   ranked hybridFD candidates to falsification-test. Default \code{25}.
#' @param iqVanishTest Character. Difference-first only: IQ-vanish rule for the
#'   Falsification Gate — \code{"jointBlock"} (default, joint Wald test) or
#'   \code{"perChannel"}; see \code{\link{computeFalsificationGate}}.
#' @param levelsFE Difference-first only: named list of candidate block-FE strategies
#'   for the winner's levels re-estimate; the FE is chosen by the Projection Sanity
#'   gate (hybridFD specs carry no FE). Default \code{{H12, OECDp, Mundlak}}; see
#'   \code{\link{selectDifferenceFirst}}.
#' @param selectFE Character vector or \code{NULL}. When set, the deliverable
#'   selection is restricted to specifications whose region-FE resolution token
#'   appears here (matched against the spec name's \code{fe:} tag), e.g.
#'   \code{c("H12", "OECDp", "Mundlak")} to exclude pooled (\code{noFE}), the
#'   inflation-prone 54-unit (\code{FE54}), and the first-difference transforms
#'   (which carry no region FE). Selection-only; the full results and best-per-sector
#'   views still include every spec. Default \code{c("H12", "OECDp", "Mundlak")} —
#'   a real region-FE (or Mundlak) deliverable is required, so a pooled \code{noFE}
#'   spec is never auto-selected. Pass \code{NULL} to lift the constraint.
#' @param nearTieEps Numeric. BIC parsimony tie-break tolerance passed to
#'   \code{\link{computeMaximinScore}} (ADR 0012). Default \code{0.025} (tightened
#'   2026-06-24 from 0.05: specs more than 0.025 mean-\eqn{\Delta R^2}(theory) below
#'   the leader are no longer treated as theory-equivalent, so a lower-\eqn{\Delta R^2}
#'   spec cannot win purely on parsimony).
#' @param feParsimonyWeight Numeric in \code{[0, 1]}. Weight on the region-FE BIC
#'   penalty in the parsimony tie-break, forwarded to \code{\link{computeMaximinScore}}.
#'   Default \code{0} (region-FE dummies are not penalised, so FE granularity does not
#'   decide the selection on parameter count alone). Set \code{1} for classic BIC.
#' @param dropIdleControls Logical. Forwarded to \code{\link{computeMaximinScore}}: within a
#'   near-tie band, prefer specs without an idle (present-but-never-significant) control over
#'   otherwise-equivalent specs that carry one. Default \code{TRUE}.
#' @param softVifGate,temporalSignGate Numeric or \code{NULL}. Forwarded to
#'   \code{\link{computeMaximinScore}}: within a near-tie band, demote high-collinearity
#'   (\code{maxVIF > softVifGate}, default 6) and temporally sign-unstable
#'   (\code{temporalSignStable < temporalSignGate}) specs behind cleaner equivalents;
#'   low trend reliance is additionally preferred as a relative within-band ordering.
#'   \code{NULL} disables either preference. \strong{\code{temporalSignGate} defaults to
#'   \code{NULL}} (2026-06-25): temporal sign-stability no longer influences the deliverable — it
#'   is reported as the Temporal-Stability Frontier in the robustness step instead.
#' @param writeSelectedConfig Logical. Write \code{selected-models-channels-<mode>.yml}.
#'   Default \code{TRUE}.
#' @param renderReports Logical. Render the pfm-reports outputs (requires
#'   \code{reportsDir} and Rscript on PATH). Default \code{TRUE}.
#' @param renderRobustness Logical. After the consumer reports, build the robustness
#'   artifact (\code{build-robustness.R}) and render \code{reports/robustness/}
#'   (ADR 0012). Heavy (control specification-curve); set \code{FALSE} to skip.
#'   Default \code{TRUE}.
#' @param updateFindings Logical. Update the auto-generated section of findings.md.
#'   Default \code{TRUE}.
#' @param overwriteConfig Logical. Regenerate the sweep YAML even if present.
#'   Default \code{FALSE}.
#' @param verbose Logical. Progress messages. Default \code{TRUE}.
#'
#' @return Invisible list: \code{results} (per-fit metrics data.frame),
#'   \code{screens} (Channel Screen per sector/stage), \code{maximin} (per-stage
#'   ranking data.frames), \code{selected} (winning spec name per stage),
#'   \code{bestPerSector} (secondary view), \code{configPath},
#'   \code{selectedConfigPath}, \code{reportStatus} (named exit codes).
#'
#' @seealso ADR 0004, ADR 0005, \code{\link{computeMaximinScore}},
#'   \code{\link{computeChannelScreen}}, \code{\link{channelSpecs}}
#'
#' @export
#' @author Renato Rodrigues
runChannelsWorkflow <- function(mode = c("guided", "exhaustive"), # nolint: cyclocomp_linter.
                                panelData = NULL,
                                y = 2000:2022,
                                outputRegionMappingFile = "regionmapping_54.csv",
                                sectors = c("Bulk", "Diffuse"),
                                reportsDir = NULL,
                                configDir = NULL,
                                modelDir = getOption("pfm.modelDir", "output"),
                                nCores = 1L,
                                forceRefit = FALSE,
                                # Stringency is fit on log(1+ECP); with logTransform = TRUE
                                # (the default) estimatePriceStringencyModel uses
                                # gaussian(identity) regardless of this label. "gaussian"
                                # here is honest about what is actually estimated (Option 1).
                                family = "gaussian",
                                scenarioData = NULL,
                                sanityBatchSize = 5,
                                sanityMaxModels = 20,
                                sanityThresholds = list(),
                                saveRds = TRUE,
                                selectModels = TRUE,
                                selectionMethod = c("levels-first", "difference-first"),
                                selectFE = c("H12", "OECDp", "Mundlak"),
                                nearTieEps = 0.025,
                                feParsimonyWeight = 0,
                                dropIdleControls = TRUE,
                                softVifGate = 6,
                                temporalSignGate = NULL,
                                requireBothSectors = TRUE,
                                falsificationPThreshold = 0.05,
                                maxFalsificationTries = 25L,
                                iqVanishTest = "jointBlock",
                                levelsFE = list(
                                  H12     = list(fe = "regionmappingH12.csv",       mundlak = FALSE),
                                  OECDp   = list(fe = "regionmapping_EU_OECDp.csv", mundlak = FALSE),
                                  Mundlak = list(fe = NULL,                          mundlak = TRUE)),
                                writeSelectedConfig = TRUE,
                                renderReports = TRUE,
                                renderRobustness = TRUE,
                                updateFindings = TRUE,
                                overwriteConfig = FALSE,
                                verbose = TRUE) {
  mode <- match.arg(mode)
  selectionMethod <- match.arg(selectionMethod)
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("runChannelsWorkflow: the 'yaml' package is required.")
  }
  say <- function(...) if (isTRUE(verbose)) message("[channels:", mode, "] ", ...)

  # ── 1. Config ─────────────────────────────────────────────────────────────────
  # configDir: where the sweep YAML and the selected-models YAML are written. Explicit
  # override (used by runSweep to target the Run-Group) takes precedence; otherwise derive
  # from reportsDir, falling back to a temp dir.
  if (is.null(configDir)) {
    configDir <- if (!is.null(reportsDir)) {
      file.path(reportsDir, "reports", "model-selection", "model-configs")
    } else {
      tempdir()
    }
  }
  dir.create(configDir, showWarnings = FALSE, recursive = TRUE)
  configPath <- createChannelConfigs(configDir, mode, overwrite = overwriteConfig)

  normalizeCfg <- function(cfg) {
    for (f in c("actorPowerDrivers", "actorPowerIndex", "instQualityDrivers", "controlDrivers")) {
      if (!is.null(cfg[[f]])) cfg[[f]] <- unlist(cfg[[f]])
    }
    for (f in c("includeLagged", "includeLaggedECP", "nickellCorrection",
                "interactRegionFE", "logisticTimeTrend",
                "useMundlak", "gdpGovInteraction", "ridgeInteractions",
                "stringencyOnly")) {           # ADR 0028: saturating twin marker
      cfg[[f]] <- isTRUE(cfg[[f]])
    }
    cfg$panelTransform <- if (is.null(cfg$panelTransform)) "levels" else cfg$panelTransform
    cfg
  }
  specs <- lapply(yaml::read_yaml(configPath), normalizeCfg)
  say(length(specs), " specs loaded from ", basename(configPath))

  # ── 2. Panel data ─────────────────────────────────────────────────────────────
  if (is.null(panelData)) {
    say("Loading historical panel data (", min(y), "-", max(y), ") ...")
    panelData <- panelDataHistorical(
      aggregate = TRUE, y = y,
      outputRegionMappingFile = outputRegionMappingFile
    )
  }

  # ── 3. Stage-0 Channel Screen ─────────────────────────────────────────────────
  screenIQ <- c(
    "State Capacity PC1 (VDem)", "State Capacity PC2 (VDem)",
    "Government Effectiveness (WGI)", "Rule of Law (VDem)",
    "Vertical Accountability (VDem)", "Horizontal Accountability (VDem)",
    "Diagonal Accountability (VDem)"
  )
  screens <- list()
  screenSectors <- if (magclass::is.magpie(panelData)) sectors else character(0)
  if (length(screenSectors) == 0) say("Stage-0 screen skipped (panelData is not a magpie object).")
  for (sec in screenSectors) {
    scrDf <- tryCatch(
      preparePanelData(
        data = panelData, sector = sec,
        actorPowerDrivers = c("Innovator Power", "Incumbent Power"),
        actorPowerIndex = "Actor Power Index",
        instQualityDrivers = intersect(screenIQ, magclass::getNames(panelData)),
        controlDrivers = "GDP per Capita (Q-centred)",
        regionMappingFixedEffects = NULL
      ),
      error = function(e) NULL
    )
    if (is.null(scrDf)) next
    scrDf$adoption <- as.integer(scrDf$ecp > 0)
    screens[[paste0(sec, "_Adoption")]] <- tryCatch(
      computeChannelScreen(scrDf, depVar = "adoption"), error = function(e) NULL
    )
    strDf <- scrDf[scrDf$ecp > 0, , drop = FALSE]
    strDf$ecp <- log1p(strDf$ecp)
    screens[[paste0(sec, "_Stringency")]] <- tryCatch(
      computeChannelScreen(strDf, depVar = "ecp"), error = function(e) NULL
    )
  }
  say("Stage-0 Channel Screen complete (", length(screens), " sector/stage screens).")

  # ── 4. Fit all specs x sectors x stages (parallel-capable; ADR 0019) ──────────
  # The triple loop is delegated to runFitGrid(): one mappable fitOneSpec path used by both
  # the sequential (nCores = 1) and parallel (future.apply) backends. Workers self-persist
  # their {id}.rds without touching the index; the index is rebuilt once inside runFitGrid.
  stages <- c("Adoption", "Stringency")
  grid <- runFitGrid(
    specs = specs, sectors = sectors, stages = stages, panelData = panelData,
    family = family, modelDir = modelDir, nCores = nCores, forceRefit = forceRefit,
    verbose = verbose, say = say
  )
  results <- grid$results
  coefficients <- grid$coefficients

  # ── 5. Selection (maximin, then Projection Sanity gate when scenario given) ──
  maximin <- list()
  selected <- list()
  sanity <- list()
  difSelection <- NULL
  bestPerSector <- NULL
  specByName <- stats::setNames(specs, vapply(specs, `[[`, character(1), "name"))
  if (isTRUE(selectModels)) {
    # Observed historical prices per sector (price-seam rule) and H12 blocks
    histPricesBySector <- .histPricesBySector(panelData, sectors)
    regionBlocks <- .h12RegionBlocks()
    if (is.null(scenarioData)) {
      say("NOTE: no scenarioData supplied - Projection Sanity gate SKIPPED; ",
          "selection is pure maximin.")
    }
  }
  if (isTRUE(selectModels) && selectionMethod == "difference-first") {
    # ── Difference-First (Dynamic Identification First) selection (ADR 0014) ────
    say("Selection method: difference-first (hybridFD maximin -> Falsification Gate -> levels).")
    difSelection <- selectDifferenceFirst(
      results = results, specByName = specByName, panelData = panelData,
      scenarioData = scenarioData, sectors = sectors, family = family, modelDir = modelDir,
      nearTieEps = nearTieEps, feParsimonyWeight = feParsimonyWeight,
      dropIdleControls = dropIdleControls, softVifGate = softVifGate,
      temporalSignGate = temporalSignGate, requireBothSectors = requireBothSectors,
      pThreshold = falsificationPThreshold, maxTries = maxFalsificationTries,
      iqVanishTest = iqVanishTest, levelsFE = levelsFE,
      sanityBatchSize = sanityBatchSize, sanityMaxModels = sanityMaxModels,
      sanityThresholds = sanityThresholds, regionBlocks = regionBlocks,
      histPricesBySector = histPricesBySector, say = say)
    for (stg in names(difSelection)) {
      maximin[[stg]] <- difSelection[[stg]]$maximin
      if (!is.na(difSelection[[stg]]$chosen)) selected[[stg]] <- difSelection[[stg]]$chosen
      if (!is.null(difSelection[[stg]]$sanity)) sanity[[stg]] <- difSelection[[stg]]$sanity
      if (is.na(difSelection[[stg]]$chosen)) say("WARNING: no ", stg,
          " spec passed the Falsification Gate (difference-first).")
    }
  } else if (isTRUE(selectModels)) {
    for (stg in stages) {
      sub <- results[results$stage == stg, , drop = FALSE]
      # Optional FE constraint (ADR 0011 / fit-reliability follow-up 2026-06-16):
      # restrict the deliverable to specs whose region-FE resolution is in selectFE
      # (e.g. c("H12","OECDp","Mundlak") to exclude pooled `noFE`, the inflation-prone
      # 54-unit `FE54`, and the FD transforms that carry no region FE). Applies to
      # selection only; the full `results`/bestPerSector views are unaffected.
      if (!is.null(selectFE)) {
        fePat <- paste0("fe:(", paste(selectFE, collapse = "|"), ")")
        keep <- grepl(fePat, sub$model)
        if (any(keep)) {
          sub <- sub[keep, , drop = FALSE]
        } else {
          say("WARNING: selectFE=[", paste(selectFE, collapse = ","),
              "] matched no ", stg, " specs; ignoring the FE constraint for this stage.")
        }
      }
      mmCols <- intersect(c("model", "sector", "sigActorPower", "sigInstQual",
                            "sigInteractions", "deltaR2Theory", "pseudoR2", "bic", "maxVIF",
                            "converged", "usesLagged", "nFE", "nObs", "sigControl", "nControl",
                            "trendShare"), colnames(sub))
      mm <- computeMaximinScore(sub[, mmCols, drop = FALSE], nearTieEps = nearTieEps,
                                feParsimonyWeight = feParsimonyWeight,
                                dropIdleControls = dropIdleControls,
                                softVifGate = softVifGate, temporalSignGate = temporalSignGate)
      # Item 3 (2026-06-24): temporal sign-stability is a SOFT within-band preference. Compute it
      # for the near-tie band candidates ONLY (early-window refit + theory-term sign compare),
      # add the column, and re-rank so temporally-unstable specs are demoted. Defensive: skipped
      # without panelData / when the band is trivial; per-candidate failures leave NA (no demotion).
      if (!is.null(temporalSignGate) && !is.null(panelData)) {
        pass0 <- mm[mm$gatePass, , drop = FALSE]
        if (nrow(pass0) > 1) {
          lead <- pass0$meanDeltaR2[1]; leadTier <- pass0$minTier[1]
          bandModels <- pass0$model[pass0$minTier == leadTier & !is.na(pass0$meanDeltaR2) &
                                      pass0$meanDeltaR2 >= lead - nearTieEps]
          tss <- tryCatch(.bandTemporalSignStable(bandModels, specByName, panelData, stg, sectors,
                                                  family, modelDir, say), error = function(e) NULL)
          if (!is.null(tss) && nrow(tss) && any(!is.na(tss$temporalSignStable))) {
            sub$temporalSignStable <- tss$temporalSignStable[
              match(paste(sub$model, sub$sector), paste(tss$model, tss$sector))]
            mm <- computeMaximinScore(sub[, c(mmCols, "temporalSignStable"), drop = FALSE],
                                      nearTieEps = nearTieEps, feParsimonyWeight = feParsimonyWeight,
                                      dropIdleControls = dropIdleControls, softVifGate = softVifGate,
                                      temporalSignGate = temporalSignGate)
          }
        }
      }
      maximin[[stg]] <- mm
      pass <- mm[mm$gatePass, , drop = FALSE]
      if (nrow(pass) == 0) {
        say("WARNING: no gate-passing shared spec for ", stg)
        next
      }
      if (is.null(scenarioData)) {
        selected[[stg]] <- pass$model[1]
        say("Selected ", stg, " shared spec (maximin only): ", pass$model[1],
            " (", pass$minTier[1], ", mean dR2 = ", round(pass$meanDeltaR2[1], 3), ")")
      } else {
        sel <- .sanitySelect(
          passModels = pass$model, stg = stg, specByName = specByName,
          sectors = sectors, panelData = panelData, scenarioData = scenarioData,
          family = family, modelDir = modelDir,
          batchSize = sanityBatchSize, maxModels = sanityMaxModels,
          thresholds = sanityThresholds, regionBlocks = regionBlocks,
          histPricesBySector = histPricesBySector, say = say
        )
        selected[[stg]] <- sel$chosen
        sanity[[stg]] <- sel
        say("Selected ", stg, " shared spec: ", sel$chosen,
            if (isTRUE(sel$forced)) " (LEAST-FLAGGED fallback - no candidate passed sanity)"
            else " (passed Projection Sanity)")
      }
    }
  }
  if (isTRUE(selectModels)) {
    results$tier <- computeTheoryTier(results$sigActorPower, results$sigInstQual,
                                      results$sigInteractions)
    tierRank <- c(Green = 3L, Blue = 2L, Yellow = 1L)
    bps <- results[results$converged %in% TRUE & results$maxVIF < 10, , drop = FALSE]
    bps <- bps[order(bps$stage, bps$sector, -tierRank[bps$tier], -bps$deltaR2Theory), ]
    bestPerSector <- do.call(rbind, lapply(
      split(bps, paste(bps$stage, bps$sector)),
      function(d) d[1, c("model", "sector", "stage", "tier", "deltaR2Theory",
                         "theoryFrac", "aic", "maxVIF", "panelTransform"), drop = FALSE]
    ))
    rownames(bestPerSector) <- NULL
  }

  # ── 6. Selected-models config ─────────────────────────────────────────────────
  selectedConfigPath <- NULL
  if (isTRUE(writeSelectedConfig) && length(selected) > 0) {
    selectedConfigPath <- if (selectionMethod == "difference-first") {
      .writeDifferenceFirstConfig(difSelection, mode, configDir, sectors)
    } else {
      .writeSelectedChannelConfig(specs, selected, maximin, mode, configDir, sectors)
    }
    say("Selected-models config written: ", selectedConfigPath)
  }

  # ── 7. Assemble results and save the RDS (the selection report consumes it) ──
  out <- list(
    mode = mode, results = results, coefficients = coefficients,
    screens = screens, maximin = maximin, selected = selected, sanity = sanity,
    bestPerSector = bestPerSector, selectionMethod = selectionMethod,
    difSelection = difSelection,
    fitSummary = grid[c("nJobs", "nNew", "nFailed")],
    configPath = configPath, selectedConfigPath = selectedConfigPath,
    generated = Sys.time()
  )
  rdsPath <- NULL
  if (isTRUE(saveRds) && !is.null(reportsDir)) {
    dir.create(file.path(reportsDir, "output"), showWarnings = FALSE, recursive = TRUE)
    rdsPath <- file.path(reportsDir, "output", paste0("channels_workflow_", mode, ".rds"))
    saveRDS(out, rdsPath)
    say("Workflow RDS saved: ", rdsPath)
  }
  out$rdsPath <- rdsPath

  # ── 8. Reports (pure consumers - ADR 0006) ────────────────────────────────────
  reportStatus <- NULL
  if (isTRUE(renderReports) && !is.null(reportsDir)) {
    reportStatus <- .renderChannelReports(reportsDir, mode, rdsPath, selectedConfigPath,
                                          verbose = verbose, robustness = renderRobustness)
  }
  out$reportStatus <- reportStatus

  # ── 9. findings.md ────────────────────────────────────────────────────────────
  if (isTRUE(updateFindings) && !is.null(reportsDir) && length(maximin) > 0) {
    .updateFindingsChannels(reportsDir, mode, maximin, selected, bestPerSector, results)
    say("findings.md updated.")
  }

  invisible(out)
}

# Internal: observed historical ECP per sector as long data.frames (seam rule).
#' @keywords internal
.histPricesBySector <- function(panelData, sectors) {
  if (!magclass::is.magpie(panelData)) return(NULL)
  out <- list()
  for (sec in sectors) {
    nm <- paste0("Effective Carbon Price|", sec)
    if (!nm %in% magclass::getNames(panelData)) next
    arr <- as.array(panelData[, , nm])[, , 1, drop = FALSE]
    dim(arr) <- dim(arr)[1:2]
    regions <- magclass::getRegions(panelData)
    years <- magclass::getYears(panelData, as.integer = TRUE)
    out[[sec]] <- data.frame(
      region = rep(regions, times = length(years)),
      year = rep(years, each = length(regions)),
      price = as.vector(arr),
      stringsAsFactors = FALSE
    )
  }
  out
}

# Internal: region -> H12 block mapping for the dead-block sanity rule.
#' @keywords internal
.h12RegionBlocks <- function() {
  tryCatch({
    mp <- madrat::toolGetMapping("regionmappingH12.csv", type = "regional",
                                 where = "mappingfolder")
    data.frame(region = mp$CountryCode, block = mp$RegionCode, stringsAsFactors = FALSE)
  }, error = function(e) NULL)
}

# Internal: expanding-batch Projection Sanity selection for one stage.
# Walks the maximin-ranked gate-passing models in batches of `batchSize` up to
# `maxModels`; returns the first model passing all severe rules in both sectors,
# else the least-flagged evaluable model (forced = TRUE).
#' @keywords internal
.sanitySelect <- function(passModels, stg, specByName, sectors, panelData,
                          scenarioData, family, modelDir, batchSize, maxModels,
                          thresholds, regionBlocks, histPricesBySector, say) {
  stageArg <- tolower(stg)
  traceRows <- list()
  flagsByModel <- list()
  chosen <- NULL
  forced <- FALSE
  least <- list(model = NULL, nSevere = Inf, nWarning = Inf)

  nEval <- 0L
  for (m in passModels) {
    if (nEval >= maxModels) break
    nEval <- nEval + 1L
    batch <- ceiling(nEval / batchSize)
    cfg <- specByName[[m]]
    nSevere <- 0L
    nWarning <- 0L
    evaluable <- TRUE
    reason <- ""
    modelFlags <- list()
    for (sec in sectors) {
      projReason <- ""
      proj <- tryCatch(
        projectSpecScenario(cfg, sec, histData = panelData, scenarioData = scenarioData,
                            family = family, modelDir = modelDir, verbose = FALSE),
        error = function(e) {
          projReason <<- conditionMessage(e)
          NULL
        }
      )
      if (is.null(proj)) {
        evaluable <- FALSE
        reason <- if (nzchar(projReason)) projReason
        else if (!identical(cfg$panelTransform %||% "levels", "levels")) {
          paste0("panelTransform '", cfg$panelTransform, "' not projectable (ADR 0005)")
        } else "projection returned NULL"
        break
      }
      sn <- computeProjectionSanity(
        proj, stage = stageArg,
        histPrices = histPricesBySector[[sec]],
        regionBlocks = regionBlocks, thresholds = thresholds
      )
      nSevere <- nSevere + sn$summary$nSevere
      nWarning <- nWarning + sn$summary$nWarning
      if (nrow(sn$flags) > 0) {
        f <- sn$flags
        f$sector <- sec
        modelFlags[[sec]] <- f
      }
    }
    pass <- evaluable && nSevere == 0L
    traceRows[[nEval]] <- data.frame(
      model = m, order = nEval, batch = batch, evaluable = evaluable,
      nSevere = if (evaluable) nSevere else NA_integer_,
      nWarning = if (evaluable) nWarning else NA_integer_,
      pass = pass, reason = reason, stringsAsFactors = FALSE
    )
    if (length(modelFlags) > 0) {
      flagsByModel[[m]] <- do.call(rbind, modelFlags)
    }
    if (evaluable && (nSevere < least$nSevere ||
                        (nSevere == least$nSevere && nWarning < least$nWarning))) {
      least <- list(model = m, nSevere = nSevere, nWarning = nWarning)
    }
    say("  [sanity:", stg, "] #", nEval, " (batch ", batch, ") ", m, " - ",
        if (!evaluable) paste0("not evaluable (", reason, ")")
        else paste0(nSevere, " severe / ", nWarning, " warnings",
                    if (pass) " - PASS" else ""))
    if (pass) {
      chosen <- m
      break
    }
    # Expanding-batch semantics: only continue past a batch boundary if no
    # model so far has passed (which is the case if we are still in the loop).
  }

  if (is.null(chosen)) {
    forced <- TRUE
    chosen <- least$model %||% passModels[1]
  }
  list(
    chosen = chosen, forced = forced,
    trace = if (length(traceRows) > 0) do.call(rbind, traceRows) else NULL,
    flags = flagsByModel
  )
}

# Internal: one metrics row per fit (mirrors the model-selection report columns
# that feed selection; NULL fits become gate-failing rows).
#' @keywords internal
.channelFitMetrics <- function(fit, cfg, sector, stage) {
  base <- data.frame(
    model = cfg$name, sector = sector, stage = stage,
    panelTransform = cfg$panelTransform,
    priceLink = cfg$priceLink %||% "log1p",   # ADR 0028: "log1p" or "saturating" (satP twin)
    sigActorPower = 0L, sigInstQual = 0L, sigInteractions = 0L,
    sigControl = 0L, nControl = length(cfg$controlDrivers),
    deltaR2Theory = NA_real_, theoryFrac = NA_real_,
    aic = NA_real_, bic = NA_real_, pseudoR2 = NA_real_, nObs = NA_integer_, nFE = NA_integer_,
    trendShare = NA_real_, nonTheoryShare = NA_real_,
    maxVIF = NA_real_, converged = FALSE, usesLagged = isTRUE(cfg$includeLagged),
    brier = NA_real_, auc = NA_real_, calibrationSlope = NA_real_, rmse = NA_real_,
    stringsAsFactors = FALSE
  )
  if (is.null(fit) || is.null(fit$model)) return(base)
  m <- fit$model

  ct <- fit$coeftest
  pvals <- if (!is.null(ct)) ct[, 4] else NULL
  sigVars <- if (!is.null(pvals)) rownames(ct)[!is.na(pvals) & pvals < 0.05] else character(0)
  sigInt <- sum(grepl(":|_x_", sigVars))
  sigBase <- sigVars[!grepl(":|_x_", sigVars)]
  countMatches <- function(vars, patterns) {
    if (length(vars) == 0 || is.null(patterns) || length(patterns) == 0) return(0L)
    pats <- make.names(patterns)
    sum(vapply(vars, function(v) any(vapply(pats, function(p) grepl(p, v, fixed = TRUE),
                                            logical(1))), logical(1)))
  }
  base$sigInteractions <- sigInt
  base$sigInstQual <- countMatches(sigBase, cfg$instQualityDrivers)
  base$sigActorPower <- countMatches(sigBase, c(cfg$actorPowerDrivers, cfg$actorPowerIndex))
  # Control-term significance (for the drop-idle-control tie-break, 2026-06-24): a control
  # is "idle" when present (nControl > 0) but never significant across sectors.
  base$sigControl <- countMatches(sigBase, cfg$controlDrivers)

  isLf <- inherits(m, "logistf")
  k <- length(stats::coef(m))
  # Region-FE parameter count (for the FE-discounted BIC parsimony tie-break, 2026-06-24).
  base$nFE <- tryCatch(sum(grepl("^regionFE", names(stats::coef(m)))), error = function(e) NA_integer_)
  # Trend reliance = share of the fitted linear-predictor variance from the time-trend term
  # (for the low-trend selection preference, 2026-06-24). NA when not computable -> no preference.
  base$trendShare <- tryCatch({
    co <- stats::coef(m)
    tn <- intersect(c("logisticTimeTrend", "timeTrend"), names(co))
    d <- fit$data %||% m$data %||% m$model
    if (length(tn) == 1L && is.finite(co[[tn]]) && !is.null(d) && tn %in% names(d)) {
      contrib <- co[[tn]] * as.numeric(d[[tn]])
      mm0 <- stats::model.matrix(stats::as.formula(fit$formula), data = d)
      sh <- intersect(colnames(mm0), names(co))
      lp <- as.numeric(mm0[, sh, drop = FALSE] %*% co[sh])
      vlp <- stats::var(lp, na.rm = TRUE)
      if (is.finite(vlp) && vlp > 0) stats::var(contrib, na.rm = TRUE) / vlp else NA_real_
    } else NA_real_
  }, error = function(e) NA_real_)
  # Non-theory contribution share (ADR 0033, DIAGNOSTIC — reported, not gated): the share of the
  # fitted linear-predictor variance carried by the NON-theory, NON-FE terms (controls + time trend).
  # High values flag a spec whose signal is atheoretical (controls/trend), the broader cousin of the
  # trend-dominance gate. Controls are legitimate confounders, so this is reported, never auto-rejected.
  base$nonTheoryShare <- tryCatch({
    co <- stats::coef(m); d <- fit$data %||% m$data %||% m$model
    mm0 <- stats::model.matrix(stats::as.formula(fit$formula), data = d)
    sh <- intersect(colnames(mm0), names(co))
    lp <- as.numeric(mm0[, sh, drop = FALSE] %*% co[sh]); vlp <- stats::var(lp, na.rm = TRUE)
    thVars <- c(cfg$actorPowerDrivers, cfg$actorPowerIndex, cfg$instQualityDrivers)
    isTheory <- function(nm) grepl(":|_x_", nm) ||
      any(vapply(thVars, function(t) nzchar(t) && grepl(t, nm, fixed = TRUE), logical(1)))
    nt <- sh[sh != "(Intercept)" & !grepl("^regionFE", sh) & !vapply(sh, isTheory, logical(1))]
    if (length(nt) && is.finite(vlp) && vlp > 0) {
      cnt <- as.numeric(mm0[, nt, drop = FALSE] %*% co[nt]); stats::var(cnt, na.rm = TRUE) / vlp
    } else NA_real_
  }, error = function(e) NA_real_)
  nObs <- if (isLf) m$n else tryCatch(stats::nobs(m), error = function(e) NA_integer_)
  loglik <- if (isLf) as.numeric(m$loglik["full"]) else
    tryCatch(as.numeric(stats::logLik(m)), error = function(e) NA_real_)
  base$aic <- if (isLf) -2 * loglik + 2 * k else as.numeric(m$aic)
  # BIC derived from the AIC already computed above (robust to slim cached fits):
  # BIC = AIC - 2*npar + npar*log(n) = AIC + npar*(log(n) - 2).
  # npar = k for the Firth logistf (penalised-likelihood BIC — approximate); k + 1 for the
  # Gaussian/Gamma stringency GLM (the +1 is the dispersion parameter), matching m$aic.
  npar <- if (isLf) k else k + 1L
  base$bic <- if (is.finite(base$aic) && is.finite(nObs) && nObs > 0) {
    base$aic + npar * (log(nObs) - 2)
  } else NA_real_
  base$pseudoR2 <- if (isLf) {
    as.numeric(1 - m$loglik["full"] / m$loglik["null"])
  } else if (!is.null(m$deviance) && !is.null(m$null.deviance) && m$null.deviance > 0) {
    1 - m$deviance / m$null.deviance
  } else NA_real_
  base$nObs <- as.integer(nObs)
  base$theoryFrac <- fit$theoryFrac %||% NA_real_
  base$maxVIF <- fit$maxVIF %||% NA_real_
  base$converged <- if (isLf) {
    !is.null(m$coefficients) && !any(is.na(m$coefficients))
  } else {
    isTRUE(m$converged)
  }
  base$deltaR2Theory <- computeDeltaR2Theory(
    fit,
    actorPowerDrivers = cfg$actorPowerDrivers, actorPowerIndex = cfg$actorPowerIndex,
    instQualityDrivers = cfg$instQualityDrivers,
    # PolicyStringency (ADR 0036) is a gaussian GLM like the price-stringency
    # stage, so its deviance-based baseline refit uses the stringency branch.
    stage = if (tolower(stage) == "adoption") "adoption" else "stringency"
  )
  pd <- fit$predictiveDiagnostics
  if (!is.null(pd)) {
    base$brier <- pd$brier %||% NA_real_
    base$auc <- pd$auc %||% NA_real_
    base$calibrationSlope <- pd$calibrationSlope %||% NA_real_
    base$rmse <- pd$rmse %||% NA_real_
  }
  base
}

# Internal: temporal sign-stability for the near-tie band candidates (Item 3, 2026-06-24). For
# each (model, sector) it refits on the early window (computeTemporalSplit single split, rolling
# disabled) and returns the fraction of THEORY-term coefficient signs that match the full fit.
# Defensive: any failure (e.g. Mundlak/lagged specs computeTemporalSplit can't fit) yields NA, so
# the spec simply carries no temporal demotion. Band-only, mirroring the .sanitySelect pattern.
#' @keywords internal
.bandTemporalSignStable <- function(models, specByName, panelData, stage, sectors, family,
                                    modelDir, say = function(...) invisible()) {
  if (!length(models)) return(NULL)
  say("temporal sign-stability for ", length(models), " band candidate(s) ...")
  rows <- list()
  for (mdl in models) {
    cfg <- specByName[[mdl]]
    if (is.null(cfg)) next
    for (sec in sectors) {
      frac <- tryCatch({
        ts <- computeTemporalSplit(
          data = panelData, sector = sec, stage = tolower(stage),
          actorPowerDrivers = cfg$actorPowerDrivers, actorPowerIndex = cfg$actorPowerIndex,
          instQualityDrivers = cfg$instQualityDrivers, controlDrivers = cfg$controlDrivers,
          regionMappingFixedEffects = cfg$regionMappingFixedEffects, family = family,
          logisticTimeTrend = isTRUE(cfg$logisticTimeTrend), rollingOrigins = integer(0),
          modelDir = modelDir, verbose = FALSE)
        cf <- ts$coef
        if (is.null(cf) || !nrow(cf)) NA_real_ else mean(as.logical(cf$signSame), na.rm = TRUE)
      }, error = function(e) NA_real_)
      rows[[length(rows) + 1L]] <- data.frame(model = mdl, sector = sec,
                                              temporalSignStable = frac, stringsAsFactors = FALSE)
    }
  }
  if (length(rows)) do.call(rbind, rows) else NULL
}

# Internal: writes selected-models-channels-<mode>.yml (4 model_type entries).
#' @keywords internal
.writeSelectedChannelConfig <- function(specs, selected, maximin, mode, configDir, sectors) {
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
      e$description <- paste0(
        "Maximin-selected shared spec (channels-", mode, ", ", format(Sys.Date()), "). ",
        "Worse-sector tier: ", mmRow$minTier[1],
        " | mean dR2(theory): ", round(mmRow$meanDeltaR2[1], 3),
        " | ", mmRow$tierBySector[1], " | ", mmRow$deltaR2BySector[1]
      )
      entries[[length(entries) + 1L]] <- e
    }
  }
  out <- file.path(configDir, paste0("selected-models-channels-", mode, ".yml"))
  header <- paste0(
    "# PFM Selected Models - Institutional Quality Channels (", mode, ") - ",
    format(Sys.Date()), "\n",
    "# GENERATED by pfm::runChannelsWorkflow() via the Maximin Selection Rule.\n",
    "# One shared spec per stage, applied to both sectors (Shared Specification).\n\n"
  )
  writeLines(paste0(header, yaml::as.yaml(entries, indent.mapping.sequence = TRUE)), out)
  out
}

# Internal: writes the Difference-First deliverable (ADR 0014) - the hybridFD
# maximin winner that passed the Falsification Gate, configured for LEVELS
# estimation. Separate file; never overwrites the levels-first selection.
#' @keywords internal
.writeDifferenceFirstConfig <- function(difSelection, mode, configDir, sectors) {
  if (is.null(difSelection)) return(NULL)
  entries <- list()
  for (stg in names(difSelection)) {
    d <- difSelection[[stg]]
    cfg <- d$chosenConfigLevels
    if (is.null(cfg) || is.na(d$chosen)) next
    fr <- d$falsification
    baseName <- if (!is.null(d$chosenBase) && !is.na(d$chosenBase)) d$chosenBase else d$chosen
    frRow <- if (!is.null(fr)) fr[fr$model == baseName, , drop = FALSE] else NULL
    for (sec in sectors) {
      e <- cfg
      e$name <- paste0(d$chosen, " [DIF->levels]")
      e$panelTransform <- "levels"
      e$model_type <- paste0(stg, ": ", sec)
      e$description <- paste0(
        "Difference-First / Dynamic Identification First (channels-", mode, ", ",
        format(Sys.Date()), "): hybridFD maximin winner that passed the Falsification ",
        "Gate (", if (!is.null(frRow) && nrow(frRow)) frRow$reason[1] else "AP persists / IQ vanishes",
        "), re-estimated in LEVELS for projection with FE = ",
        if (!is.null(d$chosenFE) && !is.na(d$chosenFE)) d$chosenFE else "H12",
        " (chosen by the levels Projection Sanity gate; ADR 0014)."
      )
      entries[[length(entries) + 1L]] <- e
    }
  }
  out <- file.path(configDir, paste0("selected-models-channels-", mode, "-difference-first.yml"))
  header <- paste0(
    "# PFM Selected Models - Difference-First / Dynamic Identification First (", mode, ") - ",
    format(Sys.Date()), "\n",
    "# GENERATED by pfm::runChannelsWorkflow(selectionMethod = 'difference-first') - ADR 0014.\n",
    "# hybridFD maximin -> Falsification Gate (pureFD) -> estimated in LEVELS for projection.\n\n"
  )
  writeLines(paste0(header, yaml::as.yaml(entries, indent.mapping.sequence = TRUE)), out)
  out
}

# Internal: renders the redesigned pure-consumer reports (ADR 0006);
# returns named exit codes.
#' @keywords internal
.renderChannelReports <- function(reportsDir, mode, rdsPath, selectedConfigPath,
                                  verbose = TRUE, robustness = TRUE) {
  oldwd <- setwd(reportsDir)
  on.exit(setwd(oldwd), add = TRUE)
  logDir <- file.path(reportsDir, "output", "logs")
  dir.create(logDir, showWarnings = FALSE, recursive = TRUE)

  runOne <- function(label, args) {
    logFile <- file.path(logDir, paste0(label, "_channels-", mode, ".log"))
    if (isTRUE(verbose)) message("[render] ", label, " ... (log: ", logFile, ")")
    status <- tryCatch(
      system2("Rscript", args, stdout = logFile, stderr = logFile),
      error = function(e) -1L
    )
    if (isTRUE(verbose)) {
      message("[render] ", label, if (identical(status, 0L)) " OK" else paste0(" FAILED (", status, ")"))
    }
    status
  }

  status <- c()
  if (!is.null(rdsPath) && file.exists(file.path(reportsDir, "reports", "selection", "run.R"))) {
    status <- c(status, selection = runOne("selection", c(
      "reports/selection/run.R",
      paste0("--workflowRds=", rdsPath),
      paste0("--reportName=channels-", mode)
    )))
  }
  if (!is.null(selectedConfigPath)) {
    if (file.exists(file.path(reportsDir, "reports", "results-adoption", "run.R"))) {
      status <- c(status, `results-adoption` = runOne("results-adoption", c(
        "reports/results-adoption/run.R",
        paste0("--reportName=channels-", mode),
        paste0("--modelConfig=", selectedConfigPath)
      )))
    }
    if (file.exists(file.path(reportsDir, "reports", "results-stringency", "run.R"))) {
      status <- c(status, `results-stringency` = runOne("results-stringency", c(
        "reports/results-stringency/run.R",
        paste0("--reportName=channels-", mode),
        paste0("--modelConfig=", selectedConfigPath)
      )))
    }
    status <- c(status, publication = runOne("publication", c(
      "reports/publication/run.R",
      paste0("--reportName=channels-", mode),
      paste0("--theoryConfig=", selectedConfigPath)
    )))
    # Robustness report (ADR 0012): build the artifact (Robustness Ladder, parsimony
    # frontier, control specification-curve — heavy, hence a separate build step) then
    # render the pure-consumer report. Skipped via robustness = FALSE.
    if (isTRUE(robustness) &&
        file.exists(file.path(reportsDir, "build-robustness.R")) &&
        file.exists(file.path(reportsDir, "reports", "robustness", "run.R"))) {
      status <- c(status, `robustness-build` = runOne("build-robustness", "build-robustness.R"))
      status <- c(status, robustness = runOne("robustness", c(
        "reports/robustness/run.R",
        paste0("--reportName=channels-", mode)
      )))
    }
  }
  status
}

# Internal: replaces/appends the auto-generated channels section in findings.md.
#' @keywords internal
.updateFindingsChannels <- function(reportsDir, mode, maximin, selected, bestPerSector,
                                    results) {
  path <- file.path(reportsDir, "findings.md")
  beginMark <- paste0("<!-- BEGIN channels-workflow:", mode, " (auto-generated) -->")
  endMark <- paste0("<!-- END channels-workflow:", mode, " -->")

  mdTable <- function(df, cols) {
    df <- df[, cols, drop = FALSE]
    fmt <- function(x) {
      if (is.numeric(x)) ifelse(is.na(x), "", format(round(x, 3), trim = TRUE)) else as.character(x)
    }
    body <- apply(as.data.frame(lapply(df, fmt), stringsAsFactors = FALSE,
                                check.names = FALSE), 1,
                  function(r) paste0("| ", paste(r, collapse = " | "), " |"))
    paste(c(
      paste0("| ", paste(cols, collapse = " | "), " |"),
      paste0("|", paste(rep("---", length(cols)), collapse = "|"), "|"),
      body
    ), collapse = "\n")
  }

  sec <- c(beginMark,
           paste0("## Channels Workflow - ", mode, " (auto-generated ", format(Sys.Date()), ")"),
           "")
  for (stg in names(maximin)) {
    mm <- utils::head(maximin[[stg]], 5)
    sec <- c(sec,
             paste0("### ", stg, " - Maximin ranking (top 5 of ", nrow(maximin[[stg]]), ")"),
             "",
             mdTable(mm, c("rank", "model", "minTier", "meanDeltaR2", "minDeltaR2",
                           "tierBySector", "gatePass")),
             "",
             if (!is.null(selected[[stg]])) {
               paste0("**Selected shared spec:** ", selected[[stg]])
             } else {
               "**No gate-passing shared spec.**"
             },
             "")
  }
  if (!is.null(bestPerSector) && nrow(bestPerSector) > 0) {
    sec <- c(sec, "### Best per sector/stage (secondary view)", "",
             mdTable(bestPerSector, c("stage", "sector", "model", "tier",
                                      "deltaR2Theory", "maxVIF", "panelTransform")),
             "")
  }
  sec <- c(sec,
           paste0("_", nrow(results), " fits; see output/model_selection_channels-",
                  mode, ".html for the full sweep._"),
           endMark)
  secText <- paste(sec, collapse = "\n")

  existing <- if (file.exists(path)) {
    readLines(path, warn = FALSE, encoding = "UTF-8")
  } else {
    character(0)
  }
  b <- which(existing == beginMark)
  e <- which(existing == endMark)
  newContent <- if (length(b) == 1 && length(e) == 1 && e > b) {
    c(existing[seq_len(b - 1)], strsplit(secText, "\n")[[1]],
      if (e < length(existing)) existing[(e + 1):length(existing)])
  } else {
    c(existing, "", strsplit(secText, "\n")[[1]])
  }
  con <- file(path, open = "wt", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(newContent, con)
}
# nolint end
