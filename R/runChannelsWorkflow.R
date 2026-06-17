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
#' @param modelDir Character or NULL. PFM model store for fit caching.
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
#' @param selectFE Character vector or \code{NULL}. When set, the deliverable
#'   selection is restricted to specifications whose region-FE resolution token
#'   appears here (matched against the spec name's \code{fe:} tag), e.g.
#'   \code{c("H12", "OECDp", "Mundlak")} to exclude pooled (\code{noFE}), the
#'   inflation-prone 54-unit (\code{FE54}), and the first-difference transforms
#'   (which carry no region FE). Selection-only; the full results and best-per-sector
#'   views still include every spec. Default \code{NULL} (no FE constraint).
#' @param writeSelectedConfig Logical. Write \code{selected-models-channels-<mode>.yml}.
#'   Default \code{TRUE}.
#' @param renderReports Logical. Render the pfm-reports outputs (requires
#'   \code{reportsDir} and Rscript on PATH). Default \code{TRUE}.
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
                                modelDir = getOption("pfm.modelDir", NULL),
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
                                selectFE = NULL,
                                writeSelectedConfig = TRUE,
                                renderReports = TRUE,
                                updateFindings = TRUE,
                                overwriteConfig = FALSE,
                                verbose = TRUE) {
  mode <- match.arg(mode)
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("runChannelsWorkflow: the 'yaml' package is required.")
  }
  say <- function(...) if (isTRUE(verbose)) message("[channels:", mode, "] ", ...)

  # ── 1. Config ─────────────────────────────────────────────────────────────────
  configDir <- if (!is.null(reportsDir)) {
    file.path(reportsDir, "reports", "model-selection", "model-configs")
  } else {
    tempdir()
  }
  configPath <- createChannelConfigs(configDir, mode, overwrite = overwriteConfig)

  normalizeCfg <- function(cfg) {
    for (f in c("actorPowerDrivers", "actorPowerIndex", "instQualityDrivers", "controlDrivers")) {
      if (!is.null(cfg[[f]])) cfg[[f]] <- unlist(cfg[[f]])
    }
    for (f in c("includeLagged", "includeLaggedECP", "nickellCorrection",
                "interactRegionFE", "logisticTimeTrend",
                "useMundlak", "gdpGovInteraction", "ridgeInteractions")) {
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

  # ── 4. Fit all specs x sectors x stages ──────────────────────────────────────
  stages <- c("Adoption", "Stringency")
  rows <- list()
  coefRows <- list()
  total <- length(specs) * length(sectors) * length(stages)
  done <- 0L
  for (i in seq_along(specs)) {
    cfg <- specs[[i]]
    for (stg in stages) {
      for (sec in sectors) {
        done <- done + 1L
        fit <- tryCatch({
          if (stg == "Adoption") {
            estimateAdoptionModel(
              data = panelData, sector = sec,
              actorPowerDrivers = cfg$actorPowerDrivers,
              actorPowerIndex = cfg$actorPowerIndex,
              instQualityDrivers = cfg$instQualityDrivers,
              controlDrivers = cfg$controlDrivers,
              includeLaggedAdoption = cfg$includeLagged,
              interactRegionFE = cfg$interactRegionFE,
              regionMappingFixedEffects = cfg$regionMappingFixedEffects,
              useMundlak = cfg$useMundlak,
              gdpGovInteraction = cfg$gdpGovInteraction,
              logisticTimeTrend = cfg$logisticTimeTrend,
              ridgeInteractions = cfg$ridgeInteractions,
              panelTransform = cfg$panelTransform,
              modelDir = modelDir, verbose = FALSE,
              compute = c(ame = FALSE, predictedProbs = FALSE)
            )
          } else {
            estimatePriceStringencyModel(
              data = panelData, sector = sec, family = family,
              actorPowerDrivers = cfg$actorPowerDrivers,
              actorPowerIndex = cfg$actorPowerIndex,
              instQualityDrivers = cfg$instQualityDrivers,
              controlDrivers = cfg$controlDrivers,
              includeLaggedECP = isTRUE(cfg$includeLaggedECP) || isTRUE(cfg$includeLagged),
              interactRegionFE = cfg$interactRegionFE,
              regionMappingFixedEffects = cfg$regionMappingFixedEffects,
              useMundlak = cfg$useMundlak,
              gdpGovInteraction = cfg$gdpGovInteraction,
              logisticTimeTrend = cfg$logisticTimeTrend,
              ridgeInteractions = cfg$ridgeInteractions,
              panelTransform = cfg$panelTransform,
              nickellCorrection = isTRUE(cfg$nickellCorrection),
              modelDir = modelDir, verbose = FALSE
            )
          }
        }, error = function(e) {
          say("FAILED ", cfg$name, " / ", stg, ": ", sec, " - ", conditionMessage(e))
          NULL
        })
        rows[[length(rows) + 1L]] <- .channelFitMetrics(fit, cfg, sec, stg)
        if (!is.null(fit) && !is.null(fit$coeftest)) {
          ct <- as.data.frame(unclass(fit$coeftest))
          names(ct) <- c("estimate", "stdError", "zValue", "pValue")
          ct$term <- rownames(fit$coeftest)
          ct$model <- cfg$name
          ct$sector <- sec
          ct$stage <- stg
          rownames(ct) <- NULL
          coefRows[[length(coefRows) + 1L]] <- ct
        }
        if (done %% 20 == 0 || done == total) say("fits: ", done, "/", total)
      }
    }
  }
  results <- do.call(rbind, rows)
  rownames(results) <- NULL
  coefficients <- if (length(coefRows) > 0) do.call(rbind, coefRows) else NULL

  # ── 5. Selection (maximin, then Projection Sanity gate when scenario given) ──
  maximin <- list()
  selected <- list()
  sanity <- list()
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
      mm <- computeMaximinScore(sub[, c("model", "sector", "sigActorPower", "sigInstQual",
                                        "sigInteractions", "deltaR2Theory", "pseudoR2", "maxVIF",
                                        "converged", "usesLagged"), drop = FALSE])
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
    selectedConfigPath <- .writeSelectedChannelConfig(
      specs, selected, maximin, mode, configDir, sectors
    )
    say("Selected-models config written: ", selectedConfigPath)
  }

  # ── 7. Assemble results and save the RDS (the selection report consumes it) ──
  out <- list(
    mode = mode, results = results, coefficients = coefficients,
    screens = screens, maximin = maximin, selected = selected, sanity = sanity,
    bestPerSector = bestPerSector,
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
                                          verbose = verbose)
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
    sigActorPower = 0L, sigInstQual = 0L, sigInteractions = 0L,
    deltaR2Theory = NA_real_, theoryFrac = NA_real_,
    aic = NA_real_, pseudoR2 = NA_real_, nObs = NA_integer_,
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

  isLf <- inherits(m, "logistf")
  k <- length(stats::coef(m))
  nObs <- if (isLf) m$n else tryCatch(stats::nobs(m), error = function(e) NA_integer_)
  loglik <- if (isLf) as.numeric(m$loglik["full"]) else
    tryCatch(as.numeric(stats::logLik(m)), error = function(e) NA_real_)
  base$aic <- if (isLf) -2 * loglik + 2 * k else as.numeric(m$aic)
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
    stage = tolower(stage)
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

# Internal: renders the redesigned pure-consumer reports (ADR 0006);
# returns named exit codes.
#' @keywords internal
.renderChannelReports <- function(reportsDir, mode, rdsPath, selectedConfigPath,
                                  verbose = TRUE) {
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
