# nolint start
#' Sanity verdicts for the whole selection-bootstrap pool
#'
#' @description
#' The sweep's sanity walk stops at the first specification that clears every severe gate, so
#' only the specifications ranked above the deployed one carry a verdict. The selection
#' bootstrap re-ranks a pool of the top \code{topK} gate-passers, and its conditional winners
#' could exclude only the walked ones: on Run-Group \code{v6}, 43\% of the conditional wins
#' went to specifications with a linear innovator term, which the actor-power extrapolation
#' gate (design note 0005 D7) rejects deterministically, but which the walk had never reached.
#'
#' This step walks every pool specification without a verdict, with the group's own gates and
#' both scenario panels (gating and reference), and writes \code{sanity-pool.rds}.
#' \code{\link{runPFMSelectionBootstrap}} then excludes every specification that fails. The
#' deployment is not touched: the verdicts are a property of each specification, computed on
#' the full sample, exactly as in the sweep.
#'
#' @param group,resultsDir,modelDir,cachefolder As in \code{\link{runPFMSelectionBootstrap}}.
#' @param topK Integer. Pool size, as the bootstrap's \code{topK}. Default \code{40}.
#' @param gdxFile,gdxRegionMappingFile The gating scenario's gdx and its native mapping.
#' @param referenceGdxFile,referenceGdxRegionMappingFile The reference scenario's, for the
#'   responsiveness gate. \code{\link{pfmRun}} passes all four from the scenario registry.
#' @param outputRegionMappingFile Panel resolution, as the sweep's.
#' @param minScenarioDelta,deltaWindow,supportShareGate,ceilingFallGate,gammaGate,vcovGate
#'   Gate settings; defaults mirror \code{\link{runPFMSweep}}.
#' @param apExtrapolationGate,apExtrapolationSd,apExtrapolationWindow The actor-power gate;
#'   \code{NULL} (default) takes the group's recorded \code{sweepOptions}.
#' @param verbose Logical.
#' @return Invisibly, the list saved as \code{sanity-pool.rds}, or \code{NULL} when skipped.
#' @seealso \code{\link{runPFMSelectionBootstrap}}, \code{\link{runPFMSweep}}, PITFALLS §29
#' @export
#' @author Renato Rodrigues
runPFMSanityPool <- function(group,
                             resultsDir = getOption("pfm.resultsDir", "output"),
                             modelDir = getOption("pfm.modelDir", "output"),
                             cachefolder = NULL,
                             topK = 40L,
                             gdxFile = NULL, gdxRegionMappingFile = "regionmappingH12.csv",
                             referenceGdxFile = NULL, referenceGdxRegionMappingFile = NULL,
                             outputRegionMappingFile = "regionmapping_54.csv",
                             minScenarioDelta = 0.05, deltaWindow = c(2040, 2060),
                             supportShareGate = 0.275, ceilingFallGate = 0.90, gammaGate = 0.999,
                             vcovGate = c("likelihood-mismatch", "flat"),
                             apExtrapolationGate = NULL, apExtrapolationSd = NULL,
                             apExtrapolationWindow = NULL,
                             verbose = TRUE) {
  groupDir <- .resolveGroupDir(group, resultsDir, modelDir, cachefolder)
  say <- function(...) if (isTRUE(verbose)) message("[pfm-sanity-pool:", group, "] ", ...)
  t0 <- Sys.time()
  stg <- "PolicyStringency"; sectors <- c("Bulk", "Diffuse")
  skip <- function(reason) {
    say("skipped - ", reason)
    .recordStep(groupDir, group, "sanity-pool", t0, status = "skipped", metrics = list(reason = reason))
    invisible(NULL)
  }

  sweepPath <- file.path(groupDir, "sweep.rds")
  if (!file.exists(sweepPath)) return(skip("no sweep.rds (run runPFMSweep first)"))
  sweep <- readRDS(sweepPath)
  mm <- sweep$maximin[[stg]]
  if (is.null(mm)) return(skip("sweep.rds has no PolicyStringency maximin"))
  specByName <- stats::setNames(sweep$specs, vapply(sweep$specs, `[[`, character(1), "name"))
  deployed <- sweep$selected[[stg]] %||% NA_character_
  # The bootstrap's pool, built the same way (runPFMSelectionBootstrap).
  pool <- utils::head(mm$model[mm$gatePass %in% TRUE], topK)
  if (!is.na(deployed) && !(deployed %in% pool)) pool <- c(pool, deployed)
  pool <- pool[pool %in% names(specByName)]

  walked <- sweep$sanity[[stg]]$trace
  walkedModels <- if (is.data.frame(walked)) walked$model else character(0)
  todo <- setdiff(pool, walkedModels)
  say(length(pool), " pool spec(s): ", length(intersect(pool, walkedModels)),
      " with a verdict from the sweep's walk, ", length(todo), " to walk")

  newWalk <- NULL
  responsive <- NA   # whether this walk had the reference panel; NA when nothing was walked
  if (length(todo)) {
    if (is.null(gdxFile) || !file.exists(gdxFile)) return(skip("no gating gdx"))
    rec <- .pfmSweepOptionsForGroup(groupDir)
    apGate <- apExtrapolationGate %||% rec$apExtrapolationGate %||% Inf
    apSd <- apExtrapolationSd %||% rec$apExtrapolationSd %||% 1
    apWin <- apExtrapolationWindow %||% rec$apExtrapolationWindow %||% c(2025, 2100)

    hash <- tryCatch(jsonlite::read_json(file.path(groupDir, "manifest.json"))$panel_hash,
                     error = function(e) NULL)
    panel <- if (!is.null(hash)) loadTrainingPanel(hash, modelDir) else NULL
    if (is.null(panel)) return(skip("training panel not in the Fit Cache"))
    if ("GDP per Capita" %in% magclass::getNames(panel) && !"GDP per Capita Sq" %in% magclass::getNames(panel)) {
      panel <- magclass::mbind(panel, magclass::setNames(panel[, , "GDP per Capita"]^2, "GDP per Capita Sq"))
    }
    .useMadratCache(cachefolder)
    scen <- function(gdx, mapping) panelDataScenario(
      gdxFile = gdx, aggregate = TRUE, gdxRegionMappingFile = mapping,
      outputRegionMappingFile = outputRegionMappingFile,
      histYears = .pfmPanelYears(), movingAverage = .pfmPanelMA())
    say("building the gating scenario panel (", gdxRegionMappingFile, ") ...")
    scenarioData <- scen(gdxFile, gdxRegionMappingFile)
    referenceScenarioData <- NULL
    if (!is.null(referenceGdxFile) && file.exists(referenceGdxFile)) {
      refMapping <- referenceGdxRegionMappingFile %||% gdxRegionMappingFile
      say("building the REFERENCE scenario panel (", refMapping, ") ...")
      referenceScenarioData <- scen(referenceGdxFile, refMapping)
    } else {
      say("WARNING: no reference scenario - the responsiveness gate (scenarioBlind) is NOT applied")
    }
    responsive <- !is.null(referenceScenarioData)
    newWalk <- .pfmSanitySelect(
      passModels = todo, specByName = specByName, sectors = sectors,
      panelData = panel, scenarioData = scenarioData, modelDir = modelDir,
      batchSize = length(todo), maxModels = length(todo), thresholds = list(),
      regionBlocks = .h12RegionBlocks(), histIndexBySector = .histIndexBySector(panel, sectors),
      indexMax = 10, referenceScenarioData = referenceScenarioData,
      minScenarioDelta = minScenarioDelta, deltaWindow = deltaWindow,
      supportShareGate = supportShareGate, ceilingFallGate = ceilingFallGate,
      gammaGate = gammaGate, vcovGate = vcovGate,
      apExtrapolationGate = apGate, apExtrapolationSd = apSd, apExtrapolationWindow = apWin,
      say = say, stopAtFirstPass = FALSE)
  }

  # One verdict table for the pool: the sweep's walk where it reached, this walk elsewhere.
  part <- function(tr, src) if (is.data.frame(tr) && nrow(tr)) cbind(tr[, c("model", "evaluable", "nSevere", "nWarning", "pass", "reason")], source = src) else NULL
  verdicts <- rbind(part(if (is.data.frame(walked)) walked[walked$model %in% pool, , drop = FALSE] else NULL, "sweep"),
                    part(newWalk$trace, "pool"))
  if (is.null(verdicts)) return(skip("no pool spec has a sanity verdict (no scenario panel in the sweep?)"))
  verdicts$pass <- verdicts$pass %in% TRUE
  # The severe rules behind each rejection, from both walks' flags.
  flags <- c(sweep$sanity[[stg]]$flags[intersect(names(sweep$sanity[[stg]]$flags), pool)], newWalk$flags)
  verdicts$severeRules <- vapply(verdicts$model, function(m) {
    f <- flags[[m]]
    if (is.null(f) || !nrow(f)) return("")
    paste(sort(unique(f$rule[f$severity == "severe"])), collapse = ",")
  }, character(1))
  out <- list(group = group, stage = stg, deployed = deployed, pool = pool, topK = topK,
              verdicts = verdicts, flags = flags,
              ceiling = c(sweep$sanity[[stg]]$ceiling, newWalk$ceiling),
              gamma = c(sweep$sanity[[stg]]$gamma, newWalk$gamma),
              vcovStatus = c(sweep$sanity[[stg]]$vcovStatus, newWalk$vcovStatus),
              responsivenessGate = responsive, generated = Sys.time())
  saveRDS(out, file.path(groupDir, "sanity-pool.rds"))
  nPass <- sum(verdicts$pass); nFail <- sum(!verdicts$pass)
  say(nPass, " of ", nrow(verdicts), " pool spec(s) pass; ", nFail, " rejected -> ",
      file.path(groupDir, "sanity-pool.rds"))
  .recordStep(groupDir, group, "sanity-pool", t0,
              metrics = list(poolSize = length(pool), walkedNow = length(todo), nPass = nPass, nFail = nFail,
                             responsivenessGate = out$responsivenessGate))
  invisible(out)
}
# nolint end
