# nolint start
#' @title selectDifferenceFirst
#' @description Difference-First (Dynamic Identification First) model selection
#' (ADR 0014), the alternative to the default levels-first maximin selection. For
#' each stage it (1) filters the sweep \code{results} to \code{hybridFD} rows,
#' (2) ranks them with the unchanged Maximin Selection Rule
#' (\code{\link{computeMaximinScore}}), (3) walks the ranking applying the
#' \code{\link{computeFalsificationGate}} (a \code{pureFD} re-fit) and then the
#' Projection Sanity gate on the \emph{levels} re-estimate, taking the first spec to
#' pass \strong{both}, and (4) returns that spec configured for \code{levels}
#' estimation (the projectable transform the IAM consumes). It never mutates the
#' levels-first deliverable.
#'
#' @param results Data.frame. The full sweep results (must contain a
#'   \code{panelTransform} column and the maximin metric columns).
#' @param specByName Named list. Spec configs keyed by \code{name} (as produced by
#'   \code{runChannelsWorkflow}); the \code{hybridFD} candidates are looked up here.
#' @param panelData A \code{magpie} object: the historical panel.
#' @param scenarioData A \code{magpie} object or NULL. When supplied, the levels
#'   re-estimate of each falsification-passing candidate is screened by the
#'   Projection Sanity gate; when NULL, the top falsification-passing candidate is
#'   selected (maximin + falsification only).
#' @param sectors Character vector. Default \code{c("Bulk", "Diffuse")}.
#' @param family Character. Stringency GLM family.
#' @param modelDir Character or NULL.
#' @param nearTieEps Numeric. BIC parsimony tie-break tolerance (ADR 0012).
#' @param requireBothSectors,pThreshold Passed to \code{\link{computeFalsificationGate}}.
#' @param maxTries Integer. Maximum number of ranked candidates to falsification-test.
#' @param iqVanishTest Character. IQ-vanish rule for the Falsification Gate:
#'   \code{"jointBlock"} (default, joint Wald test) or \code{"perChannel"}; see
#'   \code{\link{computeFalsificationGate}}.
#' @param levelsFE Named list of candidate block-FE strategies for the winner's
#'   \emph{levels} re-estimate, each \code{list(fe = <mapping file or NULL>, mundlak =
#'   <logical>)}. hybridFD specs carry no region FE (differenced out), so the levels
#'   deliverable would otherwise be pooled/inflation-prone; the FE is therefore
#'   \strong{chosen by the Projection Sanity gate} among these (ADR 0014). Default
#'   \code{{H12, OECDp, Mundlak}}. When no \code{scenarioData} is given the first
#'   (preference order) is used. A bare mapping string/vector or \code{NULL} is also
#'   accepted (coerced to single-option / pooled).
#' @param sanityBatchSize,sanityMaxModels,sanityThresholds,regionBlocks,histPricesBySector
#'   Passed through to the internal Projection Sanity selector.
#' @param say Function. Logger (default a no-op).
#'
#' @return A named list keyed by stage; each element has \code{chosen} (selected spec
#'   name, or NA), \code{chosenConfigLevels} (the levels-flipped config), \code{maximin}
#'   (the hybridFD maximin table), \code{falsification} (per-candidate gate summary),
#'   and \code{sanity} (the Projection Sanity result, or NULL).
#'
#' @seealso \code{\link{computeFalsificationGate}}, \code{\link{computeMaximinScore}}, ADR 0014
#' @export
#' @author Renato Rodrigues
selectDifferenceFirst <- function(results, specByName, panelData, scenarioData = NULL,
                                  sectors = c("Bulk", "Diffuse"), family = "gaussian",
                                  modelDir = NULL, nearTieEps = 0.025, feParsimonyWeight = 0,
                                  dropIdleControls = TRUE, softVifGate = 6, trendShareGate = 0.6,
                                  requireBothSectors = TRUE, pThreshold = 0.05, maxTries = 25L,
                                  iqVanishTest = "jointBlock",
                                  levelsFE = list(
                                    H12     = list(fe = "regionmappingH12.csv",       mundlak = FALSE),
                                    OECDp   = list(fe = "regionmapping_EU_OECDp.csv", mundlak = FALSE),
                                    Mundlak = list(fe = NULL,                          mundlak = TRUE)),
                                  sanityBatchSize = 5L, sanityMaxModels = 15L,
                                  sanityThresholds = list(), regionBlocks = NULL,
                                  histPricesBySector = NULL, say = function(...) invisible()) {
  if (is.null(sanityThresholds)) sanityThresholds <- list()
  # Accept a bare mapping string / vector / NULL for convenience.
  if (is.character(levelsFE)) levelsFE <- stats::setNames(
    lapply(levelsFE, function(f) list(fe = f, mundlak = FALSE)), levelsFE)
  if (is.null(levelsFE)) levelsFE <- list(noFE = list(fe = NULL, mundlak = FALSE))
  stages <- intersect(c("Adoption", "Stringency"), unique(results$stage))
  mmCols <- c("model", "sector", "sigActorPower", "sigInstQual", "sigInteractions",
              "deltaR2Theory", "pseudoR2", "bic", "maxVIF", "converged", "usesLagged", "nFE",
              "nObs", "sigControl", "nControl", "trendShare")

  out <- list()
  for (stg in stages) {
    sub <- results[results$stage == stg & results$panelTransform == "hybridFD", , drop = FALSE]
    if (!nrow(sub)) { say("DIF: no hybridFD rows for ", stg); next }
    mm <- computeMaximinScore(sub[, intersect(mmCols, colnames(sub)), drop = FALSE],
                              nearTieEps = nearTieEps, feParsimonyWeight = feParsimonyWeight,
                              dropIdleControls = dropIdleControls, softVifGate = softVifGate,
                              trendShareGate = trendShareGate)
    ranked <- mm[mm$gatePass, , drop = FALSE]
    if (!nrow(ranked)) { say("DIF: no gate-passing hybridFD spec for ", stg);
      out[[stg]] <- list(chosen = NA_character_, chosenConfigLevels = NULL, maximin = mm,
                         falsification = NULL, sanity = NULL); next }

    say("DIF ", stg, ": ", nrow(ranked), " ranked hybridFD candidates; applying Falsification Gate ...")
    falsRows <- list(); passers <- character(0)
    for (i in seq_len(min(maxTries, nrow(ranked)))) {
      nm <- ranked$model[i]; cfg <- specByName[[nm]]
      if (is.null(cfg)) next
      fg <- computeFalsificationGate(cfg, data = panelData, stage = stg, sectors = sectors,
                                     family = family, modelDir = modelDir,
                                     pThreshold = pThreshold, requireBothSectors = requireBothSectors,
                                     iqVanishTest = iqVanishTest)
      d <- fg$detail
      falsRows[[length(falsRows) + 1L]] <- data.frame(
        rank = i, model = nm, pass = fg$pass, reason = fg$reason,
        APsigCorrect = paste(d$nAPsigCorrect, collapse = "/"),
        APsigWrong   = paste(d$nAPsigWrong, collapse = "/"),
        IQsig        = paste(d$nIQsig, collapse = "/"),
        IntSig       = paste(d$nIntSig, collapse = "/"),
        estimable    = paste(d$estimable, collapse = "/"),
        stringsAsFactors = FALSE)
      if (isTRUE(fg$pass)) { passers <- c(passers, nm); say("  rank ", i, " PASS falsification: ", nm) }
      else say("  rank ", i, " fail: ", fg$reason)
    }
    falsification <- if (length(falsRows)) do.call(rbind, falsRows) else NULL

    chosen <- NA_character_; chosenCfg <- NULL; chosenBase <- NA_character_
    chosenFE <- NA_character_; sanityRes <- NULL
    if (length(passers)) {
      # hybridFD specs carry no region FE (differenced out), so the levels deliverable
      # would be pooled/inflation-prone. We expand each falsification-passer across the
      # candidate block-FE strategies and let the Projection Sanity gate choose the FE
      # (ADR 0014). Candidate order is passer-major (maximin) then FE-preference.
      cand <- list(); candOrder <- character(0); candMeta <- list()
      for (nm in passers) for (feName in names(levelsFE)) {
        fe <- levelsFE[[feName]]
        cc <- specByName[[nm]]; cc$panelTransform <- "levels"
        cc$regionMappingFixedEffects <- fe$fe; cc$useMundlak <- isTRUE(fe$mundlak)
        key <- paste0(nm, " | fe:", feName); cc$name <- key
        cand[[key]] <- cc; candOrder <- c(candOrder, key)
        candMeta[[key]] <- list(base = nm, fe = feName)
      }
      take <- function(key, how) {
        chosen <<- key; chosenCfg <<- cand[[key]]
        chosenBase <<- candMeta[[key]]$base; chosenFE <<- candMeta[[key]]$fe
        say("DIF ", stg, " selected (", how, "): ", key)
      }
      if (is.null(scenarioData)) {
        take(candOrder[1], paste0("maximin + falsification, no scenario; FE = ",
                                  candMeta[[candOrder[1]]]$fe, " by preference order"))
      } else {
        sanityRes <- tryCatch(.sanitySelect(
          passModels = candOrder, stg = stg, specByName = cand, sectors = sectors,
          panelData = panelData, scenarioData = scenarioData, family = family, modelDir = modelDir,
          batchSize = sanityBatchSize, maxModels = sanityMaxModels, thresholds = sanityThresholds,
          regionBlocks = regionBlocks, histPricesBySector = histPricesBySector, say = say),
          error = function(e) { say("DIF sanity failed: ", conditionMessage(e)); NULL })
        if (!is.null(sanityRes) && !is.na(sanityRes$chosen)) {
          take(sanityRes$chosen, if (isTRUE(sanityRes$forced)) "LEAST-FLAGGED fallback"
               else "FE chosen by Projection Sanity")
        } else {
          take(candOrder[1], paste0("no candidate passed sanity; top falsification-passer, FE ",
                                    candMeta[[candOrder[1]]]$fe))
        }
      }
    } else {
      say("DIF ", stg, ": NO spec passed the Falsification Gate.")
    }
    out[[stg]] <- list(chosen = chosen, chosenBase = chosenBase, chosenFE = chosenFE,
                       chosenConfigLevels = chosenCfg, maximin = mm,
                       falsification = falsification, sanity = sanityRes)
  }
  out
}
# nolint end
