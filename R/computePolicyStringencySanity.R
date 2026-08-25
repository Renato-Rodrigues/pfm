# nolint start
#' Project one PSM specification onto a scenario (sanity-gate / selection path)
#'
#' @description
#' The Policy Stringency Model counterpart of \code{\link{projectSpecScenario}}:
#' (re-)estimates the spec with the satP engine (a Fit-Cache hit after a sweep)
#' and projects the bounded index onto the scenario panel in-session — frozen
#' driver scaling, trend freeze at the last historical year, regionFE aligned to
#' the fitted levels, and the recursive lagged projection when the spec carries
#' the dynamics rung. Because the index is \eqn{indexMax \cdot logit^{-1}(\eta)},
#' there are \strong{no projection guards on this path} — boundedness is
#' structural (ADR 0036), so the price-model clamp machinery has no analogue.
#'
#' @param cfg Normalised spec (named list) from the sweep config; must be
#'   \code{panelTransform = "levels"} (anything else returns NULL, mirroring
#'   ADR 0005).
#' @param sector Character. \code{"Bulk"} or \code{"Diffuse"}.
#' @param histData Historical panel (magpie) including the Policy Stringency
#'   outcome.
#' @param scenarioData Scenario panel (magpie).
#' @param modelDir Character or NULL. Fit Cache root.
#' @param indexMax Numeric. Index ceiling. Default \code{10}.
#' @param minProjYear Numeric or NULL. Horizon cutoff (\code{NULL} = last
#'   historical year of the fit).
#' @param driverGuard Character. \code{"winsorize"} (default) clamps standardized
#'   base drivers at their estimation-sample support and recomputes interactions
#'   before projecting (see \code{\link{predictPolicyStringency}}); \code{"none"}
#'   disables the clamp (the \code{driverOutOfSupport} audit is still emitted).
#' @param verbose Logical.
#'
#' @return Data.frame \code{region, year, sector, eta, index, indexLo, indexHi,
#'   outOfCoverage, driverOutOfSupport} — the same columns as
#'   \code{\link{predictPolicyStringency}} (delta-method 95 percent interval on
#'   the non-dynamic path, NA under the lag recursion; out-of-coverage rows get
#'   the between-FE spread added to their interval; \code{outOfCoverage} flags
#'   regions absent from the estimation sample), or \code{NULL} when the spec is
#'   not projectable.
#'
#' @importFrom stats plogis coef terms delete.response model.frame model.matrix na.pass qnorm sd
#' @export
#' @author Renato Rodrigues
projectPSMSpecScenario <- function(cfg, sector, histData, scenarioData,
                                   modelDir = getOption("pfm.modelDir", "output"),
                                   indexMax = 10, minProjYear = NULL,
                                   driverGuard = c("winsorize", "none"),
                                   verbose = FALSE) {
  if (!identical(cfg$panelTransform %||% "levels", "levels")) {
    if (isTRUE(verbose)) {
      message("projectPSMSpecScenario: panelTransform '", cfg$panelTransform,
              "' not projectable - returning NULL.")
    }
    return(NULL)
  }
  fit <- estimatePolicyStringencyModel(
    data = histData, sector = sector, estimator = "satP", indexMax = indexMax,
    actorPowerDrivers = cfg$actorPowerDrivers, actorPowerIndex = cfg$actorPowerIndex,
    instQualityDrivers = cfg$instQualityDrivers, controlDrivers = cfg$controlDrivers,
    regionMappingFixedEffects = cfg$regionMappingFixedEffects,
    logisticTimeTrend = isTRUE(cfg$logisticTimeTrend),
    interactRegionFE = isTRUE(cfg$interactRegionFE),
    useMundlak = isTRUE(cfg$useMundlak),
    gdpGovInteraction = isTRUE(cfg$gdpGovInteraction),
    includeLaggedPS = isTRUE(cfg$includeLaggedPS),
    apTransform = cfg$apTransform %||% "linear",
    modelDir = modelDir, updateIndex = FALSE, verbose = FALSE
  )
  lastHistYear <- suppressWarnings(max(fit$data$year, na.rm = TRUE))

  sDf <- preparePanelData(
    data = scenarioData, sector = sector,
    actorPowerDrivers = cfg$actorPowerDrivers, actorPowerIndex = cfg$actorPowerIndex,
    instQualityDrivers = cfg$instQualityDrivers,
    controlDrivers = setdiff(cfg$controlDrivers, "lagged_ecp"),
    regionMappingFixedEffects = if (isTRUE(cfg$useMundlak)) NULL else cfg$regionMappingFixedEffects,
    useMundlak = isTRUE(cfg$useMundlak),
    gdpGovInteraction = isTRUE(cfg$gdpGovInteraction),
    driverScaling = fit$driverScaling,
    trendFreezeYear = if (is.finite(lastHistYear)) lastHistYear else NULL,
    outcomeVar = fit$outcomeVar %||% "Policy Stringency"
  )
  # Align regionFE to the levels the live fit saw.
  lv <- fit$model$xlevels$regionFE
  if (!is.null(lv) && "regionFE" %in% names(sDf)) {
    fe <- as.character(sDf$regionFE)
    fe[!fe %in% lv] <- if ("Other" %in% lv) "Other" else lv[1]
    sDf$regionFE <- factor(fe, levels = lv)
  }

  # Driver-support guard + extrapolation audit (R3): winsorize standardized base
  # drivers at the estimation-sample support and recompute interactions, so the
  # sanity gate scores projections on the same design support the coefficients
  # were estimated on. The in-session fit always carries its estimation rows.
  driverGuard <- match.arg(driverGuard)
  ranges <- .driverSupportRanges(fit$data, fit$driverScaling)
  guarded <- .psmDriverGuard(sDf, ranges)
  driverOutOfSupport <- guarded$outOfSupport
  driverOutOfSample <- guarded$outOfSample
  if (identical(driverGuard, "winsorize")) {
    sDf <- guarded$df
  }

  trainedRegions <- unique(as.character(fit$data$region))
  ocRows <- !(as.character(sDf$region) %in% trainedRegions)

  designEta <- function(df, withDesign = FALSE) {
    tt <- stats::delete.response(stats::terms(fit$formula))
    mm <- stats::model.matrix(tt, stats::model.frame(tt, data = df, na.action = stats::na.pass))
    beta <- stats::coef(fit$model)
    shared <- intersect(colnames(mm), names(beta))
    eta <- as.numeric(mm[, shared, drop = FALSE] %*% beta[shared])
    if (!withDesign) return(eta)
    list(eta = eta, mm = mm[, shared, drop = FALSE], shared = shared)
  }

  etaLo <- etaHi <- NULL
  if (!"lagged_ecp" %in% all.vars(fit$formula)) {
    d <- designEta(sDf, withDesign = TRUE)
    eta <- d$eta
    vc <- fit$vcov
    if (!is.null(vc) && all(d$shared %in% rownames(vc))) {
      vcS <- vc[d$shared, d$shared, drop = FALSE]
      seEta <- sqrt(pmax(rowSums((d$mm %*% vcS) * d$mm), 0))
      # Out-of-coverage rows inherit the reference-group FE: widen their interval
      # by the between-FE spread (R4a).
      feSpread <- .regionFESpread(stats::coef(fit$model))
      seEta[ocRows] <- sqrt(seEta[ocRows]^2 + feSpread^2)
      zc <- stats::qnorm(0.975)
      etaLo <- eta - zc * seEta
      etaHi <- eta + zc * seEta
    }
  } else {
    bLag <- tryCatch(stats::coef(fit$model)[["lagged_ecp"]], error = function(e) NA_real_)
    if (is.null(bLag) || !is.finite(bLag)) bLag <- 0
    sDf0 <- sDf
    sDf0$lagged_ecp <- 0
    etaFixed <- designEta(sDf0)
    # Seed = last historical TRANSFORMED response per region (eta scale).
    seed <- tryCatch(
      tapply(fit$data$ecp, fit$data$region, function(v) v[length(v)]),
      error = function(e) NULL
    )
    eta <- rep(NA_real_, nrow(sDf))
    for (r in unique(sDf$region)) {
      idx <- which(sDf$region == r)
      idx <- idx[order(sDf$year[idx])]
      lagv <- if (!is.null(seed) && as.character(r) %in% names(seed) &&
                    is.finite(seed[[as.character(r)]])) seed[[as.character(r)]] else 0
      for (i in idx) {
        e <- etaFixed[i] + bLag * lagv
        if (!is.finite(e)) {
          eta[i] <- NA_real_
          next
        }
        eta[i] <- e
        lagv <- e
      }
    }
  }

  out <- data.frame(
    region = sDf$region, year = sDf$year, sector = sector,
    eta = eta, index = indexMax * stats::plogis(eta),
    indexLo = if (!is.null(etaLo)) indexMax * stats::plogis(etaLo) else NA_real_,
    indexHi = if (!is.null(etaHi)) indexMax * stats::plogis(etaHi) else NA_real_,
    outOfCoverage = ocRows,
    driverOutOfSupport = driverOutOfSupport,
    driverOutOfSample = driverOutOfSample,
    stringsAsFactors = FALSE
  )
  cut <- minProjYear %||% (if (is.finite(lastHistYear)) lastHistYear else -Inf)
  out <- out[is.finite(out$year) & out$year > cut, , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Projection Sanity rules for the bounded Policy Stringency index
#'
#' @description
#' The PSM analogue of \code{\link{computeProjectionSanity}} (ADR 0036). The
#' price-model rules that exist because \code{expm1(eta)} is unbounded — price
#' explosion, the extrapolation clamp and the clamp-reliance hard filter — have
#' \strong{no analogue}: the index saturates at \code{indexMax} by construction.
#' What remains meaningful on a bounded 0-\code{indexMax} scale:
#' \describe{
#'   \item{nonfinite_index (severe)}{NaN/Inf index where a prediction exists —
#'     defensive; a bounded reconstruction should never produce it.}
#'   \item{dead_region_block (severe)}{An entire region block stays below
#'     \code{indexDeadLow} through the whole horizon — a no-policy block
#'     contradicts an ambitious gating scenario (mirrors the price model's
#'     dead-block rule).}
#'   \item{index_saturation (warning)}{ALL regions above
#'     \code{indexSaturationHigh} in some year — plausible late-century under
#'     ambition, so a warning; escalates to severe only before
#'     \code{saturationSevereBefore} (default never).}
#'   \item{seam_jump (warning)}{First projected year jumps more than
#'     \code{seamIndexJump} index points from the region's last historical
#'     value.}
#'   \item{index_spike (warning)}{Year-on-year change larger than
#'     \code{spikeIndexJump} within the projection.}
#'   \item{missing_share (warning)}{Share of NA projections above
#'     \code{missingShareWarn} (structurally absent drivers).}
#' }
#'
#' @param proj Data.frame from \code{\link{projectPSMSpecScenario}} /
#'   \code{\link{predictPolicyStringency}} (needs \code{region, year, index}).
#' @param histIndex Optional data.frame \code{region, year, index} of historical
#'   values (natural scale) for the seam rule; the last observed year per region
#'   is used.
#' @param regionBlocks Optional data.frame \code{region, block} for the
#'   dead-block rule; when NULL every region is its own block.
#' @param thresholds Named list overriding the defaults (all in index units):
#'   \code{indexDeadLow} (0.05·indexMax), \code{indexSaturationHigh}
#'   (0.95·indexMax), \code{seamIndexJump} (0.2·indexMax), \code{spikeIndexJump}
#'   (0.2·indexMax), \code{missingShareWarn} (0.05), \code{saturationSevereBefore}
#'   (NA = never).
#' @param indexMax Numeric. Index ceiling. Default \code{10}.
#'
#' @return List: \code{flags} (data.frame \code{rule, severity, region, year,
#'   value, detail}) and \code{summary} (\code{nSevere, nWarning, nRows,
#'   missingShare}) — the same shape \code{.sanitySelect}-style walkers consume.
#'
#' @export
#' @author Renato Rodrigues
computePolicyStringencySanity <- function(proj, histIndex = NULL, regionBlocks = NULL,
                                          thresholds = list(), indexMax = 10) {
  th <- utils::modifyList(list(
    indexDeadLow = 0.05 * indexMax,
    indexSaturationHigh = 0.95 * indexMax,
    seamIndexJump = 0.2 * indexMax,
    spikeIndexJump = 0.2 * indexMax,
    missingShareWarn = 0.05,
    saturationSevereBefore = NA_real_
  ), thresholds)

  flags <- list()
  addFlag <- function(rule, severity, region, year, value, detail) {
    flags[[length(flags) + 1L]] <<- data.frame(
      rule = rule, severity = severity, region = region,
      year = if (is.null(year)) NA_integer_ else as.integer(year),
      value = as.numeric(value), detail = detail, stringsAsFactors = FALSE
    )
  }

  idx <- proj$index
  nRows <- nrow(proj)
  missingShare <- if (nRows > 0) mean(is.na(idx)) else NA_real_

  # (6) coverage
  if (is.finite(missingShare) && missingShare > th$missingShareWarn) {
    addFlag("missing_share", "warning", "ALL", NULL, missingShare,
            paste0(round(100 * missingShare, 1), "% of projected region-years are NA"))
  }

  # (1) non-finite but not NA — defensive
  bad <- which(!is.na(idx) & !is.finite(idx))
  for (i in bad) {
    addFlag("nonfinite_index", "severe", proj$region[i], proj$year[i], idx[i],
            "non-finite projected index")
  }

  # (2) dead region block: every region of the block below indexDeadLow in every year
  blockOf <- if (!is.null(regionBlocks) && all(c("region", "block") %in% names(regionBlocks))) {
    stats::setNames(regionBlocks$block, regionBlocks$region)
  } else NULL
  regBlock <- if (!is.null(blockOf)) {
    b <- blockOf[as.character(proj$region)]
    ifelse(is.na(b), as.character(proj$region), b)
  } else {
    as.character(proj$region)
  }
  for (b in unique(regBlock)) {
    v <- idx[regBlock == b]
    if (all(is.na(v))) next
    mx <- max(v, na.rm = TRUE)
    if (mx < th$indexDeadLow) {
      addFlag("dead_region_block", "severe", b, NULL, mx,
              paste0("block stays below ", round(th$indexDeadLow, 2),
                     " through the whole horizon"))
    }
  }

  # (3) all-region saturation per year
  for (yr in sort(unique(proj$year))) {
    v <- idx[proj$year == yr]
    if (all(is.na(v))) next
    if (min(v, na.rm = TRUE) > th$indexSaturationHigh) {
      sev <- if (is.finite(th$saturationSevereBefore) && yr < th$saturationSevereBefore) {
        "severe"
      } else "warning"
      addFlag("index_saturation", sev, "ALL", yr, min(v, na.rm = TRUE),
              paste0("all regions above ", round(th$indexSaturationHigh, 2), " in ", yr))
    }
  }

  # (4) seam jump vs last historical value
  if (!is.null(histIndex) && all(c("region", "year", "index") %in% names(histIndex))) {
    histLast <- do.call(rbind, lapply(split(histIndex, histIndex$region), function(d) {
      d <- d[is.finite(d$index), , drop = FALSE]
      if (nrow(d) == 0) return(NULL)
      d[which.max(d$year), c("region", "index"), drop = FALSE]
    }))
    if (!is.null(histLast) && nrow(histLast) > 0) {
      lastVal <- stats::setNames(histLast$index, histLast$region)
      for (r in unique(proj$region)) {
        rc <- as.character(r)
        # lastVal is an ATOMIC named vector: `[[` on a missing name errors
        # ("subscript out of bounds"), unlike a list which returns NULL. A projected
        # region absent from the historical panel (e.g. an out-of-coverage region such
        # as USA/Brazil, which the PSM still projects) simply has no seam to check.
        hv <- if (rc %in% names(lastVal)) lastVal[[rc]] else NULL
        if (is.null(hv) || !is.finite(hv)) next
        sub <- proj[proj$region == r & is.finite(proj$index), , drop = FALSE]
        if (nrow(sub) == 0) next
        first <- sub[which.min(sub$year), ]
        jump <- abs(first$index - hv)
        if (is.finite(jump) && jump > th$seamIndexJump) {
          addFlag("seam_jump", "warning", r, first$year, jump,
                  paste0("first projected year jumps ", round(jump, 2),
                         " index points from the last historical value"))
        }
      }
    }
  }

  # (5) single-year spikes within the projection
  for (r in unique(proj$region)) {
    sub <- proj[proj$region == r, , drop = FALSE]
    sub <- sub[order(sub$year), , drop = FALSE]
    if (nrow(sub) < 2) next
    d <- diff(sub$index)
    sp <- which(is.finite(d) & abs(d) > th$spikeIndexJump)
    for (i in sp) {
      addFlag("index_spike", "warning", r, sub$year[i + 1], d[i],
              paste0("year-on-year index change of ", round(d[i], 2)))
    }
  }

  flagsDf <- if (length(flags) > 0) {
    do.call(rbind, flags)
  } else {
    data.frame(rule = character(0), severity = character(0), region = character(0),
               year = integer(0), value = numeric(0), detail = character(0),
               stringsAsFactors = FALSE)
  }
  list(
    flags = flagsDf,
    summary = list(
      nSevere = sum(flagsDf$severity == "severe"),
      nWarning = sum(flagsDf$severity == "warning"),
      nRows = nRows,
      missingShare = missingShare
    )
  )
}

# Internal: historical Policy Stringency values per sector (for the seam rule),
# mirroring .histPricesBySector.
#' @keywords internal
.histIndexBySector <- function(panelData, sectors, outcomeVar = "Policy Stringency") {
  if (!magclass::is.magpie(panelData)) return(NULL)
  out <- list()
  for (sec in sectors) {
    nm <- paste0(outcomeVar, "|", sec)
    if (!nm %in% magclass::getNames(panelData)) next
    arr <- as.array(panelData[, , nm])[, , 1, drop = FALSE]
    dim(arr) <- dim(arr)[1:2]
    regions <- magclass::getItems(panelData, dim = 1)
    years <- magclass::getYears(panelData, as.integer = TRUE)
    out[[sec]] <- data.frame(
      region = rep(regions, times = length(years)),
      year = rep(years, each = length(regions)),
      index = as.vector(arr),
      stringsAsFactors = FALSE
    )
  }
  out
}

# Internal: expanding-batch sanity selection for the PSM stage — the
# .sanitySelect walker with the PSM projection + bounded-index rules.
#' @keywords internal
.psmSanitySelect <- function(passModels, specByName, sectors, panelData, scenarioData,
                             modelDir, batchSize, maxModels, thresholds, regionBlocks,
                             histIndexBySector, indexMax = 10,
                             referenceScenarioData = NULL, minScenarioDelta = 0.05,
                             deltaWindow = c(2040, 2060),
                             supportShareGate = 0.25,
                             ceilingFallGate = NA_real_, gammaGate = NA_real_,
                             say = function(...) invisible()) {
  ceilingByModel <- list()
  gammaByModel <- list()
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
    modelCeiling <- list()
    modelGamma <- list()
    for (sec in sectors) {
      projReason <- ""
      proj <- tryCatch(
        projectPSMSpecScenario(cfg, sec, histData = panelData, scenarioData = scenarioData,
                               modelDir = modelDir, indexMax = indexMax, verbose = FALSE),
        error = function(e) {
          projReason <<- conditionMessage(e)
          NULL
        }
      )
      if (is.null(proj)) {
        evaluable <- FALSE
        reason <- if (nzchar(projReason)) projReason else "projection returned NULL"
        break
      }
      sn <- computePolicyStringencySanity(
        proj, histIndex = histIndexBySector[[sec]],
        regionBlocks = regionBlocks, thresholds = thresholds, indexMax = indexMax
      )
      nSevere <- nSevere + sn$summary$nSevere
      nWarning <- nWarning + sn$summary$nWarning
      if (nrow(sn$flags) > 0) {
        f <- sn$flags
        f$sector <- sec
        modelFlags[[sec]] <- f
      }

      # Support-share gate (ADR 0040). A spec whose projection inside the
      # evaluation window is mostly WINSORIZED is not producing model output, it
      # is producing guard output: the diagnosed failure mode is that innovator
      # power leaves its training support by ~2040 and the guard then clamps both
      # scenarios to the same edge, so the entire scenario signal (and the sign of
      # the ceiling feedback) is an artifact of where the clamp bites rather than
      # of politics. See docs/psm-ceiling-feedback-diagnosis.md. Measured on the
      # audit column the guard already emits, over in-coverage rows in the same
      # window the responsiveness gate uses.
      if (is.finite(supportShareGate) && "driverOutOfSupport" %in% colnames(proj)) {
        w <- proj[proj$year >= deltaWindow[1] & proj$year <= deltaWindow[2], , drop = FALSE]
        if ("outOfCoverage" %in% colnames(w)) {
          w <- w[!w$outOfCoverage %in% TRUE, , drop = FALSE]
        }
        supportShare <- if (nrow(w) > 0) mean(w$driverOutOfSupport, na.rm = TRUE) else NA_real_
        if (!is.finite(supportShare) || supportShare > supportShareGate) {
          nSevere <- nSevere + 1L
          modelFlags[[paste0(sec, ".extrapolationDominated")]] <- data.frame(
            rule = "extrapolationDominated", severity = "severe",
            region = "ALL", year = NA_integer_,
            value = supportShare,
            detail = paste0("mean driverOutOfSupport ", round(supportShare, 3),
                            " > ", supportShareGate, " over ", deltaWindow[1], "-",
                            deltaWindow[2], " (in-coverage)"),
            sector = sec, stringsAsFactors = FALSE
          )
          say("  [sanity:PolicyStringency] ", m, " (", sec, ") extrapolation-dominated: ",
              "mean driverOutOfSupport = ", round(supportShare, 3),
              " > ", supportShareGate)
        }
      }

      # Ceiling-collapse gate (TODO item 11, 2026-08-22). The FRONTIER ceiling
      # S*(x_t) - not the level path the rest of this walk scores - falls by ~40%
      # over the century in Bulk under the deployed spec, driven by an
      # Incumbent-Power interaction that a decarbonization scenario runs
      # backwards. Since the coupling turns the relative gap into phi, a
      # shrinking ceiling tightens the constraint every period whether or not
      # anything political happens, and a country that changes nothing is pushed
      # through its own ceiling by the passage of time.
      #
      # This is deliberately a SEVERE gate rather than an audit, but it is only
      # safe BECAUSE `extrapolationDominated` and `scenarioBlind` are severe too:
      # the cheapest way to hold a ceiling flat is to stop responding to the
      # scenario at all (composite-AP specs clamp both pathways to the same
      # support edge), so on its own this gate would select inert specs. Never
      # enable it without those two.
      if (is.finite(ceilingFallGate)) {
        ct <- tryCatch(
          .psmCeilingTrajectory(cfg, sec, panelData, scenarioData,
                                modelDir = modelDir, indexMax = indexMax),
          error = function(e) NULL)
        # gamma screen (ADR 0043 consequences, implemented 2026-08-25). OFF by
        # default (gammaGate = NA) so enabling it is a deliberate, dated act.
        #
        # gamma -> 1 means sigma_v^2 -> 0: the variance decomposition attributes
        # ALL composed error to slack and none to noise, so the "stochastic"
        # frontier has degenerated into a deterministic one and the gap is very
        # nearly the arithmetic residual (MODEL.md 3.4). ADR 0043 already directs
        # the project to screen this on any winner - X-0170 was called out there
        # for gamma = 1.0000 exactly - but nothing implemented it, and v3's
        # deployed X-2367 came in at 0.99999999 in BOTH sectors unremarked.
        #
        # REPORTED even when the gate is off, so turning it on is never the first
        # time anyone sees the number.
        if (!is.null(ct) && is.finite(ct$gamma %||% NA_real_)) {
          modelGamma[[sec]] <- ct$gamma
          if (is.finite(gammaGate) && ct$gamma > gammaGate) {
            nSevere <- nSevere + 1L
            modelFlags[[paste0(sec, ".gammaBoundary")]] <- data.frame(
              rule = "gammaBoundary", severity = "severe",
              region = "ALL", year = NA_integer_, value = ct$gamma,
              detail = paste0("frontier gamma = ", format(ct$gamma, digits = 8),
                              " > gate ", gammaGate,
                              "; variance decomposition is at the boundary, so slack ",
                              "is indistinguishable from residual (MODEL.md 3.4)"),
              sector = sec, stringsAsFactors = FALSE
            )
            say("  [sanity:PolicyStringency] ", m, " (", sec, ") gamma at boundary: ",
                format(ct$gamma, digits = 8), " > ", gammaGate)
          }
        }

        if (!is.null(ct) && is.finite(ct$ratio)) {
          modelCeiling[[sec]] <- ct$ratio
          if (ct$ratio < ceilingFallGate) {
            nSevere <- nSevere + 1L
            modelFlags[[paste0(sec, ".ceilingCollapse")]] <- data.frame(
              rule = "ceilingCollapse", severity = "severe",
              region = "ALL", year = NA_integer_, value = ct$ratio,
              detail = paste0("median frontier ceiling falls to ",
                              round(100 * ct$ratio), "% of its ", ct$year0,
                              " value by ", ct$year1, " (gate ",
                              round(100 * ceilingFallGate), "%); ",
                              round(100 * (ct$shareFalling %||% NA_real_)),
                              "% of covered countries falling"),
              sector = sec, stringsAsFactors = FALSE
            )
            say("  [sanity:PolicyStringency] ", m, " (", sec, ") ceiling collapse: ",
                "S* ratio = ", round(ct$ratio, 3), " < ", ceilingFallGate)
          }
        }
      }

      # Ratchet monotonicity audit (ADR 0040) - REPORT ONLY, never a gate. A
      # policy-stringency index is an accumulating stock: countries do not repeal
      # their portfolios. The model does not know this, and under the linear
      # actor-power form the Bulk projection DECLINED over the century. This
      # measures how often the projected path falls, so the saturating form can be
      # judged on whether it removes the decline rather than on assertion. If a
      # deployed spec still shows a material share here, the transform did not
      # work and the decline is being masked, not fixed.
      if (all(c("region", "year", "index") %in% colnames(proj))) {
        mono <- unlist(lapply(split(proj[order(proj$year), ], proj$region[order(proj$year)]),
                              function(d) {
                                if (nrow(d) < 2) return(numeric(0))
                                diff(d$index) < -1e-8
                              }), use.names = FALSE)
        monoShare <- if (length(mono)) mean(mono, na.rm = TRUE) else NA_real_
        if (is.finite(monoShare) && monoShare > 0.05) {
          nWarning <- nWarning + 1L
          modelFlags[[paste0(sec, ".nonMonotone")]] <- data.frame(
            rule = "nonMonotone", severity = "warning",
            region = "ALL", year = NA_integer_, value = monoShare,
            detail = paste0(round(100 * monoShare), "% of projected year-on-year ",
                            "steps decrease (policy stock should ratchet)"),
            sector = sec, stringsAsFactors = FALSE
          )
        }
      }

      # Scenario-responsiveness gate (ADR 0039, handoff item 7): project the same
      # spec on the REFERENCE scenario and require the gating and reference
      # projections to differ. A spec whose median |gating - reference| index
      # delta over IN-COVERAGE region-years in the evaluation window falls below
      # `minScenarioDelta` is severe-flagged "scenarioBlind" - a feasibility layer
      # that cannot distinguish an ambitious from a current-policy pathway cannot
      # inform coupling (composite-AP specs clamp to the same support edge under
      # both pathways; empirically ~0.00 vs split ~0.56 at 2050).
      if (!is.null(referenceScenarioData)) {
        ref <- tryCatch(
          projectPSMSpecScenario(cfg, sec, histData = panelData,
                                 scenarioData = referenceScenarioData,
                                 modelDir = modelDir, indexMax = indexMax, verbose = FALSE),
          error = function(e) NULL
        )
        if (is.null(ref)) {
          evaluable <- FALSE
          reason <- "reference-scenario projection failed"
          break
        }
        keep <- c("region", "year", "index")
        cmp <- merge(proj[, c(keep, intersect("outOfCoverage", colnames(proj)))],
                     ref[, keep], by = c("region", "year"),
                     suffixes = c("", ".ref"))
        if ("outOfCoverage" %in% colnames(cmp)) {
          cmp <- cmp[!cmp$outOfCoverage %in% TRUE, , drop = FALSE]
        }
        cmp <- cmp[cmp$year >= deltaWindow[1] & cmp$year <= deltaWindow[2], , drop = FALSE]
        respDelta <- if (nrow(cmp) > 0) {
          stats::median(abs(cmp$index - cmp$index.ref), na.rm = TRUE)
        } else NA_real_
        if (!is.finite(respDelta) || respDelta < minScenarioDelta) {
          nSevere <- nSevere + 1L
          modelFlags[[paste0(sec, ".scenarioBlind")]] <- data.frame(
            rule = "scenarioBlind", severity = "severe",
            region = "ALL", year = NA_integer_,
            value = respDelta,
            detail = paste0("median |gating-reference| index delta ",
                            round(respDelta, 4), " < ", minScenarioDelta,
                            " over ", deltaWindow[1], "-", deltaWindow[2],
                            " (in-coverage)"),
            sector = sec, stringsAsFactors = FALSE
          )
          say("  [sanity:PolicyStringency] ", m, " (", sec, ") scenario-blind: ",
              "median |gating-reference| = ", round(respDelta, 4),
              " < ", minScenarioDelta)
        }
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
    # Kept for every evaluated model, passing or not, so the ceiling trade-off is
    # inspectable after the fact rather than only visible as a rejection.
    if (length(modelCeiling) > 0) ceilingByModel[[m]] <- unlist(modelCeiling)
    if (length(modelGamma) > 0) gammaByModel[[m]] <- unlist(modelGamma)
    if (evaluable && (nSevere < least$nSevere ||
                        (nSevere == least$nSevere && nWarning < least$nWarning))) {
      least <- list(model = m, nSevere = nSevere, nWarning = nWarning)
    }
    say("  [sanity:PolicyStringency] #", nEval, " (batch ", batch, ") ", m, " - ",
        if (!evaluable) paste0("not evaluable (", reason, ")")
        else paste0(nSevere, " severe / ", nWarning, " warnings",
                    if (pass) " - PASS" else ""))
    if (pass) {
      chosen <- m
      break
    }
  }

  if (is.null(chosen)) {
    forced <- TRUE
    chosen <- least$model %||% passModels[1]
  }
  list(
    chosen = chosen, forced = forced,
    trace = if (length(traceRows) > 0) do.call(rbind, traceRows) else NULL,
    flags = flagsByModel,
    ceiling = ceilingByModel,
    gamma = gammaByModel
  )
}

# Median frontier-ceiling trajectory for one spec/sector across the scenario.
# Returns the end/start ratio of the median S* over COVERED countries, with the
# trend frozen at the last historical year exactly as projectFeasiblePath does.
# `ratio` < 1 means the ceiling falls; see the ceilingCollapse gate above.
#' @keywords internal
.psmCeilingTrajectory <- function(cfg, sector, histData, scenarioData,
                                  modelDir = NULL, indexMax = 10,
                                  years = c(2025, 2100)) {
  unl <- function(x) if (is.null(x)) NULL else unlist(x)
  ff <- do.call(estimatePolicyStringencyModel, c(
    list(data = histData, sector = sector, estimator = "frontier",
         indexMax = indexMax, modelDir = modelDir, updateIndex = FALSE, verbose = FALSE),
    .psmSpecArgs(cfg)))
  b <- stats::coef(ff$model)
  b <- b[!names(b) %in% c("sigmaSq", "gamma")]
  covered <- unique(as.character(ff$data$region))

  sDf <- preparePanelData(
    data = scenarioData, sector = sector,
    actorPowerDrivers = unl(cfg$actorPowerDrivers), actorPowerIndex = unl(cfg$actorPowerIndex),
    instQualityDrivers = unl(cfg$instQualityDrivers),
    controlDrivers = setdiff(unl(cfg$controlDrivers), "lagged_ecp"),
    regionMappingFixedEffects = if (isTRUE(cfg$useMundlak)) NULL else cfg$regionMappingFixedEffects,
    useMundlak = isTRUE(cfg$useMundlak), gdpGovInteraction = isTRUE(cfg$gdpGovInteraction),
    driverScaling = ff$driverScaling,
    trendFreezeYear = suppressWarnings(max(ff$data$year, na.rm = TRUE)),
    outcomeVar = ff$outcomeVar %||% "Policy Stringency",
    apTransform = cfg$apTransform %||% "linear")
  sDf$lagged_ecp <- 0
  lv <- ff$model$xlevels$regionFE %||% levels(ff$data$regionFE)
  if (!is.null(lv) && "regionFE" %in% names(sDf)) {
    fe <- as.character(sDf$regionFE)
    fe[!fe %in% lv] <- if ("Other" %in% lv) "Other" else lv[1]
    sDf$regionFE <- factor(fe, levels = lv)
  }
  sDf <- .psmDriverGuard(sDf, .driverSupportRanges(ff$data, ff$driverScaling))$df

  tt <- stats::delete.response(stats::terms(ff$formula))
  mm <- stats::model.matrix(tt, stats::model.frame(tt, data = sDf, na.action = stats::na.pass))
  shared <- intersect(colnames(mm), names(b))
  sStar <- indexMax * stats::plogis(as.numeric(mm[, shared, drop = FALSE] %*% b[shared]))

  keep <- as.character(sDf$region) %in% covered & is.finite(sStar)
  d <- data.frame(region = as.character(sDf$region)[keep], year = sDf$year[keep],
                  s = sStar[keep], stringsAsFactors = FALSE)
  d <- d[d$year >= years[1] & d$year <= years[2], , drop = FALSE]
  med <- tapply(d$s, d$year, stats::median, na.rm = TRUE)
  pick <- function(y) {
    av <- as.integer(names(med))
    if (!length(av)) return(NA_real_)
    as.numeric(med[[which.min(abs(av - y))]])
  }
  y0 <- pick(years[1])
  y1 <- pick(years[2])
  # The median can be flat while individual countries still lose their ceiling:
  # measured on v1, the best admissible spec holds the median at 1.005 with 46%
  # of countries still falling (the deployed one: 0.62 and 96%). The gate scores
  # the median because that is what survives the region aggregation into phi, but
  # the residual is reported so a passing spec is never read as "no country falls".
  perC <- vapply(split(d, d$region), function(z) {
    a <- z$s[which.min(z$year)]
    b2 <- z$s[which.max(z$year)]
    if (!is.finite(a) || a <= 0 || !is.finite(b2)) NA_real_ else b2 / a
  }, numeric(1))
  # gamma comes free: `ff` is already fitted above and `b` already strips it out.
  # Returning it lets the gammaGate screen cost ZERO additional frontier fits
  # (ADR 0043 consequences: "screen gamma on any winner").
  gm <- tryCatch({
    cf <- stats::coef(ff$model)
    if ("gamma" %in% names(cf)) as.numeric(cf[["gamma"]]) else ff$frontierGamma %||% NA_real_
  }, error = function(e) NA_real_)

  list(ratio = if (is.finite(y0) && y0 > 0) y1 / y0 else NA_real_,
       ceil0 = y0, ceil1 = y1, year0 = years[1], year1 = years[2],
       shareFalling = mean(perC < 1, na.rm = TRUE),
       gamma = gm)
}
# nolint end
