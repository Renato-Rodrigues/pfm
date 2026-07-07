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
  ranges <- .driverSupportRanges(fit$data)
  guarded <- .psmDriverGuard(sDf, ranges)
  driverOutOfSupport <- guarded$outOfSupport
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
                             say = function(...) invisible()) {
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
    flags = flagsByModel
  )
}
# nolint end
