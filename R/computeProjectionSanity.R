# nolint start
#' @title projectSpecScenario
#' @description Produces scenario projections for one channel specification and
#' sector: adoption probability, stringency response, implied price (USD/tCO2),
#' and hurdle-expected price. Reusable by the Projection Sanity gate in
#' \code{\link{runChannelsWorkflow}} and by the results reports.
#'
#' FD-transformed specs (\code{panelTransform != "levels"}) are not projectable
#' with this helper (hazard composition / scenario differencing is not yet
#' implemented — ADR 0005); the function returns \code{NULL} with attribute
#' \code{reason}.
#'
#' @param cfg List. A channel spec (fields as in \code{\link{channelSpecs}}).
#' @param sector Character. \code{"Bulk"} or \code{"Diffuse"}.
#' @param histData \code{magpie}. Historical panel (for fitting via cache).
#' @param scenarioData \code{magpie}. Scenario panel (e.g. \code{panelDataScenario}).
#' @param family Character. Stringency family. Default \code{"Gamma"}.
#' @param modelDir Character or NULL. Model store for cached fits.
#' @param extrapLogMargin Numeric. Extrapolation guard for the stringency price.
#'   The projected response (\code{E[log(1+ECP)]}) is clamped to the in-sample
#'   fitted maximum plus this margin (additive on the log scale), so projected
#'   prices stay finite even when ill-conditioned coefficients drive the linear
#'   predictor far beyond the fitted range. A margin of \code{m} permits prices up
#'   to \eqn{e^m} times the highest in-sample fitted price. Default \code{2}
#'   (~7.4x). The Projection Sanity gate still flags region-years where the clamp
#'   binds, so the model fragility remains visible.
#' @param priceCeiling Numeric. Absolute hard ceiling (USD/tCO2) on the projected
#'   price, applied after \code{extrapLogMargin}. Catches the case where the
#'   in-sample fitted range is itself inflated by ill-conditioned coefficients so
#'   the data-anchored clamp is too loose. Default \code{5000} (above any plausible
#'   feasible carbon price; \code{Inf} disables it). The sanity gate still flags
#'   where it binds.
#' @param minProjYear Numeric or NULL. Projection horizon cutoff: only years
#'   strictly greater than this are returned. \code{NULL} (default) auto-derives it
#'   from the training data's last year, so the returned projection starts after the
#'   last historical year — dropping the scenario panel's history-overlap years and
#'   the first-year driver-lag NA artifact.
#' @param verbose Logical.
#'
#' @return Data.frame \code{region, year, sector, prob, stringencyResponse,
#'   price, expectedPrice}, or \code{NULL} when not projectable.
#'
#' @importFrom stats plogis predict delete.response model.frame model.matrix na.pass fitted
#'
#' @export
#' @author Renato Rodrigues
projectSpecScenario <- function(cfg, sector, histData, scenarioData,
                                family = "Gamma", modelDir = getOption("pfm.modelDir", "output"),
                                extrapLogMargin = c(Bulk = 2, Diffuse = 1), priceCeiling = 5000,
                                minProjYear = NULL, verbose = FALSE) {
  if (!identical(cfg$panelTransform %||% "levels", "levels")) {
    if (isTRUE(verbose)) {
      message("projectSpecScenario: panelTransform '", cfg$panelTransform,
              "' not projectable (ADR 0005) - returning NULL.")
    }
    return(NULL)
  }
  if (isTRUE(cfg$ridgeInteractions)) {
    if (isTRUE(verbose)) {
      message("projectSpecScenario: ridge fits not supported - returning NULL.")
    }
    return(NULL)
  }

  adoptionFit <- estimateAdoptionModel(
    data = histData, sector = sector,
    actorPowerDrivers = cfg$actorPowerDrivers, actorPowerIndex = cfg$actorPowerIndex,
    instQualityDrivers = cfg$instQualityDrivers, controlDrivers = cfg$controlDrivers,
    includeLaggedAdoption = isTRUE(cfg$includeLagged),
    interactRegionFE = isTRUE(cfg$interactRegionFE),
    regionMappingFixedEffects = cfg$regionMappingFixedEffects,
    useMundlak = isTRUE(cfg$useMundlak), gdpGovInteraction = isTRUE(cfg$gdpGovInteraction),
    logisticTimeTrend = isTRUE(cfg$logisticTimeTrend),
    modelDir = modelDir, verbose = verbose,
    compute = c(ame = FALSE, predictedProbs = FALSE)
  )
  stringencyFit <- estimatePriceStringencyModel(
    data = histData, sector = sector, family = family,
    actorPowerDrivers = cfg$actorPowerDrivers, actorPowerIndex = cfg$actorPowerIndex,
    instQualityDrivers = cfg$instQualityDrivers, controlDrivers = cfg$controlDrivers,
    includeLaggedECP = isTRUE(cfg$includeLaggedECP) || isTRUE(cfg$includeLagged),
    interactRegionFE = isTRUE(cfg$interactRegionFE),
    regionMappingFixedEffects = cfg$regionMappingFixedEffects,
    useMundlak = isTRUE(cfg$useMundlak), gdpGovInteraction = isTRUE(cfg$gdpGovInteraction),
    logisticTimeTrend = isTRUE(cfg$logisticTimeTrend),
    nickellCorrection = isTRUE(cfg$nickellCorrection),
    modelDir = modelDir, verbose = verbose
  )

  # Driver standardization (CONTEXT.md "Driver Standardization"): driver columns
  # and interaction factors are standardized with the mean/sd FROZEN at their
  # historical-panel values. Recompute those constants from the same historical
  # panel the models were fit on (one no-fit preparePanelData pass) and reuse them
  # when building the scenario panel, so historical and projected panels share one
  # reference (no seam discontinuity, exactly as for GDP-Q and the PCA rotation).
  histDf <- preparePanelData(
    data = histData, sector = sector,
    actorPowerDrivers = cfg$actorPowerDrivers, actorPowerIndex = cfg$actorPowerIndex,
    instQualityDrivers = cfg$instQualityDrivers, controlDrivers = cfg$controlDrivers,
    regionMappingFixedEffects = cfg$regionMappingFixedEffects,
    useMundlak = isTRUE(cfg$useMundlak), gdpGovInteraction = isTRUE(cfg$gdpGovInteraction)
  )
  dscale <- attr(histDf, "driverScaling")
  # ADR 0010: freeze the time trend at the last historical year out of sample.
  lastHistYear <- tryCatch(max(histDf$year, na.rm = TRUE), error = function(e) NULL)

  scenDf <- preparePanelData(
    data = scenarioData, sector = sector,
    actorPowerDrivers = cfg$actorPowerDrivers, actorPowerIndex = cfg$actorPowerIndex,
    instQualityDrivers = cfg$instQualityDrivers, controlDrivers = cfg$controlDrivers,
    regionMappingFixedEffects = cfg$regionMappingFixedEffects,
    useMundlak = isTRUE(cfg$useMundlak), gdpGovInteraction = isTRUE(cfg$gdpGovInteraction),
    driverScaling = dscale, trendFreezeYear = lastHistYear
  )

  # ── Adoption probability ─────────────────────────────────────────────────────
  aModel <- adoptionFit$model
  aDf <- .alignRegionFE(scenDf, adoptionFit$data, model = aModel)
  tt <- stats::delete.response(stats::terms(adoptionFit$formula))
  mf <- stats::model.frame(tt, data = aDf, na.action = stats::na.pass)
  mm <- stats::model.matrix(tt, mf)
  beta <- stats::coef(aModel)
  shared <- intersect(colnames(mm), names(beta))
  eta <- as.numeric(mm[, shared, drop = FALSE] %*% beta[shared])
  prob <- stats::plogis(eta)

  # ── Stringency response and price ────────────────────────────────────────────
  sDf <- .alignRegionFE(scenDf, stringencyFit$data, model = stringencyFit$model)

  # Extrapolation guard cap (additive on the log(1+ECP) response scale): the
  # projected response cannot exceed the in-sample fitted maximum + extrapLogMargin,
  # keeping prices finite when ill-conditioned coefficients overshoot. Used both as
  # the one-shot clamp and as the per-year clamp in the recursive (lagged) path.
  # Per-sector extrapolation margin (2026-06-24): diffuse prices are bounded more tightly.
  marg <- if (!is.null(names(extrapLogMargin)) && sector %in% names(extrapLogMargin)) {
    extrapLogMargin[[sector]]
  } else extrapLogMargin[[1]]
  # Anchor the clamp to the in-sample OBSERVED response max, not the fitted max (an
  # ill-conditioned GLM can inflate the fitted range); fall back to fitted if unavailable.
  insObs <- tryCatch(as.numeric(stringencyFit$data$ecp), error = function(e) NULL)
  if (is.null(insObs) || !length(insObs) || all(is.na(insObs))) {
    insObs <- tryCatch(as.numeric(stats::fitted(stringencyFit$model)), error = function(e) NULL)
  }
  capVal <- if (!is.null(insObs) && length(insObs) > 0) {
    max(insObs, na.rm = TRUE) + marg
  } else Inf

  hasLag <- "lagged_ecp" %in% all.vars(stringencyFit$formula)
  respRaw <- rep(NA_real_, nrow(sDf))   # pre-clamp response, for the clamp-reliance signal
  if (!hasLag) {
    resp <- tryCatch(
      as.numeric(stats::predict(stringencyFit$model, newdata = sDf, type = "response")),
      error = function(e) rep(NA_real_, nrow(sDf))
    )
    respRaw <- pmax(resp, 0)
    resp <- if (is.finite(capVal)) pmin(respRaw, capVal) else respRaw
  } else {
    # ── Recursive (dynamic) projection: a lagged-price model needs last year's
    # PREDICTED price as an input, so future years are simulated forward, not
    # solved in one shot (CONTEXT.md "Lagged Stringency Projection").
    # eta_t = eta_fixed_t + beta_lag * lag_t, with lag seeded at the region's last
    # historical log(1+ECP) (0 for never-adopters -> hurdle-consistent: a region's
    # conditional price builds up from zero once it switches on), and carried
    # forward as the (clamped) predicted log(1+ECP). Clamping inside the loop stops
    # a runaway year from propagating through the recursion.
    bLag <- tryCatch(stats::coef(stringencyFit$model)[["lagged_ecp"]], error = function(e) NA_real_)
    if (is.null(bLag) || !is.finite(bLag)) bLag <- 0
    sDf0 <- sDf
    sDf0$lagged_ecp <- 0
    etaFixed <- tryCatch(
      as.numeric(stats::predict(stringencyFit$model, newdata = sDf0, type = "link")),
      error = function(e) rep(NA_real_, nrow(sDf0))
    )
    td <- stringencyFit$data # ecp column is already log(1+ECP) for adopters
    seed <- tryCatch(tapply(td$ecp, td$region, function(v) v[length(v)]),
                     error = function(e) NULL)
    resp <- rep(NA_real_, nrow(sDf))
    for (r in unique(sDf$region)) {
      idx <- which(sDf$region == r)
      idx <- idx[order(sDf$year[idx])]
      lagv <- if (!is.null(seed) && as.character(r) %in% names(seed) &&
                    is.finite(seed[[as.character(r)]])) seed[[as.character(r)]] else 0
      for (i in idx) {
        e <- etaFixed[i] + bLag * lagv
        if (!is.finite(e)) { resp[i] <- NA_real_; next }
        eRaw <- max(e, 0)
        respRaw[i] <- eRaw
        e <- if (is.finite(capVal)) min(eRaw, capVal) else eRaw
        resp[i] <- e
        lagv <- e
      }
    }
  }
  price <- expm1(resp) # depVar is log(1+ECP); response is E[log1p(price)]
  # Absolute safety ceiling: a data-anchored clamp still permits absurd values
  # when the in-sample fitted range is itself inflated by ill-conditioned
  # coefficients. priceCeiling is a hard, interpretable bound ("no region feasibly
  # sustains more than this USD/tCO2"); the Projection Sanity gate flags where it
  # binds. Set priceCeiling = Inf to disable.
  if (is.finite(priceCeiling)) price <- pmin(price, priceCeiling)

  # Pre-clamp price + which region-years were pinned at the extrapolation guard. A spec whose
  # projection leans heavily on the clamp is extrapolating beyond its data (clamp-reliance gate).
  priceUnclamped <- expm1(respRaw)
  clampPinned <- is.finite(respRaw) & is.finite(capVal) & (respRaw > capVal + 1e-9)

  out <- data.frame(
    region = aDf$region, year = aDf$year, sector = sector,
    prob = prob, stringencyResponse = resp,
    price = price, expectedPrice = prob * price,
    priceUnclamped = priceUnclamped, clampPinned = clampPinned,
    stringsAsFactors = FALSE
  )

  # Restrict to the genuine projection horizon: years AFTER the last historical
  # year. The scenario panel republishes recent history (e.g. 2005-2020) and its
  # first year has NA driver lags; those are not projections. minProjYear = NULL
  # auto-derives the cutoff from the training data's last year.
  cut <- minProjYear
  if (is.null(cut)) {
    ty <- tryCatch(stringencyFit$data$year, error = function(e) NULL)
    if (is.null(ty)) ty <- tryCatch(adoptionFit$data$year, error = function(e) NULL)
    cut <- if (!is.null(ty)) max(ty, na.rm = TRUE) else -Inf
  }
  out <- out[is.finite(out$year) & out$year > cut, , drop = FALSE]
  rownames(out) <- NULL
  out
}

# Internal: align the scenario regionFE factor to the levels the fitted model
# actually saw (glm xlevels when available — the estimation sample may lack
# blocks present in the full panel), mapping unknown regions to "Other".
#' @keywords internal
.alignRegionFE <- function(scenDf, trainDf, model = NULL) {
  if (!"regionFE" %in% colnames(scenDf)) return(scenDf)
  lv <- NULL
  if (!is.null(model) && !is.null(model$xlevels) && !is.null(model$xlevels$regionFE)) {
    lv <- model$xlevels$regionFE
  } else if ("regionFE" %in% colnames(trainDf)) {
    lv <- levels(droplevels(trainDf$regionFE))
  }
  if (is.null(lv)) return(scenDf)
  fe <- as.character(scenDf$regionFE)
  fe[!fe %in% lv] <- if ("Other" %in% lv) "Other" else lv[1]
  scenDf$regionFE <- factor(fe, levels = lv)
  scenDf
}

#' @title computeProjectionSanity
#' @description Evaluates the five Projection Sanity rules (CONTEXT.md, decided
#' 2026-06-12) on scenario projections. A post-maximin \strong{selection gate}.
#' \emph{Severe} flags disqualify a spec; \emph{warnings} are reported only. Rule
#' applicability depends on \code{stage}: adoption specs are judged on probability
#' rules, stringency specs on price rules, and \code{"hurdle"} evaluates everything
#' on the combined expected price.
#'
#' Severe: (1) price explosion — price > \code{priceExplosion} USD/tCO2 anywhere;
#' (2) negative / non-finite price; (3b) an entire region block stays below
#' \code{probDeadLow} through the horizon (a region that never adopts contradicts
#' an ambitious scenario). Warnings: (3a) all-region adoption saturation
#' (P > \code{probSaturationHigh}) — expected under ambitious policy, only severe
#' if it occurs before \code{saturationSevereBefore}; (4) seam jump vs the last
#' historical observation; (5) single-year price spikes >
#' \code{spikeFactor} x neighbouring years.
#'
#' @param proj Data.frame from \code{\link{projectSpecScenario}} (or rbind of
#'   them) with columns \code{region, year, sector, prob, price} (and
#'   \code{expectedPrice} for \code{stage = "hurdle"}).
#' @param stage Character. \code{"adoption"}, \code{"stringency"}, or \code{"hurdle"}.
#' @param histPrices Optional data.frame \code{region, year, price} of observed
#'   historical ECP (per the projection's sector) for the seam rule.
#' @param histProbs Optional data.frame \code{region, prob} of fitted historical
#'   adoption probabilities at the last historical year for the seam rule.
#' @param regionBlocks Optional data.frame \code{region, block} (e.g. H12) for
#'   the dead-block rule; falls back to per-region evaluation when NULL.
#' @param thresholds Named list overriding defaults: \code{priceExplosion = 2000},
#'   \code{probSaturationHigh = 0.99}, \code{probDeadLow = 0.01},
#'   \code{saturationSevereBefore = NA} (all-region adoption saturation is a
#'   \emph{warning} — expected under an ambitious scenario; set this to a year to
#'   escalate to severe only when saturation occurs implausibly early),
#'   \code{seamProbJump = 0.3}, \code{seamPriceFactor = 2}, \code{spikeFactor = 2},
#'   \code{spikeMinPrice = 10}, \code{missingShareWarn = 0.05} (NA projections are a
#'   coverage warning, not a severe model pathology — negative or infinite prices
#'   remain severe).
#'
#' @return List: \code{flags} (data.frame \code{rule, severity, region, year,
#'   value, detail}), \code{summary} (one row: \code{nSevere, nWarning, pass}),
#'   \code{thresholds}.
#'
#' @export
#' @author Renato Rodrigues
computeProjectionSanity <- function(proj, stage = c("adoption", "stringency", "hurdle"),
                                    histPrices = NULL, histProbs = NULL,
                                    regionBlocks = NULL, thresholds = list()) {
  stage <- match.arg(stage)
  th <- utils::modifyList(list(
    priceExplosion = 2000, probSaturationHigh = 0.99, probDeadLow = 0.01,
    saturationSevereBefore = NA_real_, seamProbJump = 0.3, seamPriceFactor = 2,
    spikeFactor = 2, spikeMinPrice = 10, missingShareWarn = 0.05, clampReliance = 0.25
  ), thresholds)

  flags <- list()
  addFlag <- function(rule, severity, region, year, value, detail) {
    flags[[length(flags) + 1L]] <<- data.frame(
      rule = rule, severity = severity, region = region,
      year = if (is.null(year)) NA_integer_ else as.integer(year),
      value = as.numeric(value), detail = detail, stringsAsFactors = FALSE
    )
  }

  priceCol <- if (stage == "hurdle") "expectedPrice" else "price"
  useProb <- stage %in% c("adoption", "hurdle")
  usePrice <- stage %in% c("stringency", "hurdle")

  if (usePrice && priceCol %in% colnames(proj)) {
    p <- proj[[priceCol]]
    # Missing projections are a coverage warning (structurally absent drivers),
    # NOT a severe model pathology.
    naShare <- mean(is.na(p))
    if (naShare > th$missingShareWarn) {
      addFlag("price-missing", "warning", "(coverage)", NA, naShare,
              sprintf("%.0f%% of region-years have no price projection (missing drivers)",
                      naShare * 100))
    }
    # Rule 1: explosion (severe)
    bad <- which(is.finite(p) & p > th$priceExplosion)
    for (i in utils::head(bad, 50)) {
      addFlag("price-explosion", "severe", proj$region[i], proj$year[i], p[i],
              paste0("> ", th$priceExplosion, " USD/tCO2"))
    }
    if (length(bad) > 50) {
      addFlag("price-explosion", "severe", "(many)", NA, length(bad),
              paste0(length(bad), " region-years above threshold (first 50 listed)"))
    }
    # Rule 2: negative or infinite (severe) — NA handled above as coverage
    bad <- which((!is.na(p) & !is.finite(p)) | (is.finite(p) & p < 0))
    if (length(bad) > 0) {
      addFlag("price-invalid", "severe", proj$region[bad[1]], proj$year[bad[1]],
              p[bad[1]], paste0(length(bad), " negative/infinite price values"))
    }
    # Rule 1b: clamp-reliance (severe) — the projection-plausibility hard filter (2026-06-24).
    # A spec whose forward projection is pinned against the extrapolation guard for more than
    # `clampReliance` of region-years is extrapolating beyond its data; .sanitySelect rejects it
    # rather than reporting a clamped (guard-rail) price.
    if ("clampPinned" %in% colnames(proj)) {
      pinFrac <- mean(proj$clampPinned, na.rm = TRUE)
      if (is.finite(pinFrac) && pinFrac > th$clampReliance) {
        addFlag("clamp-reliance", "severe", "(extrapolation)", NA, pinFrac,
                sprintf("%.0f%% of region-years pinned at the extrapolation clamp", pinFrac * 100))
      }
    }
    # Rule 5: spikes (warning)
    for (r in unique(proj$region)) {
      sub <- proj[proj$region == r, , drop = FALSE]
      sub <- sub[order(sub$year), , drop = FALSE]
      pv <- sub[[priceCol]]
      n <- length(pv)
      if (n >= 3) {
        for (i in 2:(n - 1)) {
          nbVals <- c(pv[i - 1], pv[i + 1])
          nbVals <- nbVals[is.finite(nbVals)]
          nb <- if (length(nbVals) > 0) max(nbVals) else NA_real_
          if (is.finite(pv[i]) && is.finite(nb) && pv[i] > th$spikeMinPrice &&
                nb > 0 && pv[i] > th$spikeFactor * nb) {
            addFlag("price-spike", "warning", r, sub$year[i], pv[i],
                    paste0("> ", th$spikeFactor, "x neighbouring years"))
          }
        }
      }
    }
    # Rule 4 (price seam, warning)
    if (!is.null(histPrices)) {
      firstYear <- min(proj$year)
      for (r in unique(proj$region)) {
        hp <- histPrices$price[histPrices$region == r]
        hp <- hp[is.finite(hp)]
        if (length(hp) == 0) next
        hLast <- utils::tail(hp, 1)
        sFirst <- proj[[priceCol]][proj$region == r & proj$year == firstYear][1]
        if (is.finite(hLast) && is.finite(sFirst) && hLast > 1 &&
              abs(log1p(sFirst) - log1p(hLast)) > log(th$seamPriceFactor)) {
          addFlag("seam-jump-price", "warning", r, firstYear, sFirst,
                  paste0("vs ", round(hLast, 1), " observed at seam (>", th$seamPriceFactor, "x)"))
        }
      }
    }
  }

  if (useProb && "prob" %in% colnames(proj)) {
    # Rule 3a: all-region saturation. WARNING by default (2026-06-15): under an
    # ambitious climate-policy scenario, near-universal adoption by mid-century is
    # the expected outcome, not a model pathology, so this no longer disqualifies a
    # spec — a human judges it. The old linear-trend pathology it once guarded
    # against is now prevented mechanically by the bounded saturating trend. It is
    # only escalated to severe if saturation occurs implausibly early (before
    # `saturationSevereBefore`, default NA = never severe), a tunable guard.
    byYear <- tapply(proj$prob, proj$year, function(v) min(v, na.rm = TRUE))
    satYears <- names(byYear)[is.finite(byYear) & byYear > th$probSaturationHigh]
    if (length(satYears) > 0) {
      firstSat <- suppressWarnings(min(as.integer(satYears)))
      sev <- if (is.finite(th$saturationSevereBefore) && is.finite(firstSat) &&
                   firstSat < th$saturationSevereBefore) "severe" else "warning"
      addFlag("prob-saturation", sev, "(all regions)", satYears[1],
              byYear[satYears[1]],
              paste0("every region P > ", th$probSaturationHigh, " from ", satYears[1],
                     if (sev == "severe") paste0(" (before ", th$saturationSevereBefore,
                                                 " - implausibly fast)") else ""))
    }
    # Rule 3b: dead block/region through the horizon (severe)
    blockOf <- if (!is.null(regionBlocks)) {
      stats::setNames(regionBlocks$block, regionBlocks$region)
    } else NULL
    grp <- if (is.null(blockOf)) proj$region else {
      b <- blockOf[proj$region]
      ifelse(is.na(b), proj$region, b)
    }
    maxByGrp <- tapply(proj$prob, grp, function(v) max(v, na.rm = TRUE))
    dead <- names(maxByGrp)[is.finite(maxByGrp) & maxByGrp < th$probDeadLow]
    for (d in dead) {
      addFlag("prob-dead", "severe", d, NA, maxByGrp[d],
              paste0("P < ", th$probDeadLow, " for the entire horizon"))
    }
    # Rule 4 (prob seam, warning)
    if (!is.null(histProbs)) {
      firstYear <- min(proj$year)
      for (r in unique(proj$region)) {
        hp <- histProbs$prob[histProbs$region == r][1]
        sp <- proj$prob[proj$region == r & proj$year == firstYear][1]
        if (is.finite(hp) && is.finite(sp) && abs(sp - hp) > th$seamProbJump) {
          addFlag("seam-jump-prob", "warning", r, firstYear, sp,
                  paste0("vs ", round(hp, 2), " fitted at seam (|d| > ", th$seamProbJump, ")"))
        }
      }
    }
  }

  flagsDf <- if (length(flags) > 0) do.call(rbind, flags) else
    data.frame(rule = character(0), severity = character(0), region = character(0),
               year = integer(0), value = numeric(0), detail = character(0),
               stringsAsFactors = FALSE)
  nSevere <- sum(flagsDf$severity == "severe")
  nWarning <- sum(flagsDf$severity == "warning")
  list(
    flags = flagsDf,
    summary = data.frame(stage = stage, nSevere = nSevere, nWarning = nWarning,
                         pass = nSevere == 0, stringsAsFactors = FALSE),
    thresholds = th
  )
}
# nolint end
