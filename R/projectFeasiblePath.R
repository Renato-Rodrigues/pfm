# nolint start
#' Politically feasible stringency path (gap dynamics, ADR 0040/0041)
#'
#' @description
#' The speed-limited stringency path a polity can follow along an IAM scenario:
#' the projection object the IAM coupling consumes, and the successor to the
#' ad-hoc recursion that used to live in the paper's figure script.
#'
#' Two corrections relative to that script are the reason this function exists
#' (see \code{docs/psm-ceiling-feedback-diagnosis.md}):
#' \enumerate{
#'   \item \strong{Scale.} The ECM is estimated on the transformed response
#'     \eqn{y^* = logit(S/indexMax)}, so \eqn{\lambda} is a fractional gap-closure
#'     rate \emph{on the logit scale}. The recursion therefore runs on \eqn{y^*}
#'     and is transformed once at the end. Applying \eqn{\lambda} to natural-scale
#'     0-\code{indexMax} gaps (as the figure script did) misstates the speed for
#'     polities near either end of the index - exactly where the interesting cases
#'     sit.
#'   \item \strong{Attractor.} The path converges to the \strong{ECM equilibrium}
#'     \eqn{y^{*,eq} = (c_0 + \theta'x)/(-\phi)}, \emph{not} to the SFA frontier.
#'     \eqn{\lambda} was validated as the rate of convergence to \eqn{S^{eq}};
#'     using the frontier as the operational target imports its boundary-\eqn{\gamma}
#'     attribution degeneracy into the IAM. The frontier enters only as an
#'     \emph{upper bound} and as the gap exhibit.
#' }
#'
#' \strong{Scenario rules.} \code{"speed-limited"} closes the gap at the
#' politically exhibited speed (the ambition scenario). \code{"frozen-gap"} holds
#' the polity at its seed-year position relative to its own ceiling - politics as
#' usual, where policy moves only because the fundamentals move (the current-policy
#' scenario). Together they are the published scenario pair: \emph{the gap is a
#' political choice, the speed is the constraint}.
#'
#' @param spec Normalised specification (named list) as stored in
#'   \code{selected-models-psm.yml}; must be \code{panelTransform = "levels"}.
#' @param sector Character. \code{"Bulk"} or \code{"Diffuse"}.
#' @param histData Historical panel (magpie) carrying the Policy Stringency outcome.
#' @param scenarioData Scenario panel (magpie) for one IAM pathway.
#' @param lambda Numeric or \code{NULL}. Adjustment speed on the logit scale.
#'   \code{NULL} (default) estimates it here from the ECM fit of \code{spec}. Pass
#'   a value to impose a sector-resolved rate (e.g. the validated electricity rate)
#'   - see \code{\link{runPSMSectorSpeeds}} for the honesty labels that must travel
#'   with any such number.
#' @param rule \code{"speed-limited"} (default) or \code{"frozen-gap"}; see above.
#' @param gapMeasure How \code{"frozen-gap"} holds the polity fixed:
#'   \code{"ratio"} (default) freezes the \emph{efficiency ratio} \eqn{S/S^*} - the
#'   frontier's own dimensionless quantity, and the one the coupling tiers use;
#'   \code{"absolute"} freezes the gap in index points. Ratio is the default
#'   because it is scale-free and conservative for laggards: a polity at 1.4 with a
#'   ceiling of 6.7 gains 0.2 index points from a 1-point ceiling rise under
#'   \code{"ratio"} but a full 1.0 under \code{"absolute"}.
#' @param frontierBeta Named numeric vector of frontier coefficients (the
#'   \code{estimate} column of \code{frontier.rds}
#'   \code{$bySector$<sector>$coefTable}, \code{sigmaSq}/\code{gamma} dropped), or
#'   \code{NULL}. Required for \code{"frozen-gap"} and for the ceiling bound;
#'   without it the ceiling columns are \code{NA} and no bound is applied.
#' @param seedYear Integer or \code{NULL}. Year the path is seeded from
#'   (\code{NULL} = last historical year of the fit).
#' @param ratchet Logical. If \code{TRUE}, enforce a non-decreasing path
#'   (a policy stock does not shrink). Default \code{FALSE}: the diagnosis showed
#'   this \emph{masks} a declining projection rather than fixing it, and the
#'   saturating actor-power form (ADR 0040) is the fix. Enable only to test
#'   sensitivity, and read \code{nonMonotoneShare} in the attributes first.
#' @param indexMax Numeric. Index ceiling. Default \code{10}.
#' @param modelDir Fit Cache root, or \code{NULL}.
#' @param verbose Logical.
#'
#' @return Data.frame with one row per projected region-year:
#'   \code{region, year, sector, rule, lambda, seedIndex, feasibleIndex,
#'   equilibriumIndex, ceilingIndex, gapIndex, efficiencyRatio, outOfCoverage,
#'   driverOutOfSupport, driverOutOfSample}. Attributes: \code{"lambda"},
#'   \code{"phi"}, \code{"seedYear"}, \code{"nonMonotoneShare"},
#'   \code{"ceilingBindShare"}.
#'
#' @seealso \code{\link{projectPSMSpecScenario}} (one-shot level projection used by
#'   the sanity gate), \code{\link{runPSMTemporalValidation}} (where \eqn{\lambda}
#'   is estimated and validated), \code{\link{aggregateFeasibilityToRegions}}
#'   (delivery to IAM regions). ADR 0040, ADR 0041.
#'
#' @importFrom stats plogis qlogis coef terms delete.response model.frame model.matrix na.pass
#' @export
#' @author Renato Rodrigues
projectFeasiblePath <- function(spec, sector, histData, scenarioData,
                                lambda = NULL,
                                rule = c("speed-limited", "frozen-gap"),
                                gapMeasure = c("ratio", "absolute"),
                                frontierBeta = NULL,
                                seedYear = NULL,
                                ratchet = FALSE,
                                indexMax = 10,
                                modelDir = getOption("pfm.modelDir", "output"),
                                verbose = FALSE) {
  rule <- match.arg(rule)
  gapMeasure <- match.arg(gapMeasure)
  if (!identical(spec$panelTransform %||% "levels", "levels")) {
    stop("projectFeasiblePath: panelTransform '", spec$panelTransform,
         "' is not projectable (levels only).")
  }
  if (identical(rule, "frozen-gap") && is.null(frontierBeta)) {
    stop("projectFeasiblePath: rule = 'frozen-gap' holds the polity fixed relative ",
         "to its own ceiling, so 'frontierBeta' is required.")
  }
  unl <- function(x) if (is.null(x)) NULL else unlist(x)

  # --- 1. ECM fit: source of lambda, of the equilibrium, and of the design ------
  fit <- estimatePolicyStringencyModel(
    data = histData, sector = sector, estimator = "satP", form = "ecm",
    indexMax = indexMax,
    actorPowerDrivers = unl(spec$actorPowerDrivers),
    actorPowerIndex = unl(spec$actorPowerIndex),
    instQualityDrivers = unl(spec$instQualityDrivers),
    controlDrivers = unl(spec$controlDrivers),
    regionMappingFixedEffects = spec$regionMappingFixedEffects,
    logisticTimeTrend = isTRUE(spec$logisticTimeTrend),
    interactRegionFE = isTRUE(spec$interactRegionFE),
    useMundlak = isTRUE(spec$useMundlak),
    gdpGovInteraction = isTRUE(spec$gdpGovInteraction),
    apTransform = spec$apTransform %||% "linear",
    modelDir = modelDir, updateIndex = FALSE, verbose = FALSE
  )
  phi <- tryCatch(stats::coef(fit$model)[["lagged_ecp"]], error = function(e) NA_real_)
  if (!is.finite(phi) || phi >= 0) {
    stop("projectFeasiblePath: the ECM fit carries no usable error-correction ",
         "coefficient (lagged_ecp = ", format(phi), "); lambda = -phi must be > 0.")
  }
  lam <- if (is.null(lambda)) -phi else lambda
  if (!is.finite(lam) || lam <= 0 || lam >= 1) {
    stop("projectFeasiblePath: lambda must lie in (0, 1); got ", format(lam), ".")
  }
  lastHist <- suppressWarnings(max(fit$data$year, na.rm = TRUE))
  sy <- seedYear %||% lastHist

  # --- 2. Scenario design (frozen transforms, guarded) -------------------------
  sDf <- preparePanelData(
    data = scenarioData, sector = sector,
    actorPowerDrivers = unl(spec$actorPowerDrivers),
    actorPowerIndex = unl(spec$actorPowerIndex),
    instQualityDrivers = unl(spec$instQualityDrivers),
    controlDrivers = setdiff(unl(spec$controlDrivers), "lagged_ecp"),
    regionMappingFixedEffects = if (isTRUE(spec$useMundlak)) NULL else spec$regionMappingFixedEffects,
    useMundlak = isTRUE(spec$useMundlak),
    gdpGovInteraction = isTRUE(spec$gdpGovInteraction),
    driverScaling = fit$driverScaling,
    trendFreezeYear = if (is.finite(lastHist)) lastHist else NULL,
    outcomeVar = fit$outcomeVar %||% "Policy Stringency"
  )
  lv <- fit$model$xlevels$regionFE
  if (!is.null(lv) && "regionFE" %in% names(sDf)) {
    fe <- as.character(sDf$regionFE)
    fe[!fe %in% lv] <- if ("Other" %in% lv) "Other" else lv[1]
    sDf$regionFE <- factor(fe, levels = lv)
  }
  guard <- .psmDriverGuard(sDf, .driverSupportRanges(fit$data, fit$driverScaling))
  sDf <- guard$df

  # --- 3. Equilibrium on the TRANSFORMED scale ---------------------------------
  # The ECM is  d y* = c0 + phi y*_{t-1} + theta' x_{t-1}. Setting d y* = 0 gives
  #   y*_eq = (c0 + theta' x) / (-phi) = etaFixed / lambda_fit,
  # where etaFixed is the design WITHOUT the lagged term (intercept, drivers,
  # trend and FE included) - the same quantity the validated recursion uses.
  sDf0 <- sDf
  sDf0$lagged_ecp <- 0
  tt <- stats::delete.response(stats::terms(fit$formula))
  mm <- stats::model.matrix(tt, stats::model.frame(tt, data = sDf0, na.action = stats::na.pass))
  beta <- stats::coef(fit$model)
  shared <- intersect(colnames(mm), names(beta))
  etaFixed <- as.numeric(mm[, shared, drop = FALSE] %*% beta[shared])
  # The equilibrium is a property of the FITTED ECM, so it is always divided by the
  # fit's own -phi even when a sector-resolved `lambda` is imposed on the dynamics.
  etaEq <- etaFixed / (-phi)

  # --- 4. Ceiling from the frontier (bound + exhibit only) ---------------------
  etaCeil <- rep(NA_real_, nrow(sDf))
  if (!is.null(frontierBeta)) {
    fb <- frontierBeta[!names(frontierBeta) %in% c("sigmaSq", "gamma")]
    missing <- setdiff(names(fb), colnames(mm))
    if (length(missing)) {
      stop("projectFeasiblePath: frontier terms absent from the scenario design: ",
           paste(missing, collapse = ", "))
    }
    etaCeil <- as.numeric(mm[, names(fb), drop = FALSE] %*% fb)
  }

  # --- 5. Seed: the OBSERVED transformed level in the seed year ----------------
  nSV <- fit$squeeze$n %||% sum(is.finite(fit$data$ecp))
  toEta <- function(v) stats::qlogis(.psmSqueeze(pmin(pmax(v / indexMax, 0), 1), nSV))
  seedTab <- .psmSeedIndex(histData, sector, sy,
                           outcomeVar = fit$outcomeVar %||% "Policy Stringency")
  trained <- unique(as.character(fit$data$region))

  reg <- as.character(sDf$region)
  yr <- sDf$year
  keep <- is.finite(yr) & yr > sy
  ord <- order(reg, yr)
  ord <- ord[keep[ord]]

  yStar <- rep(NA_real_, nrow(sDf))
  nBind <- 0L
  nStep <- 0L
  for (r in unique(reg[ord])) {
    idx <- ord[reg[ord] == r]
    if (!r %in% names(seedTab) || !is.finite(seedTab[[r]])) next
    prev <- toEta(seedTab[[r]])
    prevYear <- sy
    for (i in idx) {
      dt <- yr[i] - prevYear
      if (!is.finite(dt) || dt <= 0) next
      if (identical(rule, "speed-limited")) {
        # Compounded over the step, so 5-year IAM periods and annual steps agree.
        lamEff <- 1 - (1 - lam)^dt
        v <- prev + lamEff * (etaEq[i] - prev)
      } else {
        v <- NA_real_  # frozen-gap is resolved on the natural scale below
      }
      if (isTRUE(ratchet) && is.finite(v) && is.finite(prev)) v <- max(v, prev)
      if (is.finite(v) && is.finite(etaCeil[i]) && v > etaCeil[i]) {
        v <- etaCeil[i]
        nBind <- nBind + 1L
      }
      yStar[i] <- v
      nStep <- nStep + 1L
      if (is.finite(v)) prev <- v
      prevYear <- yr[i]
    }
  }

  ceilingIndex <- indexMax * stats::plogis(etaCeil)
  feasible <- indexMax * stats::plogis(yStar)

  if (identical(rule, "frozen-gap")) {
    # Politics as usual: the polity holds its seed-year position relative to its
    # own ceiling, so its policy moves only because its fundamentals move.
    seedIdx <- as.numeric(seedTab[reg])
    ceilSeed <- .psmCeilingAtSeed(etaCeil, reg, yr, sy, indexMax)
    feasible <- if (identical(gapMeasure, "ratio")) {
      ceilingIndex * pmin(pmax(seedIdx / ceilSeed, 0), 1)
    } else {
      pmax(ceilingIndex - (ceilSeed - seedIdx), 0)
    }
    # The ceiling is an upper bound under BOTH rules, exactly as in the
    # speed-limited recursion above. It binds here when a polity is observed above
    # its own estimated ceiling - possible because the frontier is stochastic, so
    # some observations legitimately sit above it - and without this the two
    # gapMeasure variants would disagree for precisely those polities.
    feasible <- pmin(feasible, ceilingIndex)
    feasible[!keep] <- NA_real_
  }

  out <- data.frame(
    region = reg, year = yr, sector = sector, rule = rule, lambda = lam,
    seedIndex = as.numeric(seedTab[reg]),
    feasibleIndex = feasible,
    equilibriumIndex = indexMax * stats::plogis(etaEq),
    ceilingIndex = ceilingIndex,
    outOfCoverage = !(reg %in% trained),
    driverOutOfSupport = guard$outOfSupport,
    driverOutOfSample = guard$outOfSample,
    stringsAsFactors = FALSE
  )
  out$gapIndex <- out$ceilingIndex - out$feasibleIndex
  out$efficiencyRatio <- pmin(pmax(out$feasibleIndex / pmax(out$ceilingIndex, 1e-9), 0), 1)
  out <- out[keep, , drop = FALSE]
  out <- out[order(out$region, out$year), , drop = FALSE]
  rownames(out) <- NULL

  dec <- unlist(lapply(split(out, out$region), function(d) {
    if (nrow(d) < 2) return(logical(0))
    diff(d$feasibleIndex) < -1e-8
  }), use.names = FALSE)
  attr(out, "lambda") <- lam
  attr(out, "phi") <- phi
  attr(out, "seedYear") <- sy
  attr(out, "nonMonotoneShare") <- if (length(dec)) mean(dec, na.rm = TRUE) else NA_real_
  attr(out, "ceilingBindShare") <- if (nStep > 0) nBind / nStep else NA_real_
  if (isTRUE(verbose)) {
    message("[projectFeasiblePath] ", sector, " (", rule, "): lambda = ",
            round(lam, 4), ", seed ", sy, ", ", nrow(out), " rows; ceiling binds ",
            round(100 * (attr(out, "ceilingBindShare") %||% 0)), "%, non-monotone ",
            round(100 * (attr(out, "nonMonotoneShare") %||% 0)), "%.")
  }
  out
}

# Observed natural-scale index per region in the seed year.
#' @keywords internal
.psmSeedIndex <- function(histData, sector, seedYear, outcomeVar = "Policy Stringency") {
  if (is.data.frame(histData)) {
    d <- histData[histData$year == seedYear, , drop = FALSE]
    return(stats::setNames(d$ecp, as.character(d$region)))
  }
  v <- paste0(outcomeVar, "|", sector)
  yrs <- magclass::getYears(histData, as.integer = TRUE)
  if (!seedYear %in% yrs) {
    stop(".psmSeedIndex: seed year ", seedYear, " absent from the historical panel ",
         "(available ", paste(range(yrs), collapse = "-"), ").")
  }
  x <- histData[, seedYear, v]
  stats::setNames(as.numeric(x), magclass::getItems(x, dim = 1))
}

# Ceiling in the seed year, broadcast to every row of the same region. The seed
# year itself is not in the projection window, so the earliest projected year is
# used as its proxy (the ceiling moves smoothly and this is only a normaliser).
#' @keywords internal
.psmCeilingAtSeed <- function(etaCeil, reg, yr, seedYear, indexMax) {
  ceilIdx <- indexMax * stats::plogis(etaCeil)
  firstBy <- tapply(seq_along(reg), reg, function(i) i[which.min(yr[i])])
  base <- stats::setNames(ceilIdx[unlist(firstBy)], names(firstBy))
  as.numeric(base[reg])
}
# nolint end
