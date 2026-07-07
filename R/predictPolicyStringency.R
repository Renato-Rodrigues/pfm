# nolint start
#' @title predictPolicyStringency
#' @description Produces scenario projections of the Policy Stringency Model
#' (ADR 0036) from an already fitted, \emph{loaded} \code{PFMModel} of stage
#' \code{"policyStringency"} and a scenario panel — WITHOUT refitting and WITHOUT
#' the historical training panel (the \code{\link{predictFeasibility}} discipline,
#' ADR 0009). The projected index is \strong{bounded by construction}:
#' \eqn{index = indexMax \cdot logit^{-1}(\eta)} (the satP engine form), so no
#' extrapolation clamp exists on this path — there is nothing to clamp.
#'
#' Only the satP engine is ever persisted to the model store, so this is a
#' satP-only reconstruction; other suite estimators are in-session exhibits
#' (see \code{\link{computeEstimatorAgreement}}).
#'
#' Uncertainty: for the non-dynamic path, a delta-method 95\% interval on
#' \eqn{\eta} from the stored clustered vcov, mapped through the bounded
#' response (\code{indexLo}/\code{indexHi}). For the dynamic (lagged) path the
#' interval is not propagated through the recursion and is returned \code{NA}.
#'
#' Coverage: regions absent from the estimation sample (USA, Brazil, ... — see
#' "Out-of-Coverage Projection" in CONTEXT.md) still receive driver-based
#' predictions but are flagged \code{outOfCoverage = TRUE}; the IAM gets a full
#' map, the paper states which rows are extrapolated.
#'
#' @param model A \code{PFMModel} with \code{stage == "policyStringency"}.
#' @param scenarioData \code{magpie}. Scenario panel; must be built with the
#'   frozen transforms in scope (\code{\link{rehydrateModelTransforms}} is called
#'   defensively here, as in \code{predictFeasibility}).
#' @param minProjYear Numeric or NULL. Horizon cutoff; only years strictly
#'   greater are returned. \code{NULL} derives it from
#'   \code{model$training_years[2]}.
#' @param driverGuard Character. \code{"winsorize"} (default) clamps each
#'   standardized base driver to its training-support range (stored on the model
#'   as \code{transforms$driverRanges}) and recomputes the interaction columns
#'   from the guarded factors before building \code{eta} — a disclosed
#'   regressor-support restriction (the response stays bounded-by-construction;
#'   nothing clamps the outcome). The per-row share of drivers that were outside
#'   support is returned as \code{driverOutOfSupport} (the extrapolation audit).
#'   \code{"none"} disables the guard (pre-guard behaviour; the audit column is
#'   still computed when ranges are available). Models persisted before the
#'   guard existed carry no ranges: the guard is skipped with a message.
#' @param verbose Logical.
#'
#' @return Data.frame \code{region, year, sector, eta, index, indexLo, indexHi,
#'   outOfCoverage, driverOutOfSupport} with \code{index} in \code{[0, indexMax]}
#'   (natural CAPMF units). For out-of-coverage rows the interval additionally
#'   carries the between-region-FE spread (they inherit the reference-group
#'   fixed effect, so the FE assignment uncertainty is part of their interval).
#'
#' @author Renato Rodrigues
#'
#' @importFrom stats plogis coef terms delete.response model.frame model.matrix na.pass qnorm sd
#'
#' @export
predictPolicyStringency <- function(model, scenarioData, minProjYear = NULL,
                                    driverGuard = c("winsorize", "none"),
                                    verbose = FALSE) {
  driverGuard <- match.arg(driverGuard)
  stopifnot(inherits(model, "PFMModel"))
  if (!identical(model$stage, "policyStringency")) {
    stop("predictPolicyStringency: model must be a stage='policyStringency' PFMModel ",
         "(got '", model$stage, "').", call. = FALSE)
  }
  sp <- model$transforms$prepSpec
  if (is.null(sp)) {
    stop("PFMModel ", model$id, " carries no prepSpec; re-fit with the current ",
         "pfm version so the scenario design can be rebuilt (ADR 0009).", call. = FALSE)
  }
  if (!identical(sp$estimator %||% "satP", "satP")) {
    stop("predictPolicyStringency: only satP-engine fits are persisted/projected ",
         "(got estimator '", sp$estimator, "').", call. = FALSE)
  }
  indexMax <- sp$indexMax %||% 10
  sector <- model$sector

  rehydrateModelTransforms(model)

  freezeYr <- if (length(model$training_years) >= 2 && is.finite(model$training_years[2])) {
    model$training_years[2]
  } else NULL
  sDf <- preparePanelData(
    data                      = scenarioData,
    sector                    = sector,
    actorPowerDrivers         = sp$actorPowerDrivers,
    actorPowerIndex           = sp$actorPowerIndex,
    instQualityDrivers        = sp$instQualityDrivers,
    controlDrivers            = setdiff(sp$controlDrivers, "lagged_ecp"),
    regionMappingFixedEffects = sp$regionMappingFixedEffects,
    lag                       = sp$lag %||% 1,
    useMundlak                = isTRUE(sp$useMundlak),
    gdpGovInteraction         = isTRUE(sp$gdpGovInteraction),
    driverScaling             = model$transforms$driverScaling,
    trendFreezeYear           = freezeYr,
    outcomeVar                = sp$outcomeVar %||% "Policy Stringency"
  )
  sDf <- .alignRegionFEStored(sDf, model)

  # Driver-support guard + extrapolation audit (R3). Ranges are stored on models
  # built after 2026-07-06; older slim fits fall back to unguarded projection.
  ranges <- model$transforms$driverRanges
  driverOutOfSupport <- rep(NA_real_, nrow(sDf))
  if (!is.null(ranges) && length(ranges) > 0) {
    guarded <- .psmDriverGuard(sDf, ranges)
    driverOutOfSupport <- guarded$outOfSupport
    if (identical(driverGuard, "winsorize")) {
      sDf <- guarded$df
    }
  } else if (identical(driverGuard, "winsorize")) {
    message("predictPolicyStringency: model ", model$id, " carries no driverRanges ",
            "(pre-guard fit); projecting unguarded.")
  }

  designEta <- function(df) {
    tt <- stats::delete.response(stats::terms(model$formula))
    mm <- stats::model.matrix(tt, stats::model.frame(tt, data = df, na.action = stats::na.pass))
    beta <- stats::coef(model$model)
    shared <- intersect(colnames(mm), names(beta))
    list(
      eta = as.numeric(mm[, shared, drop = FALSE] %*% beta[shared]),
      mm = mm[, shared, drop = FALSE],
      shared = shared
    )
  }

  trainedRegions <- names(model$applyState$seed_prices %||% character(0))
  outOfCoverage <- if (length(trainedRegions) > 0) {
    !(as.character(sDf$region) %in% trainedRegions)
  } else {
    rep(NA, nrow(sDf))
  }

  hasLag <- "lagged_ecp" %in% all.vars(model$formula)
  isECM <- identical(sp$form %||% "static", "ecm")
  etaLo <- etaHi <- NULL
  if (!hasLag) {
    d <- designEta(sDf)
    eta <- d$eta
    vc <- model$vcov
    if (!is.null(vc) && all(d$shared %in% rownames(vc))) {
      vcS <- vc[d$shared, d$shared, drop = FALSE]
      seEta <- sqrt(pmax(rowSums((d$mm %*% vcS) * d$mm), 0))
      # Out-of-coverage rows inherit the reference-group FE; their interval must
      # carry the between-FE spread on top of coefficient uncertainty (R4a).
      feSpread <- .regionFESpread(stats::coef(model$model))
      oc <- outOfCoverage %in% TRUE
      seEta[oc] <- sqrt(seEta[oc]^2 + feSpread^2)
      zc <- stats::qnorm(0.975)
      etaLo <- eta - zc * seEta
      etaHi <- eta + zc * seEta
    }
  } else {
    # Dynamic (lagged) projection: eta_t = etaFixed_t + bLag * lag_t, seeded from the
    # stored per-region last historical TRANSFORMED response (applyState$seed_prices —
    # the response scale is logit(y/indexMax), so the carried value stays on eta scale).
    # ECM form (R5): the fit is Delta-y* = c + phi * y*_{t-1} + beta x, so the level
    # recursion is y*_t = etaFixed_t + (1 + phi) * y*_{t-1} — same machinery with the
    # lag coefficient shifted by one.
    bLag <- tryCatch(stats::coef(model$model)[["lagged_ecp"]], error = function(e) NA_real_)
    if (is.null(bLag) || !is.finite(bLag)) bLag <- 0
    effLag <- if (isECM) 1 + bLag else bLag
    sDf0 <- sDf
    sDf0$lagged_ecp <- 0
    etaFixed <- designEta(sDf0)$eta
    seed <- model$applyState$seed_prices
    eta <- rep(NA_real_, nrow(sDf))
    for (r in unique(sDf$region)) {
      idx <- which(sDf$region == r)
      idx <- idx[order(sDf$year[idx])]
      lagv <- if (!is.null(seed) && as.character(r) %in% names(seed) &&
                    is.finite(seed[[as.character(r)]])) seed[[as.character(r)]] else 0
      for (i in idx) {
        e <- etaFixed[i] + effLag * lagv
        if (!is.finite(e)) {
          eta[i] <- NA_real_
          next
        }
        eta[i] <- e
        lagv <- e
      }
    }
    if (isTRUE(verbose)) {
      message("  [psm] dynamic projection (", if (isECM) "error-correction" else "lagged",
              " response); CI not propagated (NA).")
    }
  }

  out <- data.frame(
    region = sDf$region,
    year = sDf$year,
    sector = sector,
    eta = eta,
    index = indexMax * stats::plogis(eta),
    indexLo = if (!is.null(etaLo)) indexMax * stats::plogis(etaLo) else NA_real_,
    indexHi = if (!is.null(etaHi)) indexMax * stats::plogis(etaHi) else NA_real_,
    outOfCoverage = outOfCoverage,
    driverOutOfSupport = driverOutOfSupport,
    stringsAsFactors = FALSE
  )

  cut <- minProjYear
  if (is.null(cut)) {
    ty <- model$training_years
    cut <- if (length(ty) >= 2 && is.finite(ty[2])) ty[2] else -Inf
  }
  out <- out[is.finite(out$year) & out$year > cut, , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Derive the Implementability Factor from a Policy Stringency projection
#'
#' Post-processing (ADR 0036): rescales the projected policy-stringency index to
#' a 0-1 multiplier REMIND can apply to non-price policy assumptions, WITHOUT the
#' model layer baking in coupling semantics — the raw \code{index} remains the
#' canonical, coupling-agnostic output ("Implementability Factor" in
#' CONTEXT.md). Default transform \code{index / indexMax}; the normalisation is
#' deliberately simple, documented, and revisable.
#'
#' @param projection Data.frame from \code{\link{predictPolicyStringency}}.
#' @param indexMax Numeric. The index ceiling used at fit time. Default \code{10}.
#'
#' @return \code{projection} with added columns \code{implementability} (and
#'   \code{implementabilityLo}/\code{implementabilityHi} where the projection
#'   carries an interval).
#'
#' @export
computeImplementabilityFactor <- function(projection, indexMax = 10) {
  stopifnot(is.data.frame(projection), "index" %in% names(projection))
  projection$implementability <- pmin(pmax(projection$index / indexMax, 0), 1)
  if ("indexLo" %in% names(projection)) {
    projection$implementabilityLo <- pmin(pmax(projection$indexLo / indexMax, 0), 1)
  }
  if ("indexHi" %in% names(projection)) {
    projection$implementabilityHi <- pmin(pmax(projection$indexHi / indexMax, 0), 1)
  }
  projection
}
# nolint end
