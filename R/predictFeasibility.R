# nolint start
#' @title predictFeasibility
#' @description Produces scenario feasibility projections from a pair of already
#' fitted, \emph{loaded} PFMModels (one adoption, one stringency) and a scenario
#' panel — WITHOUT refitting and WITHOUT the historical training panel. This is
#' the REMIND-coupling entry point (ADR 0009): in an iterative coupling the same
#' loaded models are applied to a gdx-derived panel that updates each iteration,
#' and the returned prices/probabilities are fed back to REMIND.
#'
#' Where \code{\link{projectSpecScenario}} re-fits both stages from the historical
#' panel via the model cache, \code{predictFeasibility} reads everything it needs
#' from the slim Fitted Models themselves: the (data-stripped but predict-capable)
#' fit, the frozen application transforms (\code{driverScaling}, GDP-Q fit, PCA
#' rotation, the \code{preparePanelData} spec) and the minimal apply-state
#' (per-region seed prices for the lag recursion, the in-sample response maximum
#' for the extrapolation clamp, and the regionFE levels).
#'
#' @param adoptionModel A \code{PFMModel} with \code{stage == "adoption"}.
#' @param stringencyModel A \code{PFMModel} with \code{stage == "stringency"}.
#' @param scenarioData \code{magpie}. Scenario panel (e.g. \code{panelDataScenario}
#'   on a REMIND gdx). \strong{Must be built in a session where the frozen GDP-Q
#'   and PCA transforms are in scope}; call \code{\link{rehydrateModelTransforms}}
#'   before \code{panelDataScenario} in a fresh (REMIND) session. This function
#'   rehydrates them defensively for its own \code{preparePanelData} pass.
#' @param extrapLogMargin,priceCeiling Projection guards, identical semantics to
#'   \code{\link{projectSpecScenario}}.
#' @param minProjYear Numeric or NULL. Horizon cutoff; only years strictly greater
#'   are returned. \code{NULL} auto-derives it from \code{stringencyModel$training_years[2]}.
#' @param verbose Logical.
#'
#' @return Data.frame \code{region, year, sector, prob, stringencyResponse,
#'   price, expectedPrice}.
#'
#' @importFrom stats plogis predict delete.response model.frame model.matrix na.pass coef terms
#' @export
#' @author Renato Rodrigues
predictFeasibility <- function(adoptionModel, stringencyModel, scenarioData,
                               extrapLogMargin = c(Bulk = 2, Diffuse = 1), priceCeiling = 5000,
                               minProjYear = NULL, verbose = FALSE) {
  stopifnot(inherits(adoptionModel, "PFMModel"),
            inherits(stringencyModel, "PFMModel"))
  if (!identical(adoptionModel$stage, "adoption")) {
    stop("adoptionModel must be a stage='adoption' PFMModel.", call. = FALSE)
  }
  if (!identical(stringencyModel$stage, "stringency")) {
    stop("stringencyModel must be a stage='stringency' PFMModel.", call. = FALSE)
  }
  sector <- stringencyModel$sector

  # Restore the frozen GDP-Q fit and PCA rotation so preparePanelData reuses the
  # historical reference instead of re-deriving from the scenario panel.
  rehydrateModelTransforms(stringencyModel)

  scenDf <- function(model) {
    sp <- model$transforms$prepSpec
    if (is.null(sp)) {
      stop("PFMModel ", model$id, " carries no prepSpec; re-fit with the current ",
           "pfm version so the scenario design can be rebuilt (ADR 0009).", call. = FALSE)
    }
    freezeYr <- if (length(model$training_years) >= 2 && is.finite(model$training_years[2])) {
      model$training_years[2]
    } else NULL
    preparePanelData(
      data                      = scenarioData,
      sector                    = sector,
      actorPowerDrivers         = sp$actorPowerDrivers,
      actorPowerIndex           = sp$actorPowerIndex,
      instQualityDrivers        = sp$instQualityDrivers,
      controlDrivers            = sp$controlDrivers,
      regionMappingFixedEffects = sp$regionMappingFixedEffects,
      lag                       = sp$lag %||% 1,
      useMundlak                = isTRUE(sp$useMundlak),
      gdpGovInteraction         = isTRUE(sp$gdpGovInteraction),
      driverScaling             = model$transforms$driverScaling,
      trendFreezeYear           = freezeYr
    )
  }

  # ── Adoption probability ─────────────────────────────────────────────────────
  aDf <- .alignRegionFEStored(scenDf(adoptionModel), adoptionModel)
  ttA <- stats::delete.response(stats::terms(adoptionModel$formula))
  mmA <- stats::model.matrix(ttA, stats::model.frame(ttA, data = aDf, na.action = stats::na.pass))
  betaA <- stats::coef(adoptionModel$model)
  sharedA <- intersect(colnames(mmA), names(betaA))
  prob <- stats::plogis(as.numeric(mmA[, sharedA, drop = FALSE] %*% betaA[sharedA]))

  # ── Stringency response and price ────────────────────────────────────────────
  sDf <- .alignRegionFEStored(scenDf(stringencyModel), stringencyModel)
  marg <- if (!is.null(names(extrapLogMargin)) && sector %in% names(extrapLogMargin)) {
    extrapLogMargin[[sector]]
  } else extrapLogMargin[[1]]
  capVal <- if (is.finite(stringencyModel$applyState$insMaxResp %||% NA_real_)) {
    stringencyModel$applyState$insMaxResp + marg
  } else Inf

  hasLag <- "lagged_ecp" %in% all.vars(stringencyModel$formula)
  if (!hasLag) {
    resp <- tryCatch(
      as.numeric(stats::predict(stringencyModel$model, newdata = sDf, type = "response")),
      error = function(e) rep(NA_real_, nrow(sDf))
    )
    if (is.finite(capVal)) resp <- pmin(pmax(resp, 0), capVal)
  } else {
    # Recursive (dynamic) projection seeded from the stored per-region last
    # historical log(1+ECP) — see projectSpecScenario / CONTEXT.md "Lagged
    # Stringency Projection". No historical panel needed here.
    bLag <- tryCatch(stats::coef(stringencyModel$model)[["lagged_ecp"]],
                     error = function(e) NA_real_)
    if (is.null(bLag) || !is.finite(bLag)) bLag <- 0
    sDf0 <- sDf
    sDf0$lagged_ecp <- 0
    etaFixed <- tryCatch(
      as.numeric(stats::predict(stringencyModel$model, newdata = sDf0, type = "link")),
      error = function(e) rep(NA_real_, nrow(sDf0))
    )
    seed <- stringencyModel$applyState$seed_prices
    resp <- rep(NA_real_, nrow(sDf))
    for (r in unique(sDf$region)) {
      idx <- which(sDf$region == r)
      idx <- idx[order(sDf$year[idx])]
      lagv <- if (!is.null(seed) && as.character(r) %in% names(seed) &&
                    is.finite(seed[[as.character(r)]])) seed[[as.character(r)]] else 0
      for (i in idx) {
        e <- etaFixed[i] + bLag * lagv
        if (!is.finite(e)) { resp[i] <- NA_real_; next }
        if (is.finite(capVal)) e <- min(max(e, 0), capVal)
        resp[i] <- e
        lagv <- e
      }
    }
  }
  price <- expm1(resp)
  if (is.finite(priceCeiling)) price <- pmin(price, priceCeiling)

  out <- data.frame(
    region = aDf$region, year = aDf$year, sector = sector,
    prob = prob, stringencyResponse = resp,
    price = price, expectedPrice = prob * price,
    stringsAsFactors = FALSE
  )

  cut <- minProjYear
  if (is.null(cut)) {
    ty <- stringencyModel$training_years
    cut <- if (length(ty) >= 2 && is.finite(ty[2])) ty[2] else -Inf
  }
  out <- out[is.finite(out$year) & out$year > cut, , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Restore a loaded model's frozen transforms into the package environment
#'
#' Writes \code{model$transforms$gdpQ} and \code{model$transforms$scPCA} into
#' \code{.pfm_env} so that a subsequent \code{\link{panelDataScenario}} /
#' \code{\link{preparePanelData}} reuses the historical GDP-quantile fit and the
#' V-Dem state-capacity PCA rotation frozen at fit time, instead of re-deriving
#' them from the scenario data. Call this once after loading a model in a fresh
#' (e.g. REMIND) session, before building the scenario panel (ADR 0009).
#'
#' @param model A \code{PFMModel}.
#' @return \code{model} invisibly.
#' @export
rehydrateModelTransforms <- function(model) {
  stopifnot(inherits(model, "PFMModel"))
  if (!is.null(model$transforms$gdpQ)) .pfm_env$gdppc_q_fit <- model$transforms$gdpQ
  if (!is.null(model$transforms$scPCA)) .pfm_env$sc_pca_rotation <- model$transforms$scPCA
  invisible(model)
}

# Align scenario regionFE to the levels the fitted model saw, using the levels
# stored on the slim model (applyState$regionFE_levels) — no training data or
# glm xlevels required. Falls back to the model's xlevels when present.
#' @keywords internal
.alignRegionFEStored <- function(scenDf, model) {
  if (!"regionFE" %in% colnames(scenDf)) return(scenDf)
  lv <- model$applyState$regionFE_levels
  if (is.null(lv) && !is.null(model$model$xlevels$regionFE)) lv <- model$model$xlevels$regionFE
  if (is.null(lv)) return(scenDf)
  fe <- as.character(scenDf$regionFE)
  fe[!fe %in% lv] <- if ("Other" %in% lv) "Other" else lv[1]
  scenDf$regionFE <- factor(fe, levels = lv)
  scenDf
}
# nolint end
