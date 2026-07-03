# nolint start
#' @title estimatePolicyStringencyModel
#' @description Estimates the Policy Stringency Model (PSM, ADR 0036): a
#' single-stage bounded-response panel model of the CAPMF policy-stringency
#' index (0 to \code{indexMax}) on Actor Power, Institutional Quality and their
#' interactions. There is \strong{no adoption hurdle} — zero-stringency
#' observations are genuine data and are kept (unlike
#' \code{\link{estimatePriceStringencyModel}}, which subsets to positive prices).
#'
#' The dependent variable is bounded structurally by the \emph{estimator}
#' ("PSM Estimator Suite" in CONTEXT.md), never by clamps:
#' \describe{
#'   \item{satP}{\emph{The selection engine.} Gaussian-identity GLM on
#'     \code{logit(y/indexMax)} (the ADR 0026 saturating form with the index's true
#'     ceiling). Full likelihood, so AIC/BIC/pseudo-R2 and every maximin tie-break
#'     stay valid. Boundary values are handled by the Smithson-Verkuilen squeeze
#'     \code{(p*(n-1)+0.5)/n}.}
#'   \item{fractional}{\emph{The headline estimator} (paper coefficient/AME tables).
#'     Papke-Wooldridge fractional logit: quasi-binomial GLM on \code{y/indexMax}.
#'     Handles exact 0 and \code{indexMax} natively; quasi-likelihood, so \strong{no
#'     AIC/BIC} — never used for selection.}
#'   \item{beta}{Beta regression (\code{betareg}, logit mean link) on the squeezed
#'     share — the full-likelihood cross-check. Optional dependency.}
#'   \item{levels}{Gaussian-identity GLM on the raw index — the literature-benchmark
#'     rung; unbounded, never projected.}
#' }
#' Estimators are compared ONLY on signs/AMEs and scale-free out-of-sample metrics
#' (see \code{\link{computeEstimatorAgreement}}), never on information criteria.
#'
#' @param data A \code{magpie} object or a \code{data.frame}. A \code{data.frame}
#'   is assumed to be an already-assembled panel (the internal
#'   \code{preparePanelData} pass-through), with the outcome still on its natural
#'   0-\code{indexMax} scale unless \code{prepared = TRUE}.
#' @param sector Character. \code{"Bulk"} or \code{"Diffuse"}.
#' @param estimator Character. \code{"satP"} (default), \code{"fractional"},
#'   \code{"beta"} or \code{"levels"} — see Description.
#' @param indexMax Numeric. The structural ceiling of the index. Default \code{10}
#'   (the CAPMF scale).
#' @param outcomeVar Character. Outcome base name passed to
#'   \code{\link{preparePanelData}}. Default \code{"Policy Stringency"}.
#' @param actorPowerDrivers,actorPowerIndex,instQualityDrivers,controlDrivers
#'   Specification lists, as in \code{\link{estimatePriceStringencyModel}}.
#' @param regionMappingFixedEffects Character or NULL. Region mapping file for
#'   fixed effects; \code{NULL} omits them.
#' @param timeTrend,logisticTimeTrend,interactRegionFE,useMundlak,gdpGovInteraction
#'   Formula options, as in the sibling estimator.
#' @param lag Integer. Driver lag in years. Default \code{1}.
#' @param includeLaggedPS Logical. If \code{TRUE}, includes the lagged policy
#'   stringency (transformed onto the response scale) as a predictor — the
#'   policy-ratcheting dynamics rung. Default \code{FALSE}.
#' @param modelDir Character or NULL. Model-store directory. Persistence and the
#'   cache apply to the \code{satP} engine only (the only estimator that enters
#'   selection and projection); other estimators are cheap one-off refits for the
#'   agreement exhibit and are never cached.
#' @param updateIndex,ignoreCache,label,verbose,maxit As in the sibling estimator.
#' @param prepared Logical. When \code{TRUE}, \code{data} is an already-prepared
#'   data.frame with the response \emph{already transformed} for this estimator
#'   (bootstrap/refit entry, ADR 0025); prep, transforms, cache and save are skipped.
#'
#' @return A list with elements \code{model}, \code{coeftest} (cluster-robust),
#'   \code{vcov} (clustered by region), \code{sector}, \code{family},
#'   \code{formula}, \code{data} (response-transformed), \code{estimator},
#'   \code{indexMax}, \code{outcomeVar}, \code{outcomeNatural} (the untransformed
#'   0-\code{indexMax} outcome for the estimation rows), \code{boundaryShares}
#'   (share of observations at 0 and at \code{indexMax} — the empirical gate for
#'   the conditional Tobit/two-part rung), \code{squeeze} (boundary-transform
#'   parameters), \code{ameIndex} (average marginal effects on the natural index
#'   scale with delta-method clustered SEs), plus \code{vifRaw}/\code{maxVIF} and
#'   \code{predictiveDiagnostics} as in the sibling estimator.
#'
#' @author Renato Rodrigues
#'
#' @importFrom stats glm gaussian quasibinomial qlogis plogis dlogis coef vcov fitted model.matrix pnorm
#' @importFrom lmtest coeftest
#' @importFrom sandwich vcovCL
#'
#' @export
#'
estimatePolicyStringencyModel <- function(
    data,
    sector = "Bulk",
    estimator = "satP",
    indexMax = 10,
    outcomeVar = "Policy Stringency",
    actorPowerDrivers = c(
      "VRE share", "Electrification",
      "Coal primary energy share", "Oil/Gas primary energy share",
      "Fossil share in Industry"
    ),
    actorPowerIndex = "Actor Power Index",
    instQualityDrivers = c(
      "Government Effectiveness (WGI)", "Rule of Law (VDem)",
      "Vertical Accountability (VDem)"
    ),
    controlDrivers = NULL,
    regionMappingFixedEffects = "regionmappingH12.csv",
    timeTrend = TRUE,
    logisticTimeTrend = FALSE,
    lag = 1,
    interactRegionFE = FALSE,
    useMundlak = FALSE,
    gdpGovInteraction = FALSE,
    includeLaggedPS = FALSE,
    modelDir = getOption("pfm.modelDir", "output"),
    updateIndex = TRUE,
    ignoreCache = FALSE,
    label = "",
    verbose = TRUE,
    maxit = 3000,
    prepared = FALSE) {
  estimator <- match.arg(estimator, c("satP", "fractional", "beta", "levels"))
  if (!is.numeric(indexMax) || length(indexMax) != 1 || indexMax <= 0) {
    stop("estimatePolicyStringencyModel: indexMax must be a positive scalar (CAPMF: 10).")
  }
  if (estimator == "beta" && !requireNamespace("betareg", quietly = TRUE)) {
    stop("estimatePolicyStringencyModel: estimator 'beta' requires the 'betareg' package. ",
         "Please install it (it is an optional Suggests dependency).")
  }

  # --- 1. Prepare panel (skipped when `prepared`: response already transformed) ---
  if (isTRUE(prepared)) {
    df <- as.data.frame(data)
    ignoreCache <- TRUE
    updateIndex <- FALSE
  } else {
    df <- preparePanelData(
      data = data,
      sector = sector,
      actorPowerDrivers = actorPowerDrivers,
      actorPowerIndex = actorPowerIndex,
      instQualityDrivers = instQualityDrivers,
      controlDrivers = controlDrivers,
      regionMappingFixedEffects = if (isTRUE(useMundlak)) NULL else regionMappingFixedEffects,
      lag = lag,
      useMundlak = useMundlak,
      gdpGovInteraction = gdpGovInteraction,
      outcomeVar = outcomeVar
    )
  }
  .dscale <- attr(df, "driverScaling")

  # --- 2. Outcome validation, boundary shares, response transform -------------
  # No positive-price subset: zero stringency is genuine data (no hurdle).
  outcomeNatural <- NULL
  boundaryShares <- NULL
  squeeze <- list(type = "none", n = NA_integer_)
  if (!isTRUE(prepared)) {
    y <- df$ecp
    if (any(y < -1e-8 | y > indexMax + 1e-8, na.rm = TRUE)) {
      stop("estimatePolicyStringencyModel: outcome outside [0, ", indexMax, "] — ",
           "check outcomeVar/indexMax (observed range ",
           paste(round(range(y, na.rm = TRUE), 3), collapse = " to "), ").")
    }
    outcomeNatural <- stats::setNames(y, rownames(df))
    tol <- 1e-8
    boundaryShares <- c(
      atZero = mean(y <= tol, na.rm = TRUE),
      atMax  = mean(y >= indexMax - tol, na.rm = TRUE)
    )
    nSV <- sum(is.finite(y))
    transformResponse <- function(v) {
      p <- pmin(pmax(v / indexMax, 0), 1)
      switch(estimator,
        satP = stats::qlogis(.psmSqueeze(p, nSV)),
        beta = .psmSqueeze(p, nSV),
        fractional = p,
        levels = v
      )
    }
    df$ecp <- transformResponse(df$ecp)
    if (estimator %in% c("satP", "beta")) {
      squeeze <- list(type = "smithson-verkuilen", n = nSV)
      if (isTRUE(verbose)) {
        message("  [psm] ", estimator, ": response -> ",
                if (estimator == "satP") "logit(" else "", "squeeze(y/", indexMax, ")",
                if (estimator == "satP") ")" else "",
                " (Smithson-Verkuilen n = ", nSV, ")")
      }
    }
    if (isTRUE(includeLaggedPS)) {
      df$lagged_ecp <- transformResponse(df$lagged_ecp)
    }
  }
  if (isTRUE(includeLaggedPS)) {
    controlDrivers <- c(controlDrivers, "lagged_ecp")
  }

  # --- 3. Formula --------------------------------------------------------------
  fml <- buildModelFormula(
    depVar = "ecp",
    actorPowerDrivers = actorPowerDrivers,
    actorPowerIndex = actorPowerIndex,
    instQualityDrivers = instQualityDrivers,
    controlDrivers = controlDrivers,
    regionMappingFixedEffects = if (isTRUE(useMundlak)) NULL else regionMappingFixedEffects,
    timeTrend = timeTrend,
    logisticTimeTrend = logisticTimeTrend,
    interactRegionFE = if (isTRUE(useMundlak)) FALSE else interactRegionFE,
    useMundlak = useMundlak,
    gdpGovInteraction = gdpGovInteraction
  )

  # --- 4. Cache (satP engine only — the only estimator entering selection) -----
  cacheExtra <- paste0("psm-", estimator, "+max", indexMax)
  usesCache <- identical(estimator, "satP") && !is.null(modelDir)
  if (usesCache && !isTRUE(ignoreCache)) {
    ids <- computeModelId(fml, df, extra = cacheExtra)
    cachedPath <- file.path(modelDir, "models", paste0(ids[["id"]], ".rds"))
    if (file.exists(cachedPath)) {
      cached_result <- tryCatch({
        cached <- loadPFMModel(ids[["id"]], modelDir)
        cr <- list(
          model = .rehydrateFitForConsumers(cached$model, df, fml, "ecp"),
          coeftest = cached$coeftest,
          vcov = cached$vcov,
          sector = sector,
          family = "gaussian (satP)",
          formula = fml,
          data = df,
          estimator = estimator,
          indexMax = indexMax,
          outcomeVar = outcomeVar,
          outcomeNatural = outcomeNatural,
          boundaryShares = boundaryShares,
          squeeze = squeeze,
          driverScaling = cached$transforms$driverScaling %||% .dscale
        )
        cached_vif <- cached$diagnostics$vif
        if (!is.null(cached_vif) && length(cached_vif$values) > 0) {
          cr$vifRaw <- cached_vif$values
          cr$maxVIF <- cached_vif$maxVIF
          cr$highVIF <- cached_vif$highVIF
          cr$vifFlagged <- cached_vif$flagged %||% character(0)
        }
        cr$predictiveDiagnostics <- tryCatch(
          computePredictiveDiagnostics(cr$model, stage = "stringency"),
          error = function(e) NULL
        )
        cr$ameIndex <- tryCatch(
          .psmAMEIndex(cr$model, cr$vcov, estimator, indexMax, df, fml),
          error = function(e) NULL
        )
        cr
      }, error = function(e) {
        warning("estimatePolicyStringencyModel: cached fit '", ids[["id"]],
                "' unreadable (", conditionMessage(e), "); refitting.", call. = FALSE)
        NULL
      })
      if (!is.null(cached_result)) {
        if (isTRUE(verbose)) {
          message("  [cache hit] Loading policy-stringency model ", ids[["id"]], " from disk.")
        }
        return(cached_result)
      }
    }
  }

  # --- 5. Fit -------------------------------------------------------------------
  if (isTRUE(verbose)) {
    message("  [running] Estimating policy-stringency model (", sector, ", ", estimator, ")...")
  }
  if (estimator == "beta") {
    fit <- tryCatch(
      betareg::betareg(fml, data = df, control = betareg::betareg.control(maxit = maxit)),
      error = function(e) stop("estimatePolicyStringencyModel: betareg failed: ", conditionMessage(e))
    )
    familyLabel <- "beta(logit)"
    convergedFlag <- isTRUE(fit$converged)
  } else {
    glmFamily <- switch(estimator,
      satP = gaussian(link = "identity"),
      levels = gaussian(link = "identity"),
      fractional = quasibinomial(link = "logit")
    )
    fit <- stats::glm(fml, data = df, family = glmFamily, control = list(maxit = maxit))
    familyLabel <- switch(estimator,
      satP = "gaussian (satP)",
      levels = "gaussian (levels)",
      fractional = "quasibinomial(logit)"
    )
    convergedFlag <- isTRUE(fit$converged)
    if (!convergedFlag && isTRUE(verbose)) {
      message("  [psm] ", estimator, " GLM did not converge within maxit = ", maxit, ".")
    }
  }
  # Clustered (by region) sandwich SEs for every suite member. betareg supports the
  # sandwich estfun/bread interface; fall back to the model vcov if clustering fails.
  vcovClust <- tryCatch(
    sandwich::vcovCL(fit, cluster = df$region, type = "HC1"),
    error = function(e) {
      warning("estimatePolicyStringencyModel: clustered vcov failed (",
              conditionMessage(e), "); using model vcov.", call. = FALSE)
      stats::vcov(fit)
    }
  )
  robustTest <- lmtest::coeftest(fit, vcov. = vcovClust)

  result <- list(
    model = fit,
    coeftest = robustTest,
    vcov = vcovClust,
    sector = sector,
    family = familyLabel,
    formula = fml,
    data = df,
    estimator = estimator,
    indexMax = indexMax,
    outcomeVar = outcomeVar,
    outcomeNatural = outcomeNatural,
    boundaryShares = boundaryShares,
    squeeze = squeeze,
    driverScaling = .dscale,
    converged = convergedFlag
  )

  # --- 6. Persist (satP engine only) --------------------------------------------
  if (usesCache && !isTRUE(prepared)) {
    pfmModel <- buildPFMModel(
      fit = result,
      training_data = df,
      sector = sector,
      stage = "policyStringency",
      family = familyLabel,
      useFirth = FALSE,
      label = label,
      driverScaling = .dscale,
      idExtra = cacheExtra,
      prepSpec = list(
        actorPowerDrivers = actorPowerDrivers,
        actorPowerIndex = actorPowerIndex,
        instQualityDrivers = instQualityDrivers,
        controlDrivers = controlDrivers,
        regionMappingFixedEffects = if (isTRUE(useMundlak)) NULL else regionMappingFixedEffects,
        lag = lag,
        useMundlak = useMundlak,
        gdpGovInteraction = gdpGovInteraction,
        panelTransform = "levels",
        outcomeVar = outcomeVar,
        estimator = estimator,
        indexMax = indexMax,
        squeezeN = squeeze$n
      )
    )
    savePFMModel(pfmModel, modelDir, updateIndex = updateIndex)
    if (isTRUE(verbose)) {
      message("  [saved] Policy-stringency model ", pfmModel$id, " -> ", modelDir)
    }
  }

  # VIF — design-matrix based, estimator-independent
  vifRes <- tryCatch(computeVIF(data = df, formula = fml), error = function(e) NULL)
  if (!is.null(vifRes)) {
    result$vifRaw <- vifRes$values
    result$maxVIF <- vifRes$maxVIF
    result$highVIF <- vifRes$highVIF
    result$vifFlagged <- vifRes$flagged %||% character(0)
  }

  result$predictiveDiagnostics <- tryCatch(
    computePredictiveDiagnostics(result$model, stage = "stringency"),
    error = function(e) NULL
  )

  # AMEs on the natural 0-indexMax scale — the cross-estimator comparable quantity
  result$ameIndex <- tryCatch(
    .psmAMEIndex(fit, vcovClust, estimator, indexMax, df, fml),
    error = function(e) NULL
  )

  result
}

# Smithson-Verkuilen (2006) boundary squeeze: maps [0,1] into the open interval,
# p' = (p*(n-1) + 0.5)/n. Required by logit/beta transforms that cannot take 0/1.
.psmSqueeze <- function(p, n) {
  (p * (n - 1) + 0.5) / n
}

# Natural-scale fitted values E[y] in 0-indexMax units (no squeeze un-doing: the
# squeeze is an estimation device; the structural mean is indexMax * logit^{-1}(eta)).
# Names (model-frame row labels) are preserved so callers can align with the
# original panel rows even when the fit dropped NA-driver rows.
.psmNaturalFitted <- function(fit, estimator, indexMax) {
  fv <- stats::fitted(fit)
  switch(estimator,
    satP = indexMax * stats::plogis(fv), # identity link: fitted = eta
    fractional = indexMax * fv,          # fitted = mu (share)
    beta = indexMax * fv,                # fitted = mu (share)
    levels = fv
  )
}

# Average marginal effects on the natural index scale, with delta-method SEs from
# the clustered vcov. For logit-mean estimators (satP/fractional/beta):
#   AME_k = indexMax * mean_i[ dlogis(eta_i) ] * beta_k evaluated per observation:
#   AME_k = indexMax * (1/N) sum_i dlogis(eta_i) * beta_k
# gradient wrt beta_j: indexMax * [ (1/N) sum_i l''(eta_i) x_ij * beta_k + 1(j=k) (1/N) sum_i dlogis(eta_i) ]
# with l''(eta) = dlogis(eta) * (1 - 2*plogis(eta)). For levels, AME_k = beta_k.
.psmAMEIndex <- function(fit, vcovMat, estimator, indexMax, df, fml) {
  mm <- stats::model.matrix(fml, data = df)
  beta <- if (inherits(fit, "betareg")) stats::coef(fit, model = "mean") else stats::coef(fit)
  beta <- beta[colnames(mm)]
  keep <- setdiff(colnames(mm), "(Intercept)")
  keep <- keep[!grepl("^regionFE", keep)]
  vc <- vcovMat[colnames(mm), colnames(mm), drop = FALSE]

  if (estimator == "levels") {
    est <- beta[keep]
    se <- sqrt(diag(vc)[keep])
  } else {
    eta <- as.numeric(mm %*% beta)
    w <- stats::dlogis(eta)
    wPrime <- w * (1 - 2 * stats::plogis(eta))
    meanW <- mean(w)
    est <- indexMax * meanW * beta[keep]
    # gradient matrix: rows = target term k, cols = coefficient j
    gradCommon <- indexMax * (crossprod(wPrime, mm) / nrow(mm)) # 1 x J: mean(l'' * x_j)
    se <- vapply(keep, function(k) {
      g <- as.numeric(gradCommon) * beta[[k]]
      names(g) <- colnames(mm)
      g[[k]] <- g[[k]] + indexMax * meanW
      sqrt(max(t(g) %*% vc %*% g, 0))
    }, numeric(1))
  }
  z <- est / se
  data.frame(
    term = keep,
    ame = as.numeric(est),
    se = as.numeric(se),
    z = as.numeric(z),
    p = 2 * stats::pnorm(-abs(as.numeric(z))),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}
# nolint end
