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
#'   \item{satP-re}{Partial-pooling cross-check (R4, 2026-07-06): the satP response
#'     with random region intercepts (\code{lme4::lmer}, ML) replacing the region-FE
#'     dummies — the shrinkage answer to the reference-FE-inheritance problem for
#'     regions with thin support. Model-based (non-clustered) fixed-effect SEs: the
#'     random intercept absorbs the within-region correlation the sandwich would
#'     target; stated, not hidden. Suite exhibit only — never cached, never projected,
#'     no AMEs. Optional dependency \code{lme4}.}
#' }
#' Estimators are compared ONLY on signs/AMEs and scale-free out-of-sample metrics
#' (see \code{\link{computeEstimatorAgreement}}), never on information criteria.
#'
#' \strong{Dynamics (R5, 2026-07-06).} \code{form = "ecm"} (satP engine only)
#' estimates the error-correction form \eqn{\Delta y*_t = c + \phi y*_{t-1} +
#' \beta' x_{t-1}} on the transformed response — the feasibility-as-speed reading:
#' \code{adjustmentSpeed} (\eqn{-\phi}) is how fast a polity closes the gap to its
#' politically determined equilibrium stringency, and \code{longRun} reports the
#' equilibrium effects \eqn{-\beta/\phi}. Projection integrates the recursion
#' \eqn{y*_t = (1+\phi) y*_{t-1} + c + \beta'x} (see
#' \code{\link{predictPolicyStringency}}). Link-scale AMEs are suppressed for this
#' form (the short-run \eqn{\Delta}-scale AME is not comparable to the suite).
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
#' @param trendMidpoint,trendSteepness Numeric. Shape of the logistic time trend,
#'   forwarded to \code{\link{preparePanelData}} and defaulting to its values.
#'   Exposed here (2026-08-22) so the trend can be swept like any other modelling
#'   choice; they are part of the Fit-Cache key and are stored on the returned fit
#'   as \code{trendParams}, which \code{\link{projectFeasiblePath}} reuses so a
#'   projection can never be built on a different curve from the fit.
#' @param apTransform Character. \code{"linear"} (default) or \code{"saturating"}
#'   — the functional form of the actor-power drivers (ADR 0040). See
#'   \code{\link{preparePanelData}}. The saturating form bounds the extrapolation
#'   of the actor-power slopes (and hence of the AP x IQ interactions) outside
#'   the historical share range; it is swept as an axis by
#'   \code{\link{psmSpecs}}. Part of the fit-cache key, so linear and saturating
#'   fits never collide.
#' @param form Character. \code{"static"} (default) or \code{"ecm"} — the
#'   error-correction dynamics form (satP engine only; see Description). Appended
#'   last for backward compatibility.
#' @param yearFixedEffects Logical. If \code{TRUE}, adds year dummies
#'   (\code{factor(year)}) to the formula — the year-FE robustness rung
#'   (ADR 0038): pair with \code{timeTrend = FALSE, logisticTimeTrend = FALSE} to
#'   show the theory channels survive within-year identification. \strong{Never a
#'   swept/projection option} (year FE cannot be extrapolated); estimator-agreement
#'   rung only. Default \code{FALSE}.
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
    prepared = FALSE,
    form = "static",
    yearFixedEffects = FALSE,
    apTransform = "linear",
    trendMidpoint = formals(preparePanelData)$trendMidpoint,
    trendSteepness = formals(preparePanelData)$trendSteepness) {
  estimator <- match.arg(estimator, c("satP", "fractional", "beta", "levels",
                                      "satP-re", "frontier", "satP-iv"))
  form <- match.arg(form, c("static", "ecm"))
  if (!is.numeric(indexMax) || length(indexMax) != 1 || indexMax <= 0) {
    stop("estimatePolicyStringencyModel: indexMax must be a positive scalar (CAPMF: 10).")
  }
  if (estimator == "beta" && !requireNamespace("betareg", quietly = TRUE)) {
    stop("estimatePolicyStringencyModel: estimator 'beta' requires the 'betareg' package. ",
         "Please install it (it is an optional Suggests dependency).")
  }
  if (estimator == "satP-re" && !requireNamespace("lme4", quietly = TRUE)) {
    stop("estimatePolicyStringencyModel: estimator 'satP-re' requires the 'lme4' package. ",
         "Please install it (it is an optional Suggests dependency).")
  }
  if (estimator == "frontier" && !requireNamespace("frontier", quietly = TRUE)) {
    stop("estimatePolicyStringencyModel: estimator 'frontier' requires the 'frontier' package. ",
         "Please install it (it is an optional Suggests dependency).")
  }
  if (estimator == "satP-iv" && !requireNamespace("AER", quietly = TRUE)) {
    stop("estimatePolicyStringencyModel: estimator 'satP-iv' requires the 'AER' package. ",
         "Please install it (it is an optional Suggests dependency).")
  }
  if (identical(form, "ecm") && !identical(estimator, "satP")) {
    stop("estimatePolicyStringencyModel: form = 'ecm' is defined for the satP engine only ",
         "(got estimator '", estimator, "').")
  }
  if (estimator == "satP-iv" && isTRUE(prepared)) {
    stop("estimatePolicyStringencyModel: estimator 'satP-iv' builds its shift-share ",
         "instrument from the raw panel and cannot run on a prepared data.frame.")
  }
  if (estimator == "satP-iv" &&
        !"Incumbent Power" %in% c(actorPowerIndex, actorPowerDrivers)) {
    stop("estimatePolicyStringencyModel: estimator 'satP-iv' instruments Incumbent Power ",
         "(shift-share: base-year fossil exposure x leave-one-out global VRE diffusion) - ",
         "use the split actor-power form with 'Incumbent Power' in actorPowerIndex.")
  }

  # --- 1. Prepare panel (skipped when `prepared`: response already transformed) ---
  if (isTRUE(prepared)) {
    df <- as.data.frame(data)
    ignoreCache <- TRUE
    updateIndex <- FALSE
  } else {
    # Shift-share instrument (Tier-1 #3, 2026-07-07): base-year Incumbent Power
    # exposure x leave-one-out global VRE-share diffusion. Built on the raw panel
    # (needs base-year values and the cross-region mean), then rides through
    # preparePanelData like any driver (lagged, standardized) via a prep-only
    # control that is NEVER in the structural formula.
    prepControls <- controlDrivers
    if (estimator == "satP-iv") {
      data <- .psmAddShiftShareIV(data, sector)
      prepControls <- c(prepControls, "ShiftShare IV")
    }
    df <- preparePanelData(
      data = data,
      sector = sector,
      actorPowerDrivers = actorPowerDrivers,
      actorPowerIndex = actorPowerIndex,
      instQualityDrivers = instQualityDrivers,
      controlDrivers = prepControls,
      regionMappingFixedEffects = if (isTRUE(useMundlak)) NULL else regionMappingFixedEffects,
      lag = lag,
      useMundlak = useMundlak,
      gdpGovInteraction = gdpGovInteraction,
      outcomeVar = outcomeVar,
      apTransform = apTransform,
      trendMidpoint = trendMidpoint,
      trendSteepness = trendSteepness
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
        `satP-re` = stats::qlogis(.psmSqueeze(p, nSV)),
        frontier = stats::qlogis(.psmSqueeze(p, nSV)),
        `satP-iv` = stats::qlogis(.psmSqueeze(p, nSV)),
        beta = .psmSqueeze(p, nSV),
        fractional = p,
        levels = v
      )
    }
    df$ecp <- transformResponse(df$ecp)
    if (estimator %in% c("satP", "satP-re", "frontier", "satP-iv", "beta")) {
      squeeze <- list(type = "smithson-verkuilen", n = nSV)
      if (isTRUE(verbose)) {
        message("  [psm] ", estimator, ": response -> ",
                if (estimator != "beta") "logit(" else "", "squeeze(y/", indexMax, ")",
                if (estimator != "beta") ")" else "",
                " (Smithson-Verkuilen n = ", nSV, ")")
      }
    }
    if (identical(form, "ecm")) {
      # Error-correction form (R5): response becomes Delta y* and the lagged
      # transformed LEVEL is forced onto the RHS. Rows keep their labels so the
      # level (Delta + lag) is recoverable for the projection seed.
      df$lagged_ecp <- transformResponse(df$lagged_ecp)
      df$ecp <- df$ecp - df$lagged_ecp
      includeLaggedPS <- TRUE
    } else if (isTRUE(includeLaggedPS)) {
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
  if (isTRUE(yearFixedEffects)) {
    # Year-FE rung (ADR 0038): common-year shocks absorbed by dummies instead of
    # the (logistic) time trend; identification is within-year only.
    df$yearFE <- factor(df$year)
    fml <- stats::update(fml, . ~ . + yearFE)
  }

  # --- 4. Cache (satP engine only — the only estimator entering selection) -----
  # The trend shape and the scaling convention both change the design without
  # necessarily changing the formula, so they belong in the cache key: a fit
  # cached before the 2026-08-22 re-parameterization must never be served for a
  # request made under the new one.
  cacheExtra <- paste0("psm-", estimator, "+max", indexMax,
                       if (identical(form, "ecm")) "+ecm" else "",
                       if (identical(apTransform, "saturating")) "+satAP" else "",
                       "+trend", trendMidpoint, "_", trendSteepness, "+scaledTrend")
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
          driverScaling = cached$transforms$driverScaling %||% .dscale,
          form = form,
          trendParams = c(midpoint = trendMidpoint, steepness = trendSteepness)
        )
        if (identical(form, "ecm")) {
          phi <- tryCatch(cached$coeftest["lagged_ecp", 1], error = function(e) NA_real_)
          cr$adjustmentSpeed <- -phi
          cr$halfLife <- if (is.finite(phi) && abs(1 + phi) < 1 && (1 + phi) > 0) {
            log(0.5) / log(1 + phi)
          } else NA_real_
        }
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
        cr$ameIndex <- if (identical(form, "ecm")) NULL else tryCatch(
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
  } else if (estimator == "satP-re") {
    # Partial-pooling rung (R4): random region intercepts replace the FE dummies.
    if (!"regionFE" %in% colnames(df)) {
      stop("estimatePolicyStringencyModel: estimator 'satP-re' needs region groups - ",
           "fit with a regionMappingFixedEffects (useMundlak specs have no regionFE).")
    }
    fmlRe <- stats::update(fml, . ~ . - regionFE + (1 | regionFE))
    fit <- tryCatch(
      lme4::lmer(fmlRe, data = df, REML = FALSE),
      error = function(e) stop("estimatePolicyStringencyModel: lmer failed: ", conditionMessage(e))
    )
    familyLabel <- "gaussian (satP+RE)"
    convergedFlag <- length(fit@optinfo$conv$lme4$messages %||% character(0)) == 0
  } else if (estimator == "frontier") {
    # Stochastic feasibility frontier (Tier-1 #1): y* = frontier(X) + v - u, u >= 0.
    # The frontier is the estimand — the maximum attainable transformed stringency
    # given political-economy fundamentals; u is political slack. MLE needs
    # complete cases explicitly.
    keepVars <- intersect(all.vars(fml), colnames(df))
    df <- df[stats::complete.cases(df[, keepVars, drop = FALSE]), , drop = FALSE]
    # regionFE keeps mapping levels with no estimation rows (out-of-coverage H12
    # regions); glm silently NA-drops their dummies but frontier::sfa refuses
    # ("OLS coefficient NA") — first real-data run, 2026-07-07.
    if ("regionFE" %in% colnames(df) && is.factor(df$regionFE)) {
      df$regionFE <- droplevels(df$regionFE)
    }
    fit <- tryCatch(
      frontier::sfa(fml, data = df),
      error = function(e) stop("estimatePolicyStringencyModel: frontier::sfa failed: ",
                               conditionMessage(e))
    )
    familyLabel <- "normal-halfnormal frontier (satP)"
    convergedFlag <- all(is.finite(stats::coef(fit)))
  } else if (estimator == "satP-iv") {
    # 2SLS with the shift-share instrument: Incumbent Power (and its IQ
    # interactions) endogenous; instruments = z and z x IQ (just identified).
    rhs <- attr(stats::terms(fml), "term.labels")
    endog <- rhs[grepl("^Incumbent\\.Power($|_x_)", rhs)]
    if (length(endog) == 0) {
      stop("estimatePolicyStringencyModel: satP-iv found no Incumbent Power terms in the formula.")
    }
    zCols <- "ShiftShare.IV"
    for (b in make.names(instQualityDrivers)) {
      zc <- paste0("ShiftShare.IV_x_", b)
      df[[zc]] <- df[["ShiftShare.IV"]] * df[[b]]
      zCols <- c(zCols, zc)
    }
    zCols <- zCols[seq_len(length(endog))]  # one instrument per endogenous term
    exog <- setdiff(rhs, endog)
    ivFml <- stats::as.formula(paste(
      "ecp ~", paste(rhs, collapse = " + "),
      "|", paste(c(exog, zCols), collapse = " + ")
    ))
    fit <- tryCatch(
      AER::ivreg(ivFml, data = df),
      error = function(e) stop("estimatePolicyStringencyModel: ivreg failed: ",
                               conditionMessage(e))
    )
    familyLabel <- "2SLS shift-share (satP)"
    convergedFlag <- all(is.finite(stats::coef(fit)))
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
  if (estimator == "satP-re") {
    # Model-based fixed-effect SEs: the random intercept models the within-region
    # correlation the clustered sandwich would otherwise absorb (documented choice).
    vcovClust <- as.matrix(stats::vcov(fit))
    b <- lme4::fixef(fit)
    se <- sqrt(diag(vcovClust))
    z <- b / se
    robustTest <- cbind(Estimate = b, `Std. Error` = se, `z value` = z,
                        `Pr(>|z|)` = 2 * stats::pnorm(-abs(z)))
  } else if (estimator == "frontier") {
    # MLE vcov (no clustered sandwich exists for the SFA likelihood — stated).
    # The table includes sigmaSq and gamma: gamma = share of composed-error
    # variance from the one-sided slack term (gamma ~ 0 => no frontier structure).
    vcovClust <- as.matrix(stats::vcov(fit))
    b <- stats::coef(fit)
    se <- sqrt(pmax(diag(vcovClust), 0))
    z <- b / se
    robustTest <- cbind(Estimate = b, `Std. Error` = se, `z value` = z,
                        `Pr(>|z|)` = 2 * stats::pnorm(-abs(z)))
  } else {
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
  }

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
    converged = convergedFlag,
    form = form,
    # Carried so any downstream design built from this fit reproduces the trend
    # it was estimated with, instead of silently picking up the current default.
    trendParams = c(midpoint = trendMidpoint, steepness = trendSteepness)
  )
  if (estimator == "frontier") {
    # Frontier quantities (Tier-1 #1): per-row feasibility frontier, political
    # slack (Jondrow decomposition) and the principled Implementability ratio.
    result$frontierGamma <- tryCatch(as.numeric(stats::coef(fit)[["gamma"]]),
                                     error = function(e) NA_real_)
    result$frontierLR <- tryCatch(
      as.numeric(2 * (fit$mleLogl - fit$olsLogl)),  # mixed chi-square vs OLS (no frontier)
      error = function(e) NA_real_
    )
    result$frontier <- tryCatch(computeFeasibilityFrontier(result),
                                error = function(e) NULL)
  }
  if (estimator == "satP-iv") {
    result$instrument <- paste(
      "ShiftShare IV = Incumbent Power at the panel base year x leave-one-out",
      "global mean VRE share (lagged + standardized like all drivers)"
    )
    result$ivDiagnostics <- tryCatch({
      d <- summary(fit, diagnostics = TRUE)$diagnostics
      as.data.frame(d)
    }, error = function(e) NULL)
  }
  if (identical(form, "ecm")) {
    phi <- tryCatch(stats::coef(fit)[["lagged_ecp"]], error = function(e) NA_real_)
    # Feasibility-as-speed quantities (R5): -phi is the per-year share of the gap
    # to equilibrium closed; the long-run effect of a driver is -beta/phi.
    result$adjustmentSpeed <- -phi
    result$halfLife <- if (is.finite(phi) && abs(1 + phi) < 1 && (1 + phi) > 0) {
      log(0.5) / log(1 + phi)
    } else NA_real_
    bet <- stats::coef(fit)
    keepLR <- setdiff(names(bet), c("(Intercept)", "lagged_ecp"))
    keepLR <- keepLR[!grepl("^regionFE|^yearFE", keepLR)]
    result$longRun <- if (is.finite(phi) && phi != 0) {
      data.frame(term = keepLR, longRunEffect = as.numeric(-bet[keepLR] / phi),
                 stringsAsFactors = FALSE)
    } else NULL
  }

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
        squeezeN = squeeze$n,
        form = form
      )
    )
    if (identical(form, "ecm")) {
      # The lag-recursion seed must be the last TRANSFORMED LEVEL y* = Delta + lag
      # per region, not the Delta response buildPFMModel extracted from df$ecp (R5).
      lvl <- df$ecp + df$lagged_ecp
      pfmModel$applyState$seed_prices <- tapply(lvl, df$region, function(v) v[length(v)])
    }
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

  # AMEs on the natural 0-indexMax scale — the cross-estimator comparable quantity.
  # Suppressed for the ECM form (Delta-scale AMEs are not suite-comparable; longRun
  # carries the equilibrium effects) and for satP-re (merMod coef() is not a vector;
  # the rung is a sign/SE cross-check only).
  result$ameIndex <- if (identical(form, "ecm") || identical(estimator, "satP-re")) {
    NULL
  } else {
    tryCatch(
      .psmAMEIndex(fit, vcovClust, estimator, indexMax, df, fml),
      error = function(e) NULL
    )
  }

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
    `satP-re` = indexMax * stats::plogis(fv), # lmer fitted incl. random intercepts
    `satP-iv` = indexMax * stats::plogis(fv), # 2SLS second-stage eta
    fractional = indexMax * fv,          # fitted = mu (share)
    beta = indexMax * fv,                # fitted = mu (share)
    levels = fv
    # frontier: deliberately unmatched — fitted() is the FRONTIER, not E[y|x];
    # a natural-scale RMSE against observations would be meaningless.
  )
}

# Shift-share (Bartik) instrument as a magpie variable "ShiftShare IV" (Tier-1 #3):
# z_it = IncumbentPower_i(base year) x leave-one-out global mean VRE share_t.
# Exposure is PRE-DETERMINED (frozen at the first panel year); the shift is a
# global technology-diffusion aggregate no single mid-sized polity drives; the
# leave-one-out mean removes the own-country component. Rides through
# preparePanelData (lagged, standardized) like any driver.
.psmAddShiftShareIV <- function(data, sector) {
  vre <- "VRE share"
  incName <- paste0("Incumbent Power|", sector)
  nms <- magclass::getNames(data)
  if (!vre %in% nms || !incName %in% nms) {
    stop("estimatePolicyStringencyModel: satP-iv needs '", vre, "' and '", incName,
         "' in the panel to build the shift-share instrument.")
  }
  years <- magclass::getYears(data, as.integer = TRUE)
  baseYear <- min(years)
  expo <- as.numeric(data[, baseYear, incName])                # exposure_i (frozen)
  vreArr <- as.array(data[, , vre])[, , 1, drop = FALSE]
  dim(vreArr) <- dim(vreArr)[1:2]
  fin <- is.finite(vreArr)
  tots <- colSums(vreArr * fin, na.rm = TRUE)
  cnts <- colSums(fin)
  z <- vreArr
  for (j in seq_len(ncol(vreArr))) {
    ownFin <- fin[, j]
    loo <- (tots[j] - ifelse(ownFin, vreArr[, j], 0)) / pmax(cnts[j] - as.integer(ownFin), 1)
    z[, j] <- expo * loo
  }
  zM <- magclass::new.magpie(magclass::getItems(data, dim = 1), years,
                             "ShiftShare IV", fill = NA)
  zM[, , 1] <- as.vector(z)
  magclass::mbind(data, zM)
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
  keep <- keep[!grepl("^regionFE|^yearFE", keep)]
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
