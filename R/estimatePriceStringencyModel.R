#' @title estimatePriceStringencyModel
#' @description Estimates the Stage 2 Price Stringency (GLM Model) of the
#' two-stage Hurdle model for carbon pricing. For each sector, a GLM with a
#' log link estimates the carbon price level conditional on adoption (ECP > 0).
#'
#' Robustness safeguards include a linear time trend, region fixed effects
#' (at a configurable spatial resolution), and clustered standard errors by
#' region.
#'
#' @param data A \code{magpie} object or a \code{data.frame}. If a \code{data.frame}
#'   is provided, it is assumed to be already prepared and the internal
#'   call to \code{preparePanelData} is skipped.
#' @param sector Character. The sector to estimate: \code{"Bulk"} or
#'   \code{"Diffuse"}. Default: \code{"Bulk"}.
#' @param family Character. GLM family to use: \code{"Gamma"} or
#'   \code{"gaussian"}. Both use a log link.
#'   \describe{
#'     \item{Gamma}{Natural for positive, right-skewed data where variance
#'       scales with the mean. Recommended for carbon prices.}
#'     \item{gaussian}{Assumes constant variance. More familiar but less
#'       suited for skewed, heteroskedastic price data.}
#'   }
#'   Default: \code{"Gamma"}.
#' @param actorPowerDrivers Character vector of individual Actor Power driver
#'   names. If \code{actorPowerIndex} is in this list, it takes priority as
#'   the sole Actor Power main effect. Otherwise, all drivers in this list
#'   are included as individual main effects.
#' @param actorPowerIndex Character or NULL. Name of the Actor Power Index
#'   variable. If provided, it is used for interaction terms. It only acts
#'   as a main effect if its name is also in \code{actorPowerDrivers}.
#'   Set to \code{NULL} to exclude the interaction.
#' @param instQualityDrivers Character vector of Institutional Quality
#'   indicator names.
#' @param controlDrivers Character vector of control variable names.
#' @param regionMappingFixedEffects Character or NULL. Region mapping file for fixed effects.
#'   If \code{NULL}, region fixed effects are omitted.
#'   Default: \code{"regionmappingH12.csv"}.
#' @param timeTrend Logical. If \code{TRUE} (default), adds a linear time trend
#'   to the model.
#' @param logTransform Logical. If \code{TRUE}, the dependent variable is
#'   transformed to \code{log(1 + ECP)}. Default: \code{TRUE}.
#' @param lag Integer. Time lag for drivers in years. Default: \code{1}.
#' @param useFirth Logical. If \code{TRUE}, uses Firth-type bias reduction
#'   (via \code{brglm2::brglmFit}) for the GLM estimation. Default: \code{FALSE}.
#' @param includeLaggedECP Logical. If \code{TRUE}, includes the lagged
#'   carbon price (\code{lagged_ecp}) as a predictor. Default: \code{FALSE}.
#' @param modelDir Character or NULL. Directory for saving/loading \code{PFMModel}
#'   files. Defaults to \code{getOption("pfm.modelDir", NULL)}. Set to \code{NULL}
#'   to disable persistence.
#' @param updateIndex Logical. Forwarded to \code{\link{savePFMModel}}; when \code{FALSE}
#'   the fit is written to disk but the shared \code{index.json} is not touched (parallel
#'   sweep workers pass \code{FALSE}; the master rebuilds the index once — ADR 0019).
#' @param ignoreCache Logical. When \code{TRUE}, skip the cache read and always re-estimate
#'   (forceRefit), overwriting any stale fit on disk. Default \code{FALSE}.
#' @param label Character. Optional human label stored in the saved model manifest.
#' @param verbose Logical. If \code{TRUE} (default), prints progress messages when
#'   loading from cache or estimating.
#' @param ridgeInteractions Logical. If \code{TRUE}, applies Ridge (L2) regularization
#'   to interaction terms (\code{_x_} pattern) via \code{\link{fitRidgeStringency}}.
#'   Uses a Gaussian OLS Ridge approximation via \code{glmnet}, which is most accurate
#'   when \code{logTransform = TRUE} (the default). Default: \code{FALSE}.
#' @param ridgeLambda Numeric or NULL. Ridge penalty \eqn{\lambda}. When \code{NULL}
#'   (default) and \code{ridgeInteractions = TRUE}, selected automatically by 5-fold
#'   cross-validation. Pass a numeric value to fix \eqn{\lambda}.
#' @param nickellCorrection Logical. If \code{TRUE} and the spec includes the
#'   lagged carbon price (\code{includeLaggedECP}) together with region fixed
#'   effects, applies a split-panel jackknife (Dhaene-Jochmans) bias correction to
#'   the coefficients to counter the dynamic-panel / Nickell bias. Falls back to
#'   the uncorrected fit (with a message) if the half-panels are not estimable.
#'   Standard-errors are kept from the clustered full-fit vcov. Default
#'   \code{FALSE}. See \code{\link{splitPanelJackknife}}.
#' @param panelTransform Character. Panel Transform axis (ADR 0005): \code{"levels"}
#'   (default — ECP level, current behaviour), \code{"hybridFD"} (Actor Power terms
#'   differenced, Institutional Quality in levels), or \code{"pureFD"} (all drivers
#'   differenced). Under any FD transform the dependent variable becomes the
#'   within-spell change (\eqn{\Delta\log(1+ECP)} when \code{logTransform = TRUE}),
#'   the GLM family is forced to \code{gaussian(link = "identity")} (differences can
#'   be negative), and region fixed effects are suppressed (differenced out by
#'   construction). Incompatible with \code{useMundlak = TRUE} and
#'   \code{ridgeInteractions = TRUE}.
#'
#' @return A list with elements:
#'   \describe{
#'     \item{model}{The fitted \code{glm} object.}
#'     \item{coeftest}{Robust coefficient table with clustered SE.}
#'     \item{vcov}{The clustered variance-covariance matrix.}
#'     \item{sector}{The sector estimated.}
#'     \item{family}{The GLM family used.}
#'     \item{formula}{The formula used.}
#'   }
#'
#' @author Renato Rodrigues
#'
#' @importFrom stats glm Gamma gaussian as.formula
#' @importFrom lmtest coeftest
#' @importFrom sandwich vcovCL
#'
#' @export
#'
estimatePriceStringencyModel <- function(
    data,
    sector = "Bulk",
    family = "Gamma",
    actorPowerDrivers = c(
      "VRE share", "Electrification",
      "Coal primary energy share", "Oil/Gas primary energy share",
      "Fossil share in Industry"
    ),
    actorPowerIndex = "Actor Power Index",
    instQualityDrivers = c(
      "Government Effectiveness (WGI)", "Control of Corruption (WGI)",
      "Voice and Accountability (WGI)", "Political Stability (WGI)", "Regulatory Quality (WGI)", "Rule of Law (WGI)"
    ),
    controlDrivers = c(
      "Population", "GDP per Capita", "Land Area",
      "Urban Population Share",
      "Gini Income Inequality Coefficient",
      "Gender Inequality Index", "Energy Intensity"
    ),
    regionMappingFixedEffects = "regionmappingH12.csv",
    timeTrend = TRUE,
    logisticTimeTrend = FALSE,
    logTransform = TRUE,
    lag = 1,
    useFirth = FALSE,
    includeLaggedECP = FALSE,
    interactRegionFE = FALSE,
    modelDir = getOption("pfm.modelDir", NULL),
    updateIndex = TRUE,
    ignoreCache = FALSE,
    label = "",
    verbose = TRUE,
    maxit = 3000,
    ridgeInteractions = FALSE,
    ridgeLambda = NULL,
    useMundlak = FALSE,
    gdpGovInteraction = FALSE,
    fePenaltyFactor = 0.5,
    panelTransform = "levels",
    nickellCorrection = FALSE) {
  panelTransform <- match.arg(panelTransform, c("levels", "hybridFD", "pureFD"))

  # --- Resolve the effective GLM family/link BEFORE the cache check, so the
  # model-store key reflects it (see computeModelId `extra`). Option 1 (2026-06-14):
  # when the outcome is already log-transformed (logTransform = TRUE) OR
  # differenced (FD), use gaussian(identity) — a single, sane back-transform
  # (ECP = expm1(eta)) rather than a log link on a logged outcome, which
  # double-exponentiates and explodes under projection. Gamma/gaussian log links
  # remain available only for logTransform = FALSE (modelling raw ECP).
  usesIdentity <- (panelTransform != "levels") || isTRUE(logTransform)
  if (usesIdentity && family != "gaussian" && isTRUE(verbose)) {
    message("  [stringency] using gaussian(identity) on the log/differenced outcome ",
            "instead of ", family, "(log) to avoid a double-log that explodes under ",
            "projection (Option 1).")
  }
  if (usesIdentity) family <- "gaussian"

  if (panelTransform != "levels") {
    if (isTRUE(useMundlak)) {
      stop("estimatePriceStringencyModel: useMundlak is incompatible with panelTransform = '",
           panelTransform, "' (ADR 0005).")
    }
    if (isTRUE(ridgeInteractions)) {
      stop("estimatePriceStringencyModel: ridgeInteractions is not supported with ",
           "panelTransform = '", panelTransform, "' (fitRidgeStringency assumes a log link).")
    }
    if (!is.null(regionMappingFixedEffects)) {
      if (isTRUE(verbose)) {
        message("  [", panelTransform, "] Region fixed effects suppressed: ",
                "differenced out by construction (ADR 0005).")
      }
      regionMappingFixedEffects <- NULL
    }
  }
  # --- 1. Prepare data.frame ---
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
    gdpGovInteraction = gdpGovInteraction
  )
  # Capture the frozen driver-scaling reference before the ecp>0 subset drops the
  # attribute (ADR 0009: bundled into the saved Fitted Model).
  .dscale <- attr(df, "driverScaling")

  if (panelTransform == "levels") {
    # --- 2. Subset to positive prices ---
    df <- df[df$ecp > 0, , drop = FALSE]

    # --- 2b. Optional log-transform ---
    if (isTRUE(logTransform)) {
      df$ecp <- log(1 + df$ecp)
      if (isTRUE(verbose)) {
        message("Log-transform applied: ecp -> log(1 + ecp)")
      }
    }
  } else {
    # --- 2. Panel Transform (ADR 0005): within-spell differences ---
    # Handles spell subsetting and the (log) differencing of the outcome itself.
    df <- applyPanelTransform(
      df, panelTransform = panelTransform, stage = "stringency",
      actorPowerDrivers = actorPowerDrivers, actorPowerIndex = actorPowerIndex,
      instQualityDrivers = instQualityDrivers, controlDrivers = controlDrivers,
      logTransform = logTransform, verbose = verbose
    )
  }

  if (nrow(df) < 5) {
    warning(
      "Only ", nrow(df),
      " observations with positive carbon prices for sector '",
      sector, "'. Model may be unreliable."
    )
  }

  if (isTRUE(includeLaggedECP)) {
    # If the dependent variable is log-transformed, we should log-transform the lagged predictor as well
    if (isTRUE(logTransform)) {
      df$lagged_ecp <- log(1 + df$lagged_ecp)
    }
    controlDrivers <- c(controlDrivers, "lagged_ecp")
  }

  # --- 2c. Cache check: return saved model if formula + data unchanged ---
  # (formula built after data prep so we check after steps 1-2b)
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

  # nickellCorrection is an estimation choice not reflected in the formula/data,
  # so fold it into the cache key (alongside family) to avoid reusing an
  # uncorrected fit for a corrected spec.
  cacheExtra <- paste0(family, if (isTRUE(nickellCorrection)) "+nickellSPJ" else "")
  # ignoreCache (forceRefit) skips the cache read and re-estimates, overwriting any stale fit.
  if (!is.null(modelDir) && !isTRUE(ignoreCache)) {
    ids <- computeModelId(fml, df, extra = cacheExtra)
    cachedPath <- file.path(modelDir, "models", paste0(ids[["id"]], ".rds"))
    if (file.exists(cachedPath)) {
      # A truncated/corrupt cached fit is treated as a cache miss and refit (ADR 0019).
      cached_result <- tryCatch({
        cached <- loadPFMModel(ids[["id"]], modelDir)
        cr <- list(
          model    = .rehydrateFitForConsumers(cached$model, df, fml, "ecp"),
          coeftest = cached$coeftest,
          vcov     = cached$vcov,
          sector   = sector,
          family   = family,
          formula  = fml,
          data     = df
        )
        # Restore VIF from model store; fall back to computing from the formula
        cached_vif <- cached$diagnostics$vif
        if (!is.null(cached_vif) && length(cached_vif$values) > 0) {
          cr$vifRaw    <- cached_vif$values
          cr$maxVIF    <- cached_vif$maxVIF
          cr$highVIF   <- cached_vif$highVIF
          cr$vifFlagged <- cached_vif$flagged %||% character(0)
        } else {
          vifRes <- tryCatch(computeVIF(data = df, formula = fml), error = function(e) NULL)
          if (!is.null(vifRes)) {
            cr$vifRaw    <- vifRes$values
            cr$maxVIF    <- vifRes$maxVIF
            cr$highVIF   <- vifRes$highVIF
            cr$vifFlagged <- vifRes$flagged %||% character(0)
          }
        }
        cr$predictiveDiagnostics <- tryCatch(
          computePredictiveDiagnostics(cr$model, stage = "stringency"),
          error = function(e) NULL
        )
        cr
      }, error = function(e) {
        warning("estimatePriceStringencyModel: cached fit '", ids[["id"]],
                "' unreadable (", conditionMessage(e), "); refitting.", call. = FALSE)
        NULL
      })
      if (!is.null(cached_result)) {
        if (isTRUE(verbose)) {
          message("  [cache hit] Loading stringency model ", ids[["id"]], " from disk.")
        }
        return(cached_result)
      }
    }
  }

  # --- 4. Choose GLM family ---
  if (isTRUE(verbose)) {
    message("  [running] Estimating stringency model (", sector, " sector)...")
  }
  # Family/link resolved above (usesIdentity). Identity link for log-transformed
  # or differenced outcomes; log link only for raw-ECP modelling.
  if (usesIdentity) {
    glmFamily <- gaussian(link = "identity")
  } else if (family == "Gamma") {
    glmFamily <- Gamma(link = "log")
  } else if (family == "gaussian") {
    glmFamily <- gaussian(link = "log")
  } else {
    stop("Unsupported family '", family, "'. Use 'Gamma' or 'gaussian'.")
  }

  # --- 5. Estimate GLM ---
  if (isTRUE(ridgeInteractions)) {
    # ── Ridge GLM: L2 penalty on interaction terms only ──────────────────────
    if (isTRUE(verbose)) {
      lambda_msg <- if (is.null(ridgeLambda)) "lambda via 5-fold CV" else paste0("lambda = ", ridgeLambda)
      message("    [ridge] Applying Ridge regularization on interaction terms (", lambda_msg, ")...")
    }
    ridgeRes <- fitRidgeStringency(fml, df, depVar = "ecp",
                                   glmFamily       = glmFamily,
                                   ridgeLambda     = ridgeLambda,
                                   clusterVar      = df$region,
                                   maxit           = maxit,
                                   fePenaltyFactor = fePenaltyFactor)
    if (is.null(ridgeRes)) {
      stop("Ridge stringency regression failed for sector '", sector, "'.")
    }
    fit <- ridgeRes$model
    # When no conflict was detected, fitRidgeStringency returns coeftest/vcov = NULL.
    # Fall back to clustered sandwich SE on the standard GLM in that case.
    if (is.null(ridgeRes$vcov) || is.null(ridgeRes$coeftest)) {
      vcovClust  <- tryCatch(
        sandwich::vcovCL(fit, cluster = df$region, type = "HC1"),
        error = function(e) vcov(fit)
      )
      robustTest <- lmtest::coeftest(fit, vcov. = vcovClust)
    } else {
      vcovClust  <- ridgeRes$vcov
      robustTest <- ridgeRes$coeftest
    }
    if (isTRUE(verbose)) {
      message("    [ridge] lambda = ", round(ridgeRes$ridgeLambda, 5),
              ", penalized terms = ", ridgeRes$nInteractionTerms)
    }

  } else if (isTRUE(useFirth)) {
    # ── Firth-type bias reduction (brglm2) ────────────────────────────────────
    if (!requireNamespace("brglm2", quietly = TRUE)) {
      stop("Package 'brglm2' is required for bias-reduced estimation. Please install it.")
    }
    fit        <- stats::glm(fml, data = df, family = glmFamily,
                             method = brglm2::brglmFit, control = list(maxit = maxit))
    vcovClust  <- sandwich::vcovCL(fit, cluster = df$region, type = "HC1")
    robustTest <- lmtest::coeftest(fit, vcov. = vcovClust)

  } else {
    # ── Standard GLM with optional convergence fallback ───────────────────────
    fit <- stats::glm(fml, data = df, family = glmFamily, control = list(maxit = maxit))
    if (!fit$converged) {
      if (isTRUE(verbose)) message("  [fallback] GLM failed to converge. Attempting robust starting values...")
      dep_var  <- as.character(fml[[2]])
      df_init  <- df
      df_init[[dep_var]] <- log(abs(df_init[[dep_var]]) + 1e-6)
      init_fit <- stats::lm(fml, data = df_init)
      fit2 <- tryCatch(
        stats::glm(fml, data = df, family = glmFamily,
                   control = list(maxit = maxit), start = coef(init_fit)),
        error = function(e) fit, warning = function(w) fit
      )
      if (isTRUE(fit2$converged)) {
        fit <- fit2
        if (isTRUE(verbose)) message("  [fallback] Successfully converged with robust starting values!")
      }
    }
    vcovClust  <- sandwich::vcovCL(fit, cluster = df$region, type = "HC1")
    robustTest <- lmtest::coeftest(fit, vcov. = vcovClust)
  }

  # --- 6. Nickell-bias correction (split-panel jackknife) -----------------------
  # Only meaningful when a lagged dependent variable is combined with region FE
  # (the dynamic-panel / Nickell bias). Estimator-agnostic; overwrites the
  # coefficients used downstream (projection builds the linear predictor from
  # coef). SEs are kept from the full-fit clustered vcov (the SPJ variance is not
  # estimated here), and z/p are recomputed from the corrected estimate.
  nickellApplied <- FALSE
  if (isTRUE(nickellCorrection) && !isTRUE(ridgeInteractions) &&
        "lagged_ecp" %in% all.vars(fml) && !is.null(regionMappingFixedEffects)) {
    spj <- tryCatch(splitPanelJackknife(fml, df, glmFamily, maxit = maxit),
                    error = function(e) NULL)
    if (!is.null(spj)) {
      cn <- names(spj$coefficients)
      fit$coefficients[cn] <- spj$coefficients[cn]
      fit$fitted.values <- tryCatch(as.numeric(stats::predict(fit, type = "response")),
                                    error = function(e) fit$fitted.values)
      inTab <- intersect(cn, rownames(robustTest))
      robustTest[inTab, "Estimate"] <- spj$coefficients[inTab]
      se <- robustTest[inTab, "Std. Error"]
      z  <- robustTest[inTab, "Estimate"] / se
      robustTest[inTab, 3] <- z
      robustTest[inTab, 4] <- 2 * stats::pnorm(-abs(z))
      nickellApplied <- TRUE
      if (isTRUE(verbose)) {
        message("  [nickell] split-panel jackknife applied to ", length(spj$corrected),
                " coefficients (SEs uncorrected).")
      }
    } else if (isTRUE(verbose)) {
      message("  [nickell] SPJ not estimable (sparse half-panels); using uncorrected fit.")
    }
  }

  result <- list(
    model    = fit,
    coeftest = robustTest,
    vcov     = vcovClust,
    sector   = sector,
    family   = family,
    formula  = fml,
    data     = df,
    nickellCorrected = nickellApplied
  )

  # --- 7. Save PFMModel if modelDir is configured ---
  if (!is.null(modelDir)) {
    pfmModel <- buildPFMModel(
      fit           = result,
      training_data = df,
      sector        = sector,
      stage         = "stringency",
      family        = family,
      useFirth      = useFirth,
      label         = label,
      driverScaling = .dscale,
      prepSpec      = list(
        actorPowerDrivers         = actorPowerDrivers,
        actorPowerIndex           = actorPowerIndex,
        instQualityDrivers        = instQualityDrivers,
        controlDrivers            = controlDrivers,
        regionMappingFixedEffects = if (isTRUE(useMundlak)) NULL else regionMappingFixedEffects,
        lag                       = lag,
        useMundlak                = useMundlak,
        gdpGovInteraction         = gdpGovInteraction,
        panelTransform            = panelTransform,
        logTransform              = logTransform
      )
    )
    savePFMModel(pfmModel, modelDir, updateIndex = updateIndex)
    if (isTRUE(verbose)) {
      message("  [saved] Stringency model ", pfmModel$id, " -> ", modelDir)
    }
  }

  # VIF — always computed so reports can access fit$vifRaw directly
  vifRes <- tryCatch(computeVIF(data = df, formula = fml), error = function(e) NULL)
  if (!is.null(vifRes)) {
    result$vifRaw    <- vifRes$values
    result$maxVIF    <- vifRes$maxVIF
    result$highVIF   <- vifRes$highVIF
    result$vifFlagged <- vifRes$flagged %||% character(0)
  }

  # Predictive Diagnostics — cheap, report-only (no selection role)
  result$predictiveDiagnostics <- tryCatch(
    computePredictiveDiagnostics(result$model, stage = "stringency"),
    error = function(e) NULL
  )

  result
}
