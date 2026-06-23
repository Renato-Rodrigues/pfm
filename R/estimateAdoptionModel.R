# nolint start
#' @title estimateAdoptionModel
#' @description Estimates the Stage 1 Adoption Probability (Logit Model) of
#' the two-stage Hurdle model for carbon pricing. For each sector, a logistic
#' regression estimates whether a region adopts a carbon price in a given year.
#'
#' Robustness safeguards include a linear time trend, region fixed effects
#' (at a configurable spatial resolution), and clustered standard errors at
#' the country/region level (54 panel units) to account for within-country
#' serial correlation. For Firth models a manual sandwich estimator is used
#' since \code{logistf} does not support \code{vcovCL} directly.
#'
#' @param data A \code{magpie} object or a \code{data.frame}. If a \code{data.frame}
#'   is provided, it is assumed to be already prepared and the internal
#'   call to \code{preparePanelData} is skipped.
#' @param sector Character. The sector to estimate: \code{"Bulk"} or
#'   \code{"Diffuse"}. Default: \code{"Bulk"}.
#' @param actorPowerDrivers Character vector of individual Actor Power driver
#'   names. If \code{actorPowerIndex} is in this list, it takes priority as
#'   the sole Actor Power main effect. Otherwise, all drivers in this list
#'   are included as individual main effects.
#' @param actorPowerIndex Character or NULL. Name of the Actor Power Index
#'   variable. If provided, it is used for interaction terms. It only acts
#'   as a main effect if its name is also in \code{actorPowerDrivers}.
#'   \code{NULL} to exclude the interaction.
#' @param instQualityDrivers Character vector of Institutional Quality driver names.
#' @param controlDrivers Character vector of control variable names.
#' @param regionMappingFixedEffects Character or NULL. Region mapping file used to define the
#'   fixed-effects grouping. If \code{NULL}, region fixed effects are omitted.
#'   Default: \code{"regionmappingH12.csv"}.
#' @param timeTrend Logical. If \code{TRUE} (default), adds a linear time trend
#'   to the model.
#' @param useFirth Logical. If \code{TRUE} (default), uses Firth's penalized
#'   likelihood logistic regression (\code{logistf}) to handle perfect
#'   separation. Recommended for the adoption stage.
#' @param lag Integer. Time lag for drivers in years. Default: \code{1}.
#' @param includeLaggedAdoption Logical. If \code{TRUE}, includes the lagged
#'   adoption status (\code{adoption_lagged}) as a predictor. Default: \code{FALSE}.
#' @param modelDir Character or NULL. Directory for saving/loading \code{PFMModel}
#'   files. Defaults to \code{getOption("pfm.modelDir", "output")}. Set to \code{NULL}
#'   to disable persistence (default when the option is not set).
#' @param updateIndex Logical. Forwarded to \code{\link{savePFMModel}}; when \code{FALSE}
#'   the fit is written to disk but the shared \code{index.json} is not touched (parallel
#'   sweep workers pass \code{FALSE}; the master rebuilds the index once — ADR 0019).
#' @param ignoreCache Logical. When \code{TRUE}, skip the cache read and always re-estimate
#'   (forceRefit), overwriting any stale fit on disk. Default \code{FALSE}.
#' @param label Character. Optional human label stored in the saved model manifest.
#' @param verbose Logical. If \code{TRUE} (default), prints progress messages when
#'   loading from cache or estimating.
#' @param compute Named logical vector controlling which expensive post-fit diagnostics are
#'   computed. Recognised names: \code{"ame"} and \code{"predictedProbs"}. Both default
#'   to \code{TRUE}. Cheap diagnostics (group contributions, Theory Score, Theory Fraction)
#'   are always computed. See \code{\link{fitAndDiagnose}} for details.
#' @param sweepVars Character vector or NULL. Variables to sweep in predicted probability
#'   profiles. If NULL (default), theory-group variables are auto-detected.
#' @param ridgeInteractions Logical. If \code{TRUE}, applies Ridge (L2) regularization to
#'   interaction terms (\code{_x_} pattern) via \code{\link{fitRidgeLogit}}. Replaces
#'   Firth's penalized logit. Useful for reducing projection artefacts caused by large
#'   opposing interaction coefficients (e.g. the Brazil 2040 adoption-probability dip).
#'   Default: \code{FALSE} (library default; set \code{ridgeInteractions = TRUE} in
#'   the YAML config to enable for specific models).
#' @param ridgeLambda Numeric or NULL. Ridge penalty \eqn{\lambda}. When \code{NULL}
#'   (default) and \code{ridgeInteractions = TRUE}, \eqn{\lambda} is selected automatically
#'   by 5-fold cross-validation. Pass a numeric value to fix \eqn{\lambda}.
#' @param panelTransform Character. Panel Transform axis (ADR 0005): \code{"levels"}
#'   (default — status logit, current behaviour), \code{"hybridFD"} (Actor Power terms
#'   differenced, Institutional Quality in levels, discrete-time hazard/onset sample),
#'   or \code{"pureFD"} (all drivers differenced, hazard/onset sample). Under any FD
#'   transform the model predicts P(adopt this year | not yet adopted). Incompatible
#'   with \code{useMundlak = TRUE}.
#'
#' @return A list with elements:
#'   \describe{
#'     \item{model}{The fitted model object (either \code{glm} or \code{logistf}).}
#'     \item{coeftest}{Robust coefficient table (clustered SE for GLM, Firth
#'       standard errors for logistf).}
#'     \item{vcov}{The variance-covariance matrix.}
#'     \item{sector}{The sector estimated.}
#'     \item{formula}{The formula used.}
#'     \item{data}{The prepared panel data.frame used for estimation.}
#'     \item{groupContributions}{Named list: mean beta*x per Term Group, Theory Score,
#'       Theory Fraction. Always present.}
#'     \item{theoryScore}{Numeric scalar.}
#'     \item{theoryFrac}{Numeric scalar.}
#'     \item{ame}{Data.frame of Average Marginal Effects, or \code{NULL}.}
#'     \item{predictedProbs}{Named list of Predicted Probability Profile data.frames, or \code{NULL}.}
#'   }
#'
#' @author Renato Rodrigues
#'
#' @importFrom stats glm binomial as.formula
#' @importFrom lmtest coeftest
#' @importFrom sandwich vcovCL
#' @importFrom logistf logistf
#' @importFrom utils head
#'
#' @export
#'
estimateAdoptionModel <- function(
    data,
    sector = "Bulk",
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
    useFirth = TRUE,
    lag = 1,
    includeLaggedAdoption = FALSE,
    interactRegionFE = FALSE,
    modelDir = getOption("pfm.modelDir", "output"),
    updateIndex = TRUE,
    ignoreCache = FALSE,
    label = "",
    verbose = TRUE,
    maxit = 3000,
    compute = c(ame = TRUE, predictedProbs = TRUE),
    sweepVars = NULL,
    ridgeInteractions = FALSE,
    ridgeLambda = NULL,
    useMundlak = FALSE,
    gdpGovInteraction = FALSE,
    fePenaltyFactor = 0.5,
    panelTransform = "levels") {
  panelTransform <- match.arg(panelTransform, c("levels", "hybridFD", "pureFD"))
  if (panelTransform != "levels" && isTRUE(useMundlak)) {
    stop("estimateAdoptionModel: useMundlak is incompatible with panelTransform = '",
         panelTransform, "' (ADR 0005).")
  }
  # ADR 0010 (2026-06-16): the ADOPTION stage uses NO time trend. Empirically the
  # trend was deadweight in-sample (AIC ~20 worse WITH it, theory fraction lower)
  # and its large coefficient (~+28 log-odds) only inflated projections; region
  # fixed effects + drivers already capture the level/time variation. The
  # stringency stage keeps its (small, well-behaved) trend. We force both trend
  # terms off here so every caller (workflow, projection, reports, REMIND) is
  # consistent regardless of the spec's logisticTimeTrend flag.
  if (isTRUE(timeTrend) || isTRUE(logisticTimeTrend)) {
    if (isTRUE(verbose)) message("  [adoption] time trend disabled (ADR 0010).")
  }
  timeTrend <- FALSE
  logisticTimeTrend <- FALSE
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
  # Capture the frozen driver-scaling reference before any row subsetting drops
  # the attribute (ADR 0009: bundled into the saved Fitted Model).
  .dscale <- attr(df, "driverScaling")

  # --- 2. Create binary dependent variable ---
  df$adoption <- as.integer(df$ecp > 0)

  # --- 2a. Panel Transform (ADR 0005): FD drivers + hazard (onset) sample ---
  if (panelTransform != "levels") {
    df <- applyPanelTransform(
      df, panelTransform = panelTransform, stage = "adoption",
      actorPowerDrivers = actorPowerDrivers, actorPowerIndex = actorPowerIndex,
      instQualityDrivers = instQualityDrivers, controlDrivers = controlDrivers,
      verbose = verbose
    )
  }

  if (isTRUE(includeLaggedAdoption)) {
    controlDrivers <- c(controlDrivers, "lagged_adoption")
  }

  # --- 3. Build formula ---
  fml <- buildModelFormula(
    depVar = "adoption",
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

  # --- 3b. Cache check: return saved model if formula + data unchanged ---
  # A truncated/corrupt cached fit (e.g. a parallel worker killed mid-write, ADR 0019) is
  # treated as a cache miss and refit, rather than propagating a load error. ignoreCache
  # (forceRefit) skips the read entirely and re-estimates, overwriting any stale fit.
  if (!is.null(modelDir) && !isTRUE(ignoreCache)) {
    ids <- computeModelId(fml, df)
    cachedPath <- file.path(modelDir, "models", paste0(ids[["id"]], ".rds"))
    if (file.exists(cachedPath)) {
      cachedResult <- tryCatch({
        cached <- loadPFMModel(ids[["id"]], modelDir)
        result <- list(
          model    = .rehydrateFitForConsumers(cached$model, df, fml, "adoption"),
          coeftest = cached$coeftest,
          vcov     = cached$vcov,
          sector   = sector,
          formula  = fml,
          data     = df
        )
        .appendAdoptionDiagnostics(
          result, df, compute, sweepVars,
          actorPowerDrivers, actorPowerIndex, instQualityDrivers, controlDrivers
        )
      }, error = function(e) {
        warning("estimateAdoptionModel: cached fit '", ids[["id"]],
                "' unreadable (", conditionMessage(e), "); refitting.", call. = FALSE)
        NULL
      })
      if (!is.null(cachedResult)) {
        if (isTRUE(verbose)) {
          message("  [cache hit] Loading adoption model ", ids[["id"]], " from disk.")
        }
        return(cachedResult)
      }
    }
  }

  # --- 4. Estimate model ---
  if (isTRUE(verbose)) {
    message("  [running] Estimating adoption model (", sector, " sector)...")
  }

  if (isTRUE(ridgeInteractions)) {
    # ── Ridge logistic regression: L2 penalty on interaction terms only ──────
    # ridgeLambda = NULL triggers automatic 5-fold CV lambda selection.
    if (isTRUE(verbose)) {
      lambda_msg <- if (is.null(ridgeLambda)) "lambda via 5-fold CV" else paste0("lambda = ", ridgeLambda)
      message("    [ridge] Applying Ridge regularization on interaction terms (", lambda_msg, ")...")
    }
    ridgeRes <- fitRidgeLogit(fml, df, depVar = "adoption",
                              ridgeLambda        = ridgeLambda,
                              clusterVar         = df$region,
                              maxit              = maxit,
                              instQualityDrivers = instQualityDrivers,
                              fePenaltyFactor    = fePenaltyFactor)
    if (is.null(ridgeRes)) {
      stop("Ridge logistic regression failed for sector '", sector, "'.")
    }
    fit <- ridgeRes$model
    # When no conflict was detected, fitRidgeLogit returns coeftest/vcov = NULL.
    # Fall back to clustered sandwich SE on the standard GLM in that case.
    if (is.null(ridgeRes$vcov) || is.null(ridgeRes$coeftest)) {
      vcovMat    <- tryCatch(
        sandwich::vcovCL(fit, cluster = df$region, type = "HC1"),
        error = function(e) vcov(fit)
      )
      robustTest <- lmtest::coeftest(fit, vcov. = vcovMat)
    } else {
      vcovMat    <- ridgeRes$vcov
      robustTest <- ridgeRes$coeftest
    }
    if (isTRUE(verbose)) {
      message("    [ridge] lambda = ", round(ridgeRes$ridgeLambda, 5),
              ", penalized terms = ", ridgeRes$nPenalizedTerms,
              " (IQ conflict: ", ridgeRes$iqConflict,
              ", interaction conflict: ", ridgeRes$interactionConflict, ")")
    }

  } else if (isTRUE(useFirth)) {
    # ── Firth's penalized likelihood logistic regression ──────────────────────
    fit <- logistf::logistf(fml, data = df, control = logistf::logistf.control(maxit = maxit, maxstep = 5))
    fit$converged <- !is.null(fit$coefficients) && !any(is.na(fit$coefficients))

    # Clustered sandwich SE (HC1) — logistf doesn't support vcovCL directly.
    mm      <- model.matrix(fit$formula, data = fit$model)
    cluster <- df$region[seq_len(nrow(mm))]
    y       <- fit$y
    # logistf stores fitted probabilities in $predict, not $fitted.values
    p       <- if (inherits(fit, "logistf")) fit$predict else fit$fitted.values
    scores  <- sweep(mm, 1, y - p, "*")
    G       <- length(unique(cluster))
    N       <- nrow(mm)
    k       <- ncol(mm)
    B_meat  <- Reduce("+", lapply(unique(cluster), function(cl) {
      sc <- colSums(scores[cluster == cl, , drop = FALSE])
      tcrossprod(sc)
    }))
    correction <- (G / (G - 1)) * ((N - 1) / (N - k))
    vcovMat    <- correction * fit$var %*% B_meat %*% fit$var
    colnames(vcovMat) <- names(fit$coefficients)
    rownames(vcovMat) <- names(fit$coefficients)

    robustTest <- cbind(
      fit$coefficients,
      sqrt(diag(vcovMat)),
      fit$coefficients / sqrt(diag(vcovMat)),
      2 * stats::pnorm(-abs(fit$coefficients / sqrt(diag(vcovMat))))
    )
    colnames(robustTest) <- c("Estimate", "Std. Error", "z value", "Pr(>|z|)")
    rownames(robustTest) <- names(fit$coefficients)

  } else {
    # ── Standard logistic regression with clustered SE ────────────────────────
    fit        <- glm(fml, data = df, family = binomial(link = "logit"), control = list(maxit = maxit))
    vcovMat    <- sandwich::vcovCL(fit, cluster = df$region, type = "HC1")
    robustTest <- lmtest::coeftest(fit, vcov. = vcovMat)
  }

  result <- list(
    model    = fit,
    coeftest = robustTest,
    vcov     = vcovMat,
    sector   = sector,
    formula  = fml,
    data     = df
  )

  # --- 5. Save PFMModel if modelDir is configured ---
  if (!is.null(modelDir)) {
    pfmModel <- buildPFMModel(
      fit           = result,
      training_data = df,
      sector        = sector,
      stage         = "adoption",
      family        = "logistf",
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
        panelTransform            = panelTransform
      )
    )
    savePFMModel(pfmModel, modelDir, updateIndex = updateIndex)
    if (isTRUE(verbose)) {
      message("  [saved] Adoption model ", pfmModel$id, " -> ", modelDir)
    }
  }

  result <- .appendAdoptionDiagnostics(
    result, df, compute, sweepVars,
    actorPowerDrivers, actorPowerIndex, instQualityDrivers, controlDrivers
  )
  result
}

# Internal helper: appends group contributions, AMEs, and predicted probability
# profiles to an estimateAdoptionModel result list.
.appendAdoptionDiagnostics <- function(result, df, compute,
                                        sweepVars,
                                        actorPowerDrivers, actorPowerIndex,
                                        instQualityDrivers, controlDrivers) {
  gc <- computeGroupContributions(
    result, df = df,
    actorPowerDrivers = actorPowerDrivers, actorPowerIndex = actorPowerIndex,
    instQualityDrivers = instQualityDrivers, controlDrivers = controlDrivers
  )
  result$groupContributions <- gc
  result$theoryScore <- if (!is.null(gc)) gc[["Theory Score"]] else NA_real_
  result$theoryFrac  <- if (!is.null(gc)) gc[["Theory Frac."]] else NA_real_

  result$ame <- if (isTRUE(compute[["ame"]])) {
    computeAME(
      result,
      actorPowerDrivers = actorPowerDrivers, actorPowerIndex = actorPowerIndex,
      instQualityDrivers = instQualityDrivers, controlDrivers = controlDrivers
    )
  } else NULL

  result$predictedProbs <- if (isTRUE(compute[["predictedProbs"]])) {
    computePredictedProbabilities(
      result, df = df, sweepVars = sweepVars,
      actorPowerDrivers = actorPowerDrivers, actorPowerIndex = actorPowerIndex,
      instQualityDrivers = instQualityDrivers, controlDrivers = controlDrivers
    )
  } else NULL

  # VIF — always computed; needed by publication report and VIF diagnostic tabs
  vifRes <- tryCatch(computeVIF(data = df, formula = result$formula),
                     error = function(e) NULL)
  if (!is.null(vifRes)) {
    result$vifRaw    <- vifRes$values
    result$maxVIF    <- vifRes$maxVIF
    result$highVIF   <- vifRes$highVIF
    result$vifFlagged <- vifRes$flagged %||% character(0)
  }

  # Predictive Diagnostics — cheap, report-only (no selection role)
  result$predictiveDiagnostics <- tryCatch(
    computePredictiveDiagnostics(result$model, stage = "adoption"),
    error = function(e) NULL
  )

  result
}
# nolint end
