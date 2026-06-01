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
#'   files. Defaults to \code{getOption("pfm.modelDir", NULL)}. Set to \code{NULL}
#'   to disable persistence (default when the option is not set).
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
#'   Default: \code{FALSE}.
#' @param ridgeLambda Numeric or NULL. Ridge penalty \eqn{\lambda}. When \code{NULL}
#'   (default) and \code{ridgeInteractions = TRUE}, \eqn{\lambda} is selected automatically
#'   by 5-fold cross-validation. Pass a numeric value to fix \eqn{\lambda}.
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
    useFirth = TRUE,
    lag = 1,
    includeLaggedAdoption = FALSE,
    interactRegionFE = FALSE,
    modelDir = getOption("pfm.modelDir", NULL),
    label = "",
    verbose = TRUE,
    maxit = 3000,
    compute = c(ame = TRUE, predictedProbs = TRUE),
    sweepVars = NULL,
    ridgeInteractions = TRUE,
    ridgeLambda = NULL) {
  # --- 1. Prepare data.frame ---
  df <- preparePanelData(
    data = data,
    sector = sector,
    actorPowerDrivers = actorPowerDrivers,
    actorPowerIndex = actorPowerIndex,
    instQualityDrivers = instQualityDrivers,
    controlDrivers = controlDrivers,
    regionMappingFixedEffects = regionMappingFixedEffects,
    lag = lag
  )

  # --- 2. Create binary dependent variable ---
  df$adoption <- as.integer(df$ecp > 0)

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
    regionMappingFixedEffects = regionMappingFixedEffects,
    timeTrend = timeTrend,
    interactRegionFE = interactRegionFE
  )

  # --- 3b. Cache check: return saved model if formula + data unchanged ---
  if (!is.null(modelDir)) {
    ids <- computeModelId(fml, df)
    cachedPath <- file.path(modelDir, paste0(ids[["id"]], ".rds"))
    if (file.exists(cachedPath)) {
      if (isTRUE(verbose)) {
        message("  [cache hit] Loading adoption model ", ids[["id"]], " from disk.")
      }
      cached <- loadPFMModel(ids[["id"]], modelDir)
      result <- list(
        model    = cached$model,
        coeftest = cached$coeftest,
        vcov     = cached$vcov,
        sector   = sector,
        formula  = fml,
        data     = df
      )
      result <- .appendAdoptionDiagnostics(
        result, df, compute, sweepVars,
        actorPowerDrivers, actorPowerIndex, instQualityDrivers, controlDrivers
      )
      return(result)
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
                              ridgeLambda = ridgeLambda,
                              clusterVar  = df$region,
                              maxit       = maxit)
    if (is.null(ridgeRes)) {
      stop("Ridge logistic regression failed for sector '", sector, "'.")
    }
    fit        <- ridgeRes$model
    vcovMat    <- ridgeRes$vcov
    robustTest <- ridgeRes$coeftest
    if (isTRUE(verbose)) {
      message("    [ridge] lambda = ", round(ridgeRes$ridgeLambda, 5),
              ", penalized terms = ", ridgeRes$nInteractionTerms)
    }

  } else if (isTRUE(useFirth)) {
    # ── Firth's penalized likelihood logistic regression ──────────────────────
    fit <- logistf::logistf(fml, data = df, control = logistf::logistf.control(maxit = maxit, maxstep = 5))
    fit$converged <- !is.null(fit$coefficients) && !any(is.na(fit$coefficients))

    # Clustered sandwich SE (HC1) — logistf doesn't support vcovCL directly.
    mm      <- model.matrix(fit$formula, data = fit$model)
    cluster <- df$region[seq_len(nrow(mm))]
    y       <- fit$y
    p       <- fit$fitted.values
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
      label         = label
    )
    savePFMModel(pfmModel, modelDir)
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

  result
}
