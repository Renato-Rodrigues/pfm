#' @title fitAndDiagnose
#' @description Executes the GLM or logistf fit and extracts standard diagnostic information.
#'
#' @param fml Formula. The regression model formula.
#' @param df Data.frame. The dataset to use.
#' @param depVar Character. The dependent variable name.
#' @param stage Character. The model stage: \code{"adoption"} or \code{"stringency"}.
#' @param family Character. The GLM family for stringency.
#' @param useFirth Logical. If \code{TRUE}, applies Firth regression for logistic models.
#' @param nullLoglik Numeric. The log-likelihood of the null model.
#' @param n Integer. The total number of observations.
#' @param maxZThreshold Numeric. Maximum absolute z-value allowed before a model is flagged
#'   for separation. Even when \code{abs(coef) <= 10}, a z-value far exceeding this threshold
#'   (e.g. z = 49 for Coal) indicates near-perfect separation that \code{brglm2} did not resolve.
#'   Default: \code{15}.
#' @param compute Named logical vector controlling which expensive post-fit diagnostics are
#'   computed. Recognised names: \code{"ame"} (Average Marginal Effects) and
#'   \code{"predictedProbs"} (Predicted Probability Profiles). Both default to \code{TRUE}.
#'   Set to \code{c(ame = FALSE, predictedProbs = FALSE)} when calling from within a model
#'   selection loop to avoid per-candidate overhead. Cheap diagnostics (group contributions,
#'   Theory Score, Theory Fraction) are always computed regardless of this flag.
#' @param sweepVars Character vector or NULL. Passed to \code{computePredictedProbabilities}.
#'   If NULL (default), theory-group variables are auto-detected from the formula.
#' @param actorPowerDrivers Character vector or NULL. Passed to term classification helpers.
#' @param actorPowerIndex Character or NULL.
#' @param instQualityDrivers Character vector or NULL.
#' @param controlDrivers Character vector or NULL.
#' @param ridgeInteractions Logical. If \code{TRUE} and \code{stage == "adoption"}, uses
#'   Ridge (L2) regularization on interaction terms (\code{_x_} pattern) via
#'   \code{\link{fitRidgeLogit}} instead of Firth logit. Replaces \code{useFirth}.
#'   Default: \code{FALSE}.
#' @param ridgeLambda Numeric or NULL. Ridge penalty \eqn{\lambda}. When \code{NULL}
#'   inside \code{fitAndDiagnose} (typically a selection loop), defaults to \code{0.01}
#'   to avoid the cost of 5-fold CV on every candidate model. Pass a pre-selected
#'   \eqn{\lambda} (e.g. from \code{estimateAdoptionModel} CV) for consistent penalisation
#'   across the selection path. Ignored when \code{ridgeInteractions = FALSE}.
#'
#' @return A list containing the model fit, robust coefficients, VCOV matrix, and comprehensive
#'   diagnostic metrics (AIC, BIC, pseudo-R2, etc.), plus:
#'   \describe{
#'     \item{\code{maxAbsZ}}{Maximum absolute z-value across all coefficients (excluding intercept).}
#'     \item{\code{highZ}}{Logical. \code{TRUE} if \code{maxAbsZ} exceeds \code{maxZThreshold}.}
#'     \item{\code{groupContributions}}{Named list of mean beta*x per Term Group + Theory Score
#'       + Theory Fraction. Always computed. \code{NULL} on model failure.}
#'     \item{\code{theoryScore}}{Numeric scalar extracted from \code{groupContributions}.}
#'     \item{\code{theoryFrac}}{Numeric scalar extracted from \code{groupContributions}.}
#'     \item{\code{ame}}{Data.frame of Average Marginal Effects. \code{NULL} unless
#'       \code{compute["ame"]} is \code{TRUE} and \code{stage == "adoption"}.}
#'     \item{\code{predictedProbs}}{Named list of Predicted Probability Profile data.frames.
#'       \code{NULL} unless \code{compute["predictedProbs"]} is \code{TRUE} and
#'       \code{stage == "adoption"}.}
#'   }
#'
#' @author Renato Rodrigues
#' @keywords internal
#'
#' @importFrom stats glm binomial Gamma gaussian coef logLik terms
#' @importFrom logistf logistf logistf.control
#' @importFrom sandwich vcovCL
#' @importFrom lmtest coeftest
fitAndDiagnose <- function(fml, df, depVar, stage, family, useFirth, nullLoglik, n, # nolint: cyclocomp_linter.
                           maxZThreshold = 15, maxit = 3000,
                           compute = c(ame = TRUE, predictedProbs = TRUE),
                           sweepVars = NULL,
                           actorPowerDrivers = NULL, actorPowerIndex = NULL,
                           instQualityDrivers = NULL, controlDrivers = NULL,
                           ridgeInteractions = TRUE, ridgeLambda = NULL) {
  warnEnv <- new.env()
  warnEnv$msg <- FALSE
  warnEnv$reason <- NULL

  fmlStr <- as.character(fml)
  rhsTerms <- strsplit(fmlStr[3], " \\+ ")[[1]]
  feDummies <- rhsTerms[grepl("^regionFE_", rhsTerms)]
  for (dummyName in feDummies) {
    if (!(dummyName %in% names(df))) {
      lvl <- gsub("^regionFE_", "", dummyName)
      actualLvl <- levels(df$regionFE)[make.names(levels(df$regionFE)) == lvl]
      if (length(actualLvl) == 0) actualLvl <- lvl
      df[[dummyName]] <- as.integer(df$regionFE == actualLvl)
    }
  }

  # ── Ridge path (both stages) ──────────────────────────────────────────────────
  # When ridgeInteractions = TRUE, delegate to fitRidgeLogit (adoption) or
  # fitRidgeStringency (stringency) and jump straight to the diagnostic section.
  # ridgeLambda defaults to 0.01 here (no CV in selection loops).
  if (isTRUE(ridgeInteractions)) {
    lambda_to_use <- if (is.null(ridgeLambda)) 0.01 else ridgeLambda
    clust         <- if ("region" %in% names(df)) df$region else NULL
    if (stage == "adoption") {
      ridgeRes <- fitRidgeLogit(fml, df, depVar,
                                ridgeLambda = lambda_to_use,
                                clusterVar  = clust,
                                maxit       = maxit)
    } else {
      glmFam   <- if (family == "Gamma") Gamma(link = "log") else gaussian(link = "log")
      ridgeRes <- fitRidgeStringency(fml, df, depVar,
                                     glmFamily   = glmFam,
                                     ridgeLambda = lambda_to_use,
                                     clusterVar  = clust,
                                     maxit       = maxit)
    }
    if (is.null(ridgeRes) || is.null(ridgeRes$model)) {
      termsLvl <- attr(stats::terms(fml), "term.labels")
      k_fail   <- length(termsLvl) + 1L
      dummy    <- matrix(0, nrow = k_fail, ncol = 4,
                         dimnames = list(c("(Intercept)", termsLvl),
                                         c("Estimate", "Std. Error", "z value", "Pr(>|z|)")))
      return(list(
        model = NULL, coeftest = dummy, vcov = matrix(0, k_fail, k_fail),
        formula = fml, aic = Inf, bic = Inf, aicc = Inf, hqic = Inf,
        loglik = -1e10, pseudoR2 = -1,
        nPredictors = k_fail - 1L, nSignificant = 0L, kOverN = 1,
        overfitting = TRUE, separation = TRUE, converged = FALSE,
        maxitWarning = FALSE, rejectionReason = "Ridge fit failed",
        maxVIF = NA_real_, highVIF = FALSE, vifFlagged = character(0), vifRaw = NULL,
        maxAbsZ = NA_real_, highZ = TRUE,
        groupContributions = NULL, theoryScore = NA_real_, theoryFrac = NA_real_,
        ame = NULL, predictedProbs = NULL
      ))
    }
    fit        <- ridgeRes$model
    robustTest <- ridgeRes$coeftest
    vcovMat    <- ridgeRes$vcov
    k          <- length(stats::coef(fit))
    loglik     <- ridgeRes$loglik
    modelAIC   <- fit$aic
    modelBIC   <- -2 * loglik + log(n) * k
    modelAICc  <- modelAIC + (2 * k * (k + 1)) / max(n - k - 1, 1)
    modelHQIC  <- -2 * loglik + 2 * k * log(log(n))
    converged  <- TRUE
    # Jump to the shared diagnostics section below
  } else {

  fit <- tryCatch(
    {
      withCallingHandlers(
        {
          if (stage == "adoption") {
            if (isTRUE(useFirth)) {
              logistf::logistf(fml, data = df, control = logistf::logistf.control(maxit = maxit))
            } else {
              glm(fml, data = df, family = binomial(link = "logit"), control = list(maxit = maxit))
            }
          } else {
            glmFamily <- if (family == "Gamma") Gamma(link = "log") else gaussian(link = "log")
            if (isTRUE(useFirth)) {
              if (!requireNamespace("brglm2", quietly = TRUE)) {
                stop("Package 'brglm2' is required for bias-reduced estimation. Please install it.")
              }
              glm(fml, data = df, family = glmFamily, method = brglm2::brglmFit, control = list(maxit = maxit))
            } else {
              glm(fml, data = df, family = glmFamily, control = list(maxit = maxit))
            }
          }
        },
        warning = function(w) {
          if (grepl("Maximum number of iterations", w$message, ignore.case = TRUE) ||
                grepl("did not converge", w$message, ignore.case = TRUE)) {
            warnEnv$msg <- TRUE
            warnEnv$reason <- "Algorithm did not converge (Max iterations exceeded)"
          }
          if (grepl("step size truncated due to divergence", w$message, ignore.case = TRUE)) {
            warnEnv$msg <- TRUE
            warnEnv$reason <- "Numerical divergence (Step size truncated)"
          }
          invokeRestart("muffleWarning")
        }
      )
    },
    error = function(e) {
      warnEnv$reason <- paste("Model Error:", e$message)
      return(NULL)
    }
  )

  if (is.null(fit) || isTRUE(warnEnv$msg)) {
    isFatal <- is.null(fit) || !is.null(warnEnv$reason)
    if (isFatal) {
      termsLvl <- attr(stats::terms(fml), "term.labels")
      k <- length(termsLvl) + 1
      dummyMatrix <- matrix(0, nrow = k, ncol = 4)
      rownames(dummyMatrix) <- c("(Intercept)", termsLvl)
      colnames(dummyMatrix) <- c("Estimate", "Std. Error", "z value", "Pr(>|z|)")

      return(list(
        model = NULL, coeftest = dummyMatrix, vcov = matrix(0, k, k),
        formula = fml, aic = Inf, bic = Inf, aicc = Inf, hqic = Inf,
        loglik = -1e10, pseudoR2 = -1,
        nPredictors = k - 1, nSignificant = 0, kOverN = 1,
        overfitting = TRUE, separation = TRUE, converged = FALSE,
        maxitWarning = isTRUE(warnEnv$msg), rejectionReason = warnEnv$reason,
        maxVIF = NA_real_, highVIF = FALSE, vifFlagged = character(0), vifRaw = NULL,
        maxAbsZ = NA_real_, highZ = TRUE,
        groupContributions = NULL, theoryScore = NA_real_, theoryFrac = NA_real_,
        ame = NULL, predictedProbs = NULL
      ))
    }
  }

  if (inherits(fit, "logistf")) {
    robustTest <- cbind(
      fit$coefficients, sqrt(diag(fit$var)), fit$coefficients / sqrt(diag(fit$var)), fit$prob
    )
    colnames(robustTest) <- c("Estimate", "Std. Error", "z value", "Pr(>|z|)")
    vcovMat <- fit$var
    k <- length(fit$coefficients)
    loglik <- as.numeric(fit$loglik["full"])
    modelAIC <- -2 * loglik + 2 * k
    modelBIC <- -2 * loglik + log(n) * k
    modelAICc <- modelAIC + (2 * k * (k + 1)) / max(n - k - 1, 1)
    modelHQIC <- -2 * loglik + 2 * k * log(log(n))
    converged <- !is.null(fit$coefficients) && !any(is.na(fit$coefficients))
  } else {
    clusterVar <- if ("regionFE" %in% names(df)) df$regionFE else NULL
    vcovMat <- sandwich::vcovCL(fit, cluster = clusterVar, type = "HC1")
    robustTest <- lmtest::coeftest(fit, vcov. = vcovMat)
    k <- length(stats::coef(fit))
    loglik <- as.numeric(stats::logLik(fit))
    modelAIC <- fit$aic
    modelBIC <- BIC(fit)
    modelAICc <- modelAIC + (2 * k * (k + 1)) / max(n - k - 1, 1)
    modelHQIC <- -2 * loglik + 2 * k * log(log(n))
    converged <- fit$converged
  }

  } # end else (non-Ridge path)

  pValues <- robustTest[, 4]
  pseudoR2 <- computePseudoR2(loglik, nullLoglik)
  nSignificant <- sum(pValues < 0.05, na.rm = TRUE)
  kOverN <- k / n
  overfitting <- kOverN > 0.1

  # --- Separation detection ---
  # Original check (large coefficients or extreme fitted probabilities)
  sepFromCoef <- detectSeparation(fit, robustTest)

  # <<< NEW: additional check — extreme z-values even when abs(coef) <= 10.
  # This catches cases like Coal z = -49 or V&A z = 29 that brglm2 does not fix.
  zVals <- robustTest[, "z value"]
  # Exclude intercept from z-check (intercepts can legitimately be large)
  nonInterceptZ <- zVals[rownames(robustTest) != "(Intercept)"]
  maxAbsZ <- if (length(nonInterceptZ) > 0) max(abs(nonInterceptZ), na.rm = TRUE) else 0
  highZ <- maxAbsZ > maxZThreshold

  # Combined separation flag: either structural or z-based
  separation <- sepFromCoef | highZ # <<< UPDATED

  vifRes <- computeVIF(data = df, formula = fml)
  maxVIF <- if (!is.null(vifRes)) vifRes$maxVIF else NA_real_
  highVIF <- if (!is.null(vifRes)) vifRes$highVIF else FALSE
  vifFlagged <- if (!is.null(vifRes)) vifRes$flagged else character(0)

  fitResult <- list(model = fit, vcov = vcovMat)

  gc <- computeGroupContributions(
    fitResult, df = df,
    actorPowerDrivers = actorPowerDrivers, actorPowerIndex = actorPowerIndex,
    instQualityDrivers = instQualityDrivers, controlDrivers = controlDrivers
  )

  ameResult <- NULL
  ppResult  <- NULL
  if (stage == "adoption") {
    if (isTRUE(compute[["ame"]])) {
      ameResult <- computeAME(
        fitResult,
        actorPowerDrivers = actorPowerDrivers, actorPowerIndex = actorPowerIndex,
        instQualityDrivers = instQualityDrivers, controlDrivers = controlDrivers
      )
    }
    if (isTRUE(compute[["predictedProbs"]])) {
      ppResult <- computePredictedProbabilities(
        fitResult, df = df, sweepVars = sweepVars,
        actorPowerDrivers = actorPowerDrivers, actorPowerIndex = actorPowerIndex,
        instQualityDrivers = instQualityDrivers, controlDrivers = controlDrivers
      )
    }
  }

  list(
    model = fit, coeftest = robustTest, vcov = vcovMat, formula = fml,
    aic = modelAIC, bic = modelBIC, aicc = modelAICc, hqic = modelHQIC,
    loglik = loglik, pseudoR2 = pseudoR2,
    nPredictors = k, nSignificant = nSignificant, kOverN = kOverN,
    overfitting = overfitting, separation = separation, converged = converged,
    maxitWarning = warnEnv$msg, rejectionReason = NULL,
    maxVIF = maxVIF, highVIF = highVIF, vifFlagged = vifFlagged,
    vifRaw = if (!is.null(vifRes)) vifRes$values else NULL,
    maxAbsZ = maxAbsZ, highZ = highZ,
    groupContributions = gc,
    theoryScore        = if (!is.null(gc)) gc[["Theory Score"]] else NA_real_,
    theoryFrac         = if (!is.null(gc)) gc[["Theory Frac."]] else NA_real_,
    ame                = ameResult,
    predictedProbs     = ppResult
  )
}
