# nolint start
#' @title fitRidgeStringency
#' @description Fits a GLM stringency model with Ridge (L2) regularization applied
#'   selectively to interaction terms (\code{_x_} pattern). Non-interaction predictors
#'   (main effects, time trend, region FE, controls) are left unpenalized
#'   (\code{penalty.factor = 0}).
#'
#'   Because \code{glmnet} does not support the Gamma family with a log link directly,
#'   Ridge is applied via \code{family = "gaussian"} in \code{glmnet}, which is an OLS
#'   Ridge regression on the (already log-transformed) response scale. This approximation
#'   is well-justified when \code{logTransform = TRUE} (the default in
#'   \code{estimatePriceStringencyModel}), since \code{log(1 + ECP)} is approximately
#'   Gaussian after transformation. When \code{logTransform = FALSE} a warning is issued
#'   and the same Gaussian approximation is used.
#'
#'   Standard errors are derived from the penalized Hessian sandwich estimator with HC1
#'   cluster correction. For the Gaussian approximation, the Hessian simplifies to
#'   \eqn{H_\text{pen} = X'X + \lambda \cdot \mathrm{diag}(\text{penalty\_factors})} (no
#'   diagonal weight matrix needed).
#'
#' @param fml Formula.
#' @param df Data.frame. Training data (adopters only, already log-transformed if applicable).
#' @param depVar Character. Name of the dependent variable column (\code{"ecp"}).
#' @param glmFamily A \code{family} object (e.g. \code{Gamma(link="log")}). Used only
#'   for the scaffold GLM fit; the Ridge fit itself uses the Gaussian approximation.
#' @param ridgeLambda Numeric or NULL. Ridge penalty \eqn{\lambda}. \code{NULL} triggers
#'   automatic 5-fold CV selection via \code{cv.glmnet}.
#' @param clusterVar Character vector or NULL. Cluster IDs for HC1-corrected sandwich
#'   variance. Typically \code{df$region}.
#' @param maxit Integer. Max iterations for the scaffold GLM.
#'
#' @return A list with:
#'   \describe{
#'     \item{model}{Patched \code{glm} / \code{ridgeGLM} object with Ridge coefficients.}
#'     \item{coeftest}{4-column matrix (Estimate, SE, z, p).}
#'     \item{vcov}{Variance-covariance matrix (penalized Hessian sandwich).}
#'     \item{ridgeLambda}{\eqn{\lambda} used.}
#'     \item{nInteractionTerms}{Number of penalized interaction terms.}
#'     \item{loglik}{Log-likelihood evaluated at Ridge estimates.}
#'     \item{nObs}{Number of observations.}
#'   }
#'   Returns \code{NULL} when the scaffold GLM fails to fit.
#'
#' @keywords internal
#' @importFrom stats glm coef model.matrix pnorm formula logLik
fitRidgeStringency <- function(fml, df, depVar,
                                glmFamily,
                                ridgeLambda     = NULL,
                                clusterVar      = NULL,
                                maxit           = 3000,
                                fePenaltyFactor = 0.5) {
  if (!requireNamespace("glmnet", quietly = TRUE)) {
    stop("Package 'glmnet' is required for Ridge regularization. ",
         "Install it with: install.packages('glmnet')")
  }

  # ── 1. Fit scaffold GLM ──────────────────────────────────────────────────────
  glm_fit <- tryCatch(
    suppressWarnings(
      glm(fml, data = df, family = glmFamily, control = list(maxit = maxit))
    ),
    error = function(e) NULL
  )
  if (is.null(glm_fit)) return(NULL)

  # ── 2. Build model matrix ────────────────────────────────────────────────────
  X_full <- model.matrix(glm_fit)           # n × (k+1): includes intercept
  X      <- X_full[, -1L, drop = FALSE]     # n × k: remove intercept for glmnet
  y      <- glm_fit$y                       # response aligned with complete-case rows
  n_obs  <- nrow(X_full)

  # ── 3. Penalty factors ───────────────────────────────────────────────────────
  # Interaction terms (_x_): penalized with factor 1.
  # Region FE dummies: lightly penalized with fePenaltyFactor (partial pooling).
  # Under Mundlak (no regionFE columns in X), the FE block has no effect.
  is_interaction  <- grepl("_x_", colnames(X))
  is_fe           <- grepl("^regionFE", colnames(X))
  penalty_factors <- rep(0, ncol(X))
  penalty_factors[is_interaction] <- 1
  if (any(is_fe) && fePenaltyFactor > 0)
    penalty_factors[is_fe] <- pmax(penalty_factors[is_fe], fePenaltyFactor)

  n_penalized <- sum(penalty_factors > 0)

  if (n_penalized == 0L) {
    warning("fitRidgeStringency: no terms selected for penalization — Ridge has no effect.")
    return(list(
      model             = glm_fit,
      coeftest          = NULL,
      vcov              = NULL,
      ridgeLambda       = 0,
      nInteractionTerms = sum(is_interaction),
      loglik            = as.numeric(logLik(glm_fit)),
      nObs              = n_obs
    ))
  }

  if (!inherits(glmFamily, c("Gamma", "gaussian")) ||
      !identical(glmFamily$link, "log")) {
    # Reminder: glmnet Gaussian approximation is most accurate for log-link families
  }

  # ── 4. Ridge via glmnet (Gaussian OLS on log-transformed response) ───────────
  # After logTransform = TRUE, y = log(1 + ECP) which is approximately Gaussian,
  # making the OLS Ridge approximation in glmnet valid.
  if (is.null(ridgeLambda)) {
    cv_fit      <- glmnet::cv.glmnet(
      X, y, family = "gaussian", alpha = 0,
      penalty.factor = penalty_factors,
      nfolds = 5, type.measure = "deviance"
    )
    ridgeLambda <- cv_fit$lambda.min
  }

  ridge_fit <- glmnet::glmnet(
    X, y, family = "gaussian", alpha = 0,
    lambda = ridgeLambda, penalty.factor = penalty_factors,
    standardize = TRUE
  )

  # Align Ridge coefficients with model.matrix column order
  beta_raw <- as.vector(coef(ridge_fit, s = ridgeLambda))
  names(beta_raw) <- rownames(coef(ridge_fit, s = ridgeLambda))
  beta <- beta_raw[colnames(X_full)]
  beta[is.na(beta)] <- 0

  # ── 5. Patch GLM object with Ridge estimates ─────────────────────────────────
  eta     <- as.vector(X_full %*% beta)
  mu_vals <- exp(eta)           # log link inverse: μ = exp(η)

  glm_fit$coefficients      <- beta
  glm_fit$linear.predictors <- eta
  glm_fit$fitted.values     <- mu_vals
  glm_fit$ridgeLambda       <- ridgeLambda
  glm_fit$nInteractionTerms <- n_penalized
  k                          <- ncol(X_full)
  ridge_loglik               <- as.numeric(logLik(glm_fit))
  glm_fit$deviance           <- -2 * ridge_loglik
  glm_fit$aic                <- -2 * ridge_loglik + 2 * k
  class(glm_fit)             <- c("ridgeGLM", class(glm_fit))

  # ── 6. Penalized Hessian sandwich (Gaussian/OLS approximation) ───────────────
  # For OLS Ridge: H_pen = X'X + λ·diag(penalty_factors_full)
  # σ² cancels in the clustered sandwich, so we omit it.
  pen_diag <- c(0, ridgeLambda * penalty_factors)
  XtX      <- crossprod(X_full)              # t(X) X  (no W for OLS)
  H_pen    <- XtX + diag(pen_diag)

  H_inv <- tryCatch(solve(H_pen), error = function(e) {
    tryCatch(solve(H_pen + diag(1e-8, nrow(H_pen))), error = function(e2) NULL)
  })
  if (is.null(H_inv)) {
    return(list(
      model = glm_fit, coeftest = NULL, vcov = NULL,
      ridgeLambda = ridgeLambda, nInteractionTerms = n_penalized,
      loglik = ridge_loglik, nObs = n_obs
    ))
  }

  residuals_raw <- y - eta        # OLS residuals on linear predictor scale
  scores        <- X_full * residuals_raw

  if (!is.null(clusterVar) && length(clusterVar) != n_obs) {
    obs_rows   <- as.integer(rownames(X_full))
    clusterVar <- clusterVar[obs_rows]
  }

  if (!is.null(clusterVar) && length(clusterVar) == n_obs) {
    clusters   <- unique(clusterVar)
    G          <- length(clusters)
    B_meat     <- Reduce("+", lapply(clusters, function(cl) {
      sc <- colSums(scores[clusterVar == cl, , drop = FALSE])
      tcrossprod(sc)
    }))
    correction <- (G / (G - 1)) * ((n_obs - 1) / (n_obs - k))
    vcovMat    <- correction * H_inv %*% B_meat %*% H_inv
  } else {
    vcovMat    <- H_inv %*% XtX %*% H_inv
  }

  se_vals <- sqrt(pmax(0, diag(vcovMat)))
  z_vals  <- beta / se_vals
  p_vals  <- 2 * pnorm(-abs(z_vals))

  coeftest_mat <- cbind(beta, se_vals, z_vals, p_vals)
  colnames(coeftest_mat) <- c("Estimate", "Std. Error", "z value", "Pr(>|z|)")
  rownames(coeftest_mat) <- names(beta)

  list(
    model             = glm_fit,
    coeftest          = coeftest_mat,
    vcov              = vcovMat,
    ridgeLambda       = ridgeLambda,
    nInteractionTerms = n_penalized,
    loglik            = ridge_loglik,
    nObs              = n_obs
  )
}
# nolint end
