#' @title fitRidgeLogit
#' @description Fits a binomial logistic regression with Ridge (L2) regularization
#'   applied **selectively to interaction terms only**. Main effects, time trend, region
#'   fixed effects, and controls are left unpenalized (\code{penalty.factor = 0}).
#'
#'   This addresses the sign-flip projection artefact that occurs when a large negative
#'   interaction coefficient (e.g. \code{API x RoL}) dominates the linear predictor once
#'   the Actor Power Index transitions from negative to positive in scenario data. Ridge
#'   shrinks the interaction coefficients toward zero without forcing them to zero (unlike
#'   Lasso), preserving the theoretical interaction signal while reducing its extremity.
#'
#'   When \code{ridgeLambda = NULL} the penalty is selected by 5-fold cross-validation
#'   via \code{cv.glmnet}. Pass an explicit numeric value to fix \eqn{\lambda} (useful
#'   when Ridge is used inside a model-selection loop where CV is too expensive).
#'
#'   The returned \code{model} is a \code{glm} object whose \code{$coefficients} and
#'   \code{$fitted.values} have been patched with the Ridge estimates. All downstream
#'   \code{compute*()} functions work on this object without modification.
#'
#' @param fml Formula.
#' @param df Data.frame. Training data (already has regionFE dummies expanded).
#' @param depVar Character. Name of the binary response column.
#' @param ridgeLambda Numeric or NULL. Ridge penalty \eqn{\lambda}. \code{NULL} triggers
#'   automatic 5-fold CV selection via \code{cv.glmnet}.
#' @param clusterVar Character vector or NULL. Cluster IDs for HC1-corrected sandwich
#'   variance. Typically \code{df$region}.
#' @param maxit Integer. Max iterations for the initial unpenalized GLM (used only to
#'   build the model-object scaffold; convergence of this fit is not required).
#'
#' @return A list with:
#'   \describe{
#'     \item{model}{Patched \code{glm} / \code{ridgeLogit} object.}
#'     \item{coeftest}{4-column matrix (Estimate, SE, z, p) from the penalized Hessian.}
#'     \item{vcov}{Variance-covariance matrix (penalized Hessian sandwich).}
#'     \item{ridgeLambda}{\eqn{\lambda} used.}
#'     \item{nInteractionTerms}{Number of penalized interaction terms.}
#'     \item{loglik}{Unpenalized log-likelihood evaluated at the Ridge estimates.}
#'     \item{nObs}{Number of observations.}
#'   }
#'   Returns \code{NULL} when the initial GLM scaffold fails to fit.
#'
#' @keywords internal
#' @importFrom stats glm binomial coef model.matrix plogis pnorm formula logLik
fitRidgeLogit <- function(fml, df, depVar,
                           ridgeLambda = NULL,
                           clusterVar  = NULL,
                           maxit       = 3000) {
  if (!requireNamespace("glmnet", quietly = TRUE)) {
    stop("Package 'glmnet' is required for Ridge regularization. ",
         "Install it with: install.packages('glmnet')")
  }

  # ── 1. Fit unpenalized GLM to build model-object scaffold ───────────────────
  glm_fit <- tryCatch(
    suppressWarnings(
      glm(fml, data = df, family = binomial(link = "logit"),
          control = list(maxit = maxit))
    ),
    error = function(e) NULL
  )
  if (is.null(glm_fit)) return(NULL)

  # ── 2. Build model matrix ────────────────────────────────────────────────────
  # model.matrix(glm_fit) and glm_fit$y both use only complete-case rows;
  # pulling y from the full df would cause a dimension mismatch when NAs exist.
  X_full <- model.matrix(glm_fit)               # n × (k+1): includes intercept
  X      <- X_full[, -1L, drop = FALSE]         # n × k: remove intercept for glmnet
  y      <- as.integer(glm_fit$y)               # response aligned with model matrix rows
  n_obs  <- nrow(X_full)

  # ── 3. Penalty: 1 for _x_ interaction terms, 0 for everything else ──────────
  is_interaction  <- grepl("_x_", colnames(X))
  penalty_factors <- as.numeric(is_interaction)
  n_penalized     <- sum(is_interaction)

  if (n_penalized == 0L) {
    warning("fitRidgeLogit: no '_x_' interaction terms found in formula — ",
            "Ridge penalty has no effect. Returning standard GLM.")
    return(list(
      model             = glm_fit,
      coeftest          = NULL,
      vcov              = NULL,
      ridgeLambda       = 0,
      nInteractionTerms = 0L,
      loglik            = as.numeric(logLik(glm_fit)),
      nObs              = n_obs
    ))
  }

  # ── 4. Select lambda via 5-fold CV when not supplied ────────────────────────
  if (is.null(ridgeLambda)) {
    cv_fit      <- glmnet::cv.glmnet(
      X, y, family = "binomial", alpha = 0,
      penalty.factor = penalty_factors,
      nfolds = 5, type.measure = "deviance"
    )
    ridgeLambda <- cv_fit$lambda.min
  }

  # ── 5. Fit Ridge ─────────────────────────────────────────────────────────────
  ridge_fit <- glmnet::glmnet(
    X, y, family = "binomial", alpha = 0,
    lambda = ridgeLambda, penalty.factor = penalty_factors,
    standardize = TRUE
  )

  # Extract and align coefficients with model.matrix column order
  beta_raw <- as.vector(coef(ridge_fit, s = ridgeLambda))
  names(beta_raw) <- rownames(coef(ridge_fit, s = ridgeLambda))
  beta <- beta_raw[colnames(X_full)]
  beta[is.na(beta)] <- 0  # glmnet may zero near-zero terms; make explicit

  # ── 6. Patch GLM object with Ridge estimates ─────────────────────────────────
  eta     <- as.vector(X_full %*% beta)
  pi_vals <- plogis(eta)

  glm_fit$coefficients      <- beta
  glm_fit$fitted.values     <- pi_vals
  glm_fit$linear.predictors <- eta
  glm_fit$ridgeLambda       <- ridgeLambda
  glm_fit$nInteractionTerms <- n_penalized
  # Update deviance and AIC to reflect Ridge-estimated fitted values
  ridge_loglik        <- sum(y * log(pi_vals + 1e-15) + (1 - y) * log(1 - pi_vals + 1e-15))
  k                   <- ncol(X_full)
  glm_fit$deviance    <- -2 * ridge_loglik
  glm_fit$aic         <- -2 * ridge_loglik + 2 * k
  class(glm_fit)      <- c("ridgeLogit", class(glm_fit))

  # ── 7. Penalized Hessian variance (sandwich with HC1 cluster correction) ────
  # H_pen = X'WX + lambda * diag(penalty_factors_full)
  # where penalty_factors_full puts 0 on intercept, lambda*pf on predictors
  W             <- pi_vals * (1 - pi_vals)
  pen_diag      <- c(0, ridgeLambda * penalty_factors)  # length = k (intercept + preds)
  XtWX          <- crossprod(X_full * sqrt(W))           # t(X) diag(W) X
  H_pen         <- XtWX + diag(pen_diag)

  H_inv <- tryCatch(solve(H_pen), error = function(e) {
    # Near-singular: use ridge-regularised pseudo-inverse as fallback
    tryCatch(solve(H_pen + diag(1e-8, nrow(H_pen))), error = function(e2) NULL)
  })
  if (is.null(H_inv)) {
    return(list(
      model = glm_fit, coeftest = NULL, vcov = NULL,
      ridgeLambda = ridgeLambda, nInteractionTerms = n_penalized,
      loglik = ridge_loglik, nObs = n_obs
    ))
  }

  scores <- X_full * (y - pi_vals)  # n × k score matrix

  # Align clusterVar to complete-case rows (same rows used for the model matrix)
  if (!is.null(clusterVar) && length(clusterVar) != n_obs) {
    obs_rows   <- as.integer(rownames(X_full))   # glm stores original row indices
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
    vcovMat <- H_inv %*% XtWX %*% H_inv
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
