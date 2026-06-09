#' @title computeAME
#' @description Computes Average Marginal Effects (AMEs) for a fitted adoption logit
#'   model using manual calculation — no external packages required.
#'
#'   For each predictor k, the AME is the average derivative of Pr(Adoption) with
#'   respect to x_k across all observations:
#'   \deqn{AME_k = \frac{1}{N} \sum_i \Lambda'(\eta_i) \cdot \beta_k}
#'   where \eqn{\Lambda'(\eta) = \pi_i(1-\pi_i)} is the logistic density and
#'   \eqn{\eta_i = \mathbf{x}_i'\boldsymbol{\beta}}.
#'
#'   Standard errors are derived via the delta method using the robust
#'   variance-covariance matrix already stored in the \code{fit} object.
#'   The gradient of \eqn{AME_k} with respect to \eqn{\boldsymbol{\beta}} is:
#'   \deqn{g_{kj} = \beta_k \cdot \overline{\Lambda'(\eta)(1-2\pi) x_j} + \delta_{kj} \cdot \overline{\Lambda'(\eta)}}
#'
#' @param fit List. Output of \code{fitAndDiagnose} or \code{estimateAdoptionModel}.
#'   Must contain \code{$model} (a \code{logistf} or \code{glm} object) and
#'   \code{$vcov} (the robust variance-covariance matrix).
#' @param actorPowerDrivers Character vector or NULL.
#' @param actorPowerIndex Character or NULL.
#' @param instQualityDrivers Character vector or NULL.
#' @param controlDrivers Character vector or NULL.
#'
#' @return A \code{data.frame} with one row per model term and columns:
#'   \describe{
#'     \item{term}{Model-matrix column name.}
#'     \item{term_group}{Canonical Term Group from \code{classifyTermGroups}.}
#'     \item{ame}{Average marginal effect on Pr(Adoption). For binary predictors
#'       this approximates the discrete change; for continuous predictors it is the
#'       instantaneous slope averaged over the sample.}
#'     \item{se}{Delta-method standard error.}
#'     \item{z}{z-statistic (\code{ame / se}).}
#'     \item{p}{Two-sided p-value from standard normal.}
#'     \item{lower}{Lower 95\% confidence bound (\code{ame - 1.96*se}).}
#'     \item{upper}{Upper 95\% confidence bound (\code{ame + 1.96*se}).}
#'   }
#'   Returns \code{NULL} when the model is unavailable or the model matrix cannot
#'   be constructed.
#'
#' @author Renato Rodrigues
#' @export
#'
#' @importFrom stats coef model.matrix plogis pnorm formula
computeAME <- function(fit,
                       actorPowerDrivers  = NULL,
                       actorPowerIndex    = NULL,
                       instQualityDrivers = NULL,
                       controlDrivers     = NULL) {
  m       <- if (is.list(fit) && !is.null(fit$model)) fit$model else fit
  vcovMat <- if (is.list(fit) && !is.null(fit$vcov)) fit$vcov else NULL
  if (is.null(m)) return(NULL)

  data_src <- if (!is.null(m$model)) m$model else NULL
  if (is.null(data_src)) return(NULL)

  fml <- if (inherits(m, "logistf")) m$formula else stats::formula(m)
  mm  <- tryCatch(
    stats::model.matrix(fml, data = data_src),
    error = function(e) NULL
  )
  if (is.null(mm) || nrow(mm) == 0) return(NULL)

  beta   <- stats::coef(m)
  common <- intersect(colnames(mm), names(beta))
  if (length(common) == 0) return(NULL)
  mm   <- mm[, common, drop = FALSE]
  beta <- beta[common]

  # Predicted probabilities and logistic density at each observation
  eta      <- as.vector(mm %*% beta)
  pi_vals  <- stats::plogis(eta)
  dlp      <- pi_vals * (1 - pi_vals)     # Lambda'(eta) = pi*(1-pi)
  mean_dlp <- mean(dlp)

  K        <- length(beta)
  ame_vals <- beta * mean_dlp             # AME_k = beta_k * mean(dlp)

  # Delta-method gradient matrix G (K×K):
  #   G[k, j] = beta_k * mean(dlp*(1-2*pi)*x_j)  +  I(k==j) * mean(dlp)
  dlp_1m2p  <- dlp * (1 - 2 * pi_vals)  # Lambda''(eta) = dlp*(1-2*pi)
  mean_cross <- colMeans(dlp_1m2p * mm)  # length-K vector: mean(dlp_1m2p * x_j) for each j
  G          <- outer(beta, mean_cross) + mean_dlp * diag(K)

  se_vals <- rep(NA_real_, K)
  if (!is.null(vcovMat)) {
    if (is.null(rownames(vcovMat)) || is.null(colnames(vcovMat))) {
      if (nrow(vcovMat) == length(beta)) {
        rownames(vcovMat) <- names(beta)
        colnames(vcovMat) <- names(beta)
      }
    }
    V <- tryCatch(vcovMat[common, common, drop = FALSE], error = function(e) NULL)
    if (!is.null(V) && nrow(V) == K) {
      for (k in seq_len(K)) {
        gk         <- G[k, ]
        se_vals[k] <- sqrt(max(0, as.numeric(t(gk) %*% V %*% gk)))
      }
    }
  }

  z_vals <- ame_vals / se_vals
  p_vals <- 2 * stats::pnorm(-abs(z_vals))

  groups <- classifyTermGroups(
    common, actorPowerDrivers, actorPowerIndex, instQualityDrivers, controlDrivers
  )

  data.frame(
    term       = common,
    term_group = groups,
    ame        = round(ame_vals, 4),
    se         = round(se_vals,  4),
    z          = round(z_vals,   3),
    p          = round(p_vals,   4),
    lower      = round(ame_vals - 1.96 * se_vals, 4),
    upper      = round(ame_vals + 1.96 * se_vals, 4),
    stringsAsFactors = FALSE,
    row.names         = NULL
  )
}
