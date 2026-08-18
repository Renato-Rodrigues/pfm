# nolint start
#' Inference and support diagnostics for a fitted frontier
#'
#' @description
#' Extracts the three things a downstream consumer needs that a coefficient table alone cannot
#' supply, and that were previously discarded when \code{\link{runPSMFrontier}} assembled its
#' artifact:
#'
#' \describe{
#'   \item{\code{vcov}}{the estimated covariance matrix of the coefficients. Required for any
#'     quantity that is a \emph{linear combination} of coefficients — a marginal effect with an
#'     interaction, \eqn{\partial\eta/\partial x = \beta_x + \beta_{x:z} z}, has variance
#'     \eqn{\mathrm{Var}(\beta_x) + z^2\mathrm{Var}(\beta_{x:z}) + 2z\,\mathrm{Cov}(\beta_x,\beta_{x:z})}.
#'     Without the covariance term the interval is simply wrong, and no amount of care with the
#'     standard errors in \code{coefTable} recovers it.}
#'   \item{\code{support}}{the observed range of every model-matrix column on the estimation
#'     rows. \code{MODEL.md} §2.7 requires marginal effects to be reported only where they are
#'     identified; a consumer cannot honour that without knowing where the data actually are.}
#'   \item{\code{correlation}}{the correlation matrix among model-matrix columns. Collinearity
#'     between institutional channels is the reason the selection bootstrap cannot separate
#'     them (\code{MODEL.md} §6), and the matrix is the direct evidence for that claim.}
#' }
#'
#' All three are small — a 14x14 matrix and a 14-row table per sector — so persisting them
#' costs nothing next to the per-row scores already in the artifact.
#'
#' @param fit A fitted model from \code{\link{estimatePolicyStringencyModel}}; needs
#'   \code{$vcov}, \code{$model} and \code{$data}.
#' @param driverScaling Optional named list of \code{list(mean=, sd=)} used to standardize the
#'   drivers, so the support can also be reported in the driver's own units. Defaults to
#'   \code{fit$driverScaling}.
#' @return A list with \code{vcov}, \code{support}, \code{correlation} and \code{nObs}, or
#'   \code{NULL} if the fit does not carry what is needed.
#' @seealso \code{\link{computeFrontierRobustness}}, \code{\link{runPSMFrontier}}
#' @export
#' @author Renato Rodrigues
computeFrontierDiagnostics <- function(fit, driverScaling = NULL) {
  if (is.null(fit)) return(NULL)
  ds <- driverScaling %||% fit$driverScaling

  # ---- the model matrix, which is what vcov is indexed over -------------------
  mm <- tryCatch(stats::model.matrix(fit$model), error = function(e) NULL)
  if (is.null(mm)) {
    mm <- tryCatch(stats::model.matrix(fit$formula, data = fit$data), error = function(e) NULL)
  }
  if (is.null(mm)) {
    warning("computeFrontierDiagnostics: no model matrix available; ",
            "support and correlation skipped.", call. = FALSE)
  }

  V <- tryCatch(as.matrix(fit$vcov), error = function(e) NULL)
  if (is.null(V)) {
    warning("computeFrontierDiagnostics: fit carries no vcov.", call. = FALSE)
  }

  support <- NULL
  correlation <- NULL
  if (!is.null(mm)) {
    keep <- setdiff(colnames(mm), "(Intercept)")
    X <- mm[, keep, drop = FALSE]

    # Drop zero-variance columns before correlating: a constant column yields NA and
    # poisons the whole matrix for any consumer that plots it.
    sds <- apply(X, 2, stats::sd, na.rm = TRUE)
    live <- names(sds)[is.finite(sds) & sds > 0]
    if (length(live) >= 2) {
      correlation <- stats::cor(X[, live, drop = FALSE], use = "pairwise.complete.obs")
    }

    q <- function(x, p) as.numeric(stats::quantile(x, p, na.rm = TRUE))
    support <- data.frame(
      term = keep,
      n    = as.integer(colSums(!is.na(X))),
      mean = as.numeric(colMeans(X, na.rm = TRUE)),
      sd   = as.numeric(sds[keep]),
      min  = apply(X, 2, min, na.rm = TRUE),
      p05  = apply(X, 2, q, 0.05),
      p25  = apply(X, 2, q, 0.25),
      p50  = apply(X, 2, q, 0.50),
      p75  = apply(X, 2, q, 0.75),
      p95  = apply(X, 2, q, 0.95),
      max  = apply(X, 2, max, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
    rownames(support) <- NULL

    # Raw units, where the driver was standardized. Interaction columns are products of two
    # standardized factors and have no single natural unit, so they are left NA rather than
    # given a misleading one.
    support$rawMean <- NA_real_
    support$rawSd <- NA_real_
    # driverScaling entries are NAMED NUMERIC VECTORS c(mean=, sd=, sat=), not lists - an
    # is.list() test silently returns nothing for every driver.
    getScale <- function(s, what) {
      if (is.null(s)) return(NA_real_)
      v <- if (is.list(s)) s[[what]] else s[what]
      if (is.null(v) || !length(v)) NA_real_ else as.numeric(v)
    }
    if (length(ds)) {
      for (i in seq_len(nrow(support))) {
        s <- ds[[support$term[i]]]
        support$rawMean[i] <- getScale(s, "mean")
        support$rawSd[i]   <- getScale(s, "sd")
      }
    }
    support$standardized <- is.finite(support$rawSd)
  }

  list(vcov = V, support = support, correlation = correlation,
       nObs = if (!is.null(fit$data)) nrow(fit$data) else NA_integer_)
}
# nolint end
