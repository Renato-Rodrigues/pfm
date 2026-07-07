# nolint start
#' Marginal effect of Actor Power over the observed moderator support
#'
#' @description
#' The single most informative robustness exhibit for the PSM interaction claims
#' (R2, 2026-07-06; docs/psm-nature-readiness-assessment.md): for every
#' actor-power term \code{a} interacting with an institutional-quality moderator
#' \code{b} (a fitted \code{a_x_b} column), the marginal effect of \code{a} on the
#' natural 0-\code{indexMax} index is traced over a grid of moderator values
#' spanning the \emph{observed} (standardized) support:
#' \deqn{ME_a(m) = indexMax \cdot \bar{w} \cdot (\beta_a + \beta_{ab} m)}
#' with \eqn{\bar{w} = mean(dlogis(\eta))} over the estimation rows (satP engine),
#' and a delta-method SE on \eqn{(\beta_a, \beta_{ab})} from the clustered vcov
#' (\eqn{\bar{w}} treated as fixed — a standard first-order approximation). The
#' returned grid marks which moderator values are inside the estimation support,
#' so a reader sees exactly where the claimed moderation is identified and where
#' a scenario would extrapolate it.
#'
#' @param fit A fit result from \code{\link{estimatePolicyStringencyModel}}
#'   (satP engine; needs \code{model}, \code{vcov}, \code{formula}, \code{data},
#'   \code{indexMax}).
#' @param gridN Integer. Grid points per moderator. Default \code{25}.
#' @param padShare Numeric. Fraction of the observed range added on each side of
#'   the grid (visualises the near-out-of-support behaviour). Default \code{0.15}.
#'
#' @return Data.frame \code{apTerm, moderator, m, me, se, lo, hi, inSupport}
#'   (one block per fitted \code{a_x_b} pair; \code{m} on the standardized
#'   moderator scale), or \code{NULL} when the fit carries no interaction terms.
#'
#' @author Renato Rodrigues
#'
#' @importFrom stats coef dlogis plogis qnorm model.matrix as.formula
#'
#' @export
computeMarginalEffectSupport <- function(fit, gridN = 25, padShare = 0.15) {
  beta <- tryCatch(stats::coef(fit$model), error = function(e) NULL)
  if (is.null(beta) || !is.numeric(beta)) return(NULL)
  intTerms <- grep("_x_", names(beta), value = TRUE)
  if (length(intTerms) == 0) return(NULL)
  df <- fit$data
  indexMax <- fit$indexMax %||% 10
  vc <- fit$vcov

  mm <- tryCatch(
    stats::model.matrix(stats::as.formula(fit$formula), data = df),
    error = function(e) NULL
  )
  if (is.null(mm)) return(NULL)
  shared <- intersect(colnames(mm), names(beta))
  eta <- as.numeric(mm[, shared, drop = FALSE] %*% beta[shared])
  meanW <- mean(stats::dlogis(eta), na.rm = TRUE)
  zc <- stats::qnorm(0.975)

  rows <- list()
  for (it in intTerms) {
    parts <- strsplit(it, "_x_", fixed = TRUE)[[1]]
    if (length(parts) != 2) next
    a <- parts[1]
    b <- parts[2]
    if (!a %in% names(beta) || !b %in% colnames(df)) next
    mObs <- df[[b]][is.finite(df[[b]])]
    if (length(mObs) == 0) next
    lo <- min(mObs)
    hi <- max(mObs)
    pad <- padShare * (hi - lo)
    grid <- seq(lo - pad, hi + pad, length.out = gridN)
    va <- vc[a, a]
    vab <- vc[it, it]
    cab <- vc[a, it]
    me <- indexMax * meanW * (beta[[a]] + beta[[it]] * grid)
    se <- indexMax * meanW * sqrt(pmax(va + grid^2 * vab + 2 * grid * cab, 0))
    rows[[it]] <- data.frame(
      apTerm = a, moderator = b, m = grid, me = me, se = se,
      lo = me - zc * se, hi = me + zc * se,
      inSupport = grid >= lo & grid <= hi,
      stringsAsFactors = FALSE
    )
  }
  if (length(rows) == 0) return(NULL)
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}
# nolint end
