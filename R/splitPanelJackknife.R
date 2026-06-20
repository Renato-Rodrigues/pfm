# nolint start
#' @title splitPanelJackknife
#' @description Half-panel (split-panel) jackknife bias correction for a dynamic
#' fixed-effects model (Dhaene & Jochmans, 2015). When a lagged dependent variable
#' is combined with region fixed effects over a short panel, the within estimator
#' of every coefficient is biased by O(1/T) (Nickell bias). Splitting each unit's
#' own time series at its midpoint and refitting on each half doubles that bias to
#' O(2/T); the jackknife combination
#' \deqn{\hat\beta_{SPJ} = 2\hat\beta_{full} - \tfrac12(\hat\beta_{early} + \hat\beta_{late})}
#' cancels the leading 1/T term. It is estimator-agnostic (works for any
#' \code{glm}), which is why it fits the PFM stringency pipeline; full GMM
#' (Arellano-Bond) would require leaving the glm framework.
#'
#' Degrades gracefully: if either half is not estimable (the stringency sample is
#' sparse in the early years, so a half can be rank-deficient or fail to
#' converge) the function returns \code{NULL} and the caller keeps the
#' uncorrected fit. Coefficients absent from a half (e.g. region FE dummies for
#' regions with a one-period spell) are left uncorrected.
#'
#' @param fml Formula. The model formula (must include the lag and FE terms).
#' @param df Data.frame. The estimation sample (already subset/transformed).
#' @param glmFamily A \code{family} object (e.g. \code{gaussian("identity")}).
#' @param timeCol,groupCol Character. Column names for the time and panel-unit
#'   dimensions. Defaults \code{"year"} / \code{"region"}.
#' @param maxit Integer. GLM iteration limit.
#'
#' @return List \code{list(coefficients = <named bias-corrected vector>,
#'   corrected = <names actually corrected>)}, or \code{NULL} when not estimable.
#'
#' @importFrom stats glm coef median ave
#'
#' @keywords internal
splitPanelJackknife <- function(fml, df, glmFamily,
                                timeCol = "year", groupCol = "region",
                                maxit = 3000) {
  if (!all(c(timeCol, groupCol) %in% names(df))) return(NULL)
  if (nrow(df) < 8) return(NULL)

  # Per-unit median-year split: each region's own spell is halved, so both halves
  # retain every region that has >= 2 observed periods (handles the unbalanced,
  # staggered-adoption stringency panel).
  half <- stats::ave(df[[timeCol]], df[[groupCol]], FUN = function(y) {
    as.integer(y > stats::median(y))
  })

  fitOn <- function(d) {
    if (nrow(d) < 4) return(NULL)
    f <- tryCatch(stats::glm(fml, data = d, family = glmFamily,
                             control = list(maxit = maxit)),
                  error = function(e) NULL, warning = function(w) NULL)
    if (is.null(f) || !isTRUE(f$converged)) return(NULL)
    f
  }

  fitFull <- fitOn(df)
  fitE    <- fitOn(df[half == 0L, , drop = FALSE])
  fitL    <- fitOn(df[half == 1L, , drop = FALSE])
  if (is.null(fitFull) || is.null(fitE) || is.null(fitL)) return(NULL)

  bFull <- stats::coef(fitFull); bE <- stats::coef(fitE); bL <- stats::coef(fitL)
  common <- Reduce(intersect, list(names(bFull), names(bE), names(bL)))
  common <- common[is.finite(bFull[common]) & is.finite(bE[common]) & is.finite(bL[common])]
  if (length(common) < 2) return(NULL)

  bCorr <- bFull
  bCorr[common] <- 2 * bFull[common] - 0.5 * (bE[common] + bL[common])
  list(coefficients = bCorr, corrected = common)
}
# nolint end
