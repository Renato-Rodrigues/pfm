# nolint start
#' @title computePredictiveDiagnostics
#' @description Cheap, report-only Predictive Diagnostics (decided 2026-06-12).
#' Derived entirely from already-fitted values — no refits, no scenario runs —
#' so they are always computed. They play \strong{no role in model selection}:
#' no gates, no tie-breaks; they are surfaced in reports so a human can judge
#' whether a maximin winner is also usably calibrated for REMIND coupling.
#'
#' Adoption stage (in-sample, fitted probabilities):
#' \describe{
#'   \item{brier}{Mean squared error of predicted probabilities, \eqn{(1/N)\sum(p_i - y_i)^2}.
#'     Lower is better; compare against \code{brierBase} (Brier of always predicting
#'     the base rate).}
#'   \item{auc}{Area under the ROC curve (rank/Mann-Whitney form).}
#'   \item{calibrationSlope}{Slope of \code{glm(y ~ qlogis(p), binomial)}. 1 = perfectly
#'     calibrated; < 1 = overconfident (probabilities too extreme); > 1 = underconfident.}
#'   \item{baseRate}{Sample adoption (or onset) rate.}
#' }
#'
#' Stringency stage (in-sample, on the estimation scale — i.e. \code{log(1+ECP)}
#' or the FD change scale, whichever the model was fit on):
#' \describe{
#'   \item{rmse, mae}{Root-mean-square and mean absolute error of fitted vs observed.}
#'   \item{corObsPred}{Pearson correlation of observed and fitted values.}
#' }
#'
#' @param model A fitted model object: \code{logistf}, \code{glm}, or any object
#'   exposing \code{$y} and fitted values (\code{$predict} for logistf,
#'   \code{fitted()} otherwise).
#' @param stage Character. \code{"adoption"} or \code{"stringency"}.
#'
#' @return Named list of metrics (see above) plus \code{n}; \code{NULL} when the
#'   model is \code{NULL} or fitted values cannot be recovered.
#'
#' @importFrom stats fitted glm binomial qlogis cor
#'
#' @export
#' @author Renato Rodrigues
computePredictiveDiagnostics <- function(model, stage = c("adoption", "stringency")) {
  stage <- match.arg(stage)
  if (is.null(model)) return(NULL)

  y <- tryCatch(as.numeric(model$y), error = function(e) NULL)
  # logistf stores fitted probabilities in $predict, not $fitted.values
  p <- tryCatch({
    if (inherits(model, "logistf")) as.numeric(model$predict) else as.numeric(stats::fitted(model))
  }, error = function(e) NULL)
  if (is.null(y) || is.null(p) || length(y) == 0 || length(y) != length(p)) return(NULL)

  ok <- is.finite(y) & is.finite(p)
  y <- y[ok]
  p <- p[ok]
  n <- length(y)
  if (n < 3) return(NULL)

  if (stage == "adoption") {
    baseRate <- mean(y)
    brier <- mean((p - y)^2)
    brierBase <- mean((baseRate - y)^2)

    # Rank-based AUC (Mann-Whitney U)
    n1 <- sum(y == 1)
    n0 <- sum(y == 0)
    auc <- if (n1 > 0 && n0 > 0) {
      r <- rank(p, ties.method = "average")
      (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
    } else NA_real_

    # Calibration slope: logistic recalibration on the fitted linear predictor
    eps <- 1e-12
    lp <- stats::qlogis(pmin(pmax(p, eps), 1 - eps))
    calibrationSlope <- tryCatch({
      if (n1 > 0 && n0 > 0 && stats::var(lp) > 0) {
        suppressWarnings(
          unname(stats::glm(y ~ lp, family = stats::binomial())$coefficients["lp"])
        )
      } else NA_real_
    }, error = function(e) NA_real_)

    list(
      stage = "adoption", n = n, baseRate = baseRate,
      brier = brier, brierBase = brierBase,
      auc = auc, calibrationSlope = calibrationSlope
    )
  } else {
    err <- y - p
    list(
      stage = "stringency", n = n,
      rmse = sqrt(mean(err^2)),
      mae = mean(abs(err)),
      corObsPred = if (stats::var(y) > 0 && stats::var(p) > 0) stats::cor(y, p) else NA_real_
    )
  }
}
# nolint end
