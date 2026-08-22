# nolint start
#' Coefficients on a comparable footing (beta x SD)
#'
#' @description
#' Reports every coefficient of a fitted PSM/frontier model together with the
#' standard deviation of its own regressor in the estimation sample and the
#' product \eqn{\beta_k \cdot \mathrm{sd}(x_k)} — the change in the linear
#' predictor produced by a one-standard-deviation move in that regressor.
#'
#' \strong{Why this exists.} \code{\link{preparePanelData}} standardizes the
#' political and control drivers to mean 0 / sd 1, so their coefficients already
#' read "per SD" and are mutually comparable. It deliberately does \emph{not}
#' standardize the time trends or the region fixed effects
#' (\code{scaleExcl}), which therefore enter on their own raw scales. Comparing
#' a raw-scale coefficient against a per-SD one overstates the former by the
#' ratio of their standard deviations: the logistic trend moves over roughly
#' 0.09–0.35 in a 2000–2022 fit (sd ~0.08), so its coefficient is ~13x larger
#' than a per-SD coefficient of the same influence. Reading the raw table
#' invites the conclusion that the trend dwarfs the political drivers; on this
#' footing it does not.
#'
#' \strong{This is a reporting helper only.} It does not refit, rescale or
#' otherwise touch the model — \code{betaXsd} is computed from the stored
#' design. Ranking on \code{betaXsd} is the like-for-like comparison; ranking on
#' \code{estimate} is only meaningful among regressors that share a scale.
#'
#' \strong{In-sample influence is not projection leverage.} A regressor with a
#' small \code{betaXsd} can still dominate a projection if the scenario moves it
#' far outside its estimation range; \code{sdOutOfSample} reports that distance
#' when \code{scenarioData} is supplied. The trend is exempt from the
#' projection-time driver guard (\code{\link{.driverSupportRanges}} excludes it),
#' so it is the one regressor for which this column has no automatic backstop.
#'
#' @param fit A fitted model from \code{\link{estimatePolicyStringencyModel}}
#'   (any estimator), or a list carrying \code{model}/\code{formula}/\code{data}.
#' @param scenarioValues Optional named numeric vector giving a projection-time
#'   value per regressor (e.g. the design row at the projection horizon). When
#'   supplied, \code{sdOutOfSample} reports how many in-sample standard
#'   deviations that value sits beyond the estimation range (0 when inside).
#' @param digits Integer or \code{NULL}. Rounding for the returned numeric
#'   columns; \code{NULL} leaves them unrounded.
#'
#' @return A data.frame ordered by decreasing \code{abs(betaXsd)}, with columns
#'   \code{term}, \code{estimate}, \code{sd}, \code{betaXsd}, \code{scaled}
#'   (whether \code{preparePanelData} standardized this regressor) and, when
#'   \code{scenarioValues} is given, \code{sdOutOfSample}. The intercept and the
#'   frontier variance parameters (\code{sigmaSq}, \code{gamma}) are dropped.
#'
#' @examples
#' \dontrun{
#' fit <- estimatePolicyStringencyModel(panel, "Bulk", estimator = "frontier")
#' computeComparableCoefficients(fit)
#' }
#' @seealso \code{\link{preparePanelData}}, \code{\link{computeAME}}
#' @export
#' @author Renato Rodrigues
computeComparableCoefficients <- function(fit, scenarioValues = NULL, digits = 4) {
  if (is.null(fit$model) || is.null(fit$formula) || is.null(fit$data)) {
    stop("computeComparableCoefficients: `fit` must carry model, formula and data.")
  }
  b <- tryCatch(stats::coef(fit$model), error = function(e) NULL)
  if (is.null(b) && inherits(fit$model, "merMod")) b <- lme4::fixef(fit$model)
  if (is.null(b)) stop("computeComparableCoefficients: could not extract coefficients.")
  b <- b[!names(b) %in% c("sigmaSq", "gamma")]

  tt <- stats::delete.response(stats::terms(fit$formula))
  mm <- stats::model.matrix(
    tt, stats::model.frame(tt, data = fit$data, na.action = stats::na.pass))
  shared <- intersect(colnames(mm), names(b))
  if (length(shared) == 0) {
    stop("computeComparableCoefficients: no overlap between coefficients and design.")
  }
  sds <- apply(mm[, shared, drop = FALSE], 2, stats::sd, na.rm = TRUE)

  # Which columns preparePanelData actually standardized. Anything absent from
  # the stored scaling (trends, region FE, the raw-scale controls) is flagged so
  # a reader never ranks a raw coefficient against a per-SD one by accident.
  scaling <- fit$driverScaling %||% attr(fit$data, "driverScaling")
  scaledCols <- if (is.null(scaling)) character(0) else names(scaling)
  isScaled <- vapply(shared, function(cl) {
    base <- strsplit(cl, "_x_", fixed = TRUE)[[1]]
    length(base) > 0 && all(base %in% scaledCols)
  }, logical(1))

  out <- data.frame(
    term = shared,
    estimate = as.numeric(b[shared]),
    sd = as.numeric(sds),
    betaXsd = as.numeric(b[shared]) * as.numeric(sds),
    scaled = unname(isScaled),
    stringsAsFactors = FALSE
  )

  if (!is.null(scenarioValues)) {
    rng <- apply(mm[, shared, drop = FALSE], 2,
                 function(v) range(v[is.finite(v)], na.rm = TRUE))
    out$sdOutOfSample <- vapply(seq_along(shared), function(i) {
      # Only the regressors named in scenarioValues are audited; the rest are NA
      # rather than an error, so a caller may pass a single column of interest.
      v <- if (shared[i] %in% names(scenarioValues)) scenarioValues[[shared[i]]] else NULL
      s <- sds[[i]]
      if (is.null(v) || !is.finite(v) || !is.finite(s) || s <= 0) return(NA_real_)
      max(0, v - rng[2, i], rng[1, i] - v) / s
    }, numeric(1))
  }

  out <- out[out$term != "(Intercept)", , drop = FALSE]
  out <- out[order(-abs(out$betaXsd)), , drop = FALSE]
  rownames(out) <- NULL
  if (!is.null(digits)) {
    num <- vapply(out, is.numeric, logical(1))
    out[num] <- lapply(out[num], round, digits)
  }
  out
}
# nolint end
