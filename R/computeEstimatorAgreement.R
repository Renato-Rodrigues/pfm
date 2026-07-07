# nolint start
#' Estimator-agreement table for the Policy Stringency Model
#'
#' Re-estimates one PSM specification under every member of the
#' PSM Estimator Suite (see CONTEXT.md; satP, fractional logit, beta regression,
#' levels-gaussian benchmark) and assembles the cross-estimator comparison that
#' backs the paper's robustness claim: \emph{the political-economy channels are
#' estimator-invariant}. Estimators are compared on coefficient signs, natural-scale
#' AMEs and scale-free fit metrics (RMSE/correlation on the 0-\code{indexMax}
#' scale) — \strong{never on AIC/BIC} (the fractional logit has no likelihood, and
#' information criteria are not comparable across response transforms).
#'
#' @param data A \code{magpie} object or an already-assembled (untransformed)
#'   panel \code{data.frame}; passed to
#'   \code{\link{estimatePolicyStringencyModel}} per estimator. Passing a
#'   data.frame guarantees all estimators see identical rows and avoids repeated
#'   panel preparation.
#' @param sector Character. \code{"Bulk"} or \code{"Diffuse"}.
#' @param estimators Character vector; suite members to fit. Defaults to the full
#'   suite. An unavailable member (e.g. \code{"beta"} without the optional
#'   \code{betareg} package) or a failing fit is skipped and recorded in
#'   \code{$skipped}, not an error.
#' @param indexMax Numeric. Structural ceiling of the index (CAPMF: 10).
#' @param verbose Logical.
#' @param ... Specification arguments forwarded to
#'   \code{\link{estimatePolicyStringencyModel}} (drivers, FE, trend flags, ...).
#'   Persistence is disabled for these one-off refits (\code{modelDir = NULL}).
#'
#' @return A list with:
#'   \describe{
#'     \item{fits}{Named list of the per-estimator fit results.}
#'     \item{table}{Tidy per-term comparison: estimator, term, estimate (link scale),
#'       clustered SE, z, p, sign, plus the natural-scale AME and its delta-method SE.}
#'     \item{fitStats}{Per-estimator: n, converged, family, AIC/BIC (NA where no
#'       likelihood exists — reported for transparency, not comparison), natural-scale
#'       in-sample RMSE and correlation, boundary shares.}
#'     \item{agreement}{Per-term sign agreement across the fitted suite
#'       (region FE dummies and intercept excluded): the estimate signs, whether all
#'       agree, and in how many estimators the term is significant at the 5 percent level.}
#'     \item{skipped}{Named character vector of estimators that could not be fit.}
#'   }
#'
#' @author Renato Rodrigues
#'
#' @importFrom stats AIC BIC cor
#'
#' @export
computeEstimatorAgreement <- function(data,
                                      sector = "Bulk",
                                      estimators = c("satP", "fractional", "beta",
                                                     "levels", "satP-re"),
                                      indexMax = 10,
                                      verbose = TRUE,
                                      ...) {
  estimators <- match.arg(estimators, c("satP", "fractional", "beta", "levels", "satP-re"),
                          several.ok = TRUE)
  fits <- list()
  skipped <- character(0)
  for (e in estimators) {
    fits[[e]] <- tryCatch(
      estimatePolicyStringencyModel(
        data = data, sector = sector, estimator = e, indexMax = indexMax,
        modelDir = NULL, verbose = FALSE, ...
      ),
      error = function(err) {
        skipped[[e]] <<- conditionMessage(err)
        if (isTRUE(verbose)) {
          message("  [agreement] skipping '", e, "': ", conditionMessage(err))
        }
        NULL
      }
    )
  }
  fits <- fits[!vapply(fits, is.null, logical(1))]
  if (length(fits) == 0) {
    stop("computeEstimatorAgreement: no estimator could be fit.")
  }

  dropTerm <- function(terms) {
    terms == "(Intercept)" | grepl("^regionFE", terms)
  }

  # --- per-term comparison table ------------------------------------------------
  tableRows <- lapply(names(fits), function(e) {
    f <- fits[[e]]
    ct <- f$coeftest
    terms <- rownames(ct)
    keep <- !dropTerm(terms)
    ame <- f$ameIndex
    ameEst <- if (!is.null(ame)) stats::setNames(ame$ame, ame$term) else NULL
    ameSe <- if (!is.null(ame)) stats::setNames(ame$se, ame$term) else NULL
    data.frame(
      estimator = e,
      term = terms[keep],
      estimate = as.numeric(ct[keep, 1]),
      se = as.numeric(ct[keep, 2]),
      z = as.numeric(ct[keep, 3]),
      p = as.numeric(ct[keep, 4]),
      sign = sign(as.numeric(ct[keep, 1])),
      ameIndex = if (!is.null(ameEst)) as.numeric(ameEst[terms[keep]]) else NA_real_,
      ameSE = if (!is.null(ameSe)) as.numeric(ameSe[terms[keep]]) else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  table <- do.call(rbind, tableRows)
  rownames(table) <- NULL

  # --- per-estimator fit statistics ----------------------------------------------
  fitStats <- do.call(rbind, lapply(names(fits), function(e) {
    f <- fits[[e]]
    yNat <- f$outcomeNatural
    muNat <- tryCatch(.psmNaturalFitted(f$model, e, indexMax), error = function(err) NULL)
    rmse <- corNat <- NA_real_
    if (!is.null(muNat) && !is.null(yNat)) {
      # Align on the estimation rows by model-frame row label (glm/betareg drop
      # NA-driver rows internally, e.g. the first lag year).
      yUse <- if (!is.null(names(muNat)) && all(names(muNat) %in% names(yNat))) {
        yNat[names(muNat)]
      } else if (length(yNat) == length(muNat)) {
        yNat
      } else {
        NULL
      }
      if (!is.null(yUse)) {
        rmse <- sqrt(mean((yUse - muNat)^2, na.rm = TRUE))
        corNat <- suppressWarnings(stats::cor(yUse, muNat, use = "complete.obs"))
      }
    }
    data.frame(
      estimator = e,
      family = f$family,
      # nobs(model), NOT nrow(f$data): the prepared panel keeps first-lag-year rows
      # whose lagged drivers are NA and the fitter drops them internally, so nrow()
      # over-reports (575 vs 550 in the first real run — R12, 2026-07-06). nobs()
      # matches the sweep's nObs exactly.
      n = tryCatch(as.integer(stats::nobs(f$model)), error = function(err) nrow(f$data)),
      converged = isTRUE(f$converged),
      # Reported for transparency only — NEVER comparable across estimators
      # (different response scales; the fractional logit has no likelihood).
      aic = tryCatch(as.numeric(stats::AIC(f$model)), error = function(err) NA_real_),
      bic = tryCatch(as.numeric(stats::BIC(f$model)), error = function(err) NA_real_),
      rmseNatural = rmse,
      corNatural = corNat,
      shareAtZero = if (!is.null(f$boundaryShares)) f$boundaryShares[["atZero"]] else NA_real_,
      shareAtMax = if (!is.null(f$boundaryShares)) f$boundaryShares[["atMax"]] else NA_real_,
      stringsAsFactors = FALSE
    )
  }))
  rownames(fitStats) <- NULL

  # --- sign agreement across the fitted suite ------------------------------------
  agreement <- do.call(rbind, lapply(split(table, table$term), function(tt) {
    data.frame(
      term = tt$term[[1]],
      nEstimators = nrow(tt),
      signs = paste(ifelse(tt$sign > 0, "+", ifelse(tt$sign < 0, "-", "0")), collapse = ""),
      signsAgree = length(unique(tt$sign[tt$sign != 0])) <= 1,
      nSignificant05 = sum(tt$p < 0.05, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }))
  rownames(agreement) <- NULL

  list(
    fits = fits,
    table = table,
    fitStats = fitStats,
    agreement = agreement,
    skipped = skipped,
    # Marginal effect of each AP term over the observed moderator support (R2) —
    # the exhibit that shows where the interaction claims are identified and
    # where a scenario extrapolates them. satP engine fit only.
    meSupport = if ("satP" %in% names(fits)) {
      tryCatch(computeMarginalEffectSupport(fits[["satP"]]), error = function(err) NULL)
    } else NULL
  )
}
# nolint end
