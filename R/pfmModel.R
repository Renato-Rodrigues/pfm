#' @title PFMModel S3 class
#' @description Constructor, print, summary, and validate methods for the
#'   PFMModel class — the canonical persistence unit for fitted pfm models.
#'   See ADR 0003 for the full design rationale.
#' @name PFMModel
NULL

#' Create a new PFMModel object
#'
#' @param id Character. Short ID (first 12 chars of SHA-256 cache key).
#' @param id_full Character. Full SHA-256 cache key.
#' @param created_at Character. ISO 8601 UTC timestamp.
#' @param label Character. Optional human-readable label.
#' @param pfm_version Character. Package version at save time.
#' @param sector Character. \code{"Bulk"} or \code{"Diffuse"}.
#' @param stage Character. \code{"adoption"} or \code{"stringency"}.
#' @param formula Formula. The model formula.
#' @param family Character. Estimation family: \code{"logistf"}, \code{"Gamma"}, or \code{"gaussian"}.
#' @param training_years Integer vector length 2. \code{c(min_year, max_year)}.
#' @param useFirth Logical. Whether Firth bias reduction was applied.
#' @param training_data data.frame. The rows and columns passed to glm/logistf.
#' @param data_hash Character. SHA-256 of \code{training_data}.
#' @param model List or fit object. The raw logistf/glm fit.
#' @param coeftest Matrix. Robust coefficient table (estimate, SE, z, p).
#' @param vcov Matrix. Variance-covariance matrix.
#' @param fitted_values Numeric. In-sample fitted values.
#' @param correlations List with \code{pearson} and \code{spearman} matrices.
#' @param diagnostics List. See ADR 0003 for the full field list.
#' @param projections List or NULL. Scenario projections; populated by \code{\link{addProjections}}.
#'
#' @return An object of class \code{PFMModel}.
#'
#' @importFrom utils packageVersion
#' @keywords internal
newPFMModel <- function(id, id_full, created_at, label = "", pfm_version,
                        sector, stage, formula, family, training_years, useFirth,
                        data_hash, training_panel_hash = NA_character_,
                        model, coeftest, vcov, fitted_values,
                        correlations, diagnostics,
                        transforms = NULL, applyState = NULL,
                        projections = NULL) {
  structure(
    list(
      id             = id,
      id_full        = id_full,
      created_at     = created_at,
      label          = label,
      pfm_version    = pfm_version,
      sector         = sector,
      stage          = stage,
      formula        = formula,
      family         = family,
      training_years = training_years,
      useFirth       = useFirth,
      data_hash      = data_hash,
      # ADR 0009: training data is NOT embedded; it lives once in the content-
      # addressed Training Panel store and is referenced by hash.
      training_panel_hash = training_panel_hash,
      model          = model,        # data-bearing slots stripped (.stripFit); predict-capable
      coeftest       = coeftest,
      vcov           = vcov,
      fitted_values  = fitted_values,
      correlations   = correlations,
      diagnostics    = diagnostics,
      # ADR 0009: frozen transforms making the model self-contained for prediction
      # (GDP-Q fit, PCA rotation, driver scaling, regionFE levels, trend/spec params).
      transforms     = transforms,
      # ADR 0009: minimal state the lag recursion / clamp need at predict time
      # without the historical panel (per-region seed prices, in-sample max response).
      applyState     = applyState,
      projections    = projections
    ),
    class = "PFMModel"
  )
}

#' Strip data-bearing slots from a fitted glm/logistf so the saved object stays
#' small but remains usable for prediction on new data (ADR 0009). For newdata
#' predictions predict.glm needs terms/xlevels/coefficients/family/contrasts plus
#' the (k-pivot of the) \code{qr} decomposition; the dropped slots (model frame,
#' residuals, fitted/linear predictors, weights, y, effects, data, prior.weights)
#' each carry an O(n) copy of the training data. \code{qr} is deliberately kept —
#' predict.lm dereferences \code{qr$pivot} even when \code{se.fit = FALSE}.
#' @keywords internal
.stripFit <- function(fit) {
  if (is.null(fit)) return(NULL)
  heavy <- c("model", "data", "residuals", "effects", "linear.predictors",
             "weights", "prior.weights", "y", "fitted.values", "na.action",
             "predict")
  for (slot in heavy) if (!is.null(fit[[slot]])) fit[[slot]] <- NULL
  fit
}

#' Re-attach the cheap derived vectors a slim (stripped) fit no longer carries,
#' so in-session consumers (the results reports) that read \code{fit$model$y},
#' \code{fitted(fit$model)} or \code{logistf$predict} keep working after a cache
#' hit (ADR 0009). These are recomputed from the freshly-prepared estimation
#' \code{df} — no training data is read from disk. Fitted values are aligned to
#' the complete-case rows (\code{usedRows}), exactly as the original fit's were.
#' @keywords internal
.rehydrateFitForConsumers <- function(m, df, fml, depVar) {
  if (is.null(m)) return(m)
  # Rebuild the na.omit model frame exactly as glm/logistf would have, so it
  # carries the right rows AND the na.action attribute (consumers locate the
  # dropped rows via attr(m$model, "na.action")). Then y / fitted / residuals
  # are all aligned to those same complete-case rows.
  mf <- tryCatch(stats::model.frame(fml, data = df, na.action = stats::na.omit),
                 error = function(e) NULL)
  if (is.null(mf)) {
    v <- intersect(all.vars(fml), colnames(df))
    keep <- stats::complete.cases(df[, v, drop = FALSE])
    dfu <- df[keep, , drop = FALSE]
  } else {
    na <- attr(mf, "na.action")
    dfu <- if (!is.null(na) && length(na) > 0) df[-as.integer(na), , drop = FALSE] else df
    m$model <- mf
  }
  yv <- if (depVar %in% names(dfu)) as.numeric(dfu[[depVar]]) else NULL
  if (inherits(m, "logistf")) {
    tt <- stats::delete.response(stats::terms(fml))
    mm <- tryCatch(
      stats::model.matrix(tt, stats::model.frame(tt, dfu, na.action = stats::na.pass)),
      error = function(e) NULL)
    if (!is.null(mm)) {
      beta <- stats::coef(m)
      shared <- intersect(colnames(mm), names(beta))
      m$predict <- as.numeric(stats::plogis(mm[, shared, drop = FALSE] %*% beta[shared]))
    }
    if (!is.null(yv)) m$y <- yv
  } else {
    fv <- tryCatch(as.numeric(stats::predict(m, newdata = dfu, type = "response")),
                   error = function(e) NULL)
    if (!is.null(fv)) {
      m$fitted.values <- fv
      if (!is.null(yv) && length(yv) == length(fv)) m$residuals <- yv - fv
    }
    if (!is.null(yv)) m$y <- yv
  }
  m
}

#' @export
print.PFMModel <- function(x, ...) {
  cat("PFMModel\n")
  cat("  ID:      ", x$id, "\n", sep = "")
  cat("  Sector:  ", x$sector, " /", x$stage, "\n", sep = "")
  cat("  Formula: ", paste(deparse(x$formula, width.cutoff = 60), collapse = " "), "\n", sep = "")
  cat("  Created: ", x$created_at, "  (pfm ", x$pfm_version, ")\n", sep = "")
  diag <- x$diagnostics
  cat("  AIC:", round(diag$aic, 2),
      "  BIC:", round(diag$bic, 2),
      "  pseudoR2:", round(diag$pseudoR2, 4),
      "  n:", diag$nObs, "\n")
  flags <- c(
    if (isTRUE(diag$separation))   "separation",
    if (isTRUE(diag$highVIF))      "highVIF",
    if (isTRUE(diag$overfitting))  "overfitting",
    if (!isTRUE(diag$converged))   "not-converged"
  )
  if (length(flags) > 0) cat("  Flags:  ", paste(flags, collapse = ", "), "\n", sep = "")
  invisible(x)
}

#' @export
summary.PFMModel <- function(object, ...) {
  print(object)
  cat("\nCoefficients:\n")
  print(round(object$coeftest, 4))
  cat("\nDiagnostics:\n")
  diag <- object$diagnostics
  cat("  AIC:", round(diag$aic, 2), "  BIC:", round(diag$bic, 2),
      "  AICc:", round(diag$aicc, 2), "  HQIC:", round(diag$hqic, 2), "\n")
  cat("  loglik:", round(diag$loglik, 4), "  pseudoR2:", round(diag$pseudoR2, 4), "\n")
  cat("  nObs:", diag$nObs, "  nCountries:", diag$nCountries,
      "  nPredictors:", diag$nPredictors, "\n")
  if (!is.null(diag$vif$values) && length(diag$vif$values) > 0) {
    cat("  VIF (max:", round(diag$vif$maxVIF, 2), "):",
        paste(names(diag$vif$values), round(diag$vif$values, 2), sep = "=", collapse = ", "), "\n")
  }
  if (!is.null(object$projections)) cat("\nProjections: present\n")
  invisible(object)
}

#' Validate a PFMModel against the currently installed pfm version
#'
#' Issues a warning if the model was saved with a different version of pfm
#' than the one currently installed. Used by REMIND and the dashboard to
#' detect stale serialised objects before prediction.
#'
#' @param x A \code{PFMModel} object.
#' @param ... Unused.
#'
#' @return \code{x} invisibly.
#' @export
validate.PFMModel <- function(x, ...) {
  stopifnot(inherits(x, "PFMModel"))
  current <- as.character(utils::packageVersion("pfm"))
  if (!identical(x$pfm_version, current)) {
    warning(
      "PFMModel was saved with pfm ", x$pfm_version,
      " but pfm ", current, " is installed. ",
      "Predictions may differ. Re-fit and re-save if used in production.",
      call. = FALSE
    )
  }
  invisible(x)
}
