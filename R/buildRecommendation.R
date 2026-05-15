#' @title buildRecommendation
#' @description Formulates a concise textual synthesis for the best model recommendation.
#'
#' @param selectionPath Data.frame. The path array containing metrics for all evaluated steps.
#' @param bestStep Integer. The row index identifying the best model.
#' @param criterion Character. The selection criterion used to rank models.
#' @param allModelsList List. Container holding all fitted model components.
#'
#' @return Character string containing the finalized recommendation text.
#'
#' @author Renato Rodrigues
#' @keywords internal
buildRecommendation <- function(selectionPath, bestStep, criterion, allModelsList) {
  best <- selectionPath[bestStep, ]
  warnings <- c()
  if (best$separation) warnings <- c(warnings, "WARNING: Best model shows quasi-complete separation")
  if (best$overfitting) {
    warnings <- c(warnings, paste0(
      "WARNING: k/n = ", best$kOverN,
      " suggests potential overfitting (k/n > 0.1)"
    ))
  }
  rec <- paste0(
    "RECOMMENDATION: Step ", bestStep, " (", best$phase, ": ", best$description, ")\n",
    "  Model: ", best$formula, "\n",
    "  Criteria -> AIC: ", round(best$aic, 2),
    " | BIC: ", round(best$bic, 2),
    " | AICc: ", round(best$aicc, 2),
    " | HQIC: ", round(best$hqic, 2),
    " | Pseudo-R2: ", round(best$pseudoR2, 4), "\n",
    "  k = ", best$nPredictors, " | ", best$nSignificant, " significant predictors"
  )
  if (length(warnings) > 0) rec <- paste0(rec, "\n  ", paste(warnings, collapse = "\n  "))
  rec
}
