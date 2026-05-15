#' @title performGroupAnalysis
#' @description Analyzes the drop-one block variance contribution and formal ANOVA statistics for the final evaluated model.
#'
#' @param bestModel List. The final selected model object structure from the modelSelection sequence.
#' @param df Data.frame. The dataset used for fitting.
#' @param depVar Character. The dependent variable name.
#' @param stage Character. The overarching pipeline stage: \code{"adoption"} or \code{"stringency"}.
#' @param family Character. The stringency-stage GLM family name.
#' @param useFirth Logical. Flag for using Firth's penalized likelihood logistic regression.
#' @param nullLoglik Numeric. Log-likelihood of the intercept-only null model.
#' @param n Integer. Number of dataset observations.
#' @param timeTrend Logical. Flag indicating if a baseline linear time trend was factored.
#' @param .msg Function. Diagnostic messaging output function.
#'
#' @return A nested nested list detailing pseudo-R2 change, test statistics, and significance probabilities per predictor block mapping.
#'
#' @author Renato Rodrigues
#' @keywords internal
#'
#' @importFrom stats terms as.formula anova deviance
performGroupAnalysis <- function(bestModel, df, depVar, stage, family, useFirth, nullLoglik, n, timeTrend, .msg) {
  .msg("\n============================================================")
  .msg("FINAL MODEL GROUP CONTRIBUTION ANALYSIS (DROP-ONE & ANOVA)")
  .msg("============================================================")

  groups <- list()
  if (!is.null(bestModel$iq)) groups[["Institutional Quality"]] <- bestModel$iq
  if (!is.null(bestModel$ap)) {
    groups[["Actor Power"]] <- bestModel$ap
  } else if (!is.null(bestModel$api)) {
    groups[["Actor Power Index"]] <- bestModel$api
  }

  interact <- c()
  if (!is.null(bestModel$api) && !is.null(bestModel$iq)) {
    interact <- paste0(make.names(bestModel$api), "_x_", make.names(bestModel$iq))
  }
  if (length(interact) > 0) groups[["Interaction Term"]] <- interact
  if (!is.null(bestModel$ctrl)) groups[["Control"]] <- bestModel$ctrl
  if (!is.null(bestModel$feLevels)) {
    if (identical(bestModel$feLevels, "regionFE")) {
      groups[["Region Fixed Effects"]] <- "regionFE"
    } else {
      groups[["Region Fixed Effects"]] <- paste0("regionFE_", make.names(bestModel$feLevels))
    }
  }
  if (isTRUE(timeTrend)) groups[["Time Trend"]] <- "timeTrend"

  fullFit <- bestModel$model
  fullR2 <- bestModel$pseudoR2

  fullTerms <- attr(stats::terms(bestModel$formula), "term.labels")

  analysisRes <- list()

  for (grpName in names(groups)) {
    grpTermsRaw <- groups[[grpName]]
    grpTerms <- make.names(grpTermsRaw)

    if (grpName == "Region Fixed Effects" && identical(grpTermsRaw, "regionFE")) grpTerms <- "regionFE"
    if (grpName == "Region Fixed Effects" && !identical(grpTermsRaw, "regionFE")) grpTerms <- grpTermsRaw
    if (grpName == "Interaction Term") grpTerms <- grpTermsRaw
    if (grpName == "Time Trend") grpTerms <- "timeTrend"

    keepTerms <- setdiff(fullTerms, grpTerms)
    if (length(keepTerms) == 0) keepTerms <- "1"

    dropFml <- stats::as.formula(paste(depVar, "~", paste(keepTerms, collapse = " + ")))
    dropRes <- fitAndDiagnose(dropFml, df, depVar, stage, family, useFirth, nullLoglik, n)
    dropFit <- dropRes$model

    deltaR2 <- fullR2 - dropRes$pseudoR2

    pval <- NA
    chisq <- NA

    if (stage == "adoption" && isTRUE(useFirth)) {
      try(
        {
          anovaRes <- anova(fullFit, dropFit)
          pval <- anovaRes$pval
          chisq <- anovaRes$chisq
        },
        silent = TRUE
      )
    } else {
      try(
        {
          anovaRes <- stats::anova(dropFit, fullFit, test = "Chisq")
          pval <- anovaRes[["Pr(>Chi)"]][2]
          chisq <- max(abs(anovaRes[["Deviance"]]), na.rm = TRUE)
        },
        silent = TRUE
      )
    }

    # Count predictors and significant ones in this group
    relevantCoefs <- bestModel$coeftest[intersect(rownames(bestModel$coeftest), grpTerms), , drop = FALSE]
    nPred <- nrow(relevantCoefs)
    nSig <- sum(relevantCoefs[, 4] < 0.05, na.rm = TRUE)

    analysisRes[[grpName]] <- list(
      dropR2 = dropRes$pseudoR2,
      deltaR2 = deltaR2,
      chisq = chisq,
      pval = pval,
      nPred = nPred,
      nSig = nSig
    )

    sigLabel <- if (!is.null(pval) && !is.na(pval)) {
      if (pval < 0.05) " (Significant at p < 0.05)" else " (Non Significant at p > 0.05)"
    } else {
      ""
    }

    .msg(paste0(
      "Group: ", grpName, "\n",
      "  Contribution to Pseudo-R2: ", round(deltaR2 * 100, 2), "% ",
      "(Drops from ", round(fullR2, 4), " to ", round(dropRes$pseudoR2, 4), ")\n",
      "  ANOVA (Deviance): Chi-Sq = ", if (is.null(chisq) || is.na(chisq)) "NA" else round(chisq, 2),
      ", p-value = ", if (is.null(pval) || is.na(pval)) "NA" else signif(pval, 4),
      sigLabel
    ))
  }

  return(analysisRes)
}
