#' @title computeNullLoglik
#' @description Calculates the log-likelihood of an intercept-only (null) model.
#'
#' @param df Data.frame. The evaluation dataset.
#' @param depVar Character. The name of the dependent variable.
#' @param stage Character. The stage of evaluation: \code{"adoption"} or \code{"stringency"}.
#' @param family Character. The GLM family to use for stringency stage.
#' @param useFirth Logical. Denotes whether Firth logistic regression should be applied.
#'
#' @return Numeric. The log-likelihood value.
#'
#' @author Renato Rodrigues
#' @keywords internal
#'
#' @importFrom stats as.formula glm binomial Gamma gaussian logLik
#' @importFrom logistf logistf
computeNullLoglik <- function(df, depVar, stage, family, useFirth) {
  nullFml <- stats::as.formula(paste(depVar, "~ 1"))
  if (stage == "adoption" && isTRUE(useFirth)) {
    nullFit <- logistf::logistf(nullFml, data = df)
    return(as.numeric(nullFit$loglik["full"]))
  } else if (stage == "adoption") {
    nullFit <- glm(nullFml, data = df, family = binomial(link = "logit"))
  } else {
    glmFamily <- if (family == "Gamma") Gamma(link = "log") else gaussian(link = "log")
    nullFit <- glm(nullFml, data = df, family = glmFamily)
  }
  as.numeric(stats::logLik(nullFit))
}
