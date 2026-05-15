#' @title computePseudoR2
#' @description Computes McFadden's Pseudo R-squared.
#'
#' @param loglikFull Numeric. Log-likelihood of the fitted full model.
#' @param loglikNull Numeric. Log-likelihood of the intercept-only null model.
#'
#' @return Numeric. The pseudo R-squared value, or \code{NA_real_} if null likelihood is missing or zero.
#'
#' @author Renato Rodrigues
#' @keywords internal
computePseudoR2 <- function(loglikFull, loglikNull) {
  if (is.na(loglikNull) || loglikNull == 0) {
    return(NA_real_)
  }
  1 - loglikFull / loglikNull
}
