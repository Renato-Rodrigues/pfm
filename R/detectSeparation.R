#' @title detectSeparation
#' @description Identifies potential quasi-complete separation issues in fitted GLMs.
#'
#' @param fit The model fit object (\code{glm} or \code{logistf}).
#' @param robustTest Matrix/Data.frame. Output from the coefficient test containing estimates.
#'
#' @return Logical. \code{TRUE} if separation is detected, otherwise \code{FALSE}.
#'
#' @author Renato Rodrigues
#' @keywords internal
#'
#' @importFrom stats family
detectSeparation <- function(fit, robustTest) {
  coefs <- robustTest[, 1]
  isBinomial <- (inherits(fit, "glm") && stats::family(fit)$family == "binomial") ||
    inherits(fit, "logistf")

  # For binomial models, very large coefficients often indicate separation.
  # For Gamma/Gaussian, we allow them unless they are truly extreme (1e6)
  # suggesting a failure of the inverse link function to converge properly.
  limit <- if (isBinomial) 10 else 1e6
  if (any(abs(coefs) > limit, na.rm = TRUE)) {
    return(TRUE)
  }

  if (inherits(fit, "glm") && stats::family(fit)$family == "binomial") {
    preds <- fit$fitted.values
    if (any(preds < 1e-6 | preds > (1 - 1e-6), na.rm = TRUE)) {
      return(TRUE)
    }
  }

  # Also catch non-converged GLMs
  if (inherits(fit, "glm") && !isTRUE(fit$converged)) {
    return(TRUE)
  }

  FALSE
}
