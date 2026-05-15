#' @title computeVIF
#' @description Computes Variance Inflation Factors (VIF) for the predictors of a linear model, without requiring external dependencies like \code{car}.
#'
#' @param data A data.frame containing the predictor columns.
#' @param formula A \code{stats::formula} whose RHS defines the predictors to evaluate.
#' @param excludePattern Character. Regex pattern for columns to exclude from multicollinearity checks (e.g., regionFE dummies). Defaults to \code{"^regionFE|_x_"}.
#' @param vifThreshold Numeric. Threshold above which VIF is considered indicating high multicollinearity. Defaults to \code{10}.
#'
#' @return A named list containing:
#'   \describe{
#'     \item{values}{Named numeric vector of VIF values per predictor.}
#'     \item{maxVIF}{The absolute maximum VIF value observed.}
#'     \item{highVIF}{Logical: \code{TRUE} if any calculated VIF exceeds the \code{vifThreshold}, \code{FALSE} otherwise.}
#'     \item{flagged}{Character vector containing the names of predictors exceeding the threshold.}
#'   }
#'   Returns \code{NULL} if VIF cannot be reliably computed.
#'
#' @author Renato Rodrigues
#' @keywords internal
#'
#' @importFrom stats model.matrix complete.cases lm.fit
computeVIF <- function(data, formula, excludePattern = "^regionFE|_x_", vifThreshold = 10) {
  # Generate the full design matrix
  mm <- tryCatch(
    stats::model.matrix(formula, data = data),
    error = function(e) NULL
  )
  if (is.null(mm)) return(NULL)

  # Remove "(Intercept)" column explicitly
  intCol <- which(colnames(mm) == "(Intercept)")
  if (length(intCol) > 0) mm <- mm[, -intCol, drop = FALSE]

  # Optionally filter out Fixed Effects / categorical dummies matching the exclusion regex
  if (!is.null(excludePattern) && ncol(mm) > 0) {
    keep <- !grepl(excludePattern, colnames(mm))
    mm <- mm[, keep, drop = FALSE]
  }

  p <- ncol(mm)
  if (p < 2) return(NULL)

  # Purge missing cases to ensure balanced matrix algebra downstream
  complete <- stats::complete.cases(mm)
  mm <- mm[complete, , drop = FALSE]
  if (nrow(mm) < p + 1) return(NULL)

  vifs <- numeric(p)
  names(vifs) <- colnames(mm)

  # Manually compute R^2 for each predictor regressed against all other predictors
  for (j in seq_len(p)) {
    yJ <- mm[, j]
    xJ <- mm[, -j, drop = FALSE]
    
    # Use lm.fit() for raw speed without S3 overhead
    fitJ <- stats::lm.fit(cbind(1, xJ), yJ)
    
    # R^2 calculation
    ssRes <- sum(fitJ$residuals^2)
    ssTot <- sum((yJ - mean(yJ))^2)
    r2J <- if (ssTot > 0) 1 - (ssRes / ssTot) else 0
    
    # VIF formula: 1 / (1 - R^2)
    vifs[j] <- if (r2J < 1) 1 / (1 - r2J) else Inf
  }

  list(
    values  = vifs,
    maxVIF  = max(vifs, na.rm = TRUE),
    highVIF = any(vifs > vifThreshold, na.rm = TRUE),
    flagged = names(vifs[vifs > vifThreshold])
  )
}
