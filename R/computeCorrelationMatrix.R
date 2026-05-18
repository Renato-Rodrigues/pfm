#' @title computeCorrelationMatrix
#' @description Computes a correlation matrix for a given list of drivers from panel data.
#'
#' @param data A \code{data.frame} or a \code{magpie} object (panel data).
#' @param drivers Character vector of driver names to include in the correlation matrix.
#' @param method Character. Correlation method: "pearson", "spearman", or "kendall". Defaults to "pearson".
#' @param use Character. Handling of missing values: "everything", "all.obs",
#'   "complete.obs", "na.or.complete", or "pairwise.complete.obs".
#'   Defaults to "pairwise.complete.obs".
#'
#' @return A data.frame representing the correlation matrix with driver names as the first column.
#'
#' @author Renato Rodrigues
#' @export
#'
#' @importFrom stats cor
#' @importFrom quitte as.quitte
#' @importFrom tidyr pivot_wider
computeCorrelationMatrix <- function(data, drivers, method = "pearson", use = "pairwise.complete.obs") {
  # Handle magpie objects
  if (inherits(data, "magpie")) {
    # Ensure the 3rd dimension is named 'variable' for quitte conversion.
    # This ensures that drivers in the 3rd dimension are mapped to the 'variable' column.
    magclass::getSets(data)[3] <- "variable"
    data <- quitte::as.quitte(data)
    # Filter to requested drivers to speed up pivoting
    data <- data[data$variable %in% drivers, ]
    data <- tidyr::pivot_wider(data, names_from = "variable", values_from = "value")
  }

  # Ensure drivers exist in data
  missingDrivers <- setdiff(drivers, colnames(data))
  if (length(missingDrivers) > 0) {
    stop("The following drivers are not in the data: ", paste(missingDrivers, collapse = ", "))
  }

  # Calculate correlation matrix
  corMat <- stats::cor(data[, drivers, drop = FALSE], method = method, use = use)

  # Convert to data frame
  corDf <- as.data.frame(corMat)

  # Add driver names as the first column
  corDf <- cbind(Driver = rownames(corDf), corDf)
  rownames(corDf) <- NULL

  return(corDf)
}
