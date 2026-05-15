#' @title coeftestToDataFrame
#' @description Strips S3 matrix protections and neatly coerces coefficient test results into a unified Data.frame.
#'
#' @param coeftestObj Matrix/coeftest object. The coefficient validation output to process.
#'
#' @return A tidy Data.frame featuring columns: \code{term}, \code{estimate}, \code{stdError}, \code{statistic}, \code{pValue}.
#'
#' @author Renato Rodrigues
#' @keywords internal
#'
#' @export
#'
coeftestToDataFrame <- function(coeftestObj) {
  termNames <- rownames(coeftestObj)
  # unclass removes the "coeftest" S3 class protecting the underlying matrix, ensuring robust coercion
  df <- as.data.frame(unclass(coeftestObj))
  df$term <- termNames
  rownames(df) <- NULL
  colnames(df) <- c("estimate", "stdError", "statistic", "pValue", "term")
  df <- df[, c("term", "estimate", "stdError", "statistic", "pValue")]
  return(df)
}
