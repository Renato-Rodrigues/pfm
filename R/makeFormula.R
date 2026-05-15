#' @title makeFormula
#' @description Dynamically constructs a GLM regression formula based on specified predictors.
#'
#' @param depVar Character. The dependent variable name.
#' @param api Character vector. Actor Power Index name(s) to include.
#' @param iq Character vector. Institutional Quality driver names to include.
#' @param ap Character vector. Actor Power driver names to include.
#' @param ctrl Character vector. Control variables to include.
#' @param feLevels Character vector. Specific region fixed effects levels to include, or \code{"regionFE"} for all levels.
#' @param timeTrend Logical. If \code{TRUE}, adds a linear time trend to the model.
#'
#' @return A \code{formula} object representing the regression model.
#'
#' @author Renato Rodrigues
#' @keywords internal
#'
#' @importFrom stats as.formula
makeFormula <- function(depVar, api, iq, ap, ctrl, feLevels, timeTrend) {
  rhs <- c()

  if (is.null(ap) && !is.null(api)) {
    rhs <- c(rhs, make.names(api))
  } else if (!is.null(ap)) {
    rhs <- c(rhs, make.names(ap))
  }

  if (!is.null(iq)) {
    rhs <- c(rhs, make.names(iq))
  }

  if (!is.null(api) && !is.null(iq)) {
    rhs <- c(rhs, paste0(make.names(api), "_x_", make.names(iq)))
  }

  if (!is.null(ctrl)) {
    rhs <- c(rhs, make.names(ctrl))
  }

  if (isTRUE(timeTrend)) {
    rhs <- c(rhs, "timeTrend")
  }

  if (!is.null(feLevels)) {
    if (identical(feLevels, "regionFE")) {
      rhs <- c(rhs, "regionFE")
    } else {
      for (lvl in feLevels) {
        rhs <- c(rhs, paste0("regionFE_", make.names(lvl)))
      }
    }
  }

  if (length(rhs) == 0) {
    rhs <- "1"
  }

  stats::as.formula(paste(depVar, "~", paste(rhs, collapse = " + ")))
}
