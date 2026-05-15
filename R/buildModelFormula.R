#' Build a model formula
#'
#' Constructs a formula for the adoption or stringency model. The right-hand
#' side is assembled from the provided driver lists, the Actor Power Index
#' interaction terms, pre-computed controls, a linear time trend, and region
#' fixed effects.
#'
#' @param depVar character, name of the dependent variable column in the df
#' @param actorPowerDrivers Character vector of individual Actor Power driver
#'   names. If \code{actorPowerIndex} is in this list, it takes priority as
#'   the sole Actor Power main effect. Otherwise, all drivers in this list
#'   are included as individual main effects.
#' @param actorPowerIndex Character or NULL. Name of the Actor Power Index
#'   variable. If provided, it is used for interaction terms. It only acts
#'   as a main effect if its name is also in \code{actorPowerDrivers}.
#' @param instQualityDrivers character vector — institutional quality main effects
#' @param controlDrivers character vector — control variable main effects
#' @param regionMappingFixedEffects character or NULL — if non-NULL, adds regionFE to the formula
#' @param timeTrend logical — if TRUE, adds a linear time trend (timeTrend) to the formula
#'
#' @return A \code{formula} object ready to pass to \code{glm}.
#'
#' @keywords internal
#' @export
buildModelFormula <- function(depVar, actorPowerDrivers, actorPowerIndex,
                              instQualityDrivers, controlDrivers,
                              regionMappingFixedEffects,
                              timeTrend = TRUE) {
  rhs <- c()

  # 1. If actorPowerIndex is explicitly in the actorPowerDrivers list,
  #    it takes priority as the sole Actor Power main effect.
  # 2. Otherwise, use individual drivers as main effects.
  if (!is.null(actorPowerIndex) &&
    !is.null(actorPowerDrivers) &&
    any(actorPowerIndex %in% actorPowerDrivers)) {
    # Include all api indices that are requested as main effects
    rhs <- c(rhs, make.names(intersect(actorPowerIndex, actorPowerDrivers)))
  } else if (!is.null(actorPowerDrivers) && length(actorPowerDrivers) > 0) {
    rhs <- c(rhs, make.names(actorPowerDrivers))
  }

  # Institutional Quality main effects
  if (!is.null(instQualityDrivers) && length(instQualityDrivers) > 0) {
    rhs <- c(rhs, make.names(instQualityDrivers))
  }

  # Interaction: each actorPowerIndex × each institutional quality variable
  if (!is.null(actorPowerIndex) && !is.null(instQualityDrivers) &&
    length(instQualityDrivers) > 0) {
    for (api in actorPowerIndex) {
      intTerms <- paste0(make.names(api), "_x_", make.names(instQualityDrivers))
      rhs <- c(rhs, intTerms)
    }
  }

  # Control variables
  if (!is.null(controlDrivers) && length(controlDrivers) > 0) {
    rhs <- c(rhs, make.names(controlDrivers))
  }

  # Robustness: linear time trend
  if (isTRUE(timeTrend)) {
    rhs <- c(rhs, "timeTrend")
  }

  # Optional region fixed effects
  if (!is.null(regionMappingFixedEffects)) {
    rhs <- c(rhs, "regionFE")
  }

  fmlStr <- paste(depVar, "~", paste(rhs, collapse = " + "))
  return(stats::as.formula(fmlStr))
}
