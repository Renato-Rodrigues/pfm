#' Build a model formula
#'
#' Constructs a formula for the adoption or stringency model. The right-hand
#' side is assembled from the provided driver lists, the Actor Power Index
#' interaction terms, pre-computed controls, a time trend, and region
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
#' @param timeTrend logical — if TRUE, adds a linear time trend (timeTrend) to the formula.
#'   Ignored when \code{logisticTimeTrend = TRUE}.
#' @param logisticTimeTrend logical — if TRUE, adds a logistic (S-curve) time trend
#'   (logisticTimeTrend) instead of the linear trend. Takes precedence over \code{timeTrend}.
#'
#' @return A \code{formula} object ready to pass to \code{glm}.
#'
#' @keywords internal
#' @export
buildModelFormula <- function(depVar, actorPowerDrivers, actorPowerIndex, # nolint: cyclocomp_linter.
                              instQualityDrivers, controlDrivers,
                              regionMappingFixedEffects,
                              timeTrend = TRUE,
                              logisticTimeTrend = FALSE,
                              interactRegionFE = FALSE,
                              useMundlak = FALSE,
                              gdpGovInteraction = FALSE) {
  rhs <- c()

  # Actor Power Main Effects: exactly what is passed in actorPowerDrivers
  if (!is.null(actorPowerDrivers) && length(actorPowerDrivers) > 0) {
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

  # Interaction: each actorPowerIndex × regionFE
  if (isTRUE(interactRegionFE) && !is.null(actorPowerIndex) && !is.null(regionMappingFixedEffects)) {
    for (api in actorPowerIndex) {
      # Use colon ':' for standard R interaction with factors
      rhs <- c(rhs, paste0(make.names(api), ":regionFE"))
    }
  }

  # Control variables
  if (!is.null(controlDrivers) && length(controlDrivers) > 0) {
    rhs <- c(rhs, make.names(controlDrivers))
  }

  # Time trend: logistic S-curve takes precedence over linear when both are TRUE.
  if (isTRUE(logisticTimeTrend)) {
    rhs <- c(rhs, "logisticTimeTrend")
  } else if (isTRUE(timeTrend)) {
    rhs <- c(rhs, "timeTrend")
  }

  # GDP × IQ interactions (when requested — pre-computed columns in preparePanelData)
  if (isTRUE(gdpGovInteraction) && !is.null(instQualityDrivers) && length(instQualityDrivers) > 0) {
    gdpSafe   <- make.names("GDP per Capita")
    gdpIqTerms <- paste0(gdpSafe, "_x_", make.names(instQualityDrivers))
    rhs <- c(rhs, gdpIqTerms)
  }

  # Mundlak correction: within-region group means replace FE dummies.
  # Adds <var>_grp_mean for each theory & control variable; suppresses regionFE.
  if (isTRUE(useMundlak)) {
    mundlak_safe <- unique(c(
      make.names(actorPowerIndex),
      make.names(instQualityDrivers),
      make.names(controlDrivers)
    ))
    mundlak_safe <- setdiff(mundlak_safe,
                            c("lagged_ecp", "lagged_adoption", "timeTrend",
                              "logisticTimeTrend"))
    rhs <- c(rhs, paste0(mundlak_safe, "_grp_mean"))
    # Suppress region FE under Mundlak
    regionMappingFixedEffects <- NULL
    interactRegionFE          <- FALSE
  }

  # Optional region fixed effects (suppressed when useMundlak = TRUE)
  if (!is.null(regionMappingFixedEffects)) {
    rhs <- c(rhs, "regionFE")
  }

  fmlStr <- paste(depVar, "~", paste(rhs, collapse = " + "))
  return(stats::as.formula(fmlStr))
}
