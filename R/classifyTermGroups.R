# nolint start
#' @title classifyTermGroups
#' @description Maps a character vector of model term names (as they appear in a model
#'   matrix) to canonical Term Group names used throughout pfm and pfm-reports:
#'   \code{"Intercept"}, \code{"Actor Power"}, \code{"Inst. Quality"},
#'   \code{"Interaction"}, \code{"Controls"}, \code{"Time Trend"},
#'   \code{"Path Dep."}, \code{"Region FE"}, \code{"Other"}.
#'   Matching order: Intercept, Time Trend, Path Dep., Region FE, Interaction,
#'   Inst. Quality, Controls, Actor Power, Other. Actor Power is last so explicit
#'   driver lists take priority over the regex fallback.
#' @param terms Character vector of term names (e.g. from \code{colnames(model.matrix(...))}).
#' @param actorPowerDrivers Character vector or NULL. Original (un-safe-named) Actor Power driver names.
#' @param actorPowerIndex Character or NULL. Original Actor Power Index variable name.
#' @param instQualityDrivers Character vector or NULL.
#' @param controlDrivers Character vector or NULL.
#' @return Character vector the same length as \code{terms}, each element one of the
#'   canonical Term Group names.
#' @export
classifyTermGroups <- function(terms,
                               actorPowerDrivers  = NULL,
                               actorPowerIndex    = NULL,
                               instQualityDrivers = NULL,
                               controlDrivers     = NULL) {
  apSafe   <- make.names(c(actorPowerDrivers, actorPowerIndex))
  iqSafe   <- make.names(instQualityDrivers)
  ctrlSafe <- make.names(controlDrivers)

  vapply(terms, function(t) {
    if (t == "(Intercept)")                                      return("Intercept")
    if (t == "timeTrend")                                        return("Time Trend")
    if (t == "adoption_lagged" || grepl("lagged_adoption", t))  return("Path Dep.")
    if (grepl("^regionFE", t))                                   return("Region FE")
    if (grepl("_x_", t))                                        return("Interaction")
    if (length(iqSafe) > 0 && t %in% iqSafe)                   return("Inst. Quality")
    if (length(ctrlSafe) > 0 && t %in% ctrlSafe)               return("Controls")
    if ((length(apSafe) > 0 && t %in% apSafe) ||
        grepl("Actor\\.Power|Innovator\\.Power|Incumbent\\.Power", t))
                                                                 return("Actor Power")
    "Other"
  }, character(1))
}
# nolint end
