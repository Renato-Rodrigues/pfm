# nolint start
#' Total primary energy per capita, aligned to a driver panel
#'
#' @description
#' Scales the share-based actor-power indices into per-capita levels
#' (see \code{design-notes/0001-actor-power-share-vs-level.md}). Used by
#' \code{\link{panelDataHistorical}} and \code{\link{panelDataScenario}} to feed
#' \code{\link{actorPowerIndex}(energyPerCapita = )}.
#'
#' \strong{Population must be the raw series}, not the panel's \code{"Population"}
#' driver: that one is a log-min-max index in \eqn{[0, 1]}, and dividing energy by an
#' index is not a per-capita quantity. Getting this wrong is the same class of error
#' as the aggregation-weight defect in \code{PITFALLS.md} §20, where normalised
#' indices were multiplied as if they were physical.
#'
#' Regions and years are intersected, so a population series that is shorter than the
#' energy panel silently restricts the result rather than recycling.
#'
#' @param iamData A magpie object carrying \code{"petotal"} (total primary energy),
#'   as returned by \code{\link{iamHistoricalData}} or
#'   \code{\link{downscaleREMINDResults}}.
#' @param population A magpie object of raw population, e.g.
#'   \code{calcOutput("Population", scenario = "SSP2")}.
#'
#' @return A magpie object of total primary energy per capita over the shared
#'   region-year support, or \code{NULL} when either input is missing
#'   \code{"petotal"} or shares no support — in which case
#'   \code{\link{actorPowerIndex}} simply emits no per-capita variants.
#'
#' @keywords internal
#' @author Renato Rodrigues
.energyPerCapita <- function(iamData, population) {
  if (is.null(iamData) || is.null(population)) return(NULL)
  if (!"petotal" %in% magclass::getNames(iamData)) return(NULL)

  regs <- intersect(magclass::getItems(iamData, dim = 1),
                    magclass::getItems(population, dim = 1))
  yrs  <- intersect(magclass::getYears(iamData), magclass::getYears(population))
  if (length(regs) == 0 || length(yrs) == 0) return(NULL)

  pe  <- magclass::collapseNames(iamData[regs, yrs, "petotal"])
  pop <- magclass::collapseNames(population[regs, yrs, ])
  epc <- pe / pop
  # A zero-population cell would otherwise produce Inf and poison the standardisation
  # of every downstream driver rather than just its own row.
  epc[!is.finite(epc)] <- NA_real_
  magclass::setNames(epc, NULL)
}
# nolint end
