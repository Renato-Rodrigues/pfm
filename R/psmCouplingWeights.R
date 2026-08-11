# nolint start
#' Country-level final-energy weights for the coupling
#'
#' @description
#' A price bound governs a region's carbon price, so the aggregation from countries to
#' IAM regions should be weighted by what the price actually acts on. Emissions are the
#' ideal weight; no emissions series exists in the training panel, and
#' \strong{final energy is the closest available correlate} — far closer than GDP,
#' since it already embeds how energy-intensive an economy is.
#'
#' \strong{Why not the old GDP x energy-intensity construction.} That product is
#' dimensionally energy (intensity is energy/GDP), so it was never really a
#' GDP-driven weight — but it reconstructs energy from two \emph{normalised} panel
#' variables, which is a detour through two transformations when the underlying series
#' is available directly. This function takes \code{fe_total} from
#' \code{\link{iamHistoricalData}} instead: the same country-level final energy
#' \pkg{mrpfm} already supplies, in its own units, untransformed.
#'
#' \strong{Projection: scaled by GDP growth, per scenario.} Weights are only ever used
#' for \emph{relative} within-region shares, so their level and units are irrelevant —
#' only the cross-country pattern matters. \code{scaleBy = "gdp"} (default) grows each
#' country's observed final energy by its GDP growth between the base year and
#' \code{year}, under the named \code{scenario}.
#'
#' \strong{Why scenario-aware, and what it does \emph{not} claim.} Different SSPs put
#' growth in different places, and it is that \emph{differential} — Nigeria versus
#' Germany inside the same IAM region — that moves a within-region weight. Uniform
#' growth changes nothing, because the shares are normalised; only the spread matters.
#' The assumption being made is that final energy grows \emph{with} GDP, i.e. constant
#' energy intensity. That is wrong in level terms — intensity generally falls — but the
#' error largely cancels in a ratio, since intensity declines broadly across countries.
#' Where it would not cancel is a region mixing very different development stages, and
#' that is the case to watch.
#'
#' \code{scaleBy = "none"} carries the last observation forward unchanged, which is the
#' conservative option when the aggregation year is close to the last observation.
#'
#' @param year Year the weights are for. Default \code{NULL} = the last observed year.
#' @param scaleBy \code{"gdp"} (default) scales the last observed final energy by GDP
#'   growth from the base year to \code{year}; \code{"none"} holds it constant.
#' @param scenario SSP used for the GDP path, e.g. \code{"SSP2"} (default),
#'   \code{"SSP1"}, \code{"SSP5"}. Must match the scenario the coupling is running,
#'   or the weights describe a different world than the model does.
#' @param histData Optional pre-loaded \code{\link{iamHistoricalData}} output, to avoid
#'   re-reading it.
#' @param gdp Optional GDP magpie, needed only for \code{scaleBy = "gdp"}.
#' @param verbose Logical.
#'
#' @return Named numeric vector (names = ISO3) suitable for the \code{weights}
#'   argument of \code{\link{aggregateFeasibilityToRegions}}. Attributes:
#'   \code{"baseYear"}, \code{"scaleBy"}, \code{"source"}.
#'
#' @seealso \code{\link{aggregateFeasibilityToRegions}}, \code{\link{iamHistoricalData}}.
#' @export
#' @author Renato Rodrigues
psmCouplingWeights <- function(year = NULL, scaleBy = c("gdp", "none"),
                               scenario = "SSP2", histData = NULL, gdp = NULL,
                               verbose = FALSE) {
  scaleBy <- match.arg(scaleBy)
  h <- histData %||% iamHistoricalData(aggregate = FALSE,
                                       outputRegionMappingFile = "country")
  if (!"fe_total" %in% magclass::getNames(h)) {
    stop("psmCouplingWeights: 'fe_total' absent from the historical data - ",
         "iamHistoricalData() is the expected source.")
  }
  fe <- magclass::collapseNames(h[, , "fe_total"])
  yrs <- magclass::getYears(fe, as.integer = TRUE)
  # The last year with real coverage, not merely the last column: trailing years are
  # often all-NA or all-zero, and silently weighting on those would be equal weights
  # wearing a disguise.
  hasData <- vapply(yrs, function(y) {
    v <- as.numeric(fe[, y, ])
    sum(is.finite(v) & v > 0) > 0.5 * length(v)
  }, logical(1))
  if (!any(hasData)) stop("psmCouplingWeights: no year of fe_total has usable coverage.")
  base <- max(yrs[hasData])
  w <- stats::setNames(as.numeric(fe[, base, ]), magclass::getItems(fe, dim = 1))

  if (identical(scaleBy, "gdp") && is.null(year)) {
    # Asking for GDP scaling without a target year is a no-op, and a silent one is
    # exactly the kind of failure that reads as a result later.
    warning("psmCouplingWeights: scaleBy = 'gdp' needs a target 'year'; holding ",
            "final energy at ", base, " instead.", call. = FALSE)
  }
  if (identical(scaleBy, "gdp") && !is.null(year) && year != base) {
    # The GDP path is SCENARIO-SPECIFIC. SSPs differ most in *where* growth happens,
    # and that differential is exactly what within-region weights should track - so
    # the scenario must be an argument, never hardcoded.
    g <- gdp %||% magclass::collapseNames(
      madrat::calcOutput("GDP", scenario = scenario, average2020 = FALSE,
                         aggregate = FALSE))
    gy <- magclass::getYears(g, as.integer = TRUE)
    if (year %in% gy && base %in% gy) {
      gr <- stats::setNames(as.numeric(g[, year, ]) / as.numeric(g[, base, ]),
                            magclass::getItems(g, dim = 1))
      common <- intersect(names(w), names(gr))
      w[common] <- w[common] * ifelse(is.finite(gr[common]) & gr[common] > 0,
                                      gr[common], 1)
      if (isTRUE(verbose)) {
        message("[psmCouplingWeights] scaled ", length(common),
                " countries by ", scenario, " GDP growth ", base, " -> ", year)
      }
    } else if (isTRUE(verbose)) {
      message("[psmCouplingWeights] GDP lacks ", year, " or ", base,
              " - holding final energy constant instead")
    }
  }

  w[!is.finite(w) | w < 0] <- 0
  if (all(w == 0)) stop("psmCouplingWeights: every weight is zero.")
  attr(w, "baseYear") <- base
  attr(w, "year") <- year %||% base
  attr(w, "scaleBy") <- scaleBy
  attr(w, "scenario") <- scenario
  attr(w, "source") <- "fe_total (iamHistoricalData)"
  if (isTRUE(verbose)) {
    message("[psmCouplingWeights] ", sum(w > 0), " countries weighted on final ",
            "energy, base year ", base)
  }
  w
}
# nolint end
