#' @title preparePooledPanelData
#' @description Stacks the per-sector panels of \code{preparePanelData} into one
#' pooled panel for the \strong{pooled-estimation power experiment} sanctioned by
#' the Shared Specification decision (2026-06-12): both sectors in one estimation
#' with a sector dummy, doubling the effective sample to test whether joint
#' significance of the three theory channels (the Green-tier hurdle) is a power
#' problem. The pooled model is an \emph{experiment}, not the deliverable — the
#' deliverable remains one shared specification estimated separately per sector.
#'
#' The returned data.frame carries a \code{sector} label column and one 0/1 dummy
#' column per non-reference sector, named \code{sector<Name>} (e.g.
#' \code{sectorDiffuse} with the default sectors). Add the dummy name(s) to
#' \code{controlDrivers} when calling \code{estimateAdoptionModel} /
#' \code{estimatePriceStringencyModel} with this pooled data.frame (both accept a
#' prepared data.frame and skip their internal \code{preparePanelData} call).
#' Because the same region appears in both sectors, the estimators' existing
#' region-clustered standard errors automatically account for within-region
#' cross-sector correlation.
#'
#' @param data A \code{magpie} object (as consumed by \code{preparePanelData}).
#' @param sectors Character vector of sectors to stack. Default:
#'   \code{c("Bulk", "Diffuse")}.
#' @param referenceSector Character. Sector that gets no dummy (the baseline).
#'   Default: the first element of \code{sectors}.
#' @param actorPowerDrivers,actorPowerIndex,instQualityDrivers,controlDrivers
#'   Passed to \code{preparePanelData} (see there).
#' @param regionMappingFixedEffects Character or NULL. Passed to \code{preparePanelData}.
#' @param lag Integer. Passed to \code{preparePanelData}. Default: \code{1}.
#' @param useMundlak Logical. Passed to \code{preparePanelData}. Default: \code{FALSE}.
#' @param gdpGovInteraction Logical. Passed to \code{preparePanelData}. Default: \code{FALSE}.
#'
#' @return A pooled data.frame: the row-bound per-sector panels (common columns
#'   only), plus \code{sector} and the \code{sector<Name>} dummy column(s).
#'
#' @seealso \code{\link{preparePanelData}}, \code{\link{estimateAdoptionModel}},
#'   \code{\link{estimatePriceStringencyModel}}
#'
#' @export
#' @author Renato Rodrigues
preparePooledPanelData <- function(data, sectors = c("Bulk", "Diffuse"),
                                   referenceSector = sectors[1],
                                   actorPowerDrivers = NULL, actorPowerIndex = NULL,
                                   instQualityDrivers = NULL, controlDrivers = NULL,
                                   regionMappingFixedEffects = "regionmappingH12.csv",
                                   lag = 1, useMundlak = FALSE,
                                   gdpGovInteraction = FALSE) {
  if (length(sectors) < 2) {
    stop("preparePooledPanelData: at least two sectors are required for pooling.")
  }
  if (!referenceSector %in% sectors) {
    stop("preparePooledPanelData: referenceSector '", referenceSector,
         "' is not in sectors.")
  }

  frames <- lapply(sectors, function(s) {
    df <- preparePanelData(
      data = data, sector = s,
      actorPowerDrivers = actorPowerDrivers,
      actorPowerIndex = actorPowerIndex,
      instQualityDrivers = instQualityDrivers,
      controlDrivers = controlDrivers,
      regionMappingFixedEffects = regionMappingFixedEffects,
      lag = lag, useMundlak = useMundlak,
      gdpGovInteraction = gdpGovInteraction
    )
    df$sector <- s
    df
  })

  common <- Reduce(intersect, lapply(frames, colnames))
  pooled <- do.call(rbind, lapply(frames, function(d) d[, common, drop = FALSE]))
  rownames(pooled) <- NULL

  for (s in setdiff(sectors, referenceSector)) {
    pooled[[make.names(paste0("sector", s))]] <- as.integer(pooled$sector == s)
  }

  pooled
}
