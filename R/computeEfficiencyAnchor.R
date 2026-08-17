# nolint start
#' Derive the efficiency-anchored theta from an aggregated feasibility frame
#'
#' @description
#' \eqn{\theta} is declared and swept, never estimated (\code{MODEL.md} §5.3). The
#' \emph{efficiency anchor} is one transparent way to pick a value to sweep \emph{to}: the
#' \eqn{\theta} at which the median unit's price discount equals its own observed
#' efficiency ratio. It solves
#'
#' \deqn{1 - \theta\,\bar u = \bar E}
#'
#' with \eqn{E = S/S^{*}} the efficiency ratio, \eqn{g = 1-E} the \strong{relative} gap
#' (\code{gapMeasure = "relative"}), and \eqn{u = (g-g_{\min})/(g_{\max}-g_{\min})} the
#' normalised gap position that \code{phiRule = "continuous"} interpolates on. Bars are
#' medians over units with a valid ceiling at the tier year.
#'
#' \strong{This is an anchor by analogy, not an estimate} — index-space efficiency and
#' price-space effort share are different objects. It exists so that a swept value has a
#' stated provenance instead of being a constant nobody can re-derive.
#'
#' @section Resolution matters:
#' \eqn{u} is normalised over whatever units the frame contains, so the anchor derived over
#' 48 \strong{countries} is not the same number as the one derived over 21 REMIND
#' \strong{regions}: \eqn{g_{\min}} and \eqn{g_{\max}} differ. The coupling normalises over
#' regions, so a region-level frame is the one that describes the deployed constraint.
#' \code{resolution} records which was used, and it must travel with the number.
#'
#' @section Out of range is a result, not a failure:
#' Nothing guarantees the solution lands in \eqn{[0,1)}. A compressed gap distribution —
#' small \eqn{\bar u} relative to \eqn{1-\bar E} — needs \eqn{\theta > 1} to reproduce the
#' median efficiency, which the continuous rule does not admit. On Run-Group \code{v1} Bulk
#' does exactly this (\eqn{\theta} = 1.19). That says the rule cannot reproduce median Bulk
#' efficiency at any legal \eqn{\theta}; it is \strong{not} a reason to raise the ceiling.
#' \code{inRange} flags it.
#'
#' @param agg Aggregated feasibility frame from
#'   \code{\link{aggregateFeasibilityToRegions}} (or the country-level equivalent), with
#'   \code{region, year, efficiencyRatio, ceilingValid} and optionally \code{sector}.
#'   \eqn{\theta} does not enter the gaps, so any \eqn{\theta} the frame was built at works.
#' @param tierYear Year to evaluate at. Default: the frame's \code{"tierYear"} attribute if
#'   present, else the earliest year carrying a valid ceiling — the same rule
#'   \code{aggregateFeasibilityToRegions} applies.
#' @param resolution Label recorded alongside the result, e.g. \code{"region"} or
#'   \code{"country"}. Default guesses from the number of distinct units.
#'
#' @return A data.frame, one row per sector (or one row if the frame has no \code{sector}
#'   column): \code{sector, resolution, tierYear, n, medianE, medianU, gapMin, gapMax,
#'   theta, inRange}.
#'
#' @seealso \code{\link{aggregateFeasibilityToRegions}}, \code{\link{runPSMCouplingBound}},
#'   ADR 0041 and its 2026-08-17 amendment, \code{MODEL.md} §5.3
#' @export
#' @author Renato Rodrigues
computeEfficiencyAnchor <- function(agg, tierYear = NULL, resolution = NULL) {
  need <- c("region", "year", "efficiencyRatio")
  miss <- setdiff(need, names(agg))
  if (length(miss)) {
    stop("computeEfficiencyAnchor: the frame is missing ", paste(miss, collapse = ", "))
  }
  if (is.null(tierYear)) tierYear <- attr(agg, "tierYear")
  valid <- if ("ceilingValid" %in% names(agg)) agg$ceilingValid %in% TRUE else TRUE
  ok <- valid & is.finite(agg$efficiencyRatio)
  if (is.null(tierYear)) {
    yrs <- sort(unique(agg$year[ok & is.finite(agg$year)]))
    if (!length(yrs)) stop("computeEfficiencyAnchor: no year carries a valid ceiling")
    tierYear <- yrs[1]
  }
  secs <- if ("sector" %in% names(agg)) unique(as.character(agg$sector)) else NA_character_

  do.call(rbind, lapply(secs, function(sec) {
    keep <- ok & agg$year == tierYear
    if (!is.na(sec)) keep <- keep & as.character(agg$sector) == sec
    s <- agg[keep, , drop = FALSE]
    # One row per unit: a frame carrying several rows per region-year would weight some
    # regions more heavily in the median, silently.
    s <- s[!duplicated(as.character(s$region)), , drop = FALSE]
    if (nrow(s) < 2L) {
      return(data.frame(sector = sec, resolution = NA_character_, tierYear = tierYear,
                        n = nrow(s), medianE = NA_real_, medianU = NA_real_,
                        gapMin = NA_real_, gapMax = NA_real_, theta = NA_real_,
                        inRange = NA, stringsAsFactors = FALSE))
    }
    E <- s$efficiencyRatio
    g <- 1 - E
    span <- max(g) - min(g)
    # A degenerate spread means every unit sits at the same relative gap, so phi is flat
    # and no theta reproduces a spread that is not there.
    u <- if (span > 0) (g - min(g)) / span else rep(NA_real_, length(g))
    medE <- stats::median(E)
    medU <- stats::median(u)
    th <- if (is.finite(medU) && medU > 0) (1 - medE) / medU else NA_real_
    res <- resolution %||% if (nrow(s) > 60) "country" else "region"
    # NA, not FALSE, when there is no theta to judge: "out of range" and "undefined" are
    # different findings and a caller filtering on !inRange must not conflate them.
    inRange <- if (is.finite(th)) th >= 0 && th < 1 else NA
    data.frame(sector = sec, resolution = res, tierYear = tierYear, n = nrow(s),
               medianE = medE, medianU = medU, gapMin = min(g), gapMax = max(g),
               theta = th, inRange = inRange,
               stringsAsFactors = FALSE)
  }))
}
# nolint end
