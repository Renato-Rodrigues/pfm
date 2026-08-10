# nolint start
#' Deliver the feasibility envelope at IAM region resolution (ADR 0040/0041)
#'
#' @description
#' The model is estimated on countries; the IAM runs on regions. This function is
#' the bridge, and it enforces the three delivery disciplines that make the bridge
#' defensible.
#'
#' \strong{1. Evaluate per country, aggregate the OUTPUT - never the drivers.}
#' The ceiling is a \emph{non-linear} function of the drivers
#' (\eqn{S^* = indexMax\cdot logit^{-1}(x'\beta_F)}), so aggregating drivers and
#' then evaluating is not the same as evaluating and then aggregating, and the
#' difference is not small in a heterogeneous region. Callers therefore pass a
#' \strong{country-level} path from \code{\link{projectFeasiblePath}}; this
#' function only aggregates it.
#'
#' \strong{2. Weight by what the constraint applies to.} A price bound governs a
#' region's carbon price, so the natural weight is \strong{emissions}. Weights are
#' an explicit argument rather than sourced internally, which keeps \pkg{pfm}
#' coupling-agnostic and the aggregation auditable; equal weights are used only if
#' none are supplied, and then loudly.
#'
#' \strong{3. Never invent a ceiling out of coverage.} Countries outside the
#' estimation sample carry driver-based extrapolations, not evidence. A region
#' whose in-coverage weight share falls below \code{minCoverage} keeps its
#' \emph{speed} but its ceiling-derived quantities are returned as \code{NA} and
#' \code{ceilingValid = FALSE}; downstream, such a region receives the uncoupled
#' feasibility share \eqn{\varphi = 1}. Out-of-coverage countries are excluded
#' from the weighted means outright, so a region's reported ceiling always
#' describes the part of it we actually estimated.
#'
#' \strong{The feasibility share (ADR 0041).} Regions are sorted into \code{K}
#' ambition-gap tiers at the seed year and receive
#' \eqn{\varphi = 1 - \theta (k-1)/(K-1)}: the fraction of the IAM's
#' \emph{incremental} cost-optimal mitigation effort that a region can realise.
#' Tiers use the gap \emph{ranking}, which survived the frontier robustness
#' battery, never the point gap, whose level the boundary \eqn{\gamma} compromises.
#' \eqn{\theta} is a declared scenario parameter, not an estimate - sweep it.
#'
#' @param path Country-level data.frame from \code{\link{projectFeasiblePath}}
#'   (needs \code{region, year, feasibleIndex, ceilingIndex, outOfCoverage}).
#' @param mapping Country-to-region map: a data.frame with an ISO3 column and a
#'   region column, or the name of a mapping file readable by
#'   \code{madrat::toolGetMapping(type = "regional")}.
#' @param weights Named numeric vector (names = ISO3), or a data.frame with
#'   country and value columns, giving the aggregation weight per country -
#'   \strong{emissions} for a price-side bound. \code{NULL} (default) uses equal
#'   weights and warns.
#' @param theta Numeric in \code{[0, 1)}. Coupling severity: the share of
#'   incremental effort withheld from the largest-gap tier. \code{theta = 0} is the
#'   uncoupled null and reproduces the IAM's own run exactly. Default \code{0.5}.
#' @param nTiers Integer, number of ambition-gap tiers. Default \code{4}.
#' @param minCoverage Numeric in \code{[0, 1]}. Minimum in-coverage weight share
#'   for a region's ceiling to be considered valid. Default \code{0.5}.
#' @param tierYear Integer or \code{NULL}. Year whose gaps define the tiers
#'   (\code{NULL} = the earliest projected year). Tiers are assigned ONCE and held
#'   fixed - tier migration is a sensitivity, not a default.
#' @param verbose Logical.
#'
#' @return Data.frame \code{region, year, feasibleIndex, ceilingIndex,
#'   equilibriumIndex, gapIndex, efficiencyRatio, inCoverageShare, nCountries,
#'   nCountriesInCoverage, ceilingValid, tier, phi, driverOutOfSupport,
#'   driverOutOfSample}, one row per IAM region-year. Attributes: \code{"theta"},
#'   \code{"nTiers"}, \code{"tierYear"}, \code{"weightSource"}.
#'
#' @seealso \code{\link{projectFeasiblePath}}, ADR 0041.
#' @importFrom stats weighted.mean
#' @export
#' @author Renato Rodrigues
aggregateFeasibilityToRegions <- function(path, mapping, weights = NULL,
                                          theta = 0.5, nTiers = 4L,
                                          minCoverage = 0.5, tierYear = NULL,
                                          verbose = FALSE) {
  need <- c("region", "year", "feasibleIndex", "ceilingIndex", "outOfCoverage")
  miss <- setdiff(need, colnames(path))
  if (length(miss)) {
    stop("aggregateFeasibilityToRegions: 'path' is missing column(s): ",
         paste(miss, collapse = ", "), " - pass the output of projectFeasiblePath().")
  }
  if (!is.numeric(theta) || length(theta) != 1 || theta < 0 || theta >= 1) {
    stop("aggregateFeasibilityToRegions: theta must be a single value in [0, 1).")
  }
  nTiers <- as.integer(nTiers)
  if (!is.finite(nTiers) || nTiers < 2L) {
    stop("aggregateFeasibilityToRegions: nTiers must be an integer >= 2.")
  }

  map <- .psmResolveCountryMap(mapping)
  w <- .psmResolveWeights(weights, unique(as.character(path$region)))
  weightSource <- attr(w, "source")
  if (identical(weightSource, "equal")) {
    warning("aggregateFeasibilityToRegions: no weights supplied - using EQUAL ",
            "country weights. A price-side bound should be aggregated with ",
            "EMISSION weights; equal weights over-represent small emitters.",
            call. = FALSE)
  }

  df <- path
  df$iso3 <- as.character(df$region)
  df$iamRegion <- map[df$iso3]
  unmapped <- unique(df$iso3[is.na(df$iamRegion)])
  if (length(unmapped)) {
    if (isTRUE(verbose)) {
      message("[aggregateFeasibilityToRegions] dropping ", length(unmapped),
              " unmapped countries: ", paste(utils::head(unmapped, 8), collapse = ", "),
              if (length(unmapped) > 8) " ..." else "")
    }
    df <- df[!is.na(df$iamRegion), , drop = FALSE]
  }
  if (!nrow(df)) stop("aggregateFeasibilityToRegions: no country mapped to a region.")
  df$w <- as.numeric(w[df$iso3])
  df$w[!is.finite(df$w)] <- 0

  hasCol <- function(nm) nm %in% colnames(df)
  wmean <- function(v, wt) {
    ok <- is.finite(v) & is.finite(wt) & wt > 0
    if (!any(ok)) return(NA_real_)
    stats::weighted.mean(v[ok], wt[ok])
  }

  parts <- split(df, list(df$iamRegion, df$year), drop = TRUE)
  agg <- do.call(rbind, lapply(parts, function(d) {
    inCov <- !d$outOfCoverage %in% TRUE
    wTot <- sum(d$w, na.rm = TRUE)
    covShare <- if (wTot > 0) sum(d$w[inCov], na.rm = TRUE) / wTot else 0
    # Ceiling-derived quantities describe only the estimated part of the region.
    dc <- d[inCov, , drop = FALSE]
    data.frame(
      region = d$iamRegion[1], year = d$year[1],
      feasibleIndex = wmean(dc$feasibleIndex, dc$w),
      ceilingIndex = wmean(dc$ceilingIndex, dc$w),
      equilibriumIndex = if (hasCol("equilibriumIndex")) wmean(dc$equilibriumIndex, dc$w) else NA_real_,
      inCoverageShare = covShare,
      nCountries = nrow(d), nCountriesInCoverage = nrow(dc),
      driverOutOfSupport = if (hasCol("driverOutOfSupport")) wmean(dc$driverOutOfSupport, dc$w) else NA_real_,
      driverOutOfSample = if (hasCol("driverOutOfSample")) wmean(dc$driverOutOfSample, dc$w) else NA_real_,
      stringsAsFactors = FALSE
    )
  }))
  rownames(agg) <- NULL
  agg$gapIndex <- agg$ceilingIndex - agg$feasibleIndex
  agg$efficiencyRatio <- pmin(pmax(agg$feasibleIndex / pmax(agg$ceilingIndex, 1e-9), 0), 1)
  agg$ceilingValid <- is.finite(agg$ceilingIndex) & agg$inCoverageShare >= minCoverage
  agg$ceilingIndex[!agg$ceilingValid] <- NA_real_
  agg$gapIndex[!agg$ceilingValid] <- NA_real_
  agg$efficiencyRatio[!agg$ceilingValid] <- NA_real_

  # --- Tiers and the feasibility share, assigned ONCE at tierYear --------------
  ty <- tierYear %||% suppressWarnings(min(agg$year, na.rm = TRUE))
  base <- agg[agg$year == ty & agg$ceilingValid, c("region", "gapIndex"), drop = FALSE]
  base <- base[is.finite(base$gapIndex), , drop = FALSE]
  tierOf <- stats::setNames(rep(NA_integer_, nrow(agg)), NULL)
  if (nrow(base) > 0) {
    # Rank-based tiers: uses the gap ORDERING (robust) not its level.
    rk <- rank(base$gapIndex, ties.method = "first")
    k <- as.integer(ceiling(rk / nrow(base) * nTiers))
    k <- pmin(pmax(k, 1L), nTiers)
    tierMap <- stats::setNames(k, base$region)
    tierOf <- as.integer(tierMap[agg$region])
  } else if (isTRUE(verbose)) {
    message("[aggregateFeasibilityToRegions] no region has a valid ceiling in ",
            ty, " - every region falls back to phi = 1 (uncoupled).")
  }
  agg$tier <- tierOf
  # Out-of-coverage / untiered regions are UNCOUPLED (phi = 1): speeds only,
  # never an invented ceiling.
  agg$phi <- ifelse(is.na(agg$tier), 1,
                    1 - theta * (agg$tier - 1) / (nTiers - 1))

  agg <- agg[order(agg$region, agg$year), , drop = FALSE]
  rownames(agg) <- NULL
  attr(agg, "theta") <- theta
  attr(agg, "nTiers") <- nTiers
  attr(agg, "tierYear") <- ty
  attr(agg, "weightSource") <- weightSource
  if (isTRUE(verbose)) {
    nv <- sum(agg$ceilingValid[agg$year == ty], na.rm = TRUE)
    message("[aggregateFeasibilityToRegions] ", length(unique(agg$region)),
            " regions, ", nv, " with a valid ceiling at ", ty,
            "; theta = ", theta, ", weights = ", weightSource, ".")
  }
  agg
}

# Country -> IAM region lookup, from a data.frame or a madrat regional mapping.
#' @keywords internal
.psmResolveCountryMap <- function(mapping) {
  if (is.character(mapping) && length(mapping) == 1) {
    mapping <- madrat::toolGetMapping(mapping, type = "regional", where = "mappingfolder")
  }
  if (!is.data.frame(mapping)) {
    stop(".psmResolveCountryMap: 'mapping' must be a data.frame or a mapping file name.")
  }
  cc <- grep("^(CountryCode|iso3c?|country)$", colnames(mapping), ignore.case = TRUE)
  rc <- grep("^(RegionCode|region)$", colnames(mapping), ignore.case = TRUE)
  if (!length(cc) || !length(rc)) {
    stop(".psmResolveCountryMap: could not find country and region columns in the ",
         "mapping (have: ", paste(colnames(mapping), collapse = ", "), ").")
  }
  stats::setNames(as.character(mapping[[rc[1]]]), as.character(mapping[[cc[1]]]))
}

# Aggregation weights, defaulting to equal (flagged by the "source" attribute).
#' @keywords internal
.psmResolveWeights <- function(weights, countries) {
  if (is.null(weights)) {
    w <- stats::setNames(rep(1, length(countries)), countries)
    attr(w, "source") <- "equal"
    return(w)
  }
  if (is.data.frame(weights)) {
    cc <- grep("^(CountryCode|iso3c?|country|region)$", colnames(weights),
               ignore.case = TRUE)
    vc <- grep("^(value|weight|emissions)$", colnames(weights), ignore.case = TRUE)
    if (!length(cc) || !length(vc)) {
      stop(".psmResolveWeights: a weights data.frame needs a country column and a ",
           "value/weight column (have: ", paste(colnames(weights), collapse = ", "), ").")
    }
    weights <- stats::setNames(as.numeric(weights[[vc[1]]]), as.character(weights[[cc[1]]]))
  }
  if (is.null(names(weights))) {
    stop(".psmResolveWeights: 'weights' must be NAMED by country code.")
  }
  w <- weights[countries]
  names(w) <- countries
  w[!is.finite(w)] <- 0
  if (all(w == 0)) {
    stop(".psmResolveWeights: no supplied weight matches any country in the path.")
  }
  attr(w, "source") <- "supplied"
  w
}
# nolint end
