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
#' \strong{3. Resolve missing coverage explicitly, never by default.} Countries
#' outside the estimation sample used to be dropped, and a region below
#' \code{minCoverage} fell back to \eqn{\varphi = 1}. That rule \emph{rewarded
#' absence of data with maximal assumed political capability} — the USA, with 0\%
#' CAPMF coverage, was assigned full capability while Japan was held to 21\%. It is
#' replaced by the three-case band rule of
#' \code{\link{computeDonorAssignment}}: pass its table as \code{assignment} and each
#' uncovered country receives an efficiency ratio \eqn{E} by donor blend, low band or
#' median, from which its level is reconstructed as
#' \eqn{S = E \cdot S^{*}_{\text{own}}} — its \strong{own} driver-based ceiling,
#' never a donor's.
#'
#' \strong{Provenance is part of the result, not a footnote.} Once every country has
#' a value the output stops \emph{looking} uncertain although the evidence has not
#' changed, so each region reports \code{shareObserved}, \code{shareDonor},
#' \code{shareLowBand} and \code{shareMedian} — the weight shares behind its
#' \eqn{\varphi}. A region at 90\% \code{shareMedian} is a different object from one
#' at 90\% \code{shareObserved}, and only these columns show it. Without
#' \code{assignment} the old exclude-and-uncouple behaviour still applies, so callers
#' must opt in to the imputation.
#'
#' \strong{The feasibility share (ADR 0041).} Regions are sorted into \code{K}
#' ambition-gap tiers at the seed year and receive
#' \eqn{\varphi = 1 - \theta (k-1)/(K-1)}: the fraction of the IAM's
#' \emph{incremental} cost-optimal mitigation effort that a region can realise.
#' Tiers use the gap \emph{ranking}, which survived the frontier robustness
#' battery, never the point gap, whose level the boundary \eqn{\gamma} compromises.
#' The ranked quantity is the \strong{relative} gap \eqn{1 - E} (see
#' \code{gapMeasure}): a country's shortfall is measured against its \emph{own}
#' frontier, so a low-capability polity is not credited with having little left to do
#' simply because its ceiling is low.
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
#' @param assignment Optional \code{\link{computeDonorAssignment}} table (needs
#'   \code{region}, \code{efficiencyRatio}, \code{basis}). Supplying it activates
#'   the band rule so uncovered countries get a level instead of being dropped.
#'   \code{NULL} keeps the old exclude-and-uncouple behaviour.
#' @param minCoverage Numeric in \code{[0, 1]}. Minimum \strong{resolved} weight
#'   share - the share the band rule could place at all - for a region's ceiling to
#'   count as valid. Default \code{0}: with \code{assignment} supplied essentially
#'   every country resolves, and provenance is \emph{reported} (see the
#'   \code{share*} columns) rather than used to switch the constraint off. Raise it
#'   only to reproduce the old cliff as a sensitivity.
#' @param gapMeasure \code{"relative"} (default) ranks tiers on the share of a
#'   region's \emph{own} frontier left unclaimed, \eqn{1 - E}; \code{"absolute"}
#'   ranks on \eqn{S^{*} - S} in index points. \strong{Use relative.} The absolute
#'   gap spans only 2.35-3.21 across H12 regions - almost no discrimination - and a
#'   high ceiling mechanically creates more absolute room, so it ranks regions
#'   largely \emph{by their ceiling} rather than by their political shortfall. Under
#'   it Japan (ceiling 8.24) appears maximally constrained while Sub-Saharan Africa
#'   (ceiling 6.21) appears unconstrained, which inverts the intended reading.
#'   \code{"absolute"} is retained only as a disclosed sensitivity.
#' @param phiRule \code{"continuous"} (default) makes \eqn{arphi} a linear
#'   function of the region's position in the gap range, \eqn{arphi = 1 - 	heta u}
#'   with \eqn{u = (g - g_{\min})/(g_{\max} - g_{\min})}; \code{"tiered"} is the
#'   original rank-quantile scheme, \eqn{arphi = 1 - 	heta (k-1)/(K-1)}.
#'   Endpoints are identical under both - smallest gap 1, largest \eqn{1-	heta} -
#'   so \eqn{	heta} keeps its meaning. \strong{Prefer continuous.} The tiered rule
#'   moves \eqn{arphi} in steps of \eqn{	heta/(K-1)}, and the relative gaps
#'   cluster tightly enough (six H12 regions inside 0.29-0.33) that a 0.05 change in
#'   \eqn{E} can cross two boundaries and swing \eqn{arphi} by half of
#'   \eqn{	heta}. \code{tier} is still reported as a descriptive label.
#' @param tierYear Integer or \code{NULL}. Year whose gaps define the tiers
#'   (\code{NULL} = the earliest projected year). Tiers are assigned ONCE and held
#'   fixed - tier migration is a sensitivity, not a default.
#' @param verbose Logical.
#'
#' @return Data.frame \code{region, year, feasibleIndex, ceilingIndex,
#'   equilibriumIndex, gapIndex, relativeGap, gapPosition, efficiencyRatio, inCoverageShare, shareObserved,
#'   shareDonor, shareLowBand, shareMedian, shareResolved, nCountries,
#'   nCountriesInCoverage, nCountriesResolved, ceilingValid, tier, phi, driverOutOfSupport,
#'   driverOutOfSample}, one row per IAM region-year. Attributes: \code{"theta"},
#'   \code{"nTiers"}, \code{"tierYear"}, \code{"weightSource"}.
#'
#' @seealso \code{\link{projectFeasiblePath}}, ADR 0041.
#' @importFrom stats weighted.mean
#' @export
#' @author Renato Rodrigues
aggregateFeasibilityToRegions <- function(path, mapping, weights = NULL,
                                          assignment = NULL,
                                          theta = 0.5, nTiers = 4L,
                                          minCoverage = 0, tierYear = NULL,
                                          gapMeasure = c("relative", "absolute"),
                                          phiRule = c("continuous", "tiered"),
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
  gapMeasure <- match.arg(gapMeasure)
  phiRule <- match.arg(phiRule)
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

  # --- the band rule: give every country a level, and record where it came from --
  df$basis <- ifelse(df$outOfCoverage %in% TRUE, NA_character_, "observed")
  if (!is.null(assignment)) {
    if (!all(c("region", "efficiencyRatio", "basis") %in% colnames(assignment))) {
      stop("aggregateFeasibilityToRegions: 'assignment' needs region, ",
           "efficiencyRatio and basis - pass computeDonorAssignment() output.")
    }
    a <- assignment[is.finite(assignment$efficiencyRatio), , drop = FALSE]
    eMap <- stats::setNames(a$efficiencyRatio, as.character(a$region))
    bMap <- stats::setNames(as.character(a$basis), as.character(a$region))
    # Reconstruct the level from the country's OWN ceiling: only the relative gap is
    # ever transferred, so a well-governed country still gets a high ceiling.
    fill <- df$outOfCoverage %in% TRUE & df$iso3 %in% names(eMap) &
      is.finite(df$ceilingIndex)
    df$feasibleIndex[fill] <- eMap[df$iso3[fill]] * df$ceilingIndex[fill]
    df$basis[fill] <- bMap[df$iso3[fill]]
    if (isTRUE(verbose)) {
      message("[aggregateFeasibilityToRegions] band rule filled ", sum(fill),
              " country-years: ",
              paste(sprintf("%s %d", names(table(df$basis[fill])),
                            table(df$basis[fill])), collapse = ", "))
    }
  }
  # Anything still unresolved stays excluded - the imputation is opt-in, and a
  # country the assignment could not reach must not silently become evidence.
  df$usable <- is.finite(df$feasibleIndex) & !is.na(df$basis)

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
    # Weight share behind each provenance - this is what makes an imputed region
    # distinguishable from an observed one downstream.
    shareOf <- function(b) {
      if (wTot <= 0) return(NA_real_)
      sum(d$w[!is.na(d$basis) & d$basis == b], na.rm = TRUE) / wTot
    }
    # Ceiling-derived quantities describe every country the rule could resolve.
    dc <- d[d$usable, , drop = FALSE]
    data.frame(
      region = d$iamRegion[1], year = d$year[1],
      feasibleIndex = wmean(dc$feasibleIndex, dc$w),
      ceilingIndex = wmean(dc$ceilingIndex, dc$w),
      equilibriumIndex = if (hasCol("equilibriumIndex")) wmean(dc$equilibriumIndex, dc$w) else NA_real_,
      inCoverageShare = covShare,
      shareObserved = shareOf("observed"), shareDonor = shareOf("donor"),
      shareLowBand = shareOf("lowBand"), shareMedian = shareOf("median"),
      shareResolved = if (wTot > 0) sum(d$w[d$usable], na.rm = TRUE) / wTot else 0,
      nCountries = nrow(d), nCountriesInCoverage = sum(inCov),
      nCountriesResolved = nrow(dc),
      driverOutOfSupport = if (hasCol("driverOutOfSupport")) wmean(dc$driverOutOfSupport, dc$w) else NA_real_,
      driverOutOfSample = if (hasCol("driverOutOfSample")) wmean(dc$driverOutOfSample, dc$w) else NA_real_,
      stringsAsFactors = FALSE
    )
  }))
  rownames(agg) <- NULL
  agg$gapIndex <- agg$ceilingIndex - agg$feasibleIndex
  agg$efficiencyRatio <- pmin(pmax(agg$feasibleIndex / pmax(agg$ceilingIndex, 1e-9), 0), 1)
  # The gap a polity actually faces is the share of its OWN frontier left unclaimed.
  agg$relativeGap <- 1 - agg$efficiencyRatio
  # The gate is now RESOLVED share, not observed share: a region is usable when the
  # band rule could place its countries at all, and provenance is reported rather
  # than used to switch the constraint off.
  agg$ceilingValid <- is.finite(agg$ceilingIndex) & agg$shareResolved >= minCoverage
  agg$ceilingIndex[!agg$ceilingValid] <- NA_real_
  agg$gapIndex[!agg$ceilingValid] <- NA_real_
  agg$efficiencyRatio[!agg$ceilingValid] <- NA_real_
  agg$relativeGap[!agg$ceilingValid] <- NA_real_

  # --- Tiers and the feasibility share, assigned ONCE at tierYear --------------
  ty <- tierYear %||% suppressWarnings(min(agg$year, na.rm = TRUE))
  gapCol <- if (identical(gapMeasure, "relative")) "relativeGap" else "gapIndex"
  base <- agg[agg$year == ty & agg$ceilingValid, c("region", gapCol), drop = FALSE]
  names(base)[2] <- "gapIndex"
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

  # --- the feasibility share -----------------------------------------------
  # "tiered" quantises the gap into K rank buckets, so phi moves in steps of
  # theta/(K-1). With H12 the relative gaps cluster tightly (six regions inside
  # 0.29-0.33), which puts several regions within one step of a boundary: a 0.05
  # change in E could move a region two tiers and swing phi by half of theta. That
  # is precision the spread does not support.
  #
  # "continuous" (default) keeps the SAME endpoints - the smallest-gap region gets
  # phi = 1, the largest gets 1 - theta - but interpolates linearly in the gap
  # between them, so a small change in E produces a small change in phi.
  if (identical(phiRule, "continuous")) {
    gv <- agg[[gapCol]]
    gb <- gv[agg$year == ty & agg$ceilingValid & is.finite(gv)]
    rng <- if (length(gb)) range(gb) else c(NA_real_, NA_real_)
    span <- diff(rng)
    # Position each region ONCE, at tierYear, so the share is held fixed exactly as
    # the tiers were - phi must not drift as the projection evolves.
    posAt <- stats::setNames(rep(NA_real_, 0), character(0))
    if (is.finite(span)) {
      b <- agg[agg$year == ty & agg$ceilingValid & is.finite(gv), , drop = FALSE]
      u <- if (span > 0) (b[[gapCol]] - rng[1]) / span else rep(0, nrow(b))
      posAt <- stats::setNames(u, b$region)
    }
    agg$gapPosition <- unname(posAt[agg$region])
    agg$phi <- ifelse(is.finite(agg$gapPosition), 1 - theta * agg$gapPosition, 1)
  } else {
    agg$gapPosition <- ifelse(is.na(agg$tier), NA_real_,
                              (agg$tier - 1) / (nTiers - 1))
    # Out-of-coverage / untiered regions are UNCOUPLED (phi = 1): speeds only,
    # never an invented ceiling.
    agg$phi <- ifelse(is.na(agg$tier), 1,
                      1 - theta * (agg$tier - 1) / (nTiers - 1))
  }

  agg <- agg[order(agg$region, agg$year), , drop = FALSE]
  rownames(agg) <- NULL
  attr(agg, "theta") <- theta
  attr(agg, "nTiers") <- nTiers
  attr(agg, "tierYear") <- ty
  attr(agg, "weightSource") <- weightSource
  attr(agg, "gapMeasure") <- gapMeasure
  attr(agg, "phiRule") <- phiRule
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
