# nolint start
#' Edge-of-data gate for a frontier specification (design note 0007)
#'
#' @description
#' Checks whether a frontier's actor-power x institution interactions, evaluated at the countries' own
#' values along a scenario, push a driver against the direction the frontier is read with, or make the
#' ceilings depend on extending the interactions beyond the estimation sample. Two criteria, both declared
#' before any result is read (0007 section 5):
#' \enumerate{
#'   \item \strong{Direction.} For fossil dependence (expected: lowers the ceiling) and clean-energy
#'     build-out (expected: raises it), the effective slope at each country's own values in the first
#'     year, \eqn{\beta_d + \sum w\,\beta_{d \times m}\,x_m}, against its expected sign, counted only
#'     when the driver moves the ceiling by at least \code{effect} logit units between the two years and
#'     only in a sector whose sample-centre sign (\eqn{\beta_d}) agrees with the expectation. Fails when
#'     countries holding at least \code{share} of a region's final energy are against it.
#'   \item \strong{Cap test.} The moderators inside the interactions capped at the estimation sample's
#'     \code{cap} percentiles (main effects untouched). Fails when the final-energy-weighted regional
#'     mean of the capped minus uncapped logit ceiling, in its level in the first year or in its change
#'     to the last year, moves by more than \code{capShift}.
#' }
#' Regions are those of \code{mapping}, weighted by \code{\link{pfmCouplingWeights}} (final energy).
#' An interaction weight (\code{interactionWeight}) is honoured: its column multiplies every interaction
#' in its scope (\code{.pfmIwFactor}).
#'
#' @param beta Named frontier coefficients (\code{sigmaSq}, \code{gamma} removed).
#' @param scenDf The prepared and guarded scenario frame (every country, every year).
#' @param mm Its model matrix (columns at least \code{names(beta)}).
#' @param trainDf The estimation frame (for the sample's moderator percentiles).
#' @param years First and last year of the comparison.
#' @param gate A list: \code{share} (0.25), \code{effect} (0.25), \code{capShift} (0.5), \code{cap}
#'   (c(0.05, 0.95)), \code{mapping} (\code{"regionmapping_21_EU11.csv"}). Missing elements take these
#'   defaults.
#' @return A list: \code{pass}, \code{against} (regions failing the direction criterion, with their
#'   share and drivers), \code{capShift} (the largest regional shift), \code{capRegion}, \code{detail}.
#' @keywords internal
.pfmEdgeGate <- function(beta, scenDf, mm, trainDf, years = c(2025, 2100), gate = list()) {
  g <- utils::modifyList(.pfmEdgeGateDefaults(), gate %||% list())
  expect <- c(Innovator.Power = 1, Incumbent.Power = -1)
  ints <- grep("_x_", names(beta), value = TRUE)
  pr <- if (length(ints)) do.call(rbind, strsplit(ints, "_x_", fixed = TRUE)) else matrix(character(0), 0, 2)
  if (length(ints)) rownames(pr) <- ints
  keep <- grepl("Power", pr[, 1]) & pr[, 1] %in% colnames(scenDf) & pr[, 2] %in% colnames(scenDf)
  pr <- pr[keep, , drop = FALSE]; ints <- rownames(pr)
  # the weight of each interaction (its scope included), on every row
  wOf <- lapply(stats::setNames(ints, ints), function(k) {
    v <- rep_len(.pfmIwFactor(scenDf, pr[k, 2]), nrow(scenDf)); v[!is.finite(v)] <- 1; v
  })
  yrs <- sort(unique(scenDf$year))
  pick <- function(y) yrs[which.min(abs(yrs - y))]
  y0 <- pick(years[1]); y1 <- pick(years[2])
  i0 <- which(scenDf$year == y0); i1 <- which(scenDf$year == y1)
  cty0 <- as.character(scenDf$region[i0]); i1 <- i1[match(cty0, as.character(scenDf$region[i1]))]

  # --- regions and final-energy weights ---
  map <- .pfmResolveCountryMap(g$mapping)
  fe <- tryCatch(pfmCouplingWeights(year = 2025, scenario = "SSP2"), error = function(e) NULL)
  regionShare <- function(flag) {
    cc <- intersect(cty0, intersect(names(map), names(fe)))
    if (!length(cc)) return(numeric(0))
    f <- flag[match(cc, cty0)]; f[is.na(f)] <- FALSE
    tapply(fe[cc] * f, map[cc], sum) / tapply(fe[cc], map[cc], sum)
  }

  # --- 1. direction ---
  dirRows <- list()
  for (d in intersect(names(expect), names(beta))) {
    if (sign(beta[[d]]) != expect[[d]] || !d %in% colnames(scenDf)) next   # the centre disagrees: model-wide, not an edge
    s <- rep(beta[[d]], length(i0))
    for (k in ints[pr[, 1] == d]) s <- s + beta[[k]] * scenDf[[pr[k, 2]]][i0] * wOf[[k]][i0]
    for (k in ints[pr[, 2] == d]) s <- s + beta[[k]] * scenDf[[pr[k, 1]]][i0] * wOf[[k]][i0]
    eff <- abs((scenDf[[d]][i1] - scenDf[[d]][i0]) * s)
    flag <- sign(s) != expect[[d]] & is.finite(eff) & eff >= g$effect
    sh <- regionShare(flag)
    bad <- sh[is.finite(sh) & sh >= g$share]
    if (length(bad)) dirRows[[d]] <- data.frame(driver = d, region = names(bad), share = as.numeric(bad), stringsAsFactors = FALSE)
  }
  against <- if (length(dirRows)) do.call(rbind, dirRows) else NULL

  # --- 2. cap test ---
  mods <- unique(pr[, 2])
  capShift <- 0; capRegion <- NA_character_
  if (length(ints) && length(mods) && all(mods %in% colnames(trainDf))) {
    lo <- vapply(mods, function(m) stats::quantile(trainDf[[m]], g$cap[1], na.rm = TRUE), numeric(1))
    hi <- vapply(mods, function(m) stats::quantile(trainDf[[m]], g$cap[2], na.rm = TRUE), numeric(1))
    dEta <- rep(0, nrow(scenDf))
    for (k in ints) {
      m <- pr[k, 2]; v <- scenDf[[m]]
      dEta <- dEta + beta[[k]] * scenDf[[pr[k, 1]]] * (pmin(pmax(v, lo[[m]]), hi[[m]]) - v) * wOf[[k]]
    }
    dEta[!is.finite(dEta)] <- 0
    level <- dEta[i0]; change <- dEta[i1] - dEta[i0]
    cc <- intersect(cty0, intersect(names(map), names(fe)))
    if (length(cc)) {
      wm <- function(v) { x <- v[match(cc, cty0)]; tapply(fe[cc] * x, map[cc], sum) / tapply(fe[cc], map[cc], sum) }
      shifts <- pmax(abs(wm(level)), abs(wm(change)))
      capShift <- max(shifts, na.rm = TRUE); capRegion <- names(shifts)[which.max(shifts)]
    }
  }
  pass <- is.null(against) && !(capShift > g$capShift)
  detail <- paste0(
    if (!is.null(against)) paste0("against expectation in ", paste(sprintf("%s (%s %.0f%%)", against$region,
      sub("Innovator.Power", "clean energy", sub("Incumbent.Power", "fossil dependence", against$driver)),
      100 * against$share), collapse = ", ")) else "direction ok",
    "; cap shift ", round(capShift, 2), if (!is.na(capRegion)) paste0(" (", capRegion, ")") else "",
    " vs ", g$capShift)
  list(pass = pass, against = against, capShift = capShift, capRegion = capRegion, detail = detail)
}

# The declared defaults of the edge gate (design note 0007 section 5).
#' @keywords internal
.pfmEdgeGateDefaults <- function() {
  list(share = 0.25, effect = 0.25, capShift = 0.5, cap = c(0.05, 0.95), mapping = "regionmapping_21_EU11.csv")
}
# nolint end
