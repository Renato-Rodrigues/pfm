# nolint start
# The v6 coupling path (design note 0005 Phase 3; ADR 0049, 0050, 0054): the shares phi_{r,s}(t) from
# the anchor artifact (phi-anchor.rds) and the scenario panel of the current REMIND solution. No ECM,
# no lambda. Called by iterativePFM() when the formulation is "v6-anchor"; usable offline.
#
# SSP. Everything SSP-dependent follows the run's SSP: the scenario panel (GDP, population, SSP
# extensions, the institution rule) is built for it by the caller, and the region weights of the
# strength factor are recomputed for it when it differs from the anchor's (the anchor's u and q are
# read from history and are the same in every SSP; its weights are final energy at t0, where the SSPs
# coincide until 2029, but they are recomputed rather than assumed).

#' Options of the v6 coupling
#'
#' @description
#' The typed options of the v6 coupling (design note 0005 F8, D6, D11, D13, D15, D16), with their
#' headline values. Inside a REMIND run they come from \code{pfm-coupling.yml} (written by
#' \code{preparePFM.R} from the scenario row) and may be overridden by the GAMS runtime file; every value
#' is echoed in the log and stored in \code{pfm-phi-history.rds}.
#' \describe{
#'   \item{holdYear}{year after which \eqn{k} (and \eqn{d}) are held: 2100 (central, D6) or 2060.}
#'   \item{hold}{\code{"logit"} (the v6 rule) or \code{"ratio"} (the \eqn{E}-hold bound, D2 / C5).}
#'   \item{spread}{\code{"k"} (headline, \eqn{d = k}) or \code{"model"} (D11, diagnostic).}
#'   \item{ordering}{\code{"model"}, \code{"uniform"}, \code{"reversed"} or \code{"permuted"} (D15).}
#'   \item{orderingSeed}{seed of the permutation.}
#'   \item{kappa}{declared closure on the strength, \eqn{k(t)(1-\kappa)^{t-t_0}} (D13); 0 = none.}
#'   \item{strength}{\code{"common"} (headline) or \code{"regional"}, \eqn{k_{r,s} = g_{r,s}(t)/g_{r,s}(t_0)} (D16).}
#' }
#' @return A named list of the defaults.
#' @export
#' @author Renato Rodrigues
pfmV6CouplingDefaults <- function() {
  list(holdYear = 2100, hold = "logit", spread = "k", ordering = "model", orderingSeed = 1L,
       kappa = 0, strength = "common")
}

# Merge the option sources (later wins) and validate. Unknown keys are ignored here: the YAML files
# carry other settings too.
#' @keywords internal
.pfmV6Options <- function(...) {
  o <- pfmV6CouplingDefaults()
  for (src in list(...)) {
    if (is.null(src)) next
    for (k in intersect(names(o), names(src))) if (!is.null(src[[k]])) o[[k]] <- src[[k]]
  }
  o$holdYear <- as.numeric(o$holdYear)
  o$kappa <- as.numeric(o$kappa)
  o$orderingSeed <- as.integer(o$orderingSeed)
  o$hold <- match.arg(as.character(o$hold), c("logit", "ratio"))
  o$spread <- match.arg(as.character(o$spread), c("k", "model"))
  o$ordering <- match.arg(as.character(o$ordering), c("model", "uniform", "reversed", "permuted"))
  o$strength <- match.arg(as.character(o$strength), c("common", "regional"))
  if (!is.finite(o$holdYear)) stop("v6 coupling: holdYear must be a year", call. = FALSE)
  if (!is.finite(o$kappa) || o$kappa < 0 || o$kappa >= 1) stop("v6 coupling: kappa must lie in [0, 1)", call. = FALSE)
  if (o$strength == "regional" && o$ordering != "model") {
    stop("v6 coupling: the regional strength arm (D16) is not combined with an ordering test (D15)", call. = FALSE)
  }
  o
}

# The convergence checkpoints (D5): the years whose phi decides convergence.
#' @keywords internal
.pfmV6Checkpoints <- function(holdYear) if (holdYear <= 2060) c(2035, 2050, 2060) else c(2035, 2050, 2070, 2100)

# "auto": a Run-Group (export) with an anchor artifact couples by the v6 formulation; any other by v5.
#' @keywords internal
.pfmCouplingFormulation <- function(gd, requested = "auto") {
  requested <- match.arg(as.character(requested), c("auto", "v6-anchor", "v5-tier"))
  has <- file.exists(file.path(gd, "phi-anchor.rds"))
  if (requested == "auto") return(if (has) "v6-anchor" else "v5-tier")
  if (requested == "v6-anchor" && !has) {
    stop("formulation 'v6-anchor' needs phi-anchor.rds in '", gd, "' (pfmRun(steps = \"pfm-anchor\"), then export).",
         call. = FALSE)
  }
  requested
}

# Which of the artifact's resolutions the delivery mapping is.
#' @keywords internal
.pfmAnchorResolution <- function(art, mapping) {
  if (!is.character(mapping) || length(mapping) != 1) {
    stop("v6 coupling: the delivery mapping must be a file name (one of the anchor's resolutions)", call. = FALSE)
  }
  hit <- names(art$byResolution)[vapply(art$byResolution, function(b) identical(basename(b$mapping), basename(mapping)), logical(1))]
  if (!length(hit)) {
    stop("v6 coupling: the anchor artifact has no resolution for mapping '", mapping, "' (has: ",
         paste(vapply(art$byResolution, `[[`, "", "mapping"), collapse = ", "), "). Re-run pfm-anchor with it.", call. = FALSE)
  }
  hit[1]
}

#' The v6 shares phi(t) from an anchor and a scenario panel
#'
#' @description
#' The whole v6 coupling step except the REMIND interface: \code{\link{computeStrengthPath}} on the
#' scenario panel, the declared options (\code{\link{pfmV6CouplingDefaults}}), and
#' \code{\link{computeSharePath}} (or the regional-strength arm). Region weights are recomputed when
#' \code{weights} is given (the run's SSP or weight year differs from the anchor's).
#'
#' @param anchor An anchor, as \code{\link{pfmAnchorFor}} or \code{\link{computeAnchorGap}} returns it.
#' @param scenarioData The scenario panel of the current solution (\code{\link{panelDataScenario}},
#'   the run's SSP).
#' @param theta Severity.
#' @param options Typed options (\code{\link{pfmV6CouplingDefaults}}).
#' @param weights Optional country weights replacing the anchor's (named numeric).
#' @return A list: \code{shares} (sector, region, year, u, phiRaw, phi, clipped; sector rows only),
#'   \code{floor} (region, year, phi: the minimum over sectors), \code{strength} (per sector and year,
#'   with \code{k} after the closure arm), \code{options}.
#' @export
#' @author Renato Rodrigues
pfmV6Shares <- function(anchor, scenarioData, theta, options = pfmV6CouplingDefaults(), weights = NULL) {
  o <- .pfmV6Options(options)
  if (!is.null(weights)) {
    anchor$weights <- weights
    anchor$regionWeights <- .pfmRegionWeights(weights, anchor$mapping)
  }
  st <- computeStrengthPath(anchor, scenarioData, holdYear = o$holdYear, theta = theta, hold = o$hold)
  fade <- function(y) if (o$kappa > 0) (1 - o$kappa)^pmax(y - anchor$t0, 0) else 1
  st$strength$k <- st$strength$k * fade(st$strength$year)
  st$strength$d <- st$strength$d * fade(st$strength$year)
  if (o$strength == "common") {
    sh <- computeSharePath(anchor, st, theta = theta, spread = o$spread, ordering = o$ordering, seed = o$orderingSeed)
    sh <- sh[sh$sector != "min", , drop = FALSE]
  } else {
    # D16: each region's own strength, k_r(t) = g_r(t)/g_r(t0), held after holdYear like k.
    rg <- st$regions
    g0 <- rg[rg$year == anchor$t0, c("sector", "region", "g")]
    rg <- merge(rg, g0, by = c("sector", "region"), suffixes = c("", "0"))
    rg$kr <- rg$g / rg$g0
    held <- rg[rg$year == max(rg$year[rg$year <= o$holdYear]), c("sector", "region", "kr")]
    late <- rg$year > o$holdYear
    if (any(late)) rg$kr[late] <- held$kr[match(paste(rg$sector[late], rg$region[late]), paste(held$sector, held$region))]
    rg$kr <- rg$kr * fade(rg$year)
    sh <- merge(rg[, c("sector", "region", "year", "kr")], anchor$region[, c("sector", "region", "u")], by = c("sector", "region"))
    sh$phiRaw <- 1 - theta * sh$kr * sh$u
    sh$phi <- pmin(pmax(sh$phiRaw, 0), 1)
    sh$clipped <- sh$phiRaw < 0 | sh$phiRaw > 1
    sh <- sh[, c("sector", "region", "year", "u", "phiRaw", "phi", "clipped")]
  }
  sh <- sh[order(sh$sector, sh$region, sh$year), , drop = FALSE]
  rownames(sh) <- NULL
  fl <- stats::aggregate(phi ~ region + year, data = sh, FUN = min)
  list(shares = sh, floor = fl[order(fl$region, fl$year), ], strength = st$strength, options = o)
}

# Largest |change| in phi between two calls: over sectors (hence markets) and the floor, at the
# checkpoint years (`years`), or over every year >= t0 (years = NULL). Inf without a previous path.
#' @keywords internal
.pfmPhiPathDelta <- function(cur, prev, years = NULL, t0 = 2025) {
  if (is.null(prev)) return(Inf)
  key <- function(d) paste(d$sector, d$region, d$year)
  sel <- function(d) { d <- d[d$year >= t0, , drop = FALSE]; if (!is.null(years)) d <- d[d$year %in% years, , drop = FALSE]; d }
  a <- sel(cur); b <- sel(prev)
  common <- intersect(key(a), key(b))
  if (!length(common)) return(Inf)
  max(abs(a$phi[match(common, key(a))] - b$phi[match(common, key(b))]))
}

# Damping, only on oscillation (D5): the checkpoint shares moved back against their previous move
# (cosine of the two successive changes below -0.5, which also reads a uniform back-and-forth shift)
# without shrinking below half of it. Then phi = prev + alpha (cur - prev), alpha = 0.5.
# Returns list(shares, alpha, oscillating).
#' @keywords internal
.pfmV6Damp <- function(cur, prev1, prev2, years, alpha = 0.5) {
  none <- list(shares = cur, alpha = 1, oscillating = FALSE)
  if (is.null(prev1) || is.null(prev2)) return(none)
  key <- function(d) paste(d$sector, d$region, d$year)
  at <- function(d) d[d$year %in% years, , drop = FALSE]
  k <- Reduce(intersect, list(key(at(cur)), key(at(prev1)), key(at(prev2))))
  if (length(k) < 3) return(none)
  v <- function(d) at(d)$phi[match(k, key(at(d)))]
  d1 <- v(cur) - v(prev1); d0 <- v(prev1) - v(prev2)
  n1 <- sqrt(sum(d1^2)); n0 <- sqrt(sum(d0^2))
  if (n1 == 0 || n0 == 0) return(none)
  osc <- sum(d1 * d0) / (n1 * n0) < -0.5 && max(abs(d1)) > 0.5 * max(abs(d0))
  if (!osc) return(none)
  p <- prev1$phi[match(key(cur), key(prev1))]
  ok <- is.finite(p)
  cur$phi[ok] <- p[ok] + alpha * (cur$phi[ok] - p[ok])
  list(shares = cur, alpha = alpha, oscillating = TRUE)
}

# A share path over every period GAMS may read (REMIND's ttot, from 1900): before t0 the t0 value,
# between or after the scenario's years the last value at or before (a missing record loads as ZERO
# in GAMS, i.e. a zero share). d: region, year, phi.
#' @keywords internal
.pfmCompletePath <- function(d, years, t0) {
  years <- sort(unique(c(as.integer(years), as.integer(d$year))))
  do.call(rbind, lapply(split(d, d$region), function(x) {
    x <- x[order(x$year), ]
    at0 <- x$phi[x$year == t0]
    if (!length(at0)) at0 <- x$phi[1]
    phi <- vapply(years, function(y) {
      if (y < t0) return(at0[1])
      i <- which(x$year <= y)
      if (length(i)) x$phi[max(i)] else at0[1]
    }, numeric(1))
    data.frame(region = x$region[1], year = years, phi = phi, stringsAsFactors = FALSE)
  }))
}
# nolint end

