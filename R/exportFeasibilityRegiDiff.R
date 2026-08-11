# nolint start
#' Write the REMIND feasibility-differentiation include file (cm_taxCO2_regiDiff = 11)
#'
#' @description
#' Emits \code{p45_regiDiff_feasibility.inc}, the GAMS include that REMIND's
#' \code{45_carbonprice/functionalForm} realization reads when
#' \code{cm_taxCO2_regiDiff = 11} ("feasibility"). It carries one political
#' feasibility share \eqn{\varphi} per REMIND region, and an optional closure rate
#' \eqn{\lambda}.
#'
#' Inside REMIND the regional carbon price becomes
#' \deqn{P_{r,t} = \left[1 - (1-\varphi_r)(1-\lambda_r)^{t-t_0}\right]\, A_t}
#' where \eqn{A_t} is the global anchor trajectory. With \eqn{\lambda = 0} (the
#' default) the ratio is simply \eqn{\varphi_r} for the whole horizon: **the
#' political gap between regions persists and is never assumed away**. This is the
#' point of the option — every other \code{cm_taxCO2_regiDiff} realization forces
#' convergence to a uniform global price by an assumed date.
#'
#' Because the share multiplies the anchor rather than capping the price, this is
#' compatible with the budget iteration (\code{cm_iterative_target_adj = 5/7/9}):
#' the anchor rescales until the carbon budget is met, so the target is preserved
#' and politics only redistributes \emph{where} the abatement happens. Expect the
#' anchor — and therefore the price faced by politically unconstrained regions — to
#' rise relative to the uniform-pricing run. That shift is the result of interest.
#'
#' @param feasibility Region-level feasibility from
#'   \code{\link{aggregateFeasibilityToRegions}} — needs \code{region} and
#'   \code{phi}, and \code{sector} unless \code{sectorRule = "none"}. Regions whose
#'   ceiling was not valid carry \code{phi = 1} and are therefore uncoupled.
#' @param file Output path (\code{.inc}). The REMIND location is
#'   \code{modules/45_carbonprice/functionalForm/input/p45_regiDiff_feasibility.inc}.
#' @param lambda Closure rate: a scalar, a named-by-region vector, or \code{0}
#'   (default) for a persistent political gap. Pass the estimated political
#'   adjustment speed to let the gap close at that rate instead.
#' @param sectorRule How to reconcile sectors onto one price per region:
#'   \code{"min"} (default, the worse sector — the maximin discipline used
#'   throughout), \code{"mean"}, \code{"Bulk"}, \code{"Diffuse"} or \code{"none"}.
#' @param regions Optional character vector of \emph{all} REMIND regions. Any
#'   region absent from \code{feasibility} is written explicitly with
#'   \code{phi = 1} (uncoupled) rather than left out, so the file is total and a
#'   region can never be silently dropped.
#' @param digits Rounding for the emitted values. Default 6.
#'
#' @return Invisibly, the data.frame written (\code{region, phi, lambda}).
#'
#' @seealso \code{\link{aggregateFeasibilityToRegions}},
#'   \code{\link{exportFeasibilityBound}} (the price-path variant), ADR 0041.
#' @export
#' @author Renato Rodrigues
exportFeasibilityRegiDiff <- function(feasibility, file, lambda = 0,
                                      sectorRule = c("min", "mean", "Bulk",
                                                     "Diffuse", "none"),
                                      regions = NULL, digits = 6) {
  sectorRule <- match.arg(sectorRule)
  if (!all(c("region", "phi") %in% colnames(feasibility))) {
    stop("exportFeasibilityRegiDiff: 'feasibility' needs 'region' and 'phi' columns ",
         "- pass aggregateFeasibilityToRegions() output.")
  }
  if (!grepl("\\.inc$", file)) {
    stop("exportFeasibilityRegiDiff: 'file' must end in .inc (got '", file, "').")
  }
  f <- feasibility
  if (!identical(sectorRule, "none")) {
    if (!"sector" %in% colnames(f)) {
      stop("exportFeasibilityRegiDiff: sectorRule = '", sectorRule, "' needs a ",
           "'sector' column (or use sectorRule = 'none').")
    }
    if (sectorRule %in% c("Bulk", "Diffuse")) {
      f <- f[f$sector == sectorRule, , drop = FALSE]
    }
  }
  agg <- switch(sectorRule, min = min, mean = mean, function(x, ...) x[1])
  # phi is time-invariant by construction (tiers are assigned once and held
  # fixed), so collapse to one value per region and verify that assumption rather
  # than trusting it.
  byReg <- split(f$phi, as.character(f$region))
  spread <- vapply(byReg, function(v) diff(range(v, na.rm = TRUE)), numeric(1))
  if (any(spread > 1e-8, na.rm = TRUE)) {
    warning("exportFeasibilityRegiDiff: phi varies over time within ",
            sum(spread > 1e-8), " region(s); REMIND option 11 takes a single share ",
            "per region, so the ", sectorRule, " over all rows is used.", call. = FALSE)
  }
  phi <- vapply(byReg, function(v) {
    v <- v[is.finite(v)]
    if (!length(v)) 1 else agg(v)
  }, numeric(1))

  allRegions <- unique(c(names(phi), regions))
  phiFull <- stats::setNames(rep(1, length(allRegions)), allRegions)
  phiFull[names(phi)] <- phi
  phiFull[!is.finite(phiFull)] <- 1
  phiFull <- pmin(pmax(phiFull, 0), 1)

  lam <- if (is.null(names(lambda))) {
    stats::setNames(rep(lambda[[1]], length(allRegions)), allRegions)
  } else {
    v <- lambda[allRegions]
    v[!is.finite(v)] <- 0
    stats::setNames(as.numeric(v), allRegions)
  }
  lam <- pmin(pmax(lam, 0), 0.999)

  out <- data.frame(region = allRegions, phi = as.numeric(phiFull),
                    lambda = as.numeric(lam), stringsAsFactors = FALSE)
  out <- out[order(out$region), , drop = FALSE]
  rownames(out) <- NULL

  nUncoupled <- sum(out$phi >= 1 - 1e-12)
  lines <- c(
    "*** Political-feasibility carbon-price differentiation (cm_taxCO2_regiDiff = 11).",
    "*** GENERATED by pfm::exportFeasibilityRegiDiff() - do not edit by hand.",
    paste0("*** generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    "***",
    "*** phi    = share of the global anchor carbon price a region can politically realize,",
    "***          derived from its ambition-gap tier below the stochastic feasibility frontier.",
    "*** lambda = rate at which that gap closes. 0 = the gap PERSISTS for the whole horizon,",
    "***          which is the default and the reason this option exists.",
    "***",
    paste0("*** regions: ", nrow(out), " | uncoupled (phi = 1): ", nUncoupled,
           " | median phi: ", round(stats::median(out$phi), 3)),
    "***",
    paste0("*** phi = 1 for every region reproduces the uniform-pricing run exactly."),
    ""
  )
  # Plain assignments only. The include is pulled into a runtime if(...) block in
  # datainput.gms, and GAMS does not permit `parameter` declarations inside control
  # structures - the parameters are declared in declarations.gms instead.
  w <- max(nchar(out$region))
  num <- function(v) formatC(round(v, digits), format = "f", digits = digits)
  lines <- c(lines,
             paste0("p45_regiDiff_phi(\"", format(out$region, width = w), "\")",
                    strrep(" ", 1), " = ", num(out$phi), ";"),
             "",
             paste0("p45_regiDiff_lambda(\"", format(out$region, width = w), "\")",
                    strrep(" ", 1), " = ", num(out$lambda), ";"))
  dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
  writeLines(lines, file)
  invisible(out)
}
# nolint end
