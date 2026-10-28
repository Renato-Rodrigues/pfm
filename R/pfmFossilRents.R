# nolint start
# Fossil rents in the scenarios (design note 0009 section 8.2): the size of the frontier's slack follows each
# country's fossil rents, and in a coupled run those come from REMIND.

#' REMIND's regional fossil rents
#'
#' Per region and period of a REMIND run: the sum over coal, oil and gas of the value of extraction at the
#' region's primary-energy price less the extraction cost, as a share of GDP,
#' \deqn{\text{rents}_r(t) = 100\,\frac{\sum_f p_{r,f}(t)\,\text{extraction}_{r,f}(t) - \text{cost}_{r,f}(t)}{\text{GDP}_r(t)}}
#' from \code{pm_PEPrice}, \code{vm_fuExtr} (summed over grades), \code{vm_costFuEx} and \code{vm_cesIO("inco")}.
#'
#' @param gdxFile A REMIND gdx.
#' @return A matrix, regions x periods (2005-2150), \% of GDP.
#' @keywords internal
.pfmREMINDFossilRents <- function(gdxFile) {
  rd <- function(s, ...) suppressWarnings(gdx::readGDX(gdxFile, s, react = "silent", restore_zeros = FALSE, ...))
  ex <- rd("vm_fuExtr", field = "l"); pr <- rd("pm_PEPrice"); co <- rd("vm_costFuEx", field = "l")
  gdp <- rd("vm_cesIO", field = "l")
  if (is.null(ex) || is.null(pr) || is.null(co) || is.null(gdp)) {
    stop(".pfmREMINDFossilRents: the gdx lacks vm_fuExtr, pm_PEPrice, vm_costFuEx or vm_cesIO", call. = FALSE)
  }
  gdp <- gdp[, , "inco"]
  yy <- Reduce(intersect, list(magclass::getYears(ex, as.integer = TRUE), magclass::getYears(pr, as.integer = TRUE),
                               magclass::getYears(gdp, as.integer = TRUE)))
  yy <- yy[yy >= 2005 & yy <= 2150]
  rent <- 0
  for (f in c("pecoal", "peoil", "pegas")) {
    e <- magclass::dimSums(ex[, yy, grep(paste0("^", f, "\\."), magclass::getNames(ex), value = TRUE)], dim = 3)
    rent <- rent + pr[, yy, f] * e - co[, yy, f]
  }
  out <- 100 * as.array(rent)[, , 1] / as.array(gdp[, yy, ])[, , 1]
  colnames(out) <- as.character(yy)
  out
}

#' The scenario path of a country's fossil rents
#'
#' History (the harmonisation panel's \code{"Fossil Rents"}, log(1 + \% of GDP), moving-averaged) up to its last
#' year; after it, each country's last value scaled by its REMIND region's rents relative to \code{t0}:
#' \eqn{\text{rents}_c(t) = \text{rents}_c(t_h)\cdot \text{rents}_r(t)/\text{rents}_r(t_0)}, the ratio bounded to
#' [0, 3] (a region with almost no rents at \code{t0} would otherwise explode). A country without a history keeps
#' no value (\code{NA}); a country outside the gdx mapping keeps its last value.
#'
#' @param histPanel The historical (harmonisation) panel, with \code{"Fossil Rents"}.
#' @param gdxFile The run's gdx.
#' @param gdxRegionMappingFile The gdx's region mapping.
#' @param years The scenario panel's years.
#' @param units The scenario panel's units (countries).
#' @param t0 The REMIND period the regional ratio is taken against.
#' @return A magpie object, \code{units} x \code{years} x \code{"Fossil Rents"} (log(1 + \% of GDP)).
#' @keywords internal
.pfmScenarioFossilRents <- function(histPanel, gdxFile, gdxRegionMappingFile, years, units, t0 = 2025) {
  if (!"Fossil Rents" %in% magclass::getNames(histPanel)) {
    stop(".pfmScenarioFossilRents: the historical panel has no 'Fossil Rents' (panel key fossilRents)", call. = FALSE)
  }
  hy <- magclass::getYears(histPanel, as.integer = TRUE); th <- max(hy)
  out <- magclass::new.magpie(units, years, "Fossil Rents", fill = NA)
  hu <- intersect(units, magclass::getItems(histPanel, dim = 1))
  h <- as.array(histPanel[hu, , "Fossil Rents"])[, , 1, drop = TRUE]
  if (is.null(dim(h))) h <- matrix(h, nrow = length(hu), dimnames = list(hu, paste0("y", hy)))
  last <- h[, paste0("y", th)]
  rr <- .pfmREMINDFossilRents(gdxFile)
  base <- rr[, as.character(t0)]
  ratio <- sweep(rr, 1, base, "/"); ratio[!is.finite(ratio)] <- 1; ratio <- pmin(pmax(ratio, 0), 3)
  map <- pfmGetMapping(gdxRegionMappingFile, type = "regional")
  reg <- stats::setNames(map$RegionCode, map$CountryCode)
  ry <- as.integer(colnames(rr))
  for (yv in years) {
    if (yv <= th) {
      # history: the panel's value in that year, or the nearest panel year
      k <- paste0("y", hy[which.min(abs(hy - yv))])
      out[hu, yv, 1] <- h[, k]
    } else {
      f <- vapply(hu, function(c) { r <- unname(reg[c])
        if (is.na(r) || !r %in% rownames(rr)) 1 else stats::approx(ry, ratio[r, ], xout = yv, rule = 2)$y }, numeric(1))
      out[hu, yv, 1] <- log1p(expm1(last) * f)
    }
  }
  out
}
# nolint end
