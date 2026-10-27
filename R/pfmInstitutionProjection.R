# nolint start
#' How institution series without an SSP projection are projected
#'
#' @description
#' The Andrijevic et al. (2020) SSP extensions project Government Effectiveness, Control of
#' Corruption and (for SSP1-3) a Rule-of-Law index. Nothing projects the V-Dem series the model
#' also uses (Vertical / Horizontal / Diagonal Accountability, Rule of Law, the state-capacity
#' indicators) or the WGI Voice and Accountability, Political Stability and Regulatory Quality.
#' Their future is a declared assumption, one of three rules (design note 0005 D10):
#' \describe{
#'   \item{\code{"storyline"}}{(b) the default: logistic convergence with SSP-specific
#'     parameters, from the SSP governance narratives (Andrijevic et al. 2020): SSP1 and SSP5
#'     converge faster and higher, SSP3 and SSP4 slowly and only to the median, SSP2 exactly as
#'     (a). A declared table, not an estimate.}
#'   \item{\code{"convergence"}}{(a) the same path for every SSP: logistic convergence to the
#'     global 75th percentile (of the cross-section at the last observed year), half the gap
#'     closed by 2080, complete by 2150 - the rule every Run-Group up to v5 used. Sensitivity:
#'     the SSPs then differ only through the series that have an SSP projection.}
#'   \item{\code{"hold"}}{(d) the last observed value, held: no institutional change.
#'     Sensitivity, a bound.}
#'   \item{\code{"gapclosure"}}{(e) every country closes a fixed share of its distance to the top of
#'     the scale each year, \eqn{x(t) = 1 - (1 - x_a) e^{-\lambda (t - t_a)}}, on the panel's
#'     normalised scale (0 = worst, 1 = best value observed), from its value \eqn{x_a} in the last
#'     year of the estimation panel \eqn{t_a}: the projection starts where the fitted history ends,
#'     with no step. The rate \eqn{\lambda} is the one that best reproduces PoliClim's SSP2 country
#'     projections for the nearest PoliClim indicator (\code{\link{pfmInstitutionGapClosureRates}};
#'     \code{analysis/checks/policlimRuleFit.R}). No country declines; countries further from the top
#'     improve faster. Applied inside \code{\link{panelDataScenario}}, after normalisation
#'     (\code{.pfmGapClosurePanel}). The SSP2 central rule since 2026-10-27 (design note 0008).}
#' }
#' In the convergence rules a country already above the target keeps its value rather than being
#' pulled down (\code{keepIfAboveTarget}).
#'
#' @param rule \code{"storyline"} (default), \code{"convergence"}, \code{"hold"} or
#'   \code{"gapclosure"}.
#' @param ssp \code{"SSP1"} ... \code{"SSP5"}; only \code{"storyline"} depends on it.
#' @return A list of \code{\link[mrpfm]{toolProjectScenario}} arguments (\code{mode},
#'   \code{percentile}, \code{midpointYear}, \code{convergenceYear}, \code{shape},
#'   \code{keepIfAboveTarget}), with attribute \code{label}.
#' @seealso \code{\link{pfmInstitutionStorylines}} for the declared table.
#' @export
#' @author Renato Rodrigues
pfmInstitutionProjection <- function(rule = c("storyline", "convergence", "hold", "gapclosure"), ssp = "SSP2") {
  rule <- match.arg(rule)
  if (!ssp %in% paste0("SSP", 1:5)) stop("pfmInstitutionProjection: unknown ssp '", ssp, "'", call. = FALSE)
  base <- list(mode = "global_percentile", shape = "logistic", keepIfAboveTarget = TRUE)
  out <- switch(rule,
    convergence = c(base, list(percentile = 75, midpointYear = 2080, convergenceYear = 2150)),
    hold = list(mode = "constant", shape = "linear", keepIfAboveTarget = TRUE,
                percentile = NA_real_, midpointYear = NULL, convergenceYear = 2150),
    gapclosure = list(mode = "gapclosure", rates = pfmInstitutionGapClosureRates()),
    storyline = {
      s <- pfmInstitutionStorylines()
      r <- s[s$ssp == ssp, ]
      c(base, list(percentile = r$percentile, midpointYear = r$midpointYear,
                   convergenceYear = r$convergenceYear))
    })
  attr(out, "label") <- if (rule == "hold") "hold at the last observed value" else if (rule == "gapclosure")
    "gap closure: a fixed share of the distance to the top of the scale each year (PoliClim SSP2 rates)" else
    paste0(rule, " (", ssp, "): global ", out$percentile, "th percentile, half by ",
           out$midpointYear, ", complete by ", out$convergenceYear)
  out
}

#' The declared SSP storyline for institution series without an SSP projection
#'
#' @description The parameters of rule \code{"storyline"} in
#'   \code{\link{pfmInstitutionProjection}}. SSP2 equals the \code{"convergence"} rule. SSP4's
#'   unequal storyline (strong institutions at the top, stagnation below) cannot be expressed
#'   by one cross-sectional target; it is approximated by SSP3's slow, partial convergence.
#' @return A data frame: \code{ssp}, \code{percentile} (target, of the cross-section at the
#'   last observed year), \code{midpointYear} (half the gap closed), \code{convergenceYear}.
#' @export
#' @author Renato Rodrigues
pfmInstitutionStorylines <- function() {
  data.frame(
    ssp             = c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5"),
    percentile      = c(90,     75,     50,     50,     90),
    midpointYear    = c(2060,   2080,   2100,   2100,   2060),
    convergenceYear = c(2100,   2150,   2150,   2150,   2100),
    stringsAsFactors = FALSE
  )
}

#' The rates of the gap-closure institution rule
#'
#' @description The yearly share of the distance to the top of the scale that every country closes
#'   under rule \code{"gapclosure"} of \code{\link{pfmInstitutionProjection}}. Each rate is the
#'   least-squares fit of that rule to PoliClim's SSP2 country projections, 2030-2100, for the nearest
#'   PoliClim indicator (\code{analysis/checks/policlimRuleFit.R}): rule of law (V-Dem
#'   \code{v2xcl_rol}) for V-Dem Rule of Law, overall accountability for every other series. The rate
#'   is taken from PoliClim, the series is not. The top in the fit is the best value PoliClim observes (1996-2025), as in the rule. Rounded to two significant digits.
#' @return A data frame: \code{series} (a regular expression on the series name, matched in order;
#'   the last row matches everything), \code{rate} (per year), \code{halfLife} (years),
#'   \code{fittedOn} (the PoliClim indicator).
#' @export
#' @author Renato Rodrigues
pfmInstitutionGapClosureRates <- function() {
  data.frame(series   = c("^Rule of Law \\(VDem\\)$", "."),
             rate     = c(0.0096, 0.0056),
             halfLife = round(log(2) / c(0.0096, 0.0056)),
             fittedOn = c("PoliClim SSP2 rule of law (V-Dem v2xcl_rol)", "PoliClim SSP2 overall accountability"),
             stringsAsFactors = FALSE)
}

# Apply a rule to one magpie of institution series (wraps mrpfm::toolProjectScenario). Under
# "gapclosure" the raw series is held here; panelDataScenario replaces its projection years on the
# normalised scale, from the panel's last value (.pfmGapClosurePanel).
#' @keywords internal
.pfmProjectInstitutions <- function(x, y, rule, ssp) {
  if (identical(rule, "gapclosure")) rule <- "hold"
  a <- pfmInstitutionProjection(rule, ssp)
  args <- list(x = x, y = y, mode = a$mode, shape = a$shape, convergenceYear = a$convergenceYear,
               keepIfAboveTarget = a$keepIfAboveTarget)
  if (identical(a$mode, "global_percentile")) {
    args$percentile <- a$percentile
    args$midpointYear <- a$midpointYear
  }
  do.call(mrpfm::toolProjectScenario, args)
}

# The gap-closure path: from x0 at t0, a fixed share `rate` of the distance to `top` closed each year.
#' @keywords internal
.pfmGapClosurePath <- function(x0, t, t0, rate, top = 1) top - (top - x0) * exp(-rate * (t - t0))

# The rate of a series under the gap-closure rule (pfmInstitutionGapClosureRates, first matching row).
#' @keywords internal
.pfmGapClosureRate <- function(v) {
  r <- pfmInstitutionGapClosureRates()
  r$rate[which(vapply(r$series, grepl, logical(1), x = v))[1]]
}

# The gap-closure rule on a harmonised scenario panel (normalised scale, 1 = best observed): every year
# after the panel's last year `t0` of the series in `ruleVars` follows .pfmGapClosurePath from the
# history panel's value at t0. The democracy weights in `levelVars` move from their t0 value to the
# higher of that value and their mean over the last ten panel years, along a half cosine reaching the
# level at `fadeYear`, then held: a small recovery from the recent decline, without a step.
#' @keywords internal
.pfmGapClosurePanel <- function(out, hist, ruleVars, levelVars, t0, fadeYear) {
  yrs <- magclass::getYears(out, as.integer = TRUE); after <- yrs > t0
  if (!any(after)) return(out)
  hy <- magclass::getYears(hist, as.integer = TRUE)
  regs <- intersect(magclass::getItems(out, dim = 1), magclass::getItems(hist, dim = 1))
  for (v in intersect(ruleVars, intersect(magclass::getNames(out), magclass::getNames(hist)))) {
    x0 <- as.numeric(hist[regs, t0, v]); ok <- is.finite(x0)
    path <- outer(x0[ok], yrs[after], function(x, t) .pfmGapClosurePath(x, t, t0, .pfmGapClosureRate(v)))
    out[regs[ok], yrs[after], v] <- path
  }
  for (v in intersect(levelVars, intersect(magclass::getNames(out), magclass::getNames(hist)))) {
    x0 <- as.numeric(hist[regs, t0, v])
    last10 <- hy[hy > t0 - 10 & hy <= t0]
    m10 <- rowMeans(as.array(hist[regs, last10, v])[, , 1, drop = FALSE], na.rm = TRUE)
    lvl <- ifelse(is.finite(m10), pmax(x0, m10), x0); ok <- is.finite(x0)
    ramp <- (1 - cos(pi * pmin(1, (yrs[after] - t0) / (fadeYear - t0)))) / 2
    out[regs[ok], yrs[after], v] <- x0[ok] + outer(lvl[ok] - x0[ok], ramp)
  }
  out
}
# nolint end
