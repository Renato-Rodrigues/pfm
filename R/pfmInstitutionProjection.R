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
#' }
#' In every rule a country already above the target keeps its value rather than being pulled
#' down (\code{keepIfAboveTarget}).
#'
#' @param rule \code{"storyline"} (default), \code{"convergence"} or \code{"hold"}.
#' @param ssp \code{"SSP1"} ... \code{"SSP5"}; only \code{"storyline"} depends on it.
#' @return A list of \code{\link[mrpfm]{toolProjectScenario}} arguments (\code{mode},
#'   \code{percentile}, \code{midpointYear}, \code{convergenceYear}, \code{shape},
#'   \code{keepIfAboveTarget}), with attribute \code{label}.
#' @seealso \code{\link{pfmInstitutionStorylines}} for the declared table.
#' @export
#' @author Renato Rodrigues
pfmInstitutionProjection <- function(rule = c("storyline", "convergence", "hold"), ssp = "SSP2") {
  rule <- match.arg(rule)
  if (!ssp %in% paste0("SSP", 1:5)) stop("pfmInstitutionProjection: unknown ssp '", ssp, "'", call. = FALSE)
  base <- list(mode = "global_percentile", shape = "logistic", keepIfAboveTarget = TRUE)
  out <- switch(rule,
    convergence = c(base, list(percentile = 75, midpointYear = 2080, convergenceYear = 2150)),
    hold = list(mode = "constant", shape = "linear", keepIfAboveTarget = TRUE,
                percentile = NA_real_, midpointYear = NULL, convergenceYear = 2150),
    storyline = {
      s <- pfmInstitutionStorylines()
      r <- s[s$ssp == ssp, ]
      c(base, list(percentile = r$percentile, midpointYear = r$midpointYear,
                   convergenceYear = r$convergenceYear))
    })
  attr(out, "label") <- if (rule == "hold") "hold at the last observed value" else
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

# Apply a rule to one magpie of institution series (wraps mrpfm::toolProjectScenario).
#' @keywords internal
.pfmProjectInstitutions <- function(x, y, rule, ssp) {
  a <- pfmInstitutionProjection(rule, ssp)
  args <- list(x = x, y = y, mode = a$mode, shape = a$shape, convergenceYear = a$convergenceYear,
               keepIfAboveTarget = a$keepIfAboveTarget)
  if (identical(a$mode, "global_percentile")) {
    args$percentile <- a$percentile
    args$midpointYear <- a$midpointYear
  }
  do.call(mrpfm::toolProjectScenario, args)
}
# nolint end
