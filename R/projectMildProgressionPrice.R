# nolint start
#' The "mild progression" carbon price path (Elmar variant)
#'
#' @description
#' A carbon price built \strong{bottom-up} from the political ambition gap, rather
#' than derived top-down from a cost-optimal anchor. The price grows at a rate
#' proportional to how far a polity sits below its own feasibility frontier:
#'
#' \deqn{P(t+1) = P(t)\left(1 + \lambda \frac{S^{*}(t+1) - S(t)}{S(t)}\right)}
#'
#' starting from the observed current-policy price in the seed year.
#'
#' \strong{How this differs from the other two bind modes.} Mode R and mode L both
#' start from REMIND's cost-optimal price and \emph{constrain} it — politics is a
#' brake. Mild progression never references the cost-optimal path at all: the price is
#' \emph{generated} by the political dynamics, and whatever emissions follow are the
#' result. There is no carbon budget in it, so it cannot be infeasible; it answers
#' "where does observed political momentum take us?" rather than "can politics deliver
#' the budget?".
#'
#' \strong{The gap is relative to current stringency, not to the frontier.} Note
#' \eqn{(S^{*}-S)/S = 1/E - 1}, which is unbounded as \eqn{S \to 0} — a country doing
#' almost nothing has an almost infinite relative gap and would see explosive price
#' growth. This is a real property of the specification, not a bug, and it is why
#' \code{maxGrowth} exists: the recursion is capped at a stated per-period growth rate
#' and the cap's bite is reported, never applied silently.
#'
#' \strong{Convergence.} Step 3 of the design re-derives \eqn{S} and \eqn{S^{*}} from
#' the scenario the price itself produced, and iterates. That outer loop lives in the
#' REMIND coupling; this function is one pass of step 1.
#'
#' @param stringency Data.frame with \code{region, year, feasibleIndex}
#'   (\eqn{S}) and \code{ceilingIndex} (\eqn{S^{*}}) — the output of
#'   \code{\link{projectFeasiblePath}}.
#' @param priceSeed Named numeric vector (ISO3 or region) of the seed-year carbon
#'   price in US$/tCO2, typically read from an NPi run.
#' @param lambda Adjustment speed, scalar or named per region. The ECM speed from the
#'   temporal validation; \strong{on the logit scale}, and used here as a bare
#'   multiplier exactly as specified in the design note.
#' @param seedYear First year of the recursion. Default 2025.
#' @param maxGrowth Cap on per-period fractional growth. Default 1 (i.e. no more than
#'   a doubling per period). \code{Inf} disables it and is not recommended — see the
#'   note on \eqn{1/E - 1} above.
#' @param holdAfter Years beyond this are held constant. Default 2100, per the design
#'   note ("keep carbon prices post 2100 constant").
#'
#' @return Data.frame \code{region, year, price, gapRatio, growth, capped}, one row per
#'   region-year. \code{capped} flags where \code{maxGrowth} bound the step — always
#'   check it before quoting a path.
#'
#' @seealso \code{\link{projectFeasiblePath}}, \code{\link{exportFeasibilityBound}},
#'   \code{docs/psm-coupling-scenario-design.md}.
#' @export
#' @author Renato Rodrigues
projectMildProgressionPrice <- function(stringency, priceSeed, lambda,
                                        seedYear = 2025, maxGrowth = 1,
                                        holdAfter = 2100) {
  need <- c("region", "year", "feasibleIndex", "ceilingIndex")
  miss <- setdiff(need, colnames(stringency))
  if (length(miss)) {
    stop("projectMildProgressionPrice: 'stringency' is missing column(s): ",
         paste(miss, collapse = ", "), " - pass projectFeasiblePath() output.")
  }
  if (!is.numeric(priceSeed) || is.null(names(priceSeed))) {
    stop("projectMildProgressionPrice: 'priceSeed' must be a NAMED numeric vector ",
         "of seed-year prices in US$/tCO2.")
  }
  if (!is.numeric(maxGrowth) || length(maxGrowth) != 1 || maxGrowth <= 0) {
    stop("projectMildProgressionPrice: 'maxGrowth' must be a single positive value.")
  }

  d <- stringency[order(stringency$region, stringency$year), , drop = FALSE]
  d$region <- as.character(d$region)
  regs <- intersect(unique(d$region), names(priceSeed))
  if (!length(regs)) {
    stop("projectMildProgressionPrice: no region has both a stringency path and a ",
         "seed price.")
  }
  lamOf <- function(r) {
    if (length(lambda) == 1L) return(as.numeric(lambda))
    v <- lambda[[r]]
    if (is.null(v) || !is.finite(v)) as.numeric(lambda[[1]]) else as.numeric(v)
  }

  out <- do.call(rbind, lapply(regs, function(r) {
    dr <- d[d$region == r & d$year >= seedYear, , drop = FALSE]
    if (!nrow(dr)) return(NULL)
    lam <- lamOf(r)
    yrs <- dr$year
    p <- rep(NA_real_, length(yrs))
    gap <- rep(NA_real_, length(yrs))
    grow <- rep(0, length(yrs))
    cap <- rep(FALSE, length(yrs))
    p[1] <- as.numeric(priceSeed[[r]])
    for (i in seq_along(yrs)[-1]) {
      s <- dr$feasibleIndex[i - 1]
      sStar <- dr$ceilingIndex[i]
      # (S* - S)/S is unbounded as S -> 0. Guard ONLY against a non-positive
      # denominator: a near-zero stringency genuinely IS a huge relative gap, so the
      # right response is a large growth rate that maxGrowth then bounds - not a
      # zeroed one, which would say "nothing to close" about a country doing nothing.
      g <- if (is.finite(s) && is.finite(sStar) && s > 0) (sStar - s) / s else 0
      gap[i] <- g
      step <- lam * g
      if (is.finite(step) && step > maxGrowth) { step <- maxGrowth; cap[i] <- TRUE }
      if (!is.finite(step)) step <- 0
      # A negative gap (already above the frontier) is allowed to pull the price down,
      # but never below zero - a negative carbon price is not in this model's grammar.
      grow[i] <- step
      p[i] <- max(p[i - 1] * (1 + step), 0)
      if (yrs[i] > holdAfter) p[i] <- p[i - 1]
    }
    data.frame(region = r, year = yrs, price = p, gapRatio = gap,
               growth = grow, capped = cap, stringsAsFactors = FALSE)
  }))
  rownames(out) <- NULL
  attr(out, "seedYear") <- seedYear
  attr(out, "maxGrowth") <- maxGrowth
  attr(out, "holdAfter") <- holdAfter
  attr(out, "cappedShare") <- mean(out$capped, na.rm = TRUE)
  out
}
# nolint end
