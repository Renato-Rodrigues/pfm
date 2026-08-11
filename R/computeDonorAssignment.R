# nolint start
#' Donor assignment for countries without CAPMF coverage
#'
#' @description
#' Builds the reviewable assumptions table that lets a country with no policy data
#' inherit a *relative* ambition gap from the observed countries it most resembles.
#'
#' \strong{What is transferred, and why it is the gap and not the ceiling.} The
#' feasibility ceiling \eqn{S^{*}} is a function of drivers — governance, income,
#' energy structure — which exist for essentially every country, so it is already
#' computed everywhere. What CAPMF is needed for is the \emph{outcome}, and hence
#' the \emph{slack}: how far below its own ceiling a polity actually sits. That is
#' the only genuinely missing quantity, so it is the only thing donated. Each
#' recipient keeps its \strong{own} ceiling and inherits only the efficiency ratio
#'
#' \deqn{E = S / S^{*} \in (0, 1]}
#'
#' from its donors, giving \eqn{\hat S = \hat E \cdot S^{*}_{\text{own}}}. A
#' recipient with strong institutions therefore still gets a high ceiling; what it
#' borrows is the assumption about how much of that ceiling politics claims.
#'
#' \strong{Distance is measured in the model's own metric.} Raw driver distance
#' would treat every variable as equally relevant. Instead each standardized driver
#' is weighted by the magnitude of its fitted coefficient, so two countries are
#' "close" when they are close \emph{in the directions the model says move policy
#' stringency}:
#'
#' \deqn{d(i,j)^2 = \sum_k w_k (x_{ik} - x_{jk})^2, \qquad w_k = |\beta_k| / \sum |\beta|}
#'
#' Interactions and fixed effects are excluded — the former are functions of the
#' base drivers (double-counting), the latter are group labels rather than
#' characteristics.
#'
#' \strong{Countries no donor can reach still get an answer.} Matching covers well
#' under half the uncovered world, so the remainder is resolved by an explicit
#' \emph{band rule} rather than left to a silent \eqn{\varphi = 1} default (which
#' would assert \emph{maximal} political capability — a strong claim, and the exact
#' defect this machinery exists to remove). Each unmatched recipient is placed by its
#' position on the model's own signed linear index over the base drivers,
#' \eqn{\ell_i = \sum_k \beta_k x_{ik}} — the capability ordering the fitted equation
#' itself implies:
#'
#' \itemize{
#'   \item \eqn{\ell_i} \strong{below} every covered country's — a genuine
#'     extrapolation beyond the low end of support. Assigned the
#'     \code{bandPercentile} quantile of covered \eqn{E}. Justified because \eqn{E}
#'     genuinely falls with capability (cor(E, GovEff) = +0.52 Bulk / +0.65 Diffuse,
#'     p < 0.001) — and this is \emph{on top of} the ceiling effect, so it does not
#'     double-count \eqn{S^{*}}.
#'   \item \eqn{\ell_i} \strong{inside or above} the covered range — unmatched for
#'     \emph{structural} reasons (an unusual driver combination, not low capability).
#'     Assigned the \strong{median} covered \eqn{E}: no evidence either way, so take
#'     central tendency rather than asserting weakness.
#' }
#'
#' The split matters: only ~55\% of unmatched countries fall below the covered range,
#' so a blanket low band would mislabel the other ~45\% as politically weak.
#'
#' \strong{This is a static, one-off table by design.} It is computed once from the
#' historical panel, written out, and read as an exogenous assumption. It does not
#' enter the iterative REMIND coupling, so the loop stays interpretable and the
#' donor choices remain inspectable by hand.
#'
#' @param fit A fitted model from
#'   \code{\link{estimatePolicyStringencyModel}} — supplies the coefficients that
#'   define the metric, the frozen driver scaling and the estimation sample.
#' @param frontierScores The \code{scores} data.frame from
#'   \code{\link{computeFeasibilityFrontier}}, providing each covered country's
#'   \code{efficiencyRatio}.
#' @param panelData The full panel (magpie or prepared data.frame) including the
#'   uncovered countries whose drivers are to be matched.
#' @param year Matching year. Default \code{NULL} = the last year shared by the
#'   panel and the frontier scores.
#' @param k Number of donors per recipient. Default 3 — enough to avoid depending
#'   on a single idiosyncratic country, few enough that the table stays readable.
#' @param maxDistance Optional cutoff. Recipients whose nearest donor is further
#'   than this are returned with \code{NA} and \code{donorQuality = "none"} rather
#'   than being matched to something implausible.
#' @param bandPercentile Quantile of the covered countries' \code{efficiencyRatio}
#'   assigned to recipients that fall \emph{below} the covered capability range.
#'   Default 0.25; run 0.10 as the disclosed sensitivity. Set \code{NULL} to leave
#'   unmatched recipients as \code{NA} (the old behaviour).
#' @param basisOverride Named character vector, ISO3 -> \code{"median"},
#'   \code{"lowBand"} or \code{"donor"}, forcing a country onto a chosen branch.
#'   For the rare case where donor matching is \emph{structurally} inapplicable.
#'
#'   \strong{Why this exists.} Donor transfer assumes the efficiency ratio \eqn{E} is
#'   a function of the drivers — but if it were, it would be predicted and would not
#'   need transferring at all. So matching is weakest exactly where capability and
#'   \emph{willingness} diverge. The USA is the standing example: its drivers place it
#'   among the highest-capability polities in the world, so it matches JPN/GBR/KOR,
#'   which are high-capability \emph{and} high-willingness. The USA is not — its
#'   federal structure spans states with world-leading climate regulation and states
#'   with almost none, and the national outcome is far off the frontier its
#'   institutions could support. Inheriting a high-willingness \eqn{E} imports the
#'   one attribute the drivers cannot see. \code{c(USA = "median")} is the honest
#'   assignment: typical realized ambition, not the ambition its capacity permits.
#' @param sector \code{"Bulk"} or \code{"Diffuse"} — recorded on the output.
#'
#' @return Data.frame, one row per recipient: \code{region, sector, donors}
#'   (comma-separated), \code{donorWeights}, \code{distance} (to the nearest
#'   donor), \code{efficiencyRatio} (the assigned relative gap),
#'   \code{donorQuality} (\code{"close"}/\code{"far"}/\code{"none"}, from the
#'   distance distribution among \emph{covered} countries), \code{nDonors},
#'   \code{linearIndex} (the capability ordering) and \code{basis} —
#'   \code{"donor"}, \code{"lowBand"} or \code{"median"}. \strong{Always carry
#'   \code{basis} downstream}: once every country has a value the output stops
#'   \emph{looking} uncertain although the evidence has not changed, and the share of
#'   a region's \eqn{\varphi} resting on each basis is the only thing that shows it.
#'   Attributes: \code{"weights"}, \code{"year"}, \code{"k"},
#'   \code{"bandValues"} (the assigned low-band and median \eqn{E}) and
#'   \code{"coveredIndexRange"}.
#'
#' @seealso \code{\link{computeFeasibilityFrontier}},
#'   \code{\link{aggregateFeasibilityToRegions}},
#'   \code{docs/psm-coverage-rule-options.md}.
#' @export
#' @author Renato Rodrigues
computeDonorAssignment <- function(fit, frontierScores, panelData, year = NULL,
                                   k = 3L, maxDistance = NULL, bandPercentile = 0.25,
                                   basisOverride = NULL, sector = NA_character_) {
  if (!is.null(basisOverride) &&
        (!is.character(basisOverride) || is.null(names(basisOverride)) ||
           !all(basisOverride %in% c("median", "lowBand", "donor")))) {
    stop("computeDonorAssignment: 'basisOverride' must be a NAMED character vector ",
         "of \"median\", \"lowBand\" or \"donor\".")
  }
  if (!is.null(bandPercentile) &&
        (!is.numeric(bandPercentile) || length(bandPercentile) != 1L ||
           is.na(bandPercentile) || bandPercentile < 0 || bandPercentile > 1)) {
    stop("computeDonorAssignment: 'bandPercentile' must be a single value in [0, 1] ",
         "or NULL.")
  }
  if (is.null(fit$model) || is.null(fit$data)) {
    stop("computeDonorAssignment: 'fit' must be a fitted PSM model.")
  }
  if (!all(c("region", "year", "efficiencyRatio") %in% colnames(frontierScores))) {
    stop("computeDonorAssignment: 'frontierScores' needs region, year and ",
         "efficiencyRatio - pass computeFeasibilityFrontier() output.")
  }

  # --- the metric: |beta| over BASE drivers only -------------------------------
  beta <- stats::coef(fit$model)
  beta <- beta[!grepl("_x_|regionFE|Intercept|logisticTimeTrend|timeTrend|lagged_ecp",
                      names(beta))]
  beta <- beta[is.finite(beta) & abs(beta) > 0]
  if (!length(beta)) stop("computeDonorAssignment: no usable base-driver coefficients.")
  w <- abs(beta) / sum(abs(beta))

  # --- recipient design on the SAME frozen scaling as the fit ------------------
  sDf <- if (is.data.frame(panelData)) panelData else preparePanelData(
    data = panelData, sector = if (is.na(sector)) "Bulk" else sector,
    actorPowerDrivers = fit$prepSpec$actorPowerDrivers %||% NULL,
    actorPowerIndex = fit$prepSpec$actorPowerIndex %||% NULL,
    instQualityDrivers = fit$prepSpec$instQualityDrivers %||% NULL,
    controlDrivers = fit$prepSpec$controlDrivers %||% NULL,
    regionMappingFixedEffects = NULL,
    driverScaling = fit$driverScaling,
    outcomeVar = fit$outcomeVar %||% "Policy Stringency")
  miss <- setdiff(names(w), colnames(sDf))
  if (length(miss)) {
    stop("computeDonorAssignment: driver(s) absent from the matching panel: ",
         paste(miss, collapse = ", "))
  }

  yr <- year %||% suppressWarnings(max(intersect(sDf$year, frontierScores$year)))
  if (!is.finite(yr)) stop("computeDonorAssignment: no year shared by the panel and the scores.")
  sDf <- sDf[sDf$year == yr, , drop = FALSE]
  eff <- frontierScores[frontierScores$year == yr, c("region", "efficiencyRatio")]
  eff <- eff[is.finite(eff$efficiencyRatio), , drop = FALSE]

  X <- as.matrix(sDf[, names(w), drop = FALSE])
  rownames(X) <- as.character(sDf$region)
  X <- X[stats::complete.cases(X), , drop = FALSE]
  donors <- intersect(rownames(X), eff$region)
  recips <- setdiff(rownames(X), donors)
  if (!length(donors)) stop("computeDonorAssignment: no covered country available as a donor.")
  if (!length(recips)) return(data.frame())

  wv <- w[colnames(X)]
  dist2 <- function(a, B) colSums(wv * (t(B) - as.numeric(a))^2)

  # Calibrate "close" against how far apart the COVERED countries are from each
  # other: a recipient is only well-matched if its donor distance is typical of
  # distances the estimation sample itself spans.
  dCov <- unlist(lapply(donors, function(r) {
    d <- dist2(X[r, ], X[setdiff(donors, r), , drop = FALSE])
    sqrt(min(d))
  }))
  q <- stats::quantile(dCov, c(0.5, 0.9), na.rm = TRUE)

  effMap <- stats::setNames(eff$efficiencyRatio, eff$region)
  kk <- max(1L, min(as.integer(k), length(donors)))

  # The band rule's capability ordering: the model's own SIGNED linear index over the
  # base drivers. |beta| weights above answer "how far apart", but placing a country
  # high or low needs direction, so the raw coefficients are used here.
  bSigned <- beta[colnames(X)]
  linIdx <- stats::setNames(as.numeric(X %*% bSigned), rownames(X))
  covRange <- range(linIdx[donors], na.rm = TRUE)
  effCov <- effMap[donors]
  effCov <- effCov[is.finite(effCov)]
  bandLow <- if (is.null(bandPercentile)) NA_real_ else
    unname(stats::quantile(effCov, bandPercentile, na.rm = TRUE))
  bandMed <- if (is.null(bandPercentile)) NA_real_ else
    unname(stats::median(effCov, na.rm = TRUE))
  out <- do.call(rbind, lapply(recips, function(r) {
    d <- sqrt(dist2(X[r, ], X[donors, , drop = FALSE]))
    d <- sort(d)[seq_len(kk)]
    if (!is.null(maxDistance) && d[1] > maxDistance) {
      return(data.frame(region = r, sector = sector, donors = NA_character_,
                        donorWeights = NA_character_, distance = d[1],
                        efficiencyRatio = NA_real_, donorQuality = "none",
                        nDonors = 0L, linearIndex = unname(linIdx[r]),
                        basis = NA_character_, stringsAsFactors = FALSE))
    }
    # Inverse-distance weights: a near-identical donor dominates, but no single
    # country can be the whole answer when several are comparably close.
    iw <- 1 / pmax(d, 1e-6)
    iw <- iw / sum(iw)
    e <- sum(iw * effMap[names(d)], na.rm = TRUE)
    data.frame(region = r, sector = sector,
               donors = paste(names(d), collapse = ","),
               donorWeights = paste(round(iw, 3), collapse = ","),
               distance = unname(d[1]), efficiencyRatio = unname(e),
               donorQuality = if (d[1] <= q[[1]]) "close" else if (d[1] <= q[[2]]) "far" else "none",
               nDonors = length(d), linearIndex = unname(linIdx[r]),
               basis = NA_character_, stringsAsFactors = FALSE)
  }))

  # --- the band rule, for everyone no donor could reach ------------------------
  matched <- out$donorQuality %in% c("close", "far")
  out$basis[matched] <- "donor"
  if (!is.null(bandPercentile) && any(!matched)) {
    # "Below the covered range" is judged on the model's own capability ordering,
    # not on any single driver: a country is extrapolated downward only if the
    # fitted equation places it below every country the frontier actually observed.
    belowSupport <- !matched & is.finite(out$linearIndex) &
      out$linearIndex < covRange[1]
    out$efficiencyRatio[belowSupport] <- bandLow
    out$basis[belowSupport] <- "lowBand"
    structural <- !matched & !belowSupport
    out$efficiencyRatio[structural] <- bandMed
    out$basis[structural] <- "median"
  }

  # --- explicit, documented overrides ------------------------------------------
  if (length(basisOverride) && !is.null(bandPercentile)) {
    for (rg in intersect(names(basisOverride), out$region)) {
      b <- basisOverride[[rg]]
      if (identical(b, "donor")) next          # keep the matched blend
      out$efficiencyRatio[out$region == rg] <- if (identical(b, "median")) bandMed else bandLow
      out$basis[out$region == rg] <- b
    }
  }

  out <- out[order(out$distance), , drop = FALSE]
  rownames(out) <- NULL
  attr(out, "basisOverride") <- basisOverride
  attr(out, "weights") <- w
  attr(out, "year") <- yr
  attr(out, "k") <- kk
  attr(out, "coveredDistanceQuantiles") <- q
  attr(out, "bandValues") <- c(lowBand = bandLow, median = bandMed,
                               percentile = bandPercentile %||% NA_real_)
  attr(out, "coveredIndexRange") <- covRange
  out
}
# nolint end
