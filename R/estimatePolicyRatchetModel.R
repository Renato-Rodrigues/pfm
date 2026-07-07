# nolint start
#' Discrete-time hazard model of climate-policy ratchet-up events
#'
#' @description
#' The event-history reading of policy feasibility (Tier-1 direction 2,
#' `docs/psm-theoretical-directions.md`): instead of the smoothed 0-10 stringency
#' level, the outcome is a country-year \strong{ratchet-up event} (any instrument
#' adopted or significantly tightened — see mrpfm's
#' \code{calcPolicyRatchetEvents}), modelled as a discrete-time hazard with the
#' same political-economy drivers as the PSM. The default \code{cloglog} link is
#' the grouped-time proportional-hazards model (Prentice–Gloeckler); coefficients
#' exponentiate to hazard ratios. Drivers are lagged, standardized and
#' interacted exactly as in \code{\link{estimatePolicyStringencyModel}} (shared
#' \code{\link{preparePanelData}}), so channel results are directly comparable
#' across the level and event readings — the cross-formulation agreement is
#' itself a robustness exhibit.
#'
#' The natural IAM product of this rung is the projected probability that a
#' polity ratchets up within a period — a probabilistic feasibility input rather
#' than a pseudo-precise index path.
#'
#' @param data \code{magpie} panel carrying the drivers and the events outcome.
#'   The outcome may be sector-qualified (\code{"<eventVar>|<sector>"}) or plain
#'   (\code{"<eventVar>"} — it is then aliased to the requested sector, since
#'   events are country acts, not sector quantities; the sector only selects the
#'   sector-qualified drivers).
#' @param sector Character. Selects the sector-qualified drivers. Default
#'   \code{"Bulk"}.
#' @param eventVar Character. Outcome base name. Default \code{"Ratchet Event"}.
#' @param link Character. \code{"cloglog"} (default; discrete-time proportional
#'   hazards) or \code{"logit"}.
#' @param actorPowerDrivers,actorPowerIndex,instQualityDrivers,controlDrivers
#'   Specification lists, as in \code{\link{estimatePolicyStringencyModel}}.
#' @param regionMappingFixedEffects,timeTrend,logisticTimeTrend,lag,
#'   interactRegionFE,useMundlak,gdpGovInteraction As in the sibling estimator.
#' @param maxit,verbose As in the sibling estimator.
#'
#' @return A list with \code{model} (binomial GLM), \code{coeftest}
#'   (cluster-robust by region), \code{vcov}, \code{formula}, \code{data},
#'   \code{family}, \code{link}, \code{sector}, \code{eventRate} (share of
#'   at-risk rows with an event), \code{nEvents}, and \code{hazardRatios}
#'   (exp(coef) with clustered 95 percent intervals, FE dummies excluded).
#'
#' @author Renato Rodrigues
#'
#' @importFrom stats glm binomial coef vcov qnorm
#' @importFrom lmtest coeftest
#' @importFrom sandwich vcovCL
#'
#' @export
estimatePolicyRatchetModel <- function(
    data,
    sector = "Bulk",
    eventVar = "Ratchet Event",
    link = c("cloglog", "logit"),
    actorPowerDrivers = "Actor Power Index",
    actorPowerIndex = "Actor Power Index",
    instQualityDrivers = c("Government Effectiveness (WGI)", "Vertical Accountability (VDem)"),
    controlDrivers = NULL,
    regionMappingFixedEffects = "regionmappingH12.csv",
    timeTrend = TRUE,
    logisticTimeTrend = FALSE,
    lag = 1,
    interactRegionFE = FALSE,
    useMundlak = FALSE,
    gdpGovInteraction = FALSE,
    maxit = 200,
    verbose = TRUE) {
  link <- match.arg(link)

  # Events are country-level acts: alias a plain outcome to the sector-qualified
  # name preparePanelData expects (the sector picks the drivers, not the outcome).
  if (magclass::is.magpie(data)) {
    qn <- paste0(eventVar, "|", sector)
    nms <- magclass::getNames(data)
    if (!qn %in% nms) {
      if (!eventVar %in% nms) {
        stop("estimatePolicyRatchetModel: neither '", qn, "' nor '", eventVar,
             "' found in the panel - add the calcPolicyRatchetEvents outcome first.")
      }
      data <- magclass::mbind(data, magclass::setNames(data[, , eventVar], qn))
    }
  }

  df <- preparePanelData(
    data = data,
    sector = sector,
    actorPowerDrivers = actorPowerDrivers,
    actorPowerIndex = actorPowerIndex,
    instQualityDrivers = instQualityDrivers,
    controlDrivers = controlDrivers,
    regionMappingFixedEffects = if (isTRUE(useMundlak)) NULL else regionMappingFixedEffects,
    lag = lag,
    useMundlak = useMundlak,
    gdpGovInteraction = gdpGovInteraction,
    outcomeVar = eventVar
  )
  bad <- is.finite(df$ecp) & abs(df$ecp - round(df$ecp)) > 1e-8
  if (any(bad)) {
    stop("estimatePolicyRatchetModel: outcome '", eventVar, "' is not 0/1 (found ",
         paste(utils::head(unique(df$ecp[bad]), 3), collapse = ", "),
         ") - pass the 'Ratchet Event' variable, not counts/intensities.")
  }

  fml <- buildModelFormula(
    depVar = "ecp",
    actorPowerDrivers = actorPowerDrivers,
    actorPowerIndex = actorPowerIndex,
    instQualityDrivers = instQualityDrivers,
    controlDrivers = controlDrivers,
    regionMappingFixedEffects = if (isTRUE(useMundlak)) NULL else regionMappingFixedEffects,
    timeTrend = timeTrend,
    logisticTimeTrend = logisticTimeTrend,
    interactRegionFE = if (isTRUE(useMundlak)) FALSE else interactRegionFE,
    useMundlak = useMundlak,
    gdpGovInteraction = gdpGovInteraction
  )

  if (isTRUE(verbose)) {
    message("  [running] Estimating policy-ratchet hazard (", sector, ", ", link, ")...")
  }
  fit <- stats::glm(fml, data = df, family = stats::binomial(link = link),
                    control = list(maxit = maxit))
  vcovClust <- tryCatch(
    sandwich::vcovCL(fit, cluster = df$region, type = "HC1"),
    error = function(e) {
      warning("estimatePolicyRatchetModel: clustered vcov failed (",
              conditionMessage(e), "); using model vcov.", call. = FALSE)
      stats::vcov(fit)
    }
  )
  robustTest <- lmtest::coeftest(fit, vcov. = vcovClust)

  atRisk <- is.finite(df$ecp)
  b <- stats::coef(fit)
  keep <- setdiff(names(b), "(Intercept)")
  keep <- keep[!grepl("^regionFE", keep)]
  se <- sqrt(diag(vcovClust))[keep]
  zc <- stats::qnorm(0.975)
  hazardRatios <- data.frame(
    term = keep,
    hazardRatio = exp(b[keep]),
    lo95 = exp(b[keep] - zc * se),
    hi95 = exp(b[keep] + zc * se),
    p = as.numeric(robustTest[keep, 4]),
    row.names = NULL,
    stringsAsFactors = FALSE
  )

  list(
    model = fit,
    coeftest = robustTest,
    vcov = vcovClust,
    formula = fml,
    data = df,
    family = paste0("binomial(", link, ")"),
    link = link,
    sector = sector,
    eventVar = eventVar,
    eventRate = mean(df$ecp[atRisk] > 0),
    nEvents = sum(df$ecp[atRisk] > 0),
    hazardRatios = hazardRatios,
    converged = isTRUE(fit$converged)
  )
}
# nolint end
