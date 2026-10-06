# nolint start
# The v6 coupling formulation (design note 0005 §1, D1-D4, D11, D12; methodology "Next step 1"):
#   q_{c,s}    = eta_{c,s,ta} - y_{c,s,ta}                 logit distance below the ceiling, held
#   S_{c,s}(t) = Smax * logit^-1(eta_{c,s}(t) - q_{c,s})   stringency while the ceiling moves
#   g_{r,s}(t) = 1 - S_{r,s}(t) / S*_{r,s}(t)              regional relative gap (Step 2 aggregation)
#   u_{r,s}    = minmax_r(g_{r,s}(ta))                     the ranking, read once, on observed data
#   G_s(t)     = sum_r w_r g_{r,s}(t),  k_s(t) = G_s(t) / G_s(t0)        the strength factor
#   D_s(t)     = sum_r w_r (g_{r,s}(t) - G_s(t))^2,  d_s(t) = D_s(t) / D_s(t0)  the spread (diagnostic)
#   phi_{r,s}(t) = clip01(1 - theta [k_s(t) ubar_s + d_s(t) (u_{r,s} - ubar_s)])  (d = k: 1 - theta k u)
# No ECM, no lambda (D13).

# ── Internals ─────────────────────────────────────────────────────────────────────────────────

# A sector spec from selected-models-pfm.yml with its list fields flattened.
#' @keywords internal
.pfmNormSpec <- function(cfg) {
  for (f in c("actorPowerDrivers", "actorPowerIndex", "instQualityDrivers", "controlDrivers")) {
    if (!is.null(cfg[[f]])) cfg[[f]] <- unlist(cfg[[f]])
  }
  cfg$panelTransform <- cfg$panelTransform %||% "levels"
  cfg
}

# The frontier's design for one sector: the levels satP fit of the deployed spec supplies the
# estimation rows, the frozen driver scaling (saturation parameters included), the trend curve
# and the guard ranges, exactly as the frontier saw them; the DEPLOYED frontier coefficients
# (frontier.rds) supply eta. This is how projectFeasiblePath() builds its ceiling, without the ECM.
#' @keywords internal
.pfmAnchorDesign <- function(cfg, sector, histData, frontierBeta, modelDir = NULL, indexMax = 10) {
  cfg <- .pfmNormSpec(cfg)
  fit <- do.call(estimatePolicyStringencyModel, c(
    list(data = histData, sector = sector, estimator = "satP", indexMax = indexMax,
         modelDir = modelDir, updateIndex = FALSE, verbose = FALSE),
    .pfmSpecArgs(cfg)))
  fb <- frontierBeta[!names(frontierBeta) %in% c("sigmaSq", "gamma")]
  list(cfg = cfg, sector = sector, fit = fit, beta = fb, indexMax = indexMax,
       lastHist = suppressWarnings(max(fit$data$year, na.rm = TRUE)),
       ranges = .driverSupportRanges(fit$data, fit$driverScaling),
       nSqueeze = fit$squeeze$n %||% sum(is.finite(fit$data$ecp)),
       trend = fit$trendParams %||% c(midpoint = formals(preparePanelData)$trendMidpoint,
                                      steepness = formals(preparePanelData)$trendSteepness))
}

# eta (logit-scale ceiling) for every country-year of `data`, a historical or scenario panel,
# with the frozen scaling, the trend frozen at the last training year and the driver guard.
#' @keywords internal
.pfmFrontierEta <- function(design, data) {
  cfg <- design$cfg; fit <- design$fit
  unl <- function(x) if (is.null(x)) NULL else unlist(x)
  # Strip the outcome: preparePanelData drops rows without it, which would leave only the covered
  # countries - and eta is needed for every country (as in runPFMDonorAssumptions).
  ps <- grep("Policy Stringency", magclass::getNames(data), value = TRUE)
  if (length(ps)) data <- data[, , setdiff(magclass::getNames(data), ps)]
  df <- preparePanelData(
    data = data, sector = design$sector,
    actorPowerDrivers = unl(cfg$actorPowerDrivers), actorPowerIndex = unl(cfg$actorPowerIndex),
    instQualityDrivers = unl(cfg$instQualityDrivers),
    controlDrivers = setdiff(unl(cfg$controlDrivers), "lagged_ecp"),
    regionMappingFixedEffects = if (isTRUE(cfg$useMundlak)) NULL else cfg$regionMappingFixedEffects,
    useMundlak = isTRUE(cfg$useMundlak), gdpGovInteraction = isTRUE(cfg$gdpGovInteraction),
    driverScaling = fit$driverScaling,
    trendMidpoint = design$trend[["midpoint"]], trendSteepness = design$trend[["steepness"]],
    trendFreezeYear = if (is.finite(design$lastHist)) design$lastHist else NULL,
    outcomeVar = fit$outcomeVar %||% "Policy Stringency")
  lv <- fit$model$xlevels$regionFE
  if (!is.null(lv) && "regionFE" %in% names(df)) {
    fe <- as.character(df$regionFE)
    fe[!fe %in% lv] <- if ("Other" %in% lv) "Other" else lv[1]
    df$regionFE <- factor(fe, levels = lv)
  }
  g <- .pfmDriverGuard(df, design$ranges)
  df <- g$df
  df$lagged_ecp <- 0
  tt <- stats::delete.response(stats::terms(fit$formula))
  mm <- stats::model.matrix(tt, stats::model.frame(tt, data = df, na.action = stats::na.pass))
  missing <- setdiff(names(design$beta), colnames(mm))
  if (length(missing)) {
    stop(".pfmFrontierEta: frontier terms absent from the design: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  data.frame(region = as.character(df$region), year = df$year,
             eta = as.numeric(mm[, names(design$beta), drop = FALSE] %*% design$beta),
             driverOutOfSupport = g$outOfSupport, apExcess = g$apExcess,
             stringsAsFactors = FALSE)
}

# Region weights: each region's share of world final energy (the coupling weights summed).
#' @keywords internal
.pfmRegionWeights <- function(weights, mapping) {
  map <- .pfmResolveCountryMap(mapping)
  w <- weights[names(weights) %in% names(map)]
  rw <- tapply(as.numeric(w), map[names(w)], sum, na.rm = TRUE)
  rw <- rw[is.finite(rw) & rw > 0]
  stats::setNames(as.numeric(rw) / sum(rw), names(rw))
}

# Strength and spread from regional gaps. `gaps`: sector, region, year, g. `wReg`: region shares.
# Normalised at t0 (D4); held at its holdYear value after holdYear (D6: 2100 in the central case,
# 2060 as the sensitivity).
#' @keywords internal
.pfmStrengthFromGaps <- function(gaps, wReg, t0 = 2025, holdYear = 2100) {
  out <- do.call(rbind, lapply(split(gaps, list(gaps$sector, gaps$year), drop = TRUE), function(d) {
    d <- d[is.finite(d$g) & d$region %in% names(wReg), , drop = FALSE]
    if (!nrow(d)) return(NULL)
    w <- wReg[d$region]; w <- w / sum(w)
    G <- sum(w * d$g)
    data.frame(sector = d$sector[1], year = d$year[1], G = G, D = sum(w * (d$g - G)^2),
               nRegions = nrow(d), stringsAsFactors = FALSE)
  }))
  out <- out[order(out$sector, out$year), , drop = FALSE]
  res <- do.call(rbind, lapply(split(out, out$sector), function(d) {
    base <- d[d$year == t0, , drop = FALSE]
    if (nrow(base) != 1) stop(".pfmStrengthFromGaps: no regional gaps in t0 = ", t0, " for ", d$sector[1], call. = FALSE)
    d$kRaw <- d$G / base$G
    d$dRaw <- d$D / base$D
    held <- function(v) {
      if (is.null(holdYear) || !any(d$year == holdYear)) return(v)
      v[d$year > holdYear] <- v[d$year == holdYear]; v
    }
    d$k <- held(d$kRaw); d$d <- held(d$dRaw)
    d
  }))
  rownames(res) <- NULL
  res
}

# The shares phi_{r,s}(t) from the ranking u and the strength path (D11, D12, D15).
# ordering: "model" (u as read), "uniform" (every region at the weighted mean), "reversed"
# (1 - u), "permuted" (u shuffled across regions, seeded). spread: "k" (d = k, the headline) or
# "model" (d = D/D(t0), the diagnostic arm).
#' @keywords internal
.pfmSharesFrom <- function(u, wReg, strength, theta, spread = c("k", "model"),
                           ordering = c("model", "uniform", "reversed", "permuted"), seed = 1L) {
  spread <- match.arg(spread); ordering <- match.arg(ordering)
  if (!is.numeric(theta) || length(theta) != 1 || theta < 0 || theta > 1) stop("theta must be in [0, 1]", call. = FALSE)
  res <- do.call(rbind, lapply(split(u, u$sector), function(us) {
    v <- stats::setNames(us$u, us$region)
    v <- switch(ordering,
      model = v,
      uniform = { w <- wReg[names(v)]; stats::setNames(rep(sum(w * v) / sum(w), length(v)), names(v)) },
      reversed = 1 - v,
      permuted = { set.seed(seed); stats::setNames(sample(as.numeric(v)), names(v)) })
    w <- wReg[names(v)]; ubar <- sum(w * v) / sum(w)
    st <- strength[strength$sector == us$sector[1], , drop = FALSE]
    do.call(rbind, lapply(seq_len(nrow(st)), function(i) {
      dd <- if (spread == "k") st$k[i] else st$d[i]
      raw <- 1 - theta * (st$k[i] * ubar + dd * (v - ubar))
      data.frame(sector = us$sector[1], region = names(v), year = st$year[i], u = as.numeric(v),
                 phiRaw = as.numeric(raw), phi = pmin(pmax(as.numeric(raw), 0), 1),
                 clipped = raw < 0 | raw > 1, stringsAsFactors = FALSE)
    }))
  }))
  rownames(res) <- NULL
  res
}

# ── Exported ──────────────────────────────────────────────────────────────────────────────────

#' The anchor of the v6 coupling: each country's logit distance below its ceiling, and the ranking
#'
#' @description
#' Reads the ranking ONCE, on observed data at the anchor year \eqn{t_a} (the group's last panel
#' year), and fixes each country's logit-scale distance below its own ceiling (design note 0005
#' §1, D1, D2):
#' \itemize{
#'   \item covered countries: \eqn{q_c = \eta_{c,t_a} - y_{c,t_a}}, with \eqn{y} the observed index on
#'     the squeezed logit scale and \eqn{\eta} the deployed frontier's linear predictor;
#'   \item uncovered countries: \eqn{y = \mathrm{logit}(E_c S^*_{c,t_a}/S_{max})} with the band-rule
#'     \eqn{E_c} of the group's donor assignment (basis donor, lowBand or median; the USA override
#'     included);
#'   \item regions: \eqn{g_r = 1 - S_r / S^*_r} aggregated with the coupling weights exactly as Step 2
#'     (\code{\link{aggregateFeasibilityToRegions}}), and \eqn{u_r} its min-max position.
#' }
#' @param group,resultsDir,modelDir The Run-Group and where its artifacts and Fit Cache live.
#' @param mapping Region mapping of the delivery resolution (EU21 or H12).
#' @param weights Named country weights; \code{NULL} uses \code{pfmCouplingWeights(t0, ssp)}.
#' @param ssp SSP of the weights. Default \code{"SSP2"}.
#' @param sectors Sectors. Default Bulk and Diffuse.
#' @param anchorYear Anchor year; \code{NULL} = the last year of the group's training panel.
#' @param t0 First model period, where \eqn{k} is normalised. Default 2025.
#' @param anchorRule \code{"anchor-year"} (the v6 formulation) or \code{"t0-ceiling"}: the
#'   \code{analysis/v6/v6FormulationTests.R} "logit-u" rule behind the methodology's numbers, which
#'   holds the shortfall between the t0 ceiling and the anchor-year efficiency ratio. Needs
#'   \code{scenarioData}. For reproduction only.
#' @param scenarioData Scenario panel, needed for \code{anchorRule = "t0-ceiling"}.
#' @param histData Historical panel; \code{NULL} loads the group's training panel from the Fit Cache.
#' @param assignment Optional named list (by sector) of band-rule assignments
#'   (\code{\link{computeDonorAssignment}} output) used instead of the group's
#'   \code{donor-assignment-band-<sector>.rds}: the assignment-rule arms and their sensitivities.
#' @param verbose Logical.
#' @return A list: \code{country} (sector, region, basis, etaAnchor, yAnchor, q, ceilingAnchor,
#'   indexAnchor), \code{region} (sector, region, g, u, weight, provenance shares), \code{anchorYear},
#'   \code{t0}, \code{rule}, \code{mapping}, \code{weights}, \code{regionWeights}, \code{spec}, and
#'   \code{designs} (the per-sector frontier designs, reused by \code{\link{computeStrengthPath}}).
#' @seealso \code{\link{computeStrengthPath}}, \code{\link{computeSharePath}}
#' @export
#' @author Renato Rodrigues
computeAnchorGap <- function(group, resultsDir = getOption("pfm.resultsDir", "output"),
                             modelDir = getOption("pfm.modelDir", "output"),
                             mapping = "regionmapping_21_EU11.csv", weights = NULL, ssp = "SSP2",
                             sectors = c("Bulk", "Diffuse"), anchorYear = NULL, t0 = 2025,
                             anchorRule = c("anchor-year", "t0-ceiling"), scenarioData = NULL,
                             histData = NULL, assignment = NULL, verbose = TRUE) {
  anchorRule <- match.arg(anchorRule)
  say <- function(...) if (isTRUE(verbose)) message("[anchor:", group, "] ", ...)
  gd <- file.path(resultsDir, group)
  sel <- yaml::read_yaml(.pfmSelectedModels(gd))
  fr <- readRDS(file.path(gd, "frontier.rds"))
  if (is.null(histData)) {
    hash <- jsonlite::read_json(file.path(gd, "manifest.json"))$panel_hash
    histData <- loadTrainingPanel(hash, modelDir)
    if (is.null(histData)) stop("computeAnchorGap: training panel ", hash, " not in the Fit Cache", call. = FALSE)
  }
  if ("GDP per Capita" %in% magclass::getNames(histData) && !"GDP per Capita Sq" %in% magclass::getNames(histData)) {
    histData <- magclass::mbind(histData, magclass::setNames(histData[, , "GDP per Capita"]^2, "GDP per Capita Sq"))
  }
  if (anchorRule == "t0-ceiling" && is.null(scenarioData)) {
    stop("computeAnchorGap: anchorRule = 't0-ceiling' needs scenarioData", call. = FALSE)
  }
  weights <- weights %||% pfmAssertSizeWeights(pfmCouplingWeights(year = t0, scenario = ssp), "computeAnchorGap")
  map <- .pfmResolveCountryMap(mapping)
  indexMax <- 10

  designs <- list(); countries <- list()
  for (sec in sectors) {
    cfg <- Filter(function(x) identical(x$model_type, paste0("PolicyStringency: ", sec)), sel)[[1]]
    ct <- fr$bySector[[sec]]$coefTable
    des <- .pfmAnchorDesign(cfg, sec, histData, stats::setNames(ct$estimate, ct$term),
                              modelDir = modelDir, indexMax = indexMax)
    designs[[sec]] <- des
    ta <- anchorYear %||% des$lastHist
    sq <- function(p) .pfmSqueeze(pmin(pmax(p, 0), 1), des$nSqueeze)
    eh <- .pfmFrontierEta(des, histData)
    eh <- eh[eh$year == ta & is.finite(eh$eta), , drop = FALSE]
    etaA <- stats::setNames(eh$eta, eh$region)
    # covered: the estimation rows at the anchor year (y on the squeezed logit scale)
    fd <- des$fit$data
    cov <- fd[fd$year == ta & is.finite(fd$ecp), c("region", "ecp")]
    yCov <- stats::setNames(cov$ecp, as.character(cov$region))
    # uncovered: the band rule (donor / lowBand / median, the USA override included)
    asgFile <- file.path(gd, paste0("donor-assignment-band-", sec, ".rds"))
    asg <- if (!is.null(assignment[[sec]])) assignment[[sec]] else if (file.exists(asgFile)) readRDS(asgFile) else NULL
    eBand <- if (!is.null(asg)) stats::setNames(asg$efficiencyRatio, as.character(asg$region)) else numeric(0)
    bBand <- if (!is.null(asg)) stats::setNames(as.character(asg$basis), as.character(asg$region)) else character(0)
    reg <- names(etaA)
    basis <- ifelse(reg %in% names(yCov), "observed",
                    ifelse(reg %in% names(eBand) & is.finite(eBand[reg]), bBand[reg], NA_character_))
    keep <- !is.na(basis)
    reg <- reg[keep]; basis <- basis[keep]
    sStar <- indexMax * stats::plogis(etaA[reg])
    if (anchorRule == "anchor-year") {
      y <- ifelse(basis == "observed", yCov[reg],
                  stats::qlogis(sq(pmin(eBand[reg], 1) * sStar / indexMax)))
      q <- etaA[reg] - y
    } else {
      # The prototype's "logit-u": the shortfall between the t0 ceiling and E0 times it, with E0
      # the anchor-year efficiency ratio of frontier.rds (covered) or the band E (uncovered).
      es <- .pfmFrontierEta(des, scenarioData)
      es <- es[es$year == t0 & is.finite(es$eta), , drop = FALSE]
      s0 <- indexMax * stats::plogis(stats::setNames(es$eta, es$region)[reg])
      sc <- fr$bySector[[sec]]$scores
      e22 <- stats::setNames(sc$efficiencyRatio[sc$year == ta], as.character(sc$region[sc$year == ta]))
      e0 <- pmin(ifelse(basis == "observed", e22[reg], eBand[reg]), 1)
      q <- stats::qlogis(sq(s0 / indexMax)) - stats::qlogis(sq(e0 * s0 / indexMax))
      y <- etaA[reg] - q
    }
    countries[[sec]] <- data.frame(sector = sec, region = reg, basis = basis, etaAnchor = as.numeric(etaA[reg]),
                                   yAnchor = as.numeric(y), q = as.numeric(q),
                                   ceilingAnchor = as.numeric(sStar),
                                   indexAnchor = indexMax * stats::plogis(as.numeric(etaA[reg] - q)),
                                   stringsAsFactors = FALSE)
    say(sec, ": anchor ", ta, " | ", sum(basis == "observed"), " observed, ",
        sum(basis != "observed"), " band-rule countries")
  }
  country <- do.call(rbind, countries); rownames(country) <- NULL
  ta <- anchorYear %||% designs[[1]]$lastHist
  wReg <- .pfmRegionWeights(weights, mapping)

  region <- do.call(rbind, lapply(sectors, function(sec) {
    cc <- country[country$sector == sec & is.finite(country$q), , drop = FALSE]
    path <- data.frame(region = cc$region, year = ta, feasibleIndex = cc$indexAnchor,
                       ceilingIndex = cc$ceilingAnchor, outOfCoverage = FALSE, stringsAsFactors = FALSE)
    agg <- suppressWarnings(aggregateFeasibilityToRegions(path, mapping, weights = weights, theta = 0.5))
    agg <- agg[is.finite(agg$relativeGap), c("region", "relativeGap", "feasibleIndex", "ceilingIndex")]
    names(agg) <- c("region", "g", "indexAnchor", "ceilingAnchor")
    rng <- range(agg$g)
    agg$u <- if (diff(rng) > 0) (agg$g - rng[1]) / diff(rng) else 0
    # provenance: the share of each region's weight resolved by each basis
    cw <- data.frame(region = names(weights), w = as.numeric(weights), stringsAsFactors = FALSE)
    cw$iam <- map[cw$region]; cw <- cw[!is.na(cw$iam), , drop = FALSE]
    cw$basis <- stats::setNames(cc$basis, cc$region)[cw$region]
    prov <- do.call(rbind, lapply(split(cw, cw$iam), function(d) {
      tot <- sum(d$w, na.rm = TRUE)
      sh <- function(b) if (tot > 0) sum(d$w[d$basis %in% b], na.rm = TRUE) / tot else NA_real_
      data.frame(region = d$iam[1], shareObserved = sh("observed"), shareDonor = sh("donor"),
                 shareLowBand = sh("lowBand"), shareMedian = sh("median"),
                 shareUnresolved = if (tot > 0) sum(d$w[is.na(d$basis)], na.rm = TRUE) / tot else NA_real_,
                 stringsAsFactors = FALSE)
    }))
    agg <- merge(agg, prov, by = "region", all.x = TRUE)
    agg$weight <- as.numeric(wReg[agg$region])
    cbind(sector = sec, agg, stringsAsFactors = FALSE)
  }))
  list(group = group, rule = anchorRule, anchorYear = ta, t0 = t0, mapping = mapping, ssp = ssp,
       country = country, region = region, weights = weights, regionWeights = wReg,
       spec = vapply(designs, function(d) d$cfg$name %||% NA_character_, character(1)),
       designs = designs)
}

#' The strength path k_s(t) of the v6 coupling on one scenario panel
#'
#' @description
#' Moves every country's ceiling along the scenario (the REMIND energy system, the SSP
#' institutions and controls) with the anchor's logit distance held:
#' \eqn{S_c(t) = S_{max}\,\mathrm{logit}^{-1}(\eta_c(t) - q_c)}. Aggregates to regional gaps
#' \eqn{g_r(t)} (Step 2), and returns the world mean gap \eqn{G_s(t) = \sum_r w_r g_r(t)}, the
#' strength factor \eqn{k_s(t) = G_s(t)/G_s(t_0)}, the spread \eqn{D_s(t)} and
#' \eqn{d_s(t) = D_s(t)/D_s(t_0)}, the out-of-support share of the drivers per year (C10), and the
#' clip diagnostics of D12 for \code{theta}.
#'
#' @param anchor The output of \code{\link{computeAnchorGap}}.
#' @param scenarioData Scenario panel (\code{\link{panelDataScenario}}), country resolution.
#' @param holdYear Year after which \eqn{k} and \eqn{d} are held (D6). Default 2100 (central);
#'   2060 is the sensitivity.
#' @param theta Severity for the clip diagnostics. Default 0.5.
#' @param hold \code{"logit"} (the v6 rule) or \code{"ratio"}: the E-hold bound of D2/C5, in which
#'   each country keeps its anchor efficiency ratio, \eqn{S_c(t) = E_c S^*_c(t)}.
#' @param returnCountries Logical: also return the country paths. Default \code{FALSE}.
#' @return A list: \code{strength} (sector, year, G, D, kRaw, dRaw, k, d, nRegions,
#'   outOfSupportShare, apExtrapolationShare, clipShare, kAboveInvTheta), \code{regions} (sector,
#'   region, year, g, feasibleIndex, ceilingIndex), \code{t0}, \code{holdYear}, \code{hold}, and
#'   optionally \code{countries}.
#' @seealso \code{\link{computeAnchorGap}}, \code{\link{computeSharePath}}
#' @export
#' @author Renato Rodrigues
computeStrengthPath <- function(anchor, scenarioData, holdYear = 2100, theta = 0.5,
                                hold = c("logit", "ratio"), returnCountries = FALSE) {
  hold <- match.arg(hold)
  t0 <- anchor$t0; indexMax <- 10
  paths <- list(); regs <- list()
  for (sec in names(anchor$designs)) {
    es <- .pfmFrontierEta(anchor$designs[[sec]], scenarioData)
    cc <- anchor$country[anchor$country$sector == sec & is.finite(anchor$country$q), , drop = FALSE]
    es <- es[es$region %in% cc$region & is.finite(es$eta), , drop = FALSE]
    q <- stats::setNames(cc$q, cc$region)[es$region]
    sStar <- indexMax * stats::plogis(es$eta)
    S <- if (hold == "logit") indexMax * stats::plogis(es$eta - q) else {
      e0 <- stats::setNames(pmin(cc$indexAnchor / cc$ceilingAnchor, 1), cc$region)[es$region]
      e0 * sStar
    }
    p <- data.frame(region = es$region, year = es$year, feasibleIndex = S, ceilingIndex = sStar,
                    outOfCoverage = FALSE, driverOutOfSupport = es$driverOutOfSupport,
                    apExcess = es$apExcess, stringsAsFactors = FALSE)
    paths[[sec]] <- cbind(sector = sec, p, stringsAsFactors = FALSE)
    agg <- suppressWarnings(aggregateFeasibilityToRegions(p, anchor$mapping, weights = anchor$weights, theta = 0.5))
    regs[[sec]] <- data.frame(sector = sec, region = agg$region, year = agg$year, g = agg$relativeGap,
                              feasibleIndex = agg$feasibleIndex, ceilingIndex = agg$ceilingIndex,
                              stringsAsFactors = FALSE)
  }
  regions <- do.call(rbind, regs); rownames(regions) <- NULL
  strength <- .pfmStrengthFromGaps(regions, anchor$regionWeights, t0 = t0, holdYear = holdYear)
  # Drivers outside the training support, weighted like the aggregation (C10): the share of the
  # guarded drivers out of support, and the share of country-years whose actor power is more than
  # 1 SD beyond it (the D7 gate's measure).
  cp <- do.call(rbind, paths)
  cw <- anchor$weights[cp$region]; cw[!is.finite(cw)] <- 0
  sup <- do.call(rbind, lapply(split(data.frame(cp, w = cw), list(cp$sector, cp$year), drop = TRUE), function(d) {
    ok <- d$w > 0
    data.frame(sector = d$sector[1], year = d$year[1],
               outOfSupportShare = stats::weighted.mean(d$driverOutOfSupport[ok], d$w[ok], na.rm = TRUE),
               apExtrapolationShare = stats::weighted.mean(as.numeric(d$apExcess[ok] > 1), d$w[ok], na.rm = TRUE),
               stringsAsFactors = FALSE)
  }))
  strength <- merge(strength, sup, by = c("sector", "year"), all.x = TRUE)
  # D12: the share of regions whose phi would clip at 0 for this theta, and k > 1/theta
  u <- anchor$region[, c("sector", "region", "u")]
  sh <- .pfmSharesFrom(u, anchor$regionWeights, strength, theta = theta)
  clip <- stats::aggregate(clipped ~ sector + year, data = sh, FUN = mean)
  names(clip)[3] <- "clipShare"
  strength <- merge(strength, clip, by = c("sector", "year"), all.x = TRUE)
  strength$kAboveInvTheta <- strength$k > 1 / theta
  strength <- strength[order(strength$sector, strength$year), , drop = FALSE]
  rownames(strength) <- NULL
  out <- list(strength = strength, regions = regions, t0 = t0, holdYear = holdYear, hold = hold, theta = theta)
  if (isTRUE(returnCountries)) out$countries <- cp
  out
}

#' The feasibility shares phi_{r,s}(t) of the v6 coupling
#'
#' @description
#' \eqn{\varphi_{r,s}(t) = \mathrm{clip}_{[0,1]}\big(1 - \theta[k_s(t)\bar u_s + d_s(t)(u_{r,s} -
#' \bar u_s)]\big)}, with \eqn{\bar u_s} the weighted mean position. The headline spread is
#' \eqn{d = k} (D11), which reduces to \eqn{1 - \theta k_s(t) u_{r,s}}. Orderings other than the
#' model's are the ordering tests of D15 / C1. The region's floor is \eqn{\min_s \varphi_{r,s}};
#' market delivery (ETS from Bulk, ES and other from Diffuse) stays in REMIND.
#'
#' @param anchor Output of \code{\link{computeAnchorGap}}.
#' @param strength Output of \code{\link{computeStrengthPath}}.
#' @param theta Severity, the share's value at t0 for the most constrained region. Default 0.5.
#' @param spread \code{"k"} (headline) or \code{"model"} (\eqn{d = D/D(t_0)}, diagnostic).
#' @param ordering \code{"model"}, \code{"uniform"}, \code{"reversed"} or \code{"permuted"}.
#' @param seed Seed for \code{ordering = "permuted"}.
#' @return A data.frame: sector, region, year, u, phiRaw, phi, clipped; plus the rows of the
#'   floor, \code{sector = "min"}.
#' @export
#' @author Renato Rodrigues
computeSharePath <- function(anchor, strength, theta = 0.5, spread = c("k", "model"),
                             ordering = c("model", "uniform", "reversed", "permuted"), seed = 1L) {
  sh <- .pfmSharesFrom(anchor$region[, c("sector", "region", "u")], anchor$regionWeights,
                       strength$strength, theta = theta, spread = spread, ordering = ordering, seed = seed)
  fl <- stats::aggregate(phi ~ region + year, data = sh, FUN = min)
  fl <- data.frame(sector = "min", region = fl$region, year = fl$year, u = NA_real_, phiRaw = fl$phi,
                   phi = fl$phi, clipped = FALSE, stringsAsFactors = FALSE)
  out <- rbind(sh, fl)
  out[order(out$sector, out$region, out$year), , drop = FALSE]
}
# nolint end
