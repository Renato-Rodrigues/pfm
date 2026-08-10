# nolint start
#' Pseudo-out-of-sample validation of the deployed Policy Stringency spec
#'
#' @description
#' THE decisive test of the PSM's projection claim (R10, 2026-07-06;
#' docs/psm-nature-readiness-assessment.md Tier 1): the deployed spec (from the
#' Run-Group's \code{selected-models-psm.yml}) is re-fit on the historical panel
#' truncated at \code{trainEnd}, then projected onto the observed drivers of the
#' held-out years (trend frozen at \code{trainEnd}, frozen driver scaling, the
#' projection-time driver-support guard active — exactly the deployment
#' discipline), and scored against the \emph{observed} CAPMF index:
#' RMSE / MAE / Pearson / Spearman, 95 percent interval coverage, and the skill
#' relative to a per-region persistence forecast (last observed value carried
#' forward — the naive baseline any projection must beat). Specs with the
#' dynamics rung are validated one-step-ahead with observed lags.
#'
#' Writes \code{<group>/temporal-validation.rds}:
#' \code{list(spec, trainEnd, bySector = list(<sector> = list(metrics, rows)))} —
#' \code{rows} holds region/year/observed/predicted/lo/hi/outOfCoverage for the
#' held-out window, so the report can show WHERE the projection fails, not just
#' by how much.
#'
#' @param group Character. PSM Run-Group name.
#' @param resultsDir,modelDir,cachefolder As in \code{\link{runPSMProjection}}.
#' @param panelData Optional pre-built historical panel (with the PSM outcomes).
#' @param y Numeric vector. Full panel years. Default \code{2000:2022}.
#' @param trainEnd Numeric. Last training year; later years are held out.
#'   Default \code{2015} (7 held-out years).
#' @param extraForms Character vector. Additional feasibility-as-speed readings
#'   validated alongside the static level forecast (2026-07-07, appraisal-v3
#'   pivot): \code{"ecm"} — the error-correction form, forecast by integrating
#'   the level recursion from the observed \code{trainEnd} level (nests
#'   persistence, so beating it is a fair fight); \code{"events"} — a
#'   discrete-time ratchet-event hazard on panel-derived jump events
#'   (index increase of at least \code{eventJump} within a sector-region-year),
#'   scored on held-out events by AUC and by Brier skill against a per-region
#'   base-rate baseline. Set \code{character(0)} to skip.
#' @param eventJump Numeric. Index-points threshold defining a region-level
#'   ratchet event for the \code{"events"} form. Default \code{0.25}.
#' @param outputRegionMappingFile,indexMax,verbose As in
#'   \code{\link{runPSMProjection}}.
#'
#' @return Invisibly, the artifact list, or \code{NULL} when skipped.
#' @seealso \code{\link{runPSMSweep}}, \code{\link{predictPolicyStringency}}, ADR 0036
#' @export
#' @author Renato Rodrigues
runPSMTemporalValidation <- function(group,
                                     resultsDir = getOption("pfm.resultsDir", "output"),
                                     modelDir = getOption("pfm.modelDir", "output"),
                                     cachefolder = NULL, panelData = NULL,
                                     y = 2000:2022, trainEnd = 2015,
                                     extraForms = c("ecm", "events"),
                                     eventJump = 0.25,
                                     outputRegionMappingFile = "regionmapping_54.csv",
                                     indexMax = 10, verbose = TRUE) {
  groupDir <- .resolveGroupDir(group, resultsDir, modelDir, cachefolder)
  say <- function(...) if (isTRUE(verbose)) message("[PSM-TEMPORAL:", group, "] ", ...)
  t0 <- Sys.time()
  if (!requireNamespace("yaml", quietly = TRUE)) stop("The 'yaml' package is required.")
  sectors <- c("Bulk", "Diffuse")

  selPath <- file.path(groupDir, "selected-models-psm.yml")
  if (!file.exists(selPath)) {
    .recordStep(groupDir, group, "temporal-validation", t0, status = "skipped",
                metrics = list(reason = "no selected-models-psm.yml (run runPSMSweep first)"))
    return(invisible(NULL))
  }
  sel <- yaml::read_yaml(selPath)
  norm <- function(s) {
    for (f in c("actorPowerDrivers", "actorPowerIndex", "instQualityDrivers", "controlDrivers"))
      if (!is.null(s[[f]])) s[[f]] <- unlist(s[[f]])
    s
  }

  panel <- panelData
  if (is.null(panel)) {
    p <- file.path(groupDir, "data", "panelDataHistorical.rds")
    if (file.exists(p)) panel <- tryCatch(readRDS(p), error = function(e) NULL)
    if (is.list(panel) && !is.null(panel$data)) panel <- panel$data
  }
  if (is.null(panel)) {
    panel <- tryCatch(
      panelDataHistorical(aggregate = TRUE, y = y,
                          outputRegionMappingFile = outputRegionMappingFile,
                          includePolicyStringency = TRUE),
      error = function(e) NULL
    )
  }
  if (is.null(panel) || !magclass::is.magpie(panel)) {
    .recordStep(groupDir, group, "temporal-validation", t0, status = "failed",
                metrics = list(reason = "historical panel unavailable"))
    return(invisible(NULL))
  }
  yrs <- magclass::getYears(panel, as.integer = TRUE)
  if (!any(yrs > trainEnd)) {
    .recordStep(groupDir, group, "temporal-validation", t0, status = "skipped",
                metrics = list(reason = paste0("no panel years after trainEnd = ", trainEnd)))
    return(invisible(NULL))
  }
  trainPanel <- panel[, yrs[yrs <= trainEnd], ]

  out <- list(spec = NULL, trainEnd = trainEnd, bySector = list())
  stepMetrics <- list(trainEnd = trainEnd)
  for (sec in sectors) {
    hit <- Filter(function(x) identical(x$model_type, paste0("PolicyStringency: ", sec)), sel)
    if (length(hit) == 0) next
    cfg <- norm(hit[[1]])
    out$spec <- out$spec %||% cfg$name
    say("refit on <= ", trainEnd, " and score ", trainEnd + 1, "+ ('", cfg$name, "', ", sec, ") ...")

    fit <- tryCatch(
      estimatePolicyStringencyModel(
        data = trainPanel, sector = sec, estimator = "satP", indexMax = indexMax,
        actorPowerDrivers = cfg$actorPowerDrivers, actorPowerIndex = cfg$actorPowerIndex,
        instQualityDrivers = cfg$instQualityDrivers, controlDrivers = cfg$controlDrivers,
        regionMappingFixedEffects = cfg$regionMappingFixedEffects,
        logisticTimeTrend = isTRUE(cfg$logisticTimeTrend),
        interactRegionFE = isTRUE(cfg$interactRegionFE),
        useMundlak = isTRUE(cfg$useMundlak),
        gdpGovInteraction = isTRUE(cfg$gdpGovInteraction),
        includeLaggedPS = isTRUE(cfg$includeLaggedPS),
        modelDir = NULL, verbose = FALSE
      ),
      error = function(e) {
        say("  ", sec, " train-window fit failed: ", conditionMessage(e))
        NULL
      }
    )
    if (is.null(fit)) next

    # Held-out design from the FULL panel with the training transforms frozen:
    # scaling from the trainEnd fit, trend frozen at trainEnd (deployment
    # discipline). The observed outcome column rides along untransformed.
    vDf <- tryCatch(preparePanelData(
      data = panel, sector = sec,
      actorPowerDrivers = cfg$actorPowerDrivers, actorPowerIndex = cfg$actorPowerIndex,
      instQualityDrivers = cfg$instQualityDrivers,
      controlDrivers = setdiff(unlist(cfg$controlDrivers), "lagged_ecp"),
      regionMappingFixedEffects = if (isTRUE(cfg$useMundlak)) NULL else cfg$regionMappingFixedEffects,
      useMundlak = isTRUE(cfg$useMundlak),
      gdpGovInteraction = isTRUE(cfg$gdpGovInteraction),
      driverScaling = fit$driverScaling,
      trendFreezeYear = trainEnd,
      outcomeVar = fit$outcomeVar %||% "Policy Stringency"
    ), error = function(e) {
      say("  ", sec, " validation design failed: ", conditionMessage(e))
      NULL
    })
    if (is.null(vDf)) next
    vDf <- vDf[vDf$year > trainEnd, , drop = FALSE]
    observedNat <- vDf$ecp                       # natural 0-indexMax scale

    # One-step-ahead with observed lags for the dynamics rung: transform the
    # observed lagged level exactly as the fit did.
    if ("lagged_ecp" %in% all.vars(fit$formula)) {
      nSV <- fit$squeeze$n
      p <- pmin(pmax(vDf$lagged_ecp / indexMax, 0), 1)
      vDf$lagged_ecp <- stats::qlogis(.psmSqueeze(p, nSV))
    }

    # FE alignment + driver-support guard, as at deployment.
    lv <- fit$model$xlevels$regionFE
    if (!is.null(lv) && "regionFE" %in% names(vDf)) {
      fe <- as.character(vDf$regionFE)
      fe[!fe %in% lv] <- if ("Other" %in% lv) "Other" else lv[1]
      vDf$regionFE <- factor(fe, levels = lv)
    }
    guarded <- .psmDriverGuard(vDf, .driverSupportRanges(fit$data, fit$driverScaling))
    vDf <- guarded$df

    tt <- stats::delete.response(stats::terms(fit$formula))
    mm <- stats::model.matrix(tt, stats::model.frame(tt, data = vDf, na.action = stats::na.pass))
    beta <- stats::coef(fit$model)
    shared <- intersect(colnames(mm), names(beta))
    eta <- as.numeric(mm[, shared, drop = FALSE] %*% beta[shared])
    trainedRegions <- unique(as.character(fit$data$region))
    ooc <- !(as.character(vDf$region) %in% trainedRegions)
    seEta <- rep(NA_real_, nrow(vDf))
    vc <- fit$vcov
    if (!is.null(vc) && all(shared %in% rownames(vc))) {
      vcS <- vc[shared, shared, drop = FALSE]
      seEta <- sqrt(pmax(rowSums((mm[, shared, drop = FALSE] %*% vcS) *
                                   mm[, shared, drop = FALSE]), 0))
      feSpread <- .regionFESpread(beta)
      seEta[ooc] <- sqrt(seEta[ooc]^2 + feSpread^2)
    }
    zc <- stats::qnorm(0.975)
    pred <- indexMax * stats::plogis(eta)
    lo <- indexMax * stats::plogis(eta - zc * seEta)
    hi <- indexMax * stats::plogis(eta + zc * seEta)

    rows <- data.frame(
      region = vDf$region, year = vDf$year, sector = sec,
      observed = observedNat, predicted = pred, lo = lo, hi = hi,
      outOfCoverage = ooc, driverOutOfSupport = guarded$outOfSupport,
      stringsAsFactors = FALSE
    )
    rows <- rows[is.finite(rows$observed) & is.finite(rows$predicted), , drop = FALSE]
    if (nrow(rows) == 0) {
      say("  ", sec, ": no scored rows (no observed outcome after ", trainEnd, ").")
      next
    }

    # Persistence baseline: last observed value at/before trainEnd, carried forward.
    histArr <- as.array(panel[, yrs[yrs <= trainEnd], paste0("Policy Stringency|", sec)])[, , 1]
    lastObs <- apply(histArr, 1, function(v) {
      f <- which(is.finite(v))
      if (length(f)) v[max(f)] else NA_real_
    })
    rows$persistence <- as.numeric(lastObs[as.character(rows$region)])

    err <- rows$observed - rows$predicted
    errP <- rows$observed - rows$persistence
    metrics <- list(
      n = nrow(rows),
      rmse = sqrt(mean(err^2)),
      mae = mean(abs(err)),
      pearson = suppressWarnings(stats::cor(rows$observed, rows$predicted)),
      spearman = suppressWarnings(stats::cor(rows$observed, rows$predicted, method = "spearman")),
      ciCoverage = mean(rows$observed >= rows$lo & rows$observed <= rows$hi, na.rm = TRUE),
      rmsePersistence = sqrt(mean(errP^2, na.rm = TRUE)),
      skillVsPersistence = 1 - sqrt(mean(err^2)) / sqrt(mean(errP^2, na.rm = TRUE))
    )
    out$bySector[[sec]] <- list(metrics = metrics, rows = rows)
    stepMetrics[[paste0("rmse", sec)]] <- round(metrics$rmse, 3)
    stepMetrics[[paste0("spearman", sec)]] <- round(metrics$spearman, 3)
    stepMetrics[[paste0("skill", sec)]] <- round(metrics$skillVsPersistence, 3)
    say("  ", sec, ": n=", metrics$n, " RMSE=", round(metrics$rmse, 3),
        " rho=", round(metrics$spearman, 2),
        " CI-coverage=", round(metrics$ciCoverage, 2),
        " skill-vs-persistence=", round(metrics$skillVsPersistence, 2))

    if ("ecm" %in% extraForms) {
      out$bySector[[sec]]$ecm <- tryCatch(
        .psmValidateECMSector(cfg, sec, panel, trainPanel, trainEnd, indexMax, lastObs),
        error = function(e) {
          say("  ", sec, " ECM validation failed: ", conditionMessage(e))
          NULL
        }
      )
      me <- out$bySector[[sec]]$ecm$metrics
      if (!is.null(me)) {
        stepMetrics[[paste0("skillEcm", sec)]] <- round(me$skillVsPersistence, 3)
        say("  ", sec, " [ecm]: n=", me$n, " RMSE=", round(me$rmse, 3),
            " skill-vs-persistence=", round(me$skillVsPersistence, 2),
            " speed=", round(me$adjustmentSpeed, 2))
      }
    }
    if ("events" %in% extraForms) {
      out$bySector[[sec]]$events <- tryCatch(
        .psmValidateEventsSector(cfg, sec, panel, trainEnd, eventJump),
        error = function(e) {
          say("  ", sec, " events validation failed: ", conditionMessage(e))
          NULL
        }
      )
      me <- out$bySector[[sec]]$events$metrics
      if (!is.null(me)) {
        stepMetrics[[paste0("aucEvents", sec)]] <- round(me$auc, 3)
        stepMetrics[[paste0("skillEvents", sec)]] <- round(me$skillVsBaseRate, 3)
        say("  ", sec, " [events]: n=", me$n, " events=", me$nEvents,
            " AUC=", round(me$auc, 2),
            " Brier-skill-vs-base-rate=", round(me$skillVsBaseRate, 2))
      }
    }
  }

  if (!length(out$bySector)) {
    .recordStep(groupDir, group, "temporal-validation", t0, status = "failed",
                metrics = list(reason = "no sector produced validation rows"))
    return(invisible(NULL))
  }
  saveRDS(out, file.path(groupDir, "temporal-validation.rds"))
  .recordStep(groupDir, group, "temporal-validation", t0, metrics = stepMetrics)
  say("Saved ", file.path(groupDir, "temporal-validation.rds"))
  invisible(out)
}

# ── ECM validation: integrate the level recursion from the observed trainEnd
# level with observed drivers; the forecast nests persistence (phi = -speed;
# speed -> 0 reduces to a drifting random walk), so beating persistence is a
# fair fight the static level spec cannot offer.
.psmValidateECMSector <- function(cfg, sec, panel, trainPanel, trainEnd, indexMax, lastObs) {
  fit <- estimatePolicyStringencyModel(
    data = trainPanel, sector = sec, estimator = "satP", form = "ecm", indexMax = indexMax,
    actorPowerDrivers = cfg$actorPowerDrivers, actorPowerIndex = cfg$actorPowerIndex,
    instQualityDrivers = cfg$instQualityDrivers, controlDrivers = cfg$controlDrivers,
    regionMappingFixedEffects = cfg$regionMappingFixedEffects,
    logisticTimeTrend = isTRUE(cfg$logisticTimeTrend),
    interactRegionFE = isTRUE(cfg$interactRegionFE),
    useMundlak = isTRUE(cfg$useMundlak),
    gdpGovInteraction = isTRUE(cfg$gdpGovInteraction),
    modelDir = NULL, verbose = FALSE
  )
  vDf <- preparePanelData(
    data = panel, sector = sec,
    actorPowerDrivers = cfg$actorPowerDrivers, actorPowerIndex = cfg$actorPowerIndex,
    instQualityDrivers = cfg$instQualityDrivers,
    controlDrivers = setdiff(unlist(cfg$controlDrivers), "lagged_ecp"),
    regionMappingFixedEffects = if (isTRUE(cfg$useMundlak)) NULL else cfg$regionMappingFixedEffects,
    useMundlak = isTRUE(cfg$useMundlak),
    gdpGovInteraction = isTRUE(cfg$gdpGovInteraction),
    driverScaling = fit$driverScaling,
    trendFreezeYear = trainEnd,
    outcomeVar = fit$outcomeVar %||% "Policy Stringency"
  )
  lv <- fit$model$xlevels$regionFE
  if (!is.null(lv) && "regionFE" %in% names(vDf)) {
    fe <- as.character(vDf$regionFE)
    fe[!fe %in% lv] <- if ("Other" %in% lv) "Other" else lv[1]
    vDf$regionFE <- factor(fe, levels = lv)
  }
  vDf <- .psmDriverGuard(vDf, .driverSupportRanges(fit$data, fit$driverScaling))$df

  nSV <- fit$squeeze$n
  toEta <- function(v) {
    p <- pmin(pmax(v / indexMax, 0), 1)
    stats::qlogis(.psmSqueeze(p, nSV))
  }
  phi <- tryCatch(stats::coef(fit$model)[["lagged_ecp"]], error = function(e) NA_real_)
  if (!is.finite(phi)) stop("ECM fit carries no lagged_ecp coefficient")

  vDf0 <- vDf
  vDf0$lagged_ecp <- 0
  tt <- stats::delete.response(stats::terms(fit$formula))
  mm <- stats::model.matrix(tt, stats::model.frame(tt, data = vDf0, na.action = stats::na.pass))
  beta <- stats::coef(fit$model)
  shared <- intersect(colnames(mm), names(beta))
  etaFixed <- as.numeric(mm[, shared, drop = FALSE] %*% beta[shared])

  # seed: observed transformed level at trainEnd per region
  seedRows <- vDf$year == trainEnd & is.finite(vDf$ecp)
  seed <- stats::setNames(toEta(vDf$ecp[seedRows]), as.character(vDf$region[seedRows]))

  hold <- which(vDf$year > trainEnd)
  pred <- rep(NA_real_, nrow(vDf))
  for (r in unique(as.character(vDf$region[hold]))) {
    if (!r %in% names(seed) || !is.finite(seed[[r]])) next
    idx <- hold[as.character(vDf$region[hold]) == r]
    idx <- idx[order(vDf$year[idx])]
    lagv <- seed[[r]]
    for (i in idx) {
      e <- etaFixed[i] + (1 + phi) * lagv
      if (!is.finite(e)) break
      pred[i] <- e
      lagv <- e
    }
  }
  rows <- data.frame(
    region = vDf$region[hold], year = vDf$year[hold], sector = sec,
    observed = vDf$ecp[hold], predicted = indexMax * stats::plogis(pred[hold]),
    stringsAsFactors = FALSE
  )
  rows$persistence <- as.numeric(lastObs[as.character(rows$region)])
  rows <- rows[is.finite(rows$observed) & is.finite(rows$predicted), , drop = FALSE]
  if (nrow(rows) == 0) stop("no scored ECM rows")
  err <- rows$observed - rows$predicted
  errP <- rows$observed - rows$persistence
  list(metrics = list(
    n = nrow(rows),
    rmse = sqrt(mean(err^2)),
    mae = mean(abs(err)),
    spearman = suppressWarnings(stats::cor(rows$observed, rows$predicted, method = "spearman")),
    rmsePersistence = sqrt(mean(errP^2, na.rm = TRUE)),
    skillVsPersistence = 1 - sqrt(mean(err^2)) / sqrt(mean(errP^2, na.rm = TRUE)),
    adjustmentSpeed = -phi,
    halfLife = fit$halfLife
  ), rows = rows)
}

# ── Events validation: region-level ratchet events derived from the panel index
# (jump >= eventJump), discrete-time cloglog hazard on the same drivers, scored
# on held-out events. The country-level CAPMF-instrument version (mrpfm
# calcPolicyRatchetEvents) is the richer upgrade once run against the cluster
# source; this panel-derived variant is self-contained and runs anywhere.
.psmValidateEventsSector <- function(cfg, sec, panel, trainEnd, eventJump) {
  psVar <- paste0("Policy Stringency|", sec)
  yrs <- sort(magclass::getYears(panel, as.integer = TRUE))
  panel <- panel[, yrs, ]
  arr <- as.array(panel[, , psVar])[, , 1, drop = FALSE]
  dim(arr) <- dim(arr)[1:2]
  ev <- arr * NA_real_
  for (t in 2:ncol(arr)) {
    fin <- is.finite(arr[, t]) & is.finite(arr[, t - 1])
    ev[fin, t] <- as.numeric((arr[fin, t] - arr[fin, t - 1]) >= eventJump)
  }
  evName <- paste0("Ratchet Event|", sec)
  evM <- magclass::new.magpie(magclass::getItems(panel, dim = 1), yrs, evName, fill = NA)
  evM[, , 1] <- as.vector(ev)
  dataEv <- magclass::mbind(panel, evM)

  fitEv <- estimatePolicyRatchetModel(
    data = dataEv[, yrs[yrs <= trainEnd], ], sector = sec,
    actorPowerDrivers = cfg$actorPowerDrivers, actorPowerIndex = cfg$actorPowerIndex,
    instQualityDrivers = cfg$instQualityDrivers, controlDrivers = cfg$controlDrivers,
    regionMappingFixedEffects = cfg$regionMappingFixedEffects,
    logisticTimeTrend = isTRUE(cfg$logisticTimeTrend),
    interactRegionFE = isTRUE(cfg$interactRegionFE),
    useMundlak = isTRUE(cfg$useMundlak),
    gdpGovInteraction = isTRUE(cfg$gdpGovInteraction),
    verbose = FALSE
  )
  scal <- attr(fitEv$data, "driverScaling")
  vDf <- preparePanelData(
    data = dataEv, sector = sec,
    actorPowerDrivers = cfg$actorPowerDrivers, actorPowerIndex = cfg$actorPowerIndex,
    instQualityDrivers = cfg$instQualityDrivers,
    controlDrivers = unlist(cfg$controlDrivers),
    regionMappingFixedEffects = if (isTRUE(cfg$useMundlak)) NULL else cfg$regionMappingFixedEffects,
    useMundlak = isTRUE(cfg$useMundlak),
    gdpGovInteraction = isTRUE(cfg$gdpGovInteraction),
    driverScaling = scal,
    trendFreezeYear = trainEnd,
    outcomeVar = "Ratchet Event"
  )
  lv <- fitEv$model$xlevels$regionFE
  if (!is.null(lv) && "regionFE" %in% names(vDf)) {
    fe <- as.character(vDf$regionFE)
    fe[!fe %in% lv] <- if ("Other" %in% lv) "Other" else lv[1]
    vDf$regionFE <- factor(fe, levels = lv)
  }
  vDf <- .psmDriverGuard(vDf, .driverSupportRanges(fitEv$data, fitEv$driverScaling))$df
  vDf <- vDf[vDf$year > trainEnd & is.finite(vDf$ecp), , drop = FALSE]
  if (nrow(vDf) == 0) stop("no held-out event rows")

  tt <- stats::delete.response(stats::terms(fitEv$formula))
  mm <- stats::model.matrix(tt, stats::model.frame(tt, data = vDf, na.action = stats::na.pass))
  beta <- stats::coef(fitEv$model)
  shared <- intersect(colnames(mm), names(beta))
  eta <- as.numeric(mm[, shared, drop = FALSE] %*% beta[shared])
  p <- if (identical(fitEv$link, "cloglog")) 1 - exp(-exp(eta)) else stats::plogis(eta)

  obs <- vDf$ecp
  ok <- is.finite(obs) & is.finite(p)
  obs <- obs[ok]
  p <- p[ok]
  regionsOk <- as.character(vDf$region)[ok]
  # per-region base rate from the training window (global rate fallback)
  trainRate <- tapply(fitEv$data$ecp, as.character(fitEv$data$region), mean, na.rm = TRUE)
  globalRate <- mean(fitEv$data$ecp, na.rm = TRUE)
  pBase <- as.numeric(trainRate[regionsOk])
  pBase[!is.finite(pBase)] <- globalRate
  brier <- mean((obs - p)^2)
  brierBase <- mean((obs - pBase)^2)
  list(metrics = list(
    n = length(obs),
    nEvents = sum(obs > 0),
    eventRate = mean(obs),
    auc = .psmAUC(obs, p),
    brier = brier,
    brierBase = brierBase,
    skillVsBaseRate = 1 - brier / brierBase,
    eventJump = eventJump
  ), rows = data.frame(region = regionsOk, year = vDf$year[ok], sector = sec,
                       observed = obs, p = p, pBase = pBase, stringsAsFactors = FALSE))
}

# Rank-based (Wilcoxon) AUC — no dependency.
.psmAUC <- function(y, p) {
  ok <- is.finite(y) & is.finite(p)
  y <- y[ok]
  p <- p[ok]
  n1 <- sum(y > 0)
  n0 <- sum(y <= 0)
  if (n1 == 0 || n0 == 0) return(NA_real_)
  r <- rank(p)
  (sum(r[y > 0]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}
# nolint end
