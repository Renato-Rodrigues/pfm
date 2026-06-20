#' @title computeTemporalSplit
#' @description Out-of-time (temporal hold-out) robustness for a hurdle-stage
#' deliverable spec (ADR 0015) — the temporal counterpart to
#' \code{\link{computeLORO}}. Instead of holding out regions it holds out future
#' years: train on the past, predict held-out future years. Two schemes:
#' \describe{
#'   \item{single chronological split}{train on years \eqn{\le} \code{cutoff}, test on
#'     \code{(cutoff, testEnd]}; reports a forecast-horizon curve (1..k years ahead).}
#'   \item{rolling-origin expanding window}{train \eqn{\le t}, predict year \eqn{t+1},
#'     pooled over \code{rollingOrigins}.}
#' }
#' Driver scaling is re-frozen on the training window and applied to the test years;
#' the (stringency) logistic time trend is frozen at \code{cutoff}, mirroring real
#' deployment (ADR 0010). Because adoption status is sticky, the headline adoption
#' metrics are scored on the \strong{at-risk set} — regions not yet adopted at the
#' cutoff (the genuine "next-wave" test) — with full-test-set metrics reported too.
#' Report-only; never changes selection.
#'
#' @param data A \code{magpie} object: the historical panel.
#' @param sector,stage Character. Sector (\code{"Bulk"}/\code{"Diffuse"}) and stage
#'   (\code{"adoption"}/\code{"stringency"}).
#' @param actorPowerDrivers,actorPowerIndex,instQualityDrivers,controlDrivers Spec drivers.
#' @param regionMappingFixedEffects Character. Block FE mapping file.
#' @param family Character. Stringency GLM family (default \code{"gaussian"}).
#' @param lag Integer. Driver lag (default 1).
#' @param logisticTimeTrend Logical. Pass the deliverable's trend flag so the fit
#'   matches it (stringency); the trend is frozen at \code{cutoff} for the test years.
#' @param cutoff Integer or NULL. Last training year (default \code{maxYear - 6}).
#' @param testEnd Integer or NULL. Last test year (default \code{maxYear}).
#' @param rollingOrigins Integer vector or NULL. Expanding-window origins; each trains
#'   on \eqn{\le t} and predicts \eqn{t+1} (default \code{(cutoff-2):(maxYear-1)}).
#' @param modelDir Character or NULL.
#' @param verbose Logical.
#'
#' @return A list: \code{meta}, \code{coef} (train-vs-full coefficient stability),
#'   \code{single} (at-risk + full out-of-time scores + in-sample reference),
#'   \code{horizon} (per-test-year metrics by forecast horizon), \code{rolling}
#'   (pooled 1-year-ahead scores + per-origin), and \code{preds}. \code{NULL} if the
#'   full-sample fit fails.
#'
#' @importFrom stats coef terms model.frame model.matrix model.response plogis qlogis glm binomial var sd cor
#' @importFrom magclass getYears
#' @export
#' @author Renato Rodrigues
computeTemporalSplit <- function(data, sector, stage,
                                 actorPowerDrivers, actorPowerIndex, instQualityDrivers,
                                 controlDrivers = NULL, regionMappingFixedEffects,
                                 family = "gaussian", lag = 1, logisticTimeTrend = FALSE,
                                 cutoff = NULL, testEnd = NULL, rollingOrigins = NULL,
                                 modelDir = NULL, verbose = FALSE) {
  stage <- match.arg(tolower(stage), c("adoption", "stringency"))
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
  say <- function(...) if (isTRUE(verbose)) message("[temporal] ", ...)
  yrs <- magclass::getYears(data, as.integer = TRUE)
  maxYear <- max(yrs)
  if (is.null(cutoff))  cutoff  <- maxYear - 6L
  if (is.null(testEnd)) testEnd <- maxYear
  if (is.null(rollingOrigins)) rollingOrigins <- seq.int(cutoff - 2L, maxYear - 1L)

  prepArgs <- list(sector = sector, actorPowerDrivers = actorPowerDrivers,
                   actorPowerIndex = actorPowerIndex, instQualityDrivers = instQualityDrivers,
                   controlDrivers = controlDrivers,
                   regionMappingFixedEffects = regionMappingFixedEffects, lag = lag)
  yslice <- function(lo, hi) data[, intersect(yrs, lo:hi), ]

  fitOn <- function(panel) tryCatch({
    if (stage == "adoption")
      estimateAdoptionModel(data = panel, sector = sector,
        actorPowerDrivers = actorPowerDrivers, actorPowerIndex = actorPowerIndex,
        instQualityDrivers = instQualityDrivers, controlDrivers = controlDrivers,
        regionMappingFixedEffects = regionMappingFixedEffects, lag = lag,
        logisticTimeTrend = logisticTimeTrend,
        modelDir = modelDir, verbose = FALSE, compute = c(ame = FALSE, predictedProbs = FALSE))
    else
      estimatePriceStringencyModel(data = panel, sector = sector, family = family,
        actorPowerDrivers = actorPowerDrivers, actorPowerIndex = actorPowerIndex,
        instQualityDrivers = instQualityDrivers, controlDrivers = controlDrivers,
        regionMappingFixedEffects = regionMappingFixedEffects, lag = lag,
        logisticTimeTrend = logisticTimeTrend,
        modelDir = modelDir, verbose = FALSE)
  }, error = function(e) { say("fit failed: ", conditionMessage(e)); NULL })

  # test rows for outcome-years in (trainCut, hi]; trendFreezeYear holds the trend
  # flat at the cutoff so the hindcast mimics deployment.
  testRows <- function(trainCut, hi, scaling, trainLevels) {
    panelT <- yslice(trainCut, hi)           # includes trainCut for the lag of trainCut+1
    dfT <- do.call(preparePanelData, c(list(data = panelT, driverScaling = scaling,
                                            trendFreezeYear = trainCut), prepArgs))
    dfT <- dfT[dfT$year > trainCut, , drop = FALSE]
    if (!nrow(dfT)) return(NULL)
    if (stage == "adoption") {
      dfT$adoption <- as.integer(dfT$ecp > 0)
    } else {
      dfT <- dfT[dfT$ecp > 0, , drop = FALSE]; if (!nrow(dfT)) return(NULL)
      dfT$ecp <- log(1 + dfT$ecp)
    }
    if ("regionFE" %in% names(dfT) && !is.null(trainLevels)) {
      blk <- as.character(dfT$regionFE)
      dfT <- dfT[blk %in% trainLevels, , drop = FALSE]; if (!nrow(dfT)) return(NULL)
      dfT$regionFE <- factor(as.character(dfT$regionFE), levels = trainLevels)
    }
    dfT
  }
  predictRows <- function(fit, formula, dfT) {
    rownames(dfT) <- NULL
    mf <- tryCatch(stats::model.frame(formula, dfT), error = function(e) NULL)
    if (is.null(mf) || !nrow(mf)) return(NULL)
    idx <- as.integer(rownames(mf))
    y <- as.numeric(stats::model.response(mf)); X <- stats::model.matrix(stats::terms(formula), mf)
    cf <- stats::coef(fit); cf <- cf[!is.na(cf)]
    Xa <- matrix(0, nrow(X), length(cf), dimnames = list(NULL, names(cf)))
    common <- intersect(colnames(X), names(cf)); Xa[, common] <- X[, common]
    eta <- as.vector(Xa %*% cf)
    data.frame(y = y, pred = if (stage == "adoption") stats::plogis(eta) else eta,
               region = dfT$region[idx], year = dfT$year[idx], stringsAsFactors = FALSE)
  }
  scoreOOS <- function(y, p) {
    ok <- is.finite(y) & is.finite(p); y <- y[ok]; p <- p[ok]
    if (length(y) < 3) return(NULL)
    if (stage == "adoption") {
      base <- mean(y); n1 <- sum(y == 1); n0 <- sum(y == 0)
      auc <- if (n1 > 0 && n0 > 0) { r <- rank(p, ties.method = "average"); (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0) } else NA_real_
      eps <- 1e-12; lp <- stats::qlogis(pmin(pmax(p, eps), 1 - eps))
      cal <- tryCatch(if (n1 > 0 && n0 > 0 && stats::var(lp) > 0)
        unname(suppressWarnings(stats::glm(y ~ lp, family = stats::binomial())$coefficients["lp"])) else NA_real_,
        error = function(e) NA_real_)
      list(n = length(y), nPos = n1, brier = mean((p - y)^2), brierBase = mean((base - y)^2),
           auc = auc, calibrationSlope = cal, baseRate = base)
    } else {
      err <- y - p
      list(n = length(y), rmse = sqrt(mean(err^2)), mae = mean(abs(err)),
           corObsPred = if (stats::var(y) > 0 && stats::var(p) > 0) stats::cor(y, p) else NA_real_)
    }
  }
  cm <- function(vars, pats) { if (!length(vars) || !length(pats)) return(0L)
    pp <- make.names(pats); sum(vapply(vars, function(v) any(vapply(pp, function(q) grepl(q, v, fixed = TRUE), logical(1))), logical(1))) }
  groupOf <- function(nm) { if (grepl(":|_x_", nm)) return("Interaction")
    if (cm(nm, c(actorPowerDrivers, actorPowerIndex)) > 0) return("ActorPower")
    if (cm(nm, instQualityDrivers) > 0) return("InstQuality"); NA_character_ }
  scalingOf <- function(panel) attr(do.call(preparePanelData, c(list(data = panel, driverScaling = NULL), prepArgs)), "driverScaling")
  trainLevelsOf <- function(fit) if ("regionFE" %in% names(fit$model$model)) levels(fit$model$model$regionFE) else NULL
  # regions already adopted (ecp>0) on or before year `c` (adoption stage only)
  adoptedBy <- function(adf, c) unique(adf$region[adf$year <= c & adf$ecp > 0])

  # ── full-sample reference ─────────────────────────────────────────────────────
  full <- fitOn(data)
  if (is.null(full) || is.null(full$model)) return(NULL)
  coefFull <- stats::coef(full$model); coefFull <- coefFull[!is.na(coefFull)]
  theoryNames <- names(coefFull)[!is.na(vapply(names(coefFull), groupOf, character(1)))]
  inSample <- tryCatch(computePredictiveDiagnostics(full$model, stage = stage), error = function(e) NULL)
  adoptDf <- if (stage == "adoption") full$data else NULL

  # ── (1) single chronological split ────────────────────────────────────────────
  say("single split: train <=", cutoff, ", test ", cutoff + 1, "-", testEnd)
  fitTr <- fitOn(yslice(min(yrs), cutoff))
  single <- list(cutoff = cutoff, testEnd = testEnd); coefStab <- NULL; predsS <- NULL; horizon <- NULL
  if (!is.null(fitTr) && !is.null(fitTr$model)) {
    cfTr <- stats::coef(fitTr$model)
    coefStab <- do.call(rbind, lapply(theoryNames, function(tn) data.frame(
      term = tn, group = groupOf(tn), full = coefFull[[tn]], train = unname(cfTr[tn]),
      delta = unname(cfTr[tn]) - coefFull[[tn]],
      signSame = isTRUE(sign(unname(cfTr[tn])) == sign(coefFull[[tn]])), stringsAsFactors = FALSE)))
    dfT <- testRows(cutoff, testEnd, scalingOf(yslice(min(yrs), cutoff)), trainLevelsOf(fitTr))
    predsS <- if (!is.null(dfT)) predictRows(fitTr$model, fitTr$formula, dfT) else NULL
    if (!is.null(predsS) && nrow(predsS)) {
      if (stage == "adoption") {
        atRiskReg <- setdiff(unique(predsS$region), adoptedBy(adoptDf, cutoff))
        ar <- predsS[predsS$region %in% atRiskReg, ]
        single$atRisk <- scoreOOS(ar$y, ar$pred); single$nAtRisk <- nrow(ar); single$nAtRiskReg <- length(atRiskReg)
        single$full <- scoreOOS(predsS$y, predsS$pred)
        horizon <- do.call(rbind, lapply(sort(unique(ar$year)), function(yy) { d <- ar[ar$year == yy, ]
          s <- scoreOOS(d$y, d$pred); if (is.null(s)) return(NULL)
          data.frame(year = yy, horizon = yy - cutoff, n = s$n, nPos = s$nPos, brier = s$brier, auc = s$auc) }))
      } else {
        single$atRisk <- scoreOOS(predsS$y, predsS$pred); single$full <- single$atRisk
        single$nAtRisk <- nrow(predsS)
        horizon <- do.call(rbind, lapply(sort(unique(predsS$year)), function(yy) { d <- predsS[predsS$year == yy, ]
          s <- scoreOOS(d$y, d$pred); if (is.null(s)) return(NULL)
          data.frame(year = yy, horizon = yy - cutoff, n = s$n, rmse = s$rmse, corObsPred = s$corObsPred) }))
      }
    }
  }
  single$inSample <- inSample

  # ── (2) rolling-origin expanding window (1-year-ahead, pooled) ─────────────────
  say("rolling origins: ", paste(range(rollingOrigins), collapse = "-"))
  rollP <- list(); originRows <- list()
  for (t in rollingOrigins) {
    if (t + 1L > maxYear) next
    fr <- fitOn(yslice(min(yrs), t)); if (is.null(fr) || is.null(fr$model)) next
    dft <- testRows(t, t + 1L, scalingOf(yslice(min(yrs), t)), trainLevelsOf(fr))
    pr <- if (!is.null(dft)) predictRows(fr$model, fr$formula, dft) else NULL
    if (is.null(pr) || !nrow(pr)) next
    if (stage == "adoption") {
      arReg <- setdiff(unique(pr$region), adoptedBy(adoptDf, t)); pr <- pr[pr$region %in% arReg, ]
      if (!nrow(pr)) next
    }
    pr$origin <- t; rollP[[length(rollP) + 1L]] <- pr
    s <- scoreOOS(pr$y, pr$pred)
    if (!is.null(s)) originRows[[length(originRows) + 1L]] <- data.frame(origin = t, predictYear = t + 1L, n = s$n,
      metric1 = if (stage == "adoption") s$auc else s$corObsPred,
      metric2 = if (stage == "adoption") s$brier else s$rmse, stringsAsFactors = FALSE)
  }
  P <- if (length(rollP)) do.call(rbind, rollP) else NULL
  rolling <- list(pooled = if (!is.null(P)) scoreOOS(P$y, P$pred) else NULL,
                  origins = if (length(originRows)) do.call(rbind, originRows) else NULL,
                  nOrigins = length(rollP))

  list(meta = list(sector = sector, stage = stage, cutoff = cutoff, testEnd = testEnd,
                   maxYear = maxYear, rollingOrigins = rollingOrigins),
       coef = coefStab, single = single, horizon = horizon, rolling = rolling, preds = predsS)
}
