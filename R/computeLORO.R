# nolint start
#' @title computeLORO
#' @description Leave-One-Region-Out (LORO) robustness for a hurdle-stage
#' deliverable spec (ADR 0013). The estimation panel has 54 regions; this drops
#' each region in turn, refits on the remaining 53, and evaluates the held-out
#' region. It reports two complementary quantities:
#' \describe{
#'   \item{(A) coefficient stability}{For each theory coefficient (Actor Power,
#'     Institutional Quality, and the AP\eqn{\times}IQ interaction) the full-sample
#'     estimate, the leave-one-out mean and SD across the 54 refits, the sign-stability
#'     fraction, and the single most influential region (the drop that moves the
#'     coefficient most). Plus a per-fold Theory Tier so tier-stability is countable.}
#'   \item{(B) out-of-sample prediction}{Each fold's 53-region fit predicts the
#'     held-out region's rows (driver scaling re-frozen on the 53 and applied to the
#'     held-out region, so there is no scaling leakage). Pooled held-out predictions
#'     are scored with the same metrics as \code{\link{computePredictiveDiagnostics}}
#'     (adoption: Brier / AUC / calibration slope; stringency: RMSE / correlation on
#'     the \code{log(1+ECP)} scale) and benchmarked against the in-sample fit and the
#'     naive base rate.}
#' }
#'
#' Report-only: like the Robustness Ladder it characterises the deliverable's
#' sensitivity and never changes model selection. Because the deliverable uses
#' \emph{block} fixed effects, a held-out region keeps an identified block effect
#' from its block-mates. A held-out region whose block becomes empty in training
#' (singleton block) is excluded from the OOS metric (kept in coefficient stability)
#' and counted; OOS-stringency is defined only over held-out adopter region-years.
#'
#' @param data A \code{magpie} object: the historical panel (54-region resolution).
#' @param sector Character. \code{"Bulk"} or \code{"Diffuse"}.
#' @param stage Character. \code{"adoption"} or \code{"stringency"}.
#' @param actorPowerDrivers,actorPowerIndex,instQualityDrivers,controlDrivers
#'   Spec driver vectors, as passed to the estimate functions.
#' @param regionMappingFixedEffects Character. Block FE mapping file.
#' @param family Character. Stringency GLM family (default \code{"gaussian"}).
#' @param lag Integer. Driver lag (default 1).
#' @param modelDir Character or NULL. Passed through to the estimate functions for
#'   the per-fold fits; \code{NULL} (default) disables persistence.
#' @param verbose Logical. Progress messages (default \code{FALSE}).
#'
#' @return A list: \code{coef} (coefficient-stability data.frame), \code{tier}
#'   (per-fold tiers + counts), \code{oos} (pooled OOS metrics, in-sample reference,
#'   excluded-region counts), \code{preds} (pooled held-out predictions), and
#'   \code{meta}. \code{NULL} if the full-sample fit fails.
#'
#' @importFrom stats coef terms delete.response model.frame model.matrix model.response plogis qlogis glm binomial var sd cor
#' @importFrom magclass getItems
#' @export
#' @author Renato Rodrigues
computeLORO <- function(data, sector, stage,
                        actorPowerDrivers, actorPowerIndex, instQualityDrivers,
                        controlDrivers = NULL, regionMappingFixedEffects,
                        family = "gaussian", lag = 1,
                        modelDir = NULL, verbose = FALSE) {
  stage <- match.arg(tolower(stage), c("adoption", "stringency"))
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
  say <- function(...) if (isTRUE(verbose)) message("[LORO] ", ...)

  prepArgs <- list(sector = sector, actorPowerDrivers = actorPowerDrivers,
                   actorPowerIndex = actorPowerIndex, instQualityDrivers = instQualityDrivers,
                   controlDrivers = controlDrivers,
                   regionMappingFixedEffects = regionMappingFixedEffects, lag = lag)

  fitFold <- function(panel) {
    if (stage == "adoption")
      estimateAdoptionModel(data = panel, sector = sector,
        actorPowerDrivers = actorPowerDrivers, actorPowerIndex = actorPowerIndex,
        instQualityDrivers = instQualityDrivers, controlDrivers = controlDrivers,
        regionMappingFixedEffects = regionMappingFixedEffects, lag = lag,
        modelDir = modelDir, verbose = FALSE, compute = c(ame = FALSE, predictedProbs = FALSE))
    else
      estimatePriceStringencyModel(data = panel, sector = sector, family = family,
        actorPowerDrivers = actorPowerDrivers, actorPowerIndex = actorPowerIndex,
        instQualityDrivers = instQualityDrivers, controlDrivers = controlDrivers,
        regionMappingFixedEffects = regionMappingFixedEffects, lag = lag,
        modelDir = modelDir, verbose = FALSE)
  }

  # held-out test rows, scaled with the fold's frozen scaling, matching the real
  # in-sample sample construction (adoption: all rows; stringency: adopter rows on
  # the log(1+ECP) scale).
  testRows <- function(panelR, scaling, trainLevels) {
    dfR <- do.call(preparePanelData, c(list(data = panelR, driverScaling = scaling), prepArgs))
    if (stage == "adoption") {
      dfR$adoption <- as.integer(dfR$ecp > 0)
    } else {
      dfR <- dfR[dfR$ecp > 0, , drop = FALSE]
      if (!nrow(dfR)) return(NULL)
      dfR$ecp <- log(1 + dfR$ecp)
    }
    if ("regionFE" %in% names(dfR)) {
      blk <- as.character(dfR$regionFE)
      if (!all(blk %in% trainLevels)) return(NULL)          # singleton block -> undefined OOS
      dfR$regionFE <- factor(blk, levels = trainLevels)
    }
    dfR
  }

  predictRows <- function(fit, formula, dfTest) {
    mf <- tryCatch(stats::model.frame(formula, dfTest), error = function(e) NULL)
    if (is.null(mf) || !nrow(mf)) return(NULL)
    y  <- as.numeric(stats::model.response(mf))
    X  <- stats::model.matrix(stats::terms(formula), mf)
    cf <- stats::coef(fit); cf <- cf[!is.na(cf)]
    Xa <- matrix(0, nrow(X), length(cf), dimnames = list(NULL, names(cf)))
    common <- intersect(colnames(X), names(cf))
    Xa[, common] <- X[, common]
    eta <- as.vector(Xa %*% cf)
    pred <- if (stage == "adoption") stats::plogis(eta) else eta
    data.frame(y = y, pred = pred)
  }

  # significance counters (mirror build-robustness fitMetrics / computeMaximinScore)
  cm <- function(vars, pats) {
    if (!length(vars) || !length(pats)) return(0L)
    pp <- make.names(pats)
    sum(vapply(vars, function(v) any(vapply(pp, function(q) grepl(q, v, fixed = TRUE), logical(1))), logical(1)))
  }
  tierOf <- function(ct) {
    if (is.null(ct)) return(list(tier = "Yellow", sigAP = 0L, sigIQ = 0L, sigInt = 0L))
    p <- ct[, 4]; sig <- rownames(ct)[!is.na(p) & p < 0.05]
    sigInt <- sum(grepl(":|_x_", sig)); sb <- sig[!grepl(":|_x_", sig)]
    sigIQ <- cm(sb, instQualityDrivers); sigAP <- cm(sb, c(actorPowerDrivers, actorPowerIndex))
    list(tier = computeTheoryTier(sigAP, sigIQ, sigInt), sigAP = sigAP, sigIQ = sigIQ, sigInt = sigInt)
  }
  groupOf <- function(nm) {
    if (grepl(":|_x_", nm)) return("Interaction")
    if (cm(nm, c(actorPowerDrivers, actorPowerIndex)) > 0) return("ActorPower")
    if (cm(nm, instQualityDrivers) > 0) return("InstQuality")
    NA_character_
  }

  regions <- magclass::getItems(data, dim = 1)
  say("regions: ", length(regions))

  # ── Full-sample reference ───────────────────────────────────────────────────
  full <- tryCatch(fitFold(data), error = function(e) { say("full fit failed: ", conditionMessage(e)); NULL })
  if (is.null(full) || is.null(full$model)) return(NULL)
  coefFull   <- stats::coef(full$model); coefFull <- coefFull[!is.na(coefFull)]
  formulaRef <- full$formula
  inSample   <- tryCatch(computePredictiveDiagnostics(full$model, stage = stage), error = function(e) NULL)
  theoryNames <- names(coefFull)[!is.na(vapply(names(coefFull), groupOf, character(1)))]

  # ── 54 folds ─────────────────────────────────────────────────────────────────
  foldCoef <- matrix(NA_real_, length(regions), length(theoryNames),
                     dimnames = list(regions, theoryNames))
  tiers <- character(length(regions)); names(tiers) <- regions
  preds <- list(); nExcluded <- 0L; nFailed <- 0L

  for (r in regions) {
    panel53 <- data[setdiff(regions, r), , ]
    panelR  <- data[r, , ]
    fit <- tryCatch(fitFold(panel53), error = function(e) NULL)
    if (is.null(fit) || is.null(fit$model)) { nFailed <- nFailed + 1L; next }
    cf <- stats::coef(fit$model)
    foldCoef[r, ] <- cf[theoryNames]
    tiers[r] <- tierOf(fit$coeftest)$tier

    scaling <- attr(do.call(preparePanelData, c(list(data = panel53, driverScaling = NULL), prepArgs)),
                    "driverScaling")
    trainLevels <- if ("regionFE" %in% names(fit$model$model)) levels(fit$model$model$regionFE) else NULL
    dfR <- tryCatch(testRows(panelR, scaling, trainLevels), error = function(e) NULL)
    if (is.null(dfR)) { nExcluded <- nExcluded + 1L; next }
    pr <- tryCatch(predictRows(fit$model, fit$formula, dfR), error = function(e) NULL)
    if (!is.null(pr) && nrow(pr)) { pr$region <- r; preds[[r]] <- pr }
    say(r, " done")
  }

  # ── (A) coefficient stability ─────────────────────────────────────────────────
  coefStab <- do.call(rbind, lapply(theoryNames, function(tn) {
    v <- foldCoef[, tn]; v <- v[is.finite(v)]
    if (!length(v)) return(NULL)
    delta <- v - coefFull[[tn]]
    infl  <- names(delta)[which.max(abs(delta))]
    data.frame(term = tn, group = groupOf(tn), full = coefFull[[tn]],
               looMean = mean(v), looSD = stats::sd(v),
               signStable = mean(sign(v) == sign(coefFull[[tn]])),
               mostInfluential = infl %||% NA_character_,
               maxAbsDelta = max(abs(delta)), nFolds = length(v),
               stringsAsFactors = FALSE)
  }))

  tt <- factor(tiers[tiers != ""], levels = c("Green", "Blue", "Yellow"))
  tierStab <- list(perRegion = tiers,
                   counts = c(Green = sum(tt == "Green"), Blue = sum(tt == "Blue"),
                              Yellow = sum(tt == "Yellow")),
                   full = tierOf(full$coeftest)$tier)

  # ── (B) pooled out-of-sample ───────────────────────────────────────────────────
  P <- if (length(preds)) do.call(rbind, preds) else NULL
  oos <- list(n = 0L, nRegions = 0L, nExcludedSingletonBlock = nExcluded, nFailedFolds = nFailed,
              inSample = inSample)
  if (!is.null(P) && nrow(P)) {
    y <- P$y; p <- P$pred; ok <- is.finite(y) & is.finite(p); y <- y[ok]; p <- p[ok]
    oos$n <- length(y); oos$nRegions <- length(unique(P$region[ok]))
    if (stage == "adoption") {
      base <- mean(y)
      n1 <- sum(y == 1); n0 <- sum(y == 0)
      auc <- if (n1 > 0 && n0 > 0) {
        rk <- rank(p, ties.method = "average"); (sum(rk[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
      } else NA_real_
      eps <- 1e-12; lp <- stats::qlogis(pmin(pmax(p, eps), 1 - eps))
      cal <- tryCatch(if (n1 > 0 && n0 > 0 && stats::var(lp) > 0)
        unname(suppressWarnings(stats::glm(y ~ lp, family = stats::binomial())$coefficients["lp"])) else NA_real_,
        error = function(e) NA_real_)
      oos$brier <- mean((p - y)^2); oos$brierBase <- mean((base - y)^2)
      oos$auc <- auc; oos$calibrationSlope <- cal; oos$baseRate <- base
    } else {
      err <- y - p
      oos$rmse <- sqrt(mean(err^2)); oos$mae <- mean(abs(err))
      oos$corObsPred <- if (stats::var(y) > 0 && stats::var(p) > 0) stats::cor(y, p) else NA_real_
    }
  }

  list(coef = coefStab, tier = tierStab, oos = oos, preds = P,
       meta = list(sector = sector, stage = stage, nRegions = length(regions),
                   regionMappingFixedEffects = regionMappingFixedEffects))
}
# nolint end
