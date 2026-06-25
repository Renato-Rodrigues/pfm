# nolint start
# Post-processing steps for a Run-Group (ADR 0018): runRobustness / runTemporalSplit /
# runSubnational each read the Run-Group's selected deliverable (and, for robustness, the
# sweep result) and write their own artifact beside it. runModelGroup chains the steps.
# The heavy statistical primitives live in pfm already (computeLORO, computeTemporalSplit,
# computeMaximinScore, ...); these functions are the orchestration that used to sit in the
# pfm-reports build-*.R scripts. (`%||%` is provided package-wide by buildPFMModel.R.)

# Internal: build the historical panel the way the deliverable was trained (+ GDP^2).
#' @keywords internal
.buildHistPanel <- function(y = 2000:2022, outputRegionMappingFile = "regionmapping_54.csv",
                            movingAverage = 5) {
  p <- panelDataHistorical(aggregate = TRUE, y = y,
                           outputRegionMappingFile = outputRegionMappingFile,
                           movingAverage = movingAverage)
  if ("GDP per Capita" %in% magclass::getNames(p) &&
      !"GDP per Capita Sq" %in% magclass::getNames(p)) {
    p <- magclass::mbind(p, magclass::setNames(p[, , "GDP per Capita"]^2, "GDP per Capita Sq"))
  }
  p
}

# Internal: read the Run-Group's selected deliverable (selected-models.yml) as one normalised
# spec per stage. Errors if the sweep has not been run.
#' @keywords internal
.loadGroupDeliverable <- function(groupDir) {
  if (!requireNamespace("yaml", quietly = TRUE)) stop("The 'yaml' package is required.")
  cfgPath <- file.path(groupDir, "selected-models.yml")
  if (!file.exists(cfgPath)) {
    stop("No selected-models.yml in '", groupDir, "'. Run runSweep() for this group first.",
         call. = FALSE)
  }
  specs <- yaml::read_yaml(cfgPath)
  norm <- function(s) {
    for (f in c("actorPowerDrivers", "actorPowerIndex", "instQualityDrivers", "controlDrivers")) {
      if (!is.null(s[[f]])) s[[f]] <- unlist(s[[f]])
    }
    s
  }
  byStage <- list()
  for (e in specs) {
    stg <- sub(":.*", "", e$model_type)
    if (is.null(byStage[[stg]])) byStage[[stg]] <- norm(e)
  }
  byStage
}

# Internal: resolve + create the Run-Group dir, set the Fit-Cache option + madrat cache.
#' @keywords internal
.resolveGroupDir <- function(group, resultsDir, modelDir, cachefolder = NULL) {
  if (missing(group) || is.null(group) || !nzchar(group)) stop("'group' is required.", call. = FALSE)
  if (is.null(resultsDir)) stop("Supply 'resultsDir' or set options(pfm.resultsDir=).", call. = FALSE)
  .useMadratCache(cachefolder)
  if (!is.null(modelDir)) options(pfm.modelDir = modelDir)
  groupDir <- file.path(resultsDir, group)
  if (!dir.exists(groupDir)) stop("Run-Group '", groupDir, "' does not exist. Run runSweep() first.",
                                  call. = FALSE)
  groupDir
}

#' Robustness post-processing for a Run-Group (ADR 0012/0013)
#'
#' Anchors a one-knob Robustness Ladder on the selected deliverable, traces the BIC parsimony
#' frontier over the sweep results, optionally runs a control specification-curve on every rung,
#' and a Leave-One-Region-Out (LORO) sweep on the deliverable specs. Writes
#' \code{<group>/robustness.rds}.
#'
#' @param group,resultsDir,modelDir Run-Group locators (see \code{\link{runSweep}}).
#' @param panelData Optional pre-built panel; built via \code{panelDataHistorical} when NULL.
#' @param quick Logical. Skip the (heavy) control specification-curve. Default \code{FALSE}.
#' @param loro Logical. Run the Leave-One-Region-Out sweep. Default \code{TRUE}.
#' @param y,outputRegionMappingFile Panel build parameters.
#' @param verbose Logical. Default \code{TRUE}.
#' @return The robustness artifact (invisibly).
#' @seealso ADR 0012, ADR 0013, \code{\link{computeLORO}}, \code{\link{computeMaximinScore}}
#' @export
runRobustness <- function(group, resultsDir = getOption("pfm.resultsDir", "output"),
                          modelDir = getOption("pfm.modelDir", "output"), cachefolder = NULL,
                          panelData = NULL,
                          quick = FALSE, loro = TRUE, y = 2000:2022,
                          outputRegionMappingFile = "regionmapping_54.csv", verbose = TRUE) {
  groupDir <- .resolveGroupDir(group, resultsDir, modelDir, cachefolder)
  say <- function(...) if (isTRUE(verbose)) message("[robustness:", group, "] ", ...)
  t0 <- Sys.time()
  sectors <- c("Bulk", "Diffuse")
  if (is.null(panelData)) { say("Building panel ..."); panelData <- .buildHistPanel(y, outputRegionMappingFile) }
  panel <- panelData
  byStage <- .loadGroupDeliverable(groupDir)
  deliv <- list(Adoption = byStage[["Adoption"]], Stringency = byStage[["Stringency"]])
  ACC <- function(cfg) grep("Accountability", cfg$instQualityDrivers, value = TRUE)[1]

  fitMetrics <- function(cfg, sector, stage, rung) {
    fit <- tryCatch({
      if (stage == "Adoption")
        estimateAdoptionModel(data = panel, sector = sector,
          actorPowerDrivers = cfg$actorPowerDrivers, actorPowerIndex = cfg$actorPowerIndex,
          instQualityDrivers = cfg$instQualityDrivers, controlDrivers = cfg$controlDrivers,
          includeLaggedAdoption = isTRUE(cfg$includeLagged), interactRegionFE = isTRUE(cfg$interactRegionFE),
          regionMappingFixedEffects = cfg$regionMappingFixedEffects, useMundlak = isTRUE(cfg$useMundlak),
          gdpGovInteraction = isTRUE(cfg$gdpGovInteraction), logisticTimeTrend = isTRUE(cfg$logisticTimeTrend),
          ridgeInteractions = isTRUE(cfg$ridgeInteractions), panelTransform = cfg$panelTransform %||% "levels",
          modelDir = modelDir, verbose = FALSE, compute = c(ame = FALSE, predictedProbs = FALSE))
      else
        estimatePriceStringencyModel(data = panel, sector = sector, family = "gaussian",
          actorPowerDrivers = cfg$actorPowerDrivers, actorPowerIndex = cfg$actorPowerIndex,
          instQualityDrivers = cfg$instQualityDrivers, controlDrivers = cfg$controlDrivers,
          includeLaggedECP = isTRUE(cfg$includeLaggedECP), interactRegionFE = isTRUE(cfg$interactRegionFE),
          regionMappingFixedEffects = cfg$regionMappingFixedEffects, useMundlak = isTRUE(cfg$useMundlak),
          gdpGovInteraction = isTRUE(cfg$gdpGovInteraction), logisticTimeTrend = isTRUE(cfg$logisticTimeTrend),
          ridgeInteractions = isTRUE(cfg$ridgeInteractions), panelTransform = cfg$panelTransform %||% "levels",
          nickellCorrection = isTRUE(cfg$nickellCorrection), modelDir = modelDir, verbose = FALSE)
    }, error = function(e) { say("FAILED ", rung, "/", stage, ":", sector, " - ", conditionMessage(e)); NULL })
    base <- data.frame(rung = rung, stage = stage, sector = sector, nParams = NA_integer_,
      deltaR2Theory = NA_real_, pseudoR2 = NA_real_, aic = NA_real_, bic = NA_real_, maxVIF = NA_real_,
      sigAP = 0L, sigIQ = 0L, sigInt = 0L, tier = "Yellow", apIQcoef = NA_real_, converged = FALSE,
      stringsAsFactors = FALSE)
    if (is.null(fit) || is.null(fit$model)) return(base)
    m <- fit$model; ct <- fit$coeftest
    isLf <- inherits(m, "logistf"); k <- length(stats::coef(m))
    n <- if (isLf) m$n else tryCatch(stats::nobs(m), error = function(e) NA_integer_)
    ll <- if (isLf) as.numeric(m$loglik["full"]) else tryCatch(as.numeric(stats::logLik(m)), error = function(e) NA_real_)
    base$nParams <- k
    base$aic <- if (isLf) -2 * ll + 2 * k else as.numeric(m$aic)
    npar <- if (isLf) k else k + 1L
    base$bic <- if (is.finite(base$aic) && is.finite(n) && n > 0) base$aic + npar * (log(n) - 2) else NA_real_
    base$pseudoR2 <- if (isLf) as.numeric(1 - m$loglik["full"] / m$loglik["null"]) else
      if (!is.null(m$deviance) && !is.null(m$null.deviance) && m$null.deviance > 0) 1 - m$deviance / m$null.deviance else NA_real_
    base$maxVIF <- fit$maxVIF %||% NA_real_
    base$converged <- if (isLf) !any(is.na(m$coefficients)) else isTRUE(m$converged)
    base$deltaR2Theory <- tryCatch(computeDeltaR2Theory(fit, actorPowerDrivers = cfg$actorPowerDrivers,
      actorPowerIndex = cfg$actorPowerIndex, instQualityDrivers = cfg$instQualityDrivers,
      stage = tolower(stage)), error = function(e) NA_real_)
    if (!is.null(ct)) {
      p <- ct[, 4]; sig <- rownames(ct)[!is.na(p) & p < 0.05]
      base$sigInt <- sum(grepl(":|_x_", sig)); sb <- sig[!grepl(":|_x_", sig)]
      cm <- function(vars, pats) { if (!length(vars) || !length(pats)) return(0L)
        pp <- make.names(pats); sum(vapply(vars, function(v) any(vapply(pp, function(q) grepl(q, v, fixed = TRUE), logical(1))), logical(1))) }
      base$sigIQ <- cm(sb, cfg$instQualityDrivers); base$sigAP <- cm(sb, c(cfg$actorPowerDrivers, cfg$actorPowerIndex))
      icoef <- ct[grepl(":|_x_", rownames(ct)), 1]; base$apIQcoef <- if (length(icoef)) icoef[1] else NA_real_
    }
    base$tier <- computeTheoryTier(base$sigAP, base$sigIQ, base$sigInt)
    base
  }

  ladderCfgs <- function(stage) {
    d <- deliv[[stage]]; comp <- d; comp$actorPowerDrivers <- "Actor Power Index"; comp$actorPowerIndex <- "Actor Power Index"
    ge <- d; ge$instQualityDrivers <- "Government Effectiveness (WGI)"
    rl <- d; rl$instQualityDrivers <- "Rule of Law (VDem)"
    ac <- d; ac$instQualityDrivers <- ACC(d)
    fd <- d; fd$panelTransform <- "hybridFD"
    lg <- d; if (stage == "Adoption") lg$includeLagged <- TRUE else { lg$includeLaggedECP <- TRUE; lg$nickellCorrection <- TRUE }
    rg <- d; rg$ridgeInteractions <- TRUE
    list("1. Deliverable" = d, "2. Composite AP" = comp, "3a. IQ: GovEff only" = ge,
         "3b. IQ: Rule of Law only" = rl, "3c. IQ: best Accountability only" = ac,
         "4. First-difference" = fd, "5. Lagged DV" = lg, "6. Ridge interactions" = rg)
  }
  say("Fitting Robustness Ladder rungs ...")
  ladder <- do.call(rbind, lapply(c("Adoption", "Stringency"), function(stg) {
    cfgs <- ladderCfgs(stg)
    if (is.null(deliv[[stg]])) return(NULL)
    do.call(rbind, lapply(names(cfgs), function(rn)
      do.call(rbind, lapply(sectors, function(sec) fitMetrics(cfgs[[rn]], sec, stg, rn)))))
  }))

  say("Computing parsimony frontier ...")
  res <- readRDS(file.path(groupDir, "sweep.rds"))
  realFE <- "fe:(H12|OECDp|Mundlak)"
  mmCols <- c("model", "sector", "sigActorPower", "sigInstQual", "sigInteractions",
              "deltaR2Theory", "pseudoR2", "bic", "maxVIF", "converged", "usesLagged", "nFE",
              "nObs", "sigControl", "nControl", "trendShare")
  frontier <- do.call(rbind, lapply(c("Adoption", "Stringency"), function(stg) {
    sub <- res$results[res$results$stage == stg, ]; sub <- sub[grepl(realFE, sub$model), ]
    if (!"bic" %in% colnames(sub) || nrow(sub) == 0) return(NULL)
    do.call(rbind, lapply(c(0.05, 0.10, 0.15, 0.20, 0.30), function(eps) {
      mm <- computeMaximinScore(sub[, intersect(mmCols, colnames(sub))], nearTieEps = eps)
      top <- mm[mm$gatePass, ][1, ]
      data.frame(stage = stg, eps = eps, model = top$model, minTier = top$minTier,
                 meanDeltaR2 = top$meanDeltaR2, sumBIC = top$sumBIC, stringsAsFactors = FALSE)
    }))
  }))

  controlGrid <- function() {
    inc <- list(none = character(0), "GDP-Q" = "GDP per Capita (Q-centred)",
                "GDP-Q+Q2" = c("GDP per Capita (Q-centred)", "GDP per Capita (Q-centred) Sq"),
                "logGDP" = "GDP per Capita (log)",
                "logGDP+sq" = c("GDP per Capita (log)", "GDP per Capita (log) Sq"))
    combos <- list()
    for (inm in names(inc)) for (pop in c(FALSE, TRUE)) for (hyd in c(FALSE, TRUE)) {
      cc <- inc[[inm]]; if (pop) cc <- c(cc, "Population (log)"); if (hyd) cc <- c(cc, "Hydro Nuclear Share")
      combos[[paste0(inm, if (pop) "+pop", if (hyd) "+hyd")]] <- cc
    }
    combos
  }
  ctrlCurve <- NULL
  if (!isTRUE(quick)) {
    say("Running control specification-curve on all rungs (heavy) ...")
    grid <- controlGrid()
    ctrlCurve <- do.call(rbind, lapply(c("Adoption", "Stringency"), function(stg) {
      if (is.null(deliv[[stg]])) return(NULL)
      cfgs <- ladderCfgs(stg)
      do.call(rbind, lapply(names(cfgs), function(rn) {
        core <- cfgs[[rn]]
        do.call(rbind, lapply(names(grid), function(gn) {
          cc <- core; cc$controlDrivers <- grid[[gn]]
          do.call(rbind, lapply(sectors, function(sec) { r <- fitMetrics(cc, sec, stg, rn); r$controls <- gn; r }))
        }))
      }))
    }))
  }

  loco <- NULL
  if (isTRUE(loro)) {
    say("Running Leave-One-Region-Out (LORO) on the deliverable specs (heavy) ...")
    loco <- list()
    for (stg in c("Adoption", "Stringency")) {
      d <- deliv[[stg]]; if (is.null(d)) next
      for (sec in sectors) {
        key <- paste(stg, sec, sep = "_"); say("  LORO ", key, " ...")
        loco[[key]] <- tryCatch(computeLORO(data = panel, sector = sec, stage = tolower(stg),
          actorPowerDrivers = d$actorPowerDrivers, actorPowerIndex = d$actorPowerIndex,
          instQualityDrivers = d$instQualityDrivers, controlDrivers = d$controlDrivers,
          regionMappingFixedEffects = d$regionMappingFixedEffects, family = "gaussian",
          modelDir = NULL, verbose = FALSE),
          error = function(e) { say("LORO FAILED ", key, ": ", conditionMessage(e)); NULL })
      }
    }
  }

  out <- list(ladder = ladder, frontier = frontier, ctrlCurve = ctrlCurve, loco = loco,
              specs = deliv, generated = Sys.time(), quick = quick)
  saveRDS(out, file.path(groupDir, "robustness.rds"))
  .recordStep(groupDir, group, "robustness", t0, metrics = list(
    ladderRows = if (is.null(ladder)) 0L else nrow(ladder),
    frontierRows = if (is.null(frontier)) 0L else nrow(frontier),
    ctrlCurveRows = if (is.null(ctrlCurve)) 0L else nrow(ctrlCurve),
    locoSpecs = if (is.null(loco)) 0L else length(loco),
    quick = quick, loro = loro))
  say("Saved ", file.path(groupDir, "robustness.rds"),
      " (ladder ", if (is.null(ladder)) 0 else nrow(ladder), " rows)")
  invisible(out)
}

#' Temporal-split (out-of-time) post-processing for a Run-Group (ADR 0015)
#'
#' Runs \code{\link{computeTemporalSplit}} on the selected deliverable specs over the panel
#' extended to the latest driver-supported year. Writes \code{<group>/temporal-split.rds}.
#'
#' @param group,resultsDir,modelDir Run-Group locators.
#' @param maxYear Integer. Latest year to extend the panel to (falls back to 2022). Default 2023.
#' @param outputRegionMappingFile Panel region mapping.
#' @param panelData Optional pre-built (extended) panel; built when NULL.
#' @param verbose Logical. Default \code{TRUE}.
#' @return The temporal-split artifact (invisibly).
#' @seealso ADR 0015, \code{\link{computeTemporalSplit}}
#' @export
runTemporalSplit <- function(group, resultsDir = getOption("pfm.resultsDir", "output"),
                             modelDir = getOption("pfm.modelDir", "output"), cachefolder = NULL,
                             maxYear = 2023,
                             outputRegionMappingFile = "regionmapping_54.csv",
                             panelData = NULL, verbose = TRUE) {
  groupDir <- .resolveGroupDir(group, resultsDir, modelDir, cachefolder)
  say <- function(...) if (isTRUE(verbose)) message("[temporal:", group, "] ", ...)
  t0 <- Sys.time()
  panel <- panelData
  if (is.null(panel)) {
    panel <- tryCatch(.buildHistPanel(2000:maxYear, outputRegionMappingFile),
                      error = function(e) { say(maxYear, " panel failed (", conditionMessage(e), "); 2022."); NULL })
    if (is.null(panel)) panel <- .buildHistPanel(2000:2022, outputRegionMappingFile)
  }
  say("panel years: ", paste(range(magclass::getYears(panel, as.integer = TRUE)), collapse = "-"))
  byStage <- .loadGroupDeliverable(groupDir)

  temporal <- list()
  for (stg in c("Adoption", "Stringency")) {
    d <- byStage[[stg]]; if (is.null(d)) next
    for (sec in c("Bulk", "Diffuse")) {
      key <- paste(stg, sec, sep = "_"); say("computeTemporalSplit ", key, " ...")
      temporal[[key]] <- tryCatch(computeTemporalSplit(data = panel, sector = sec, stage = tolower(stg),
        actorPowerDrivers = d$actorPowerDrivers, actorPowerIndex = d$actorPowerIndex,
        instQualityDrivers = d$instQualityDrivers, controlDrivers = d$controlDrivers,
        regionMappingFixedEffects = d$regionMappingFixedEffects, family = "gaussian",
        logisticTimeTrend = isTRUE(d$logisticTimeTrend), modelDir = NULL, verbose = FALSE),
        error = function(e) { say("FAILED ", key, ": ", conditionMessage(e)); NULL })
    }
  }
  out <- list(temporal = temporal, specNames = sapply(byStage, function(s) s$name),
              panelYears = range(magclass::getYears(panel, as.integer = TRUE)), generated = Sys.time())
  saveRDS(out, file.path(groupDir, "temporal-split.rds"))
  .recordStep(groupDir, group, "temporal", t0, metrics = list(
    keys = length(temporal),
    panelYears = paste(range(magclass::getYears(panel, as.integer = TRUE)), collapse = "-")))
  say("Saved ", file.path(groupDir, "temporal-split.rds"))
  invisible(out)
}

#' Subnational-coverage sensitivity post-processing for a Run-Group (ADR 0016)
#'
#' Compares the deployed national-only effective carbon price with a full-coverage version that
#' folds in subnational (\dQuote{within_country}) instruments, then re-estimates the deliverable
#' on both panels. Writes \code{<group>/subnational.rds}. Cache-only assembly (national price
#' from the Fit/madrat cache plus the \code{*Subnational} reader subtypes).
#'
#' @param group,resultsDir,modelDir Run-Group locators.
#' @param y,outputRegionMappingFile Panel build parameters.
#' @param panelData Optional pre-built national panel; built when NULL.
#' @param verbose Logical. Default \code{TRUE}.
#' @return The subnational sensitivity artifact (invisibly).
#' @seealso ADR 0016
#' @importFrom magclass getNames getYears getItems mselect dimSums collapseNames setNames mbind
#' @export
runSubnational <- function(group, resultsDir = getOption("pfm.resultsDir", "output"),
                           modelDir = getOption("pfm.modelDir", "output"), cachefolder = NULL,
                           y = 2000:2022,
                           outputRegionMappingFile = "regionmapping_54.csv",
                           panelData = NULL, verbose = TRUE) {
  groupDir <- .resolveGroupDir(group, resultsDir, modelDir, cachefolder)
  say <- function(...) if (isTRUE(verbose)) message("[subnat:", group, "] ", ...)
  t0 <- Sys.time()

  natECP <- calcOutput("CarbonPrice", subtype = "effectivePrice", aggregate = FALSE)
  subP <- madrat::readSource("WBCarbonPricingDashboard", subtype = "priceSubnational")
  subC <- madrat::readSource("WBCarbonPricingDashboard", subtype = "emissionsCoveredSubnational")
  common <- intersect(getNames(subP), getNames(subC))
  common <- common[grepl("[.](bulk|diffuse|all)$", common)]
  yy <- intersect(getYears(subP), getYears(subC))
  pxc <- subP[, yy, common] * subC[, yy, common]
  cov <- subC[, yy, common]

  edg <- madrat::readSource("EDGARghg")
  bulkV <- c("Industrial Combustion", "Power Industry", "Processes", "Fuel Exploitation")
  diffV <- c("Buildings", "Transport", "Agriculture", "Waste")
  histE <- mbind(setNames(dimSums(mselect(edg, variable = bulkV), 3, na.rm = TRUE), "bulk"),
                 setNames(dimSums(mselect(edg, variable = diffV), 3, na.rm = TRUE), "diffuse"))
  ry <- intersect(yy, getYears(histE)); rr <- intersect(getItems(pxc, 1), getItems(histE, 1))
  hb <- collapseNames(histE[rr, ry, "bulk"]); hd <- collapseNames(histE[rr, ry, "diffuse"])
  tot <- hb + hd; shB <- hb / tot; shB[!is.finite(shB)] <- 0; shD <- 1 - shB; shD[!is.finite(shD)] <- 0
  zero <- hb * 0
  psum <- function(x, s) { sgv <- magclass::getItems(x, dim = 3.2)
    if (!s %in% sgv) return(zero)
    z <- collapseNames(dimSums(mselect(x, sector_group = s), dim = 3.1))[rr, ry]; z[!is.finite(z)] <- 0; z }
  pxB <- psum(pxc, "bulk") + psum(pxc, "all") * shB
  pxD <- psum(pxc, "diffuse") + psum(pxc, "all") * shD
  cvB <- psum(cov, "bulk") + psum(cov, "all") * shB
  cvD <- psum(cov, "diffuse") + psum(cov, "all") * shD
  sclB <- pmin(cvB, hb) / cvB; sclB[!is.finite(sclB)] <- 0; subB <- (pxB * sclB) / hb; subB[!is.finite(subB)] <- 0
  sclD <- pmin(cvD, hd) / cvD; sclD[!is.finite(sclD)] <- 0; subD <- (pxD * sclD) / hd; subD[!is.finite(subD)] <- 0

  addSlice <- function(ecp, add, sgi) { v <- intersect(getYears(ecp), getYears(add)); g <- intersect(getItems(ecp, 1), getItems(add, 1))
    ecp[g, v, sgi] <- ecp[g, v, sgi] + setNames(add[g, v], sgi); ecp }
  fullECP <- addSlice(addSlice(natECP, subB, "bulk"), subD, "diffuse")

  cmp <- function(sgi) {
    yrs <- intersect(getYears(natECP), getYears(fullECP))
    n <- collapseNames(natECP[, yrs, sgi]); f <- collapseNames(fullECP[, yrs, sgi])
    flips <- which(as.array(n) <= 0 & as.array(f) > 1e-6, arr.ind = TRUE)
    flipDf <- if (nrow(flips)) data.frame(region = getItems(n, 1)[flips[, 1]],
      year = as.integer(sub("y", "", getYears(n)[flips[, 2]])), fullECP = round(as.array(f)[flips], 2), stringsAsFactors = FALSE) else NULL
    rises <- which(as.array(n) > 1e-6 & (as.array(f) - as.array(n)) > 1e-6, arr.ind = TRUE)
    riseDf <- if (nrow(rises)) data.frame(region = getItems(n, 1)[rises[, 1]],
      year = as.integer(sub("y", "", getYears(n)[rises[, 2]])), natECP = round(as.array(n)[rises], 2), fullECP = round(as.array(f)[rises], 2)) else NULL
    list(sector = sgi, flips = flipDf, rises = riseDf,
         nFlipRegionYears = if (is.null(flipDf)) 0L else nrow(flipDf),
         nFlipRegions = if (is.null(flipDf)) 0L else length(unique(flipDf$region)))
  }
  comparison <- lapply(c("bulk", "diffuse"), cmp); names(comparison) <- c("bulk", "diffuse")
  for (cc in comparison) say(cc$sector, ": ", cc$nFlipRegionYears, " new adopter region-years (", cc$nFlipRegions, " regions)")

  mapping <- tryCatch(madrat::toolGetMapping(outputRegionMappingFile, type = "regional", where = "mappingfolder"),
                      error = function(e) madrat::toolGetMapping(outputRegionMappingFile, type = "regional"))
  ccol <- intersect(c("CountryCode", "countryCode"), names(mapping))[1]
  rcol <- intersect(c("RegionCode", "regionCode"), names(mapping))[1]
  agg54 <- function(sub, wt) { v <- intersect(getYears(sub), getYears(wt)); g <- intersect(getItems(sub, 1), getItems(wt, 1))
    madrat::toolAggregate(sub[g, v], rel = mapping, weight = collapseNames(wt[g, v]) + 1e-9, from = ccol, to = rcol) }

  panel <- if (is.null(panelData)) .buildHistPanel(y, outputRegionMappingFile) else panelData
  panelFull <- panel
  for (sgi in c("bulk", "diffuse")) {
    delta <- agg54(if (sgi == "bulk") subB else subD, if (sgi == "bulk") hb else hd)
    vn <- paste0("Effective Carbon Price|", tools::toTitleCase(sgi))
    py <- intersect(getYears(panelFull), getYears(delta)); pr <- intersect(getItems(panelFull, 1), getItems(delta, 1))
    panelFull[pr, py, vn] <- panelFull[pr, py, vn] + setNames(collapseNames(delta[pr, py]), vn)
  }
  byStage <- .loadGroupDeliverable(groupDir)

  fitOne <- function(pnl, d, sec, stg) {
    fit <- tryCatch({
      if (tolower(stg) == "adoption")
        estimateAdoptionModel(data = pnl, sector = sec, actorPowerDrivers = d$actorPowerDrivers,
          actorPowerIndex = d$actorPowerIndex, instQualityDrivers = d$instQualityDrivers, controlDrivers = d$controlDrivers,
          regionMappingFixedEffects = d$regionMappingFixedEffects, logisticTimeTrend = isTRUE(d$logisticTimeTrend),
          modelDir = NULL, verbose = FALSE, compute = c(ame = FALSE, predictedProbs = FALSE))
      else
        estimatePriceStringencyModel(data = pnl, sector = sec, family = "gaussian", actorPowerDrivers = d$actorPowerDrivers,
          actorPowerIndex = d$actorPowerIndex, instQualityDrivers = d$instQualityDrivers, controlDrivers = d$controlDrivers,
          regionMappingFixedEffects = d$regionMappingFixedEffects, logisticTimeTrend = isTRUE(d$logisticTimeTrend),
          modelDir = NULL, verbose = FALSE)
    }, error = function(e) NULL)
    m <- .channelFitMetrics(fit, d, sec, stg)
    m$tier <- computeTheoryTier(m$sigActorPower, m$sigInstQual, m$sigInteractions)
    m$adoptRate <- if (!is.null(fit$data)) mean(as.integer(fit$data$ecp > 0)) else NA_real_
    m
  }
  sensitivity <- list()
  for (stg in c("Adoption", "Stringency")) { d <- byStage[[stg]]; if (is.null(d)) next
    for (sec in c("Bulk", "Diffuse")) { key <- paste(stg, sec, sep = "_"); say("re-estimate ", key, " ...")
      sensitivity[[key]] <- list(national = fitOne(panel, d, sec, stg), full = fitOne(panelFull, d, sec, stg)) } }

  out <- list(comparison = comparison, sensitivity = sensitivity, sg = c("bulk", "diffuse"),
              specNames = sapply(byStage, function(s) s$name), generated = Sys.time())
  saveRDS(out, file.path(groupDir, "subnational.rds"))
  .recordStep(groupDir, group, "subnational", t0, metrics = list(
    sensitivityKeys = length(sensitivity),
    bulkFlipRegionYears = comparison$bulk$nFlipRegionYears %||% 0L,
    diffuseFlipRegionYears = comparison$diffuse$nFlipRegionYears %||% 0L))
  say("Saved ", file.path(groupDir, "subnational.rds"))
  invisible(out)
}

#' Difference-First comparison post-processing for a Run-Group (ADR 0014)
#'
#' Post-hoc consumer of the Run-Group sweep: filters to hybridFD, ranks with maximin, applies
#' the Falsification Gate (pureFD re-fit), and configures the winner for LEVELS estimation. Also
#' runs the gate on the levels-first deliverable for comparison. Writes
#' \code{<group>/difference-first.rds} (diagnostics for the report) and the difference-first
#' selected-models YAML. Does not re-fit the sweep.
#'
#' @param group,resultsDir,modelDir Run-Group locators.
#' @param gdxFile Character or NULL. Scenario gdx (enables the levels FE-by-sanity choice).
#' @param panelData,scenarioData Optional pre-built panels.
#' @param maxTries Integer. Max ranked hybridFD candidates to falsification-test. Default 25.
#' @param y,outputRegionMappingFile Panel build parameters.
#' @param verbose Logical. Default \code{TRUE}.
#' @return The difference-first diagnostics list (invisibly).
#' @seealso ADR 0014, \code{\link{selectDifferenceFirst}}, \code{\link{computeFalsificationGate}}
#' @export
runDifferenceFirst <- function(group, resultsDir = getOption("pfm.resultsDir", "output"),
                               modelDir = getOption("pfm.modelDir", "output"), cachefolder = NULL,
                               gdxFile = NULL,
                               panelData = NULL, scenarioData = NULL, maxTries = 25L,
                               y = 2000:2022, outputRegionMappingFile = "regionmapping_54.csv",
                               verbose = TRUE) {
  groupDir <- .resolveGroupDir(group, resultsDir, modelDir, cachefolder)
  say <- function(...) if (isTRUE(verbose)) message("[DIF:", group, "] ", ...)
  t0 <- Sys.time()
  if (!requireNamespace("yaml", quietly = TRUE)) stop("The 'yaml' package is required.")
  sectors <- c("Bulk", "Diffuse")
  panel <- if (is.null(panelData)) .buildHistPanel(y, outputRegionMappingFile) else panelData
  if (is.null(scenarioData) && !is.null(gdxFile) && file.exists(gdxFile)) {
    scenarioData <- tryCatch(panelDataScenario(gdxFile = gdxFile, aggregate = TRUE,
      outputRegionMappingFile = outputRegionMappingFile), error = function(e) NULL)
  }
  res <- readRDS(file.path(groupDir, "sweep.rds"))$results
  cfgFile <- list.files(groupDir, pattern = "^channels-.*\\.yml$", full.names = TRUE)
  if (length(cfgFile) == 0) stop("No sweep config (channels-*.yml) in ", groupDir, call. = FALSE)
  cfgFile <- cfgFile[[1]]
  mode <- sub("^channels-(.*)\\.yml$", "\\1", basename(cfgFile))
  specs <- yaml::read_yaml(cfgFile)
  norm <- function(s) { for (f in c("actorPowerDrivers", "actorPowerIndex", "instQualityDrivers", "controlDrivers"))
    if (!is.null(s[[f]])) s[[f]] <- unlist(s[[f]]); s }
  specByName <- stats::setNames(lapply(specs, norm), vapply(specs, function(s) s$name, character(1)))

  say("difference-first selection (maxTries = ", maxTries, ") ...")
  dif <- selectDifferenceFirst(results = res, specByName = specByName, panelData = panel,
    scenarioData = scenarioData, modelDir = modelDir, maxTries = maxTries,
    regionBlocks = .h12RegionBlocks(),
    histPricesBySector = .histPricesBySector(panel, sectors), say = say)

  for (stg in names(dif)) {
    d <- dif[[stg]]
    if (is.null(d$chosenConfigLevels) || is.na(d$chosen)) next
    cfg <- norm(d$chosenConfigLevels)
    dif[[stg]]$levelsFit <- do.call(rbind, lapply(sectors, function(sec) {
      fit <- tryCatch({
        if (tolower(stg) == "adoption")
          estimateAdoptionModel(data = panel, sector = sec, actorPowerDrivers = cfg$actorPowerDrivers,
            actorPowerIndex = cfg$actorPowerIndex, instQualityDrivers = cfg$instQualityDrivers,
            controlDrivers = cfg$controlDrivers, regionMappingFixedEffects = cfg$regionMappingFixedEffects,
            useMundlak = isTRUE(cfg$useMundlak), panelTransform = "levels", modelDir = NULL,
            verbose = FALSE, compute = c(ame = FALSE, predictedProbs = FALSE))
        else
          estimatePriceStringencyModel(data = panel, sector = sec, family = "gaussian",
            actorPowerDrivers = cfg$actorPowerDrivers, actorPowerIndex = cfg$actorPowerIndex,
            instQualityDrivers = cfg$instQualityDrivers, controlDrivers = cfg$controlDrivers,
            regionMappingFixedEffects = cfg$regionMappingFixedEffects, useMundlak = isTRUE(cfg$useMundlak),
            panelTransform = "levels", modelDir = NULL, verbose = FALSE)
      }, error = function(e) NULL)
      .channelFitMetrics(fit, cfg, sec, stg)
    }))
    lf <- dif[[stg]]$levelsFit
    dif[[stg]]$levelsFit$tier <- computeTheoryTier(lf$sigActorPower, lf$sigInstQual, lf$sigInteractions)
  }

  lf1 <- tryCatch(yaml::read_yaml(file.path(groupDir, "selected-models.yml")), error = function(e) NULL)
  if (!is.null(lf1)) {
    byStage <- list()
    for (e in lf1) { stg <- sub(":.*", "", e$model_type); if (is.null(byStage[[stg]])) byStage[[stg]] <- norm(e) }
    for (stg in names(dif)) {
      cfg <- byStage[[stg]]; if (is.null(cfg)) next
      dif[[stg]]$levelsFirstName <- cfg$name
      dif[[stg]]$levelsFirstGate <- tryCatch(computeFalsificationGate(cfg, data = panel, stage = stg,
        sectors = sectors, modelDir = NULL), error = function(e) NULL)
    }
  }

  yml <- .writeDifferenceFirstConfig(dif, mode = mode, configDir = groupDir, sectors = sectors)
  if (!is.null(yml) && file.exists(yml)) {
    file.copy(yml, file.path(groupDir, "selected-models-difference-first.yml"), overwrite = TRUE)
  }
  saveRDS(dif, file.path(groupDir, "difference-first.rds"))
  .recordStep(groupDir, group, "difference-first", t0, mode = mode, metrics = list(
    chosen = paste(vapply(names(dif), function(s)
      paste0(s, "=", if (is.na(dif[[s]]$chosen)) "none" else dif[[s]]$chosenBase %||% "?"),
      character(1)), collapse = "; ")))
  for (stg in names(dif)) { d <- dif[[stg]]
    if (is.na(d$chosen)) say(stg, " chosen: NONE passed the gate")
    else say(stg, " chosen: ", d$chosenBase, " | levels FE = ", d$chosenFE) }
  say("Saved ", file.path(groupDir, "difference-first.rds"))
  invisible(dif)
}

#' Run a full model group: sweep + post-processing steps (ADR 0018)
#'
#' Orchestrates the compute pipeline for one Run-Group: runs the requested \code{steps} in
#' order, each writing its artifact into \code{<resultsDir>/<group>/}. \code{"sweep"} must run
#' (or have run) before the post-processing steps, which read its selected deliverable.
#'
#' @param group Character. Run-Group name.
#' @param steps Character vector subset of \code{c("sweep","robustness","temporal",
#'   "subnational","difference-first")}. Default is the first four (the standard pipeline);
#'   \code{"difference-first"} is recognised but off by default (the ADR 0014 alternative
#'   selection comparison, a post-hoc consumer of the sweep).
#' @param resultsDir,modelDir Configurable Results Root / Fit Cache (the ADR 0009 model store).
#' @param cachefolder Character or NULL. The \strong{madrat} data-cache folder (distinct from
#'   \code{modelDir}); set on madrat for every step when non-NULL.
#' @param gdxFile Character or NULL. Scenario gdx (forwarded to the sweep and to
#'   difference-first's FE-by-sanity choice).
#' @param mode,selectionMethod,nCores,forceRefit Forwarded to \code{\link{runSweep}}.
#' @param verbose Logical. Default \code{TRUE}.
#' @param ... Forwarded to \code{\link{runSweep}}.
#' @return The Run-Group directory path (invisibly).
#' @seealso \code{\link{runSweep}}, \code{\link{runRobustness}}, \code{\link{runTemporalSplit}},
#'   \code{\link{runSubnational}}, \code{\link{runDifferenceFirst}}, ADR 0018
#' @export
runModelGroup <- function(group, steps = c("sweep", "robustness", "temporal", "subnational"),
                          resultsDir = getOption("pfm.resultsDir", "output"),
                          modelDir = getOption("pfm.modelDir", "output"), cachefolder = NULL,
                          gdxFile = NULL,
                          mode = c("exhaustive", "guided"),
                          selectionMethod = c("levels-first", "difference-first"),
                          nCores = 1L, forceRefit = FALSE,
                          bootstrapResamples = 200L, bootstrapDetail = "channel",
                          bootstrapTopK = 40L, verbose = TRUE, ...) {
  mode <- match.arg(mode)
  selectionMethod <- match.arg(selectionMethod)
  allSteps <- c("sweep", "robustness", "temporal", "subnational", "difference-first",
                "selection-bootstrap")
  steps <- intersect(allSteps, steps)
  if (length(steps) == 0) stop("runModelGroup: no valid steps. Choose from ",
                               paste(allSteps, collapse = ", "), ".", call. = FALSE)
  say <- function(...) if (isTRUE(verbose)) message("[runModelGroup:", group, "] ", ...)
  if ("sweep" %in% steps) {
    say("step: sweep")
    runSweep(group, mode = mode, resultsDir = resultsDir, modelDir = modelDir,
             cachefolder = cachefolder, gdxFile = gdxFile,
             nCores = nCores, forceRefit = forceRefit, selectionMethod = selectionMethod,
             verbose = verbose, ...)
  }
  if ("robustness" %in% steps) { say("step: robustness"); runRobustness(group, resultsDir = resultsDir, modelDir = modelDir, cachefolder = cachefolder, verbose = verbose) }
  if ("temporal" %in% steps) { say("step: temporal"); runTemporalSplit(group, resultsDir = resultsDir, modelDir = modelDir, cachefolder = cachefolder, verbose = verbose) }
  if ("subnational" %in% steps) { say("step: subnational"); runSubnational(group, resultsDir = resultsDir, modelDir = modelDir, cachefolder = cachefolder, verbose = verbose) }
  if ("difference-first" %in% steps) { say("step: difference-first"); runDifferenceFirst(group, resultsDir = resultsDir, modelDir = modelDir, cachefolder = cachefolder, gdxFile = gdxFile, verbose = verbose) }
  if ("selection-bootstrap" %in% steps) { say("step: selection-bootstrap"); runSelectionBootstrap(group, resultsDir = resultsDir, modelDir = modelDir, cachefolder = cachefolder, nResamples = bootstrapResamples, detail = bootstrapDetail, topK = bootstrapTopK, verbose = verbose) }
  say("done: ", paste(steps, collapse = ", "))
  invisible(file.path(resultsDir, group))
}
# nolint end
