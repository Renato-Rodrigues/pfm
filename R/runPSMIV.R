# nolint start
#' Shift-share IV Run-Group step (causal check on Incumbent Power)
#'
#' @description
#' Fits the shift-share 2SLS rung (\code{\link{estimatePolicyStringencyModel}}
#' \code{(estimator = "satP-iv")}: Incumbent Power instrumented by base-year
#' fossil exposure x leave-one-out global VRE diffusion) for both sectors, in
#' TWO trend variants — with the deployed spec's trend and with no trend — since
#' the diffusion shift is itself trend-like and the first-stage strength must be
#' shown for both (documented design caveat). IQ channels/controls/FE come from
#' the deployed spec. Writes \code{<group>/iv.rds}: per sector x variant the
#' non-FE coefficient table, the weak-instrument F and Wu-Hausman diagnostics.
#'
#' @inheritParams runPSMTemporalValidation
#' @return Invisibly, the artifact list, or \code{NULL} when skipped.
#' @export
#' @author Renato Rodrigues
runPSMIV <- function(group,
                     resultsDir = getOption("pfm.resultsDir", "output"),
                     modelDir = getOption("pfm.modelDir", "output"),
                     cachefolder = NULL, panelData = NULL,
                     y = 2000:2022,
                     outputRegionMappingFile = "regionmapping_54.csv",
                     indexMax = 10, verbose = TRUE) {
  groupDir <- .resolveGroupDir(group, resultsDir, modelDir, cachefolder)
  say <- function(...) if (isTRUE(verbose)) message("[PSM-IV:", group, "] ", ...)
  t0 <- Sys.time()
  if (!requireNamespace("AER", quietly = TRUE)) {
    .recordStep(groupDir, group, "iv", t0, status = "skipped",
                metrics = list(reason = "the 'AER' package is not installed"))
    warning("runPSMIV: 'AER' not installed - step skipped.", call. = FALSE)
    return(invisible(NULL))
  }
  selPath <- file.path(groupDir, "selected-models-psm.yml")
  if (!file.exists(selPath)) {
    .recordStep(groupDir, group, "iv", t0, status = "skipped",
                metrics = list(reason = "no selected-models-psm.yml"))
    return(invisible(NULL))
  }
  sel <- yaml::read_yaml(selPath)
  norm <- function(s) {
    for (f in c("instQualityDrivers", "controlDrivers")) {
      if (!is.null(s[[f]])) s[[f]] <- unlist(s[[f]])
    }
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
  if (is.null(panel)) {
    .recordStep(groupDir, group, "iv", t0, status = "failed",
                metrics = list(reason = "historical panel unavailable"))
    return(invisible(NULL))
  }

  out <- list(spec = NULL, bySector = list())
  stepMetrics <- list()
  for (sec in c("Bulk", "Diffuse")) {
    hit <- Filter(function(x) identical(x$model_type, paste0("PolicyStringency: ", sec)), sel)
    if (length(hit) == 0) next
    cfg <- norm(hit[[1]])
    out$spec <- out$spec %||% cfg$name
    for (variant in c("trend", "noTrend")) {
      lab <- paste0(sec, ".", variant)
      say("IV fit ", lab, " ...")
      fit <- tryCatch(
        estimatePolicyStringencyModel(
          data = panel, sector = sec, estimator = "satP-iv", indexMax = indexMax,
          actorPowerDrivers = "Incumbent Power", actorPowerIndex = "Incumbent Power",
          instQualityDrivers = cfg$instQualityDrivers, controlDrivers = cfg$controlDrivers,
          regionMappingFixedEffects = cfg$regionMappingFixedEffects,
          timeTrend = identical(variant, "trend"),
          logisticTimeTrend = identical(variant, "trend") && isTRUE(cfg$logisticTimeTrend),
          useMundlak = isTRUE(cfg$useMundlak),
          gdpGovInteraction = isTRUE(cfg$gdpGovInteraction),
          modelDir = NULL, verbose = FALSE
        ),
        error = function(e) {
          say("  ", lab, " failed: ", conditionMessage(e))
          NULL
        }
      )
      if (is.null(fit)) next
      ct <- as.data.frame(unclass(fit$coeftest))
      names(ct) <- c("estimate", "stdError", "zValue", "pValue")
      ct$term <- rownames(fit$coeftest)
      ct <- ct[!grepl("^regionFE|^\\(Intercept\\)$", ct$term), ]
      rownames(ct) <- NULL
      out$bySector[[lab]] <- list(coefTable = ct, ivDiagnostics = fit$ivDiagnostics,
                                  instrument = fit$instrument, n = nrow(fit$data))
      wf <- tryCatch(fit$ivDiagnostics[grep("Weak instruments",
                                            rownames(fit$ivDiagnostics))[1], "statistic"],
                     error = function(e) NA_real_)
      inc <- tryCatch(ct$estimate[ct$term == "Incumbent.Power"], error = function(e) NA_real_)
      stepMetrics[[paste0("weakF.", lab)]] <- round(wf, 1)
      stepMetrics[[paste0("incumbent.", lab)]] <- round(inc, 3)
      say("  ", lab, ": Incumbent.Power = ", round(inc, 3), " | weak-F = ", round(wf, 1))
    }
  }
  if (!length(out$bySector)) {
    .recordStep(groupDir, group, "iv", t0, status = "failed",
                metrics = list(reason = "no IV variant fit"))
    return(invisible(NULL))
  }
  saveRDS(out, file.path(groupDir, "iv.rds"))
  .recordStep(groupDir, group, "iv", t0, metrics = stepMetrics)
  say("Saved ", file.path(groupDir, "iv.rds"))
  invisible(out)
}
# nolint end
