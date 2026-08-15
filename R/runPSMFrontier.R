# nolint start
#' Feasibility-frontier Run-Group step for the deployed PSM spec
#'
#' @description
#' Re-estimates the Run-Group's deployed Policy Stringency spec under the
#' stochastic-frontier rung (\code{\link{estimatePolicyStringencyModel}} with
#' \code{estimator = "frontier"}) per sector and writes
#' \code{<group>/frontier.rds}: the frontier coefficient table, the
#' no-frontier-structure diagnostics (\code{gamma}, mixed chi-square LR) and the
#' per-row \code{\link{computeFeasibilityFrontier}} scores — feasibility ceiling,
#' political slack ("ambition gap") and the frontier Implementability ratio
#' (observed/frontier). See Tier-1 direction 1 in
#' `docs/psm-theoretical-directions.md`.
#'
#' @inheritParams runPSMTemporalValidation
#' @return Invisibly, the artifact list, or \code{NULL} when skipped.
#' @seealso \code{\link{computeFeasibilityFrontier}}, ADR 0036
#' @export
#' @author Renato Rodrigues
runPSMFrontier <- function(group,
                           resultsDir = getOption("pfm.resultsDir", "output"),
                           modelDir = getOption("pfm.modelDir", "output"),
                           cachefolder = NULL, panelData = NULL,
                           y = 2000:2022,
                           outputRegionMappingFile = "regionmapping_54.csv",
                           indexMax = 10, verbose = TRUE) {
  groupDir <- .resolveGroupDir(group, resultsDir, modelDir, cachefolder)
  say <- function(...) if (isTRUE(verbose)) message("[PSM-FRONTIER:", group, "] ", ...)
  t0 <- Sys.time()
  if (!requireNamespace("yaml", quietly = TRUE)) stop("The 'yaml' package is required.")
  if (!requireNamespace("frontier", quietly = TRUE)) {
    .recordStep(groupDir, group, "frontier", t0, status = "skipped",
                metrics = list(reason = "the 'frontier' package is not installed"))
    warning("runPSMFrontier: the 'frontier' package is not installed - step skipped.",
            call. = FALSE)
    return(invisible(NULL))
  }
  sectors <- c("Bulk", "Diffuse")

  selPath <- file.path(groupDir, "selected-models-psm.yml")
  if (!file.exists(selPath)) {
    .recordStep(groupDir, group, "frontier", t0, status = "skipped",
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
      .psmHistPanel(groupDir, y = y, outputRegionMappingFile = outputRegionMappingFile, verbose = verbose),
      error = function(e) NULL
    )
  }
  if (is.null(panel)) {
    .recordStep(groupDir, group, "frontier", t0, status = "failed",
                metrics = list(reason = "historical panel unavailable"))
    return(invisible(NULL))
  }

  out <- list(spec = NULL, bySector = list())
  stepMetrics <- list()
  for (sec in sectors) {
    hit <- Filter(function(x) identical(x$model_type, paste0("PolicyStringency: ", sec)), sel)
    if (length(hit) == 0) next
    cfg <- norm(hit[[1]])
    out$spec <- out$spec %||% cfg$name
    say("frontier fit for '", cfg$name, "' (", sec, ") ...")
    fit <- tryCatch(
      do.call(estimatePolicyStringencyModel, c(
        list(data = panel, sector = sec, estimator = "frontier", indexMax = indexMax,
             modelDir = NULL, verbose = FALSE),
        # See .psmSpecArgs(). Hand-listing these dropped apTransform, so the frontier
        # — the ceiling, the efficiency ratio, every gap and tier downstream of it —
        # was estimated on linear actor power while the deployed spec is satAP.
        .psmSpecArgs(cfg))),
      error = function(e) {
        say("  ", sec, " frontier fit failed: ", conditionMessage(e))
        NULL
      }
    )
    if (is.null(fit)) next
    ct <- as.data.frame(unclass(fit$coeftest))
    names(ct) <- c("estimate", "stdError", "zValue", "pValue")
    ct$term <- rownames(fit$coeftest)
    rownames(ct) <- NULL
    rob <- tryCatch(computeFrontierRobustness(fit), error = function(e) NULL)
    out$bySector[[sec]] <- list(
      gamma = fit$frontierGamma,
      lr = fit$frontierLR,
      n = nrow(fit$data),
      converged = isTRUE(fit$converged),
      coefTable = ct,
      scores = fit$frontier,
      robustness = rob
    )
    stepMetrics[[paste0("gamma", sec)]] <- round(fit$frontierGamma, 3)
    if (!is.null(rob)) {
      for (rg in names(rob)) {
        rc <- rob[[rg]]$slackRankCor
        if (!is.null(rc) && is.finite(rc)) {
          stepMetrics[[paste0(rg, "RankCor", sec)]] <- round(rc, 2)
        }
      }
      if (!is.null(rob$decay$eta) && is.finite(rob$decay$eta)) {
        stepMetrics[[paste0("decayEta", sec)]] <- round(rob$decay$eta, 4)
      }
      say("  ", sec, " robustness: ",
          paste(vapply(names(rob), function(rg) {
            if (!is.null(rob[[rg]]$error)) paste0(rg, "=FAILED")
            else sprintf("%s(gamma=%.2f, rankCor=%.2f%s)", rg,
                         rob[[rg]]$gamma %||% NA,
                         rob[[rg]]$slackRankCor %||% NA,
                         if (rg == "decay") sprintf(", eta=%.3f", rob[[rg]]$eta %||% NA) else "")
          }, character(1)), collapse = " | "))
    }
    say("  ", sec, ": gamma = ", round(fit$frontierGamma, 3),
        " | LR vs no-frontier = ", round(fit$frontierLR, 1),
        " | median slack = ",
        if (!is.null(fit$frontier)) round(stats::median(fit$frontier$slackIndex, na.rm = TRUE), 2)
        else NA)
  }

  if (!length(out$bySector)) {
    .recordStep(groupDir, group, "frontier", t0, status = "failed",
                metrics = list(reason = "no sector produced a frontier fit"))
    return(invisible(NULL))
  }
  saveRDS(out, file.path(groupDir, "frontier.rds"))
  .recordStep(groupDir, group, "frontier", t0, metrics = stepMetrics)
  say("Saved ", file.path(groupDir, "frontier.rds"))
  invisible(out)
}
# nolint end
