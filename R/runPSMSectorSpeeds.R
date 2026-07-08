# nolint start
#' Four-sector ratcheting speeds (electricity / industry / buildings / transport)
#'
#' @description
#' The sector-disaggregated extension of the validated Bulk/Diffuse speed result
#' (2026-07-08). CAPMF natively separates electricity, industry, buildings and
#' transport, and the OECD's own sectoral analysis shows they advance at
#' different rates; this step estimates a validated error-correction adjustment
#' speed for each of the four, applying the Run-Group's deployed shared
#' specification to each sector outcome. For every sector it refits on the
#' training window, forecasts the held-out years recursively (the same protocol
#' as \code{\link{runPSMTemporalValidation}}'s ECM form) and reports the speed,
#' half-life and skill against the persistence benchmark.
#'
#' @inheritParams runPSMTemporalValidation
#' @param sectors Character vector of four-sector outcome names. Default
#'   \code{c("Electricity","Industry","Buildings","Transport")}.
#'
#' @return Invisibly, a list \code{list(spec, trainEnd, bySector = list(<sector>
#'   = list(metrics, rows)))}; also writes \code{<group>/sector-speeds.rds}.
#' @seealso \code{\link{runPSMTemporalValidation}}, ADR 0036
#' @export
#' @author Renato Rodrigues
runPSMSectorSpeeds <- function(group,
                               resultsDir = getOption("pfm.resultsDir", "output"),
                               modelDir = getOption("pfm.modelDir", "output"),
                               cachefolder = NULL, panelData = NULL,
                               y = 2000:2022, trainEnd = 2015,
                               sectors = c("Electricity", "Industry", "Buildings", "Transport"),
                               outputRegionMappingFile = "regionmapping_54.csv",
                               indexMax = 10, verbose = TRUE) {
  groupDir <- .resolveGroupDir(group, resultsDir, modelDir, cachefolder)
  say <- function(...) if (isTRUE(verbose)) message("[PSM-SECTORSPEED:", group, "] ", ...)
  t0 <- Sys.time()
  if (!requireNamespace("yaml", quietly = TRUE)) stop("The 'yaml' package is required.")

  selPath <- file.path(groupDir, "selected-models-psm.yml")
  if (!file.exists(selPath)) {
    .recordStep(groupDir, group, "sector-speeds", t0, status = "skipped",
                metrics = list(reason = "no selected-models-psm.yml (run runPSMSweep first)"))
    return(invisible(NULL))
  }
  sel <- yaml::read_yaml(selPath)
  norm <- function(s) {
    for (f in c("actorPowerDrivers", "actorPowerIndex", "instQualityDrivers", "controlDrivers"))
      if (!is.null(s[[f]])) s[[f]] <- unlist(s[[f]])
    s
  }
  # The deployed spec is a SHARED specification (same RHS both sectors); take any
  # entry as the driver template applied to all four sector outcomes.
  cfg <- norm(sel[[1]])

  # Four-sector historical panel.
  panel <- panelData
  if (is.null(panel)) {
    panel <- tryCatch(
      panelDataHistorical(aggregate = TRUE, y = y,
                          outputRegionMappingFile = outputRegionMappingFile,
                          includePolicyStringency = TRUE, psSectorResolution = "four"),
      error = function(e) { say("panel build FAILED: ", conditionMessage(e)); NULL }
    )
  }
  psVars <- paste0("Policy Stringency|", sectors)
  if (is.null(panel) ||
        (magclass::is.magpie(panel) && !any(psVars %in% magclass::getNames(panel)))) {
    .recordStep(groupDir, group, "sector-speeds", t0, status = "failed",
                metrics = list(reason = "four-sector panel lacks the sector outcomes ",
                               "(build with psSectorResolution = 'four')"))
    return(invisible(NULL))
  }
  sectors <- sectors[psVars %in% magclass::getNames(panel)]

  # GDP-Q derived control present as in the two-sector path.
  if (magclass::is.magpie(panel) && "GDP per Capita" %in% magclass::getNames(panel) &&
        !"GDP per Capita Sq" %in% magclass::getNames(panel)) {
    panel <- magclass::mbind(panel,
      magclass::setNames(panel[, , "GDP per Capita"]^2, "GDP per Capita Sq"))
  }

  yrs <- magclass::getYears(panel, as.integer = TRUE)
  trainPanel <- panel[, yrs[yrs <= trainEnd], ]

  out <- list(spec = cfg$name, trainEnd = trainEnd, bySector = list())
  stepMetrics <- list(trainEnd = trainEnd)
  for (sec in sectors) {
    psv <- paste0("Policy Stringency|", sec)
    histArr <- as.array(panel[, yrs[yrs <= trainEnd], psv])[, , 1]
    lastObs <- apply(histArr, 1, function(v) {
      f <- which(is.finite(v)); if (length(f)) v[max(f)] else NA_real_
    })
    ecm <- tryCatch(
      .psmValidateECMSector(cfg, sec, panel, trainPanel, trainEnd, indexMax, lastObs),
      error = function(e) { say(sec, " ECM failed: ", conditionMessage(e)); NULL }
    )
    if (is.null(ecm)) next
    out$bySector[[sec]] <- ecm
    m <- ecm$metrics
    stepMetrics[[paste0("speed", sec)]] <- round(m$adjustmentSpeed, 3)
    stepMetrics[[paste0("halfLife", sec)]] <- round(m$halfLife, 1)
    stepMetrics[[paste0("skill", sec)]] <- round(m$skillVsPersistence, 3)
    say(sec, ": speed=", round(m$adjustmentSpeed, 3), "/yr half-life=",
        round(m$halfLife, 1), "y skill=", round(m$skillVsPersistence, 2),
        " (n=", m$n, ")")
  }
  if (!length(out$bySector)) {
    .recordStep(groupDir, group, "sector-speeds", t0, status = "failed",
                metrics = list(reason = "no sector produced a validated speed"))
    return(invisible(NULL))
  }
  saveRDS(out, file.path(groupDir, "sector-speeds.rds"))
  .recordStep(groupDir, group, "sector-speeds", t0, metrics = stepMetrics)
  say("Saved ", file.path(groupDir, "sector-speeds.rds"))
  invisible(out)
}
# nolint end
