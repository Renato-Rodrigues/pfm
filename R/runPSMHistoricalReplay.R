# nolint start
#' Historical-replay gate for the feasibility coupling (TODO 2.3)
#'
#' @description
#' The cheapest possible falsification of the whole coupling, and the one the paper
#' already promises: run the coupling machinery \emph{backwards} over the historical
#' window and ask whether it reproduces what actually happened.
#'
#' Concretely, the feasible-path recursion (\code{\link{projectFeasiblePath}}) is
#' seeded at \code{seedYear} and integrated forward over the observed drivers to
#' the end of the sample, then scored against the observed stringency index. Three
#' benchmarks are reported on the same rows:
#' \describe{
#'   \item{coupled}{the feasible path - what the coupling would have said.}
#'   \item{ecm}{the uncoupled ECM forecast (the same recursion without the frontier
#'     ceiling). This is the benchmark that matters: it isolates what the
#'     \emph{coupling} adds over the dynamics alone.}
#'   \item{persistence}{"assume nothing changes" - the seed value carried forward,
#'     the benchmark the level projections lost to (ADR 0036).}
#' }
#'
#' \strong{The gate.} \code{pass = RMSE(coupled) <= RMSE(ecm)} (within
#' \code{tolerance}). A coupling that cannot reproduce the past at least as well as
#' the dynamics alone is adding noise, not information, and must be reported as
#' such - or run one-way.
#'
#' This is deliberately a \emph{weak} test to pass and a decisive one to fail: the
#' ceiling can only bind downward, so a coupling that fires spuriously shows up
#' immediately as a worse RMSE.
#'
#' @param group Run-Group name.
#' @param resultsDir,modelDir,cachefolder Run-Group / Fit-Cache roots.
#' @param panelData Historical panel (magpie) carrying the PSM outcomes, or
#'   \code{NULL} to build/load it.
#' @param frontier Optional: the object saved by \code{\link{runPSMFrontier}}, a
#'   per-sector list of frontier coefficient tables, or \code{NULL} to read
#'   \code{frontier.rds} from the Run-Group. Without it the ceiling cannot bind and
#'   the gate degenerates to an identity check (reported, not silently passed).
#' @param seedYear Integer or \code{NULL}. Year the replay is seeded from
#'   (\code{NULL} = the first year with a complete observation).
#' @param y Years of the historical panel.
#' @param outputRegionMappingFile Regional resolution of the panel. Must match the
#'   resolution the deployed model was estimated at.
#' @param indexMax Index ceiling. Default \code{10}.
#' @param tolerance Numeric. Slack allowed on the gate, as a fraction of the ECM
#'   RMSE. Default \code{0} (strict).
#' @param verbose Logical.
#'
#' @return Invisibly, a list \code{list(spec, seedYear, pass, bySector)}; also
#'   writes \code{<group>/historical-replay.rds}. Per sector: \code{metrics}
#'   (\code{n, rmseCoupled, rmseEcm, rmsePersistence, skillVsEcm,
#'   skillVsPersistence, ceilingBindShare, pass}) and the scored \code{rows}.
#'
#' @seealso \code{\link{projectFeasiblePath}}, \code{\link{runPSMTemporalValidation}},
#'   ADR 0040/0041.
#' @export
#' @author Renato Rodrigues
runPSMHistoricalReplay <- function(group,
                                   resultsDir = getOption("pfm.resultsDir", "output"),
                                   modelDir = getOption("pfm.modelDir", "output"),
                                   cachefolder = NULL, panelData = NULL,
                                   frontier = NULL, seedYear = NULL,
                                   y = 2000:2022,
                                   outputRegionMappingFile = "regionmapping_54.csv",
                                   indexMax = 10, tolerance = 0, verbose = TRUE) {
  groupDir <- .resolveGroupDir(group, resultsDir, modelDir, cachefolder)
  say <- function(...) if (isTRUE(verbose)) message("[PSM-REPLAY:", group, "] ", ...)
  t0 <- Sys.time()
  if (!requireNamespace("yaml", quietly = TRUE)) stop("The 'yaml' package is required.")
  sectors <- c("Bulk", "Diffuse")

  selPath <- file.path(groupDir, "selected-models-psm.yml")
  if (!file.exists(selPath)) {
    .recordStep(groupDir, group, "historical-replay", t0, status = "skipped",
                metrics = list(reason = "no selected-models-psm.yml (run runPSMSweep first)"))
    return(invisible(NULL))
  }
  sel <- yaml::read_yaml(selPath)

  panel <- panelData
  if (is.null(panel)) {
    p <- file.path(groupDir, "data", "panelDataHistorical.rds")
    if (file.exists(p)) panel <- tryCatch(readRDS(p), error = function(e) NULL)
    if (is.list(panel) && !is.null(panel$data)) panel <- panel$data
  }
  if (is.null(panel)) {
    panel <- tryCatch(
      .psmHistPanel(groupDir, y = y, outputRegionMappingFile = outputRegionMappingFile, verbose = verbose),
      error = function(e) NULL)
  }
  if (is.null(panel)) {
    .recordStep(groupDir, group, "historical-replay", t0, status = "failed",
                metrics = list(reason = "historical panel unavailable"))
    return(invisible(NULL))
  }

  if (is.null(frontier)) {
    fp <- file.path(groupDir, "frontier.rds")
    if (file.exists(fp)) frontier <- tryCatch(readRDS(fp), error = function(e) NULL)
  }
  frontierBetaOf <- function(sec) {
    ct <- tryCatch(frontier$bySector[[sec]]$coefTable, error = function(e) NULL)
    if (is.null(ct)) ct <- tryCatch(frontier[[sec]]$coefTable, error = function(e) NULL)
    if (is.null(ct)) ct <- tryCatch(frontier[[sec]], error = function(e) NULL)
    if (is.null(ct) || !is.data.frame(ct)) return(NULL)
    b <- stats::setNames(ct$estimate, ct$term)
    b[!names(b) %in% c("sigmaSq", "gamma")]
  }

  norm <- function(s) {
    for (f in c("actorPowerDrivers", "actorPowerIndex", "instQualityDrivers", "controlDrivers"))
      if (!is.null(s[[f]])) s[[f]] <- unlist(s[[f]])
    s$panelTransform <- s$panelTransform %||% "levels"
    s
  }
  # Seed on the earliest year the outcome is actually OBSERVED for most regions.
  # The panel's first year is not usable: preparePanelData lags the drivers, so the
  # first outcome year is NA by construction and every path would be unseeded.
  yrs <- sort(magclass::getYears(panel, as.integer = TRUE))
  sy <- seedYear %||% .psmFirstObservedYear(panel, sectors[1])
  if (!is.finite(sy)) {
    .recordStep(groupDir, group, "historical-replay", t0, status = "failed",
                metrics = list(reason = "no year has an observed outcome to seed from"))
    return(invisible(NULL))
  }
  out <- list(spec = NULL, seedYear = sy, bySector = list())
  stepMetrics <- list()

  for (sec in sectors) {
    hit <- Filter(function(x) identical(x$model_type, paste0("PolicyStringency: ", sec)), sel)
    if (!length(hit)) next
    cfg <- norm(hit[[1]])
    out$spec <- cfg$name %||% out$spec

    res <- tryCatch({
      # The replay is a projection onto the panel's OWN drivers: same machinery,
      # observed inputs. projectFeasiblePath drops rows at or before the seed year,
      # so the scored window is seedYear+1 .. last observed year.
      fb <- frontierBetaOf(sec)
      coupled <- projectFeasiblePath(cfg, sec, histData = panel, scenarioData = panel,
                                     rule = "speed-limited", frontierBeta = fb,
                                     seedYear = sy, indexMax = indexMax,
                                     modelDir = modelDir, verbose = FALSE)
      uncoupled <- projectFeasiblePath(cfg, sec, histData = panel, scenarioData = panel,
                                       rule = "speed-limited", frontierBeta = NULL,
                                       seedYear = sy, indexMax = indexMax,
                                       modelDir = modelDir, verbose = FALSE)
      obs <- .psmObservedIndex(panel, sec)
      rows <- merge(coupled[, c("region", "year", "feasibleIndex", "seedIndex")],
                    uncoupled[, c("region", "year", "feasibleIndex")],
                    by = c("region", "year"), suffixes = c(".coupled", ".ecm"))
      rows <- merge(rows, obs, by = c("region", "year"))
      rows <- rows[is.finite(rows$observed) & is.finite(rows$feasibleIndex.coupled) &
                     is.finite(rows$feasibleIndex.ecm), , drop = FALSE]
      if (!nrow(rows)) stop("no scored rows")
      rmse <- function(e) sqrt(mean(e^2, na.rm = TRUE))
      rC <- rmse(rows$observed - rows$feasibleIndex.coupled)
      rE <- rmse(rows$observed - rows$feasibleIndex.ecm)
      rP <- rmse(rows$observed - rows$seedIndex)
      list(metrics = list(
        n = nrow(rows), rmseCoupled = rC, rmseEcm = rE, rmsePersistence = rP,
        skillVsEcm = 1 - rC / rE, skillVsPersistence = 1 - rC / rP,
        ceilingBindShare = attr(coupled, "ceilingBindShare"),
        ceilingAvailable = !is.null(fb),
        pass = rC <= rE * (1 + tolerance)
      ), rows = rows)
    }, error = function(e) {
      say("  ", sec, " replay failed: ", conditionMessage(e))
      NULL
    })
    if (is.null(res)) next
    out$bySector[[sec]] <- res
    m <- res$metrics
    stepMetrics[[paste0("replayPass", sec)]] <- m$pass
    stepMetrics[[paste0("replaySkillVsEcm", sec)]] <- round(m$skillVsEcm, 3)
    say("  ", sec, ": n=", m$n, " RMSE coupled=", round(m$rmseCoupled, 3),
        " vs ECM=", round(m$rmseEcm, 3), " vs persistence=", round(m$rmsePersistence, 3),
        " | skill-vs-ECM=", round(m$skillVsEcm, 3),
        " | ceiling binds ", round(100 * (m$ceilingBindShare %||% 0)), "%",
        " | GATE ", if (isTRUE(m$pass)) "PASS" else "FAIL")
  }

  if (!length(out$bySector)) {
    .recordStep(groupDir, group, "historical-replay", t0, status = "failed",
                metrics = list(reason = "no sector produced a replay"))
    return(invisible(NULL))
  }
  out$pass <- all(vapply(out$bySector, function(z) isTRUE(z$metrics$pass), logical(1)))
  stepMetrics$replayPass <- out$pass
  saveRDS(out, file.path(groupDir, "historical-replay.rds"))
  .recordStep(groupDir, group, "historical-replay", t0, metrics = stepMetrics)
  say("Saved ", file.path(groupDir, "historical-replay.rds"),
      " | OVERALL GATE ", if (isTRUE(out$pass)) "PASS" else "FAIL")
  invisible(out)
}

# Earliest year in which the outcome is observed for at least `minShare` of regions.
#' @keywords internal
.psmFirstObservedYear <- function(panel, sector, outcomeVar = "Policy Stringency",
                                  minShare = 0.5) {
  x <- panel[, , paste0(outcomeVar, "|", sector)]
  yrs <- magclass::getYears(x, as.integer = TRUE)
  arr <- as.array(x)
  n <- vapply(seq_along(yrs), function(i) sum(is.finite(arr[, i, 1])), numeric(1))
  # Relative to the BEST-covered year, not to the region count: a country-resolution
  # panel carries all 249 countries while only ~49 have a CAPMF observation, so a
  # threshold on the region count would never be met and the gate would never seed.
  if (max(n) == 0) return(NA_integer_)
  ok <- which(n >= minShare * max(n))
  if (!length(ok)) return(NA_integer_)
  yrs[min(ok)]
}

# Observed natural-scale index as a long data.frame (region, year, observed).
#' @keywords internal
.psmObservedIndex <- function(panel, sector, outcomeVar = "Policy Stringency") {
  v <- paste0(outcomeVar, "|", sector)
  x <- panel[, , v]
  yrs <- magclass::getYears(x, as.integer = TRUE)
  regs <- magclass::getItems(x, dim = 1)
  data.frame(
    region = rep(regs, times = length(yrs)),
    year = rep(yrs, each = length(regs)),
    observed = as.numeric(x),
    stringsAsFactors = FALSE
  )
}
# nolint end
