# nolint start
#' Selection-uncertainty bootstrap (ADR 0025)
#'
#' Region-block (cluster) bootstrap of the Maximin selection. For each resample the 54 regions are
#' drawn with replacement, the gate-passing candidate pool is refit on the resampled design (via
#' the \code{prepared = TRUE} entry — duplicated region rows just add weight to their block, no FE
#' relabel needed), re-ranked by \code{\link{computeMaximinScore}}, and the winner's \emph{channel
#' set} (and, with \code{detail = "full"}, the exact spec) is recorded. Quantifies how stable the
#' deliverable's channel choice is to the specification search. Writes
#' \code{selection-bootstrap.rds} to the Run-Group. Heavy and cluster-oriented; the default
#' \code{detail = "channel"} reports channel-set stability (the scientific claim), which is far
#' more stable than the exact winning row.
#'
#' @param group Character. Run-Group name.
#' @param resultsDir,modelDir Results Root / Fit Cache. Defaults from options.
#' @param cachefolder Character or NULL. madrat data cache (set in-session when supplied).
#' @param y,outputRegionMappingFile Panel build args (\code{\link{panelDataHistorical}}).
#' @param nResamples Integer. Number of bootstrap resamples. Default \code{200}.
#' @param detail \code{"channel"} (default — channel-set + per-channel frequency) or \code{"full"}
#'   (also exact-spec selection frequency, for the publication run).
#' @param topK Integer. Size of the candidate pool per stage (top gate-passing maximin specs).
#'   Default \code{40}.
#' @param family GLM family for stringency. Default \code{"gaussian"}.
#' @param seed Integer. RNG seed. Default \code{1}.
#' @param verbose Logical. Default \code{TRUE}.
#' @return Invisibly, the bootstrap summary list (also saved as \code{selection-bootstrap.rds}).
#' @seealso \code{\link{computeMaximinScore}}, ADR 0025
#' @export
#' @author Renato Rodrigues
runSelectionBootstrap <- function(group, resultsDir = getOption("pfm.resultsDir", "output"),
                                  modelDir = getOption("pfm.modelDir", "output"),
                                  cachefolder = NULL, y = 2000:2022,
                                  outputRegionMappingFile = "regionmapping_54.csv",
                                  nResamples = 200L, detail = c("channel", "full"),
                                  topK = 40L, family = "gaussian", seed = 1L, verbose = TRUE) {
  detail <- match.arg(detail)
  groupDir <- .resolveGroupDir(group, resultsDir, modelDir, cachefolder)
  say <- function(...) if (isTRUE(verbose)) message("[boot:", group, "] ", ...)
  t0 <- Sys.time()
  if (!requireNamespace("yaml", quietly = TRUE)) stop("The 'yaml' package is required.")
  sectors <- c("Bulk", "Diffuse")

  sweep <- readRDS(file.path(groupDir, "sweep.rds"))
  panel <- .buildHistPanel(y, outputRegionMappingFile)
  cfgFile <- list.files(groupDir, pattern = "^channels-.*\\.yml$", full.names = TRUE)
  if (length(cfgFile) == 0) stop("No sweep config (channels-*.yml) in ", groupDir, call. = FALSE)
  specs <- yaml::read_yaml(cfgFile[[1]])
  norm <- function(s) { for (f in c("actorPowerDrivers", "actorPowerIndex", "instQualityDrivers",
                                    "controlDrivers")) if (!is.null(s[[f]])) s[[f]] <- unlist(s[[f]])
    s$panelTransform <- s$panelTransform %||% "levels"; s }
  specByName <- stats::setNames(lapply(specs, norm), vapply(specs, function(s) s$name, character(1)))
  chOf <- function(m) vapply(strsplit(as.character(m), " ", fixed = TRUE),
                             function(x) if (length(x) >= 2) x[[2]] else "", character(1))
  mmCols <- c("model", "sector", "sigActorPower", "sigInstQual", "sigInteractions",
              "deltaR2Theory", "pseudoR2", "bic", "maxVIF", "converged", "usesLagged",
              "nFE", "nObs", "sigControl", "nControl", "trendShare")
  regions <- magclass::getRegions(panel)

  out <- list()
  for (stg in intersect(c("Adoption", "Stringency"), unique(sweep$results$stage))) {
    mm <- sweep$maximin[[stg]]
    pool <- utils::head(mm$model[mm$gatePass %in% TRUE], topK)
    pool <- pool[!is.na(pool) & pool %in% names(specByName)]
    if (length(pool) < 2) { say("stage ", stg, ": pool < 2, skipping"); next }
    say("stage ", stg, ": ", length(pool), " candidate(s) x ", nResamples, " resamples ...")

    # Full fit per (model, sector) once (fresh, so $data is the prepared df WITH `region`) ->
    # reuse $data for the resample refits.
    fullData <- list()
    for (mdl in pool) for (sec in sectors) {
      f <- .fitSpecModel(specByName[[mdl]], sec, stg, panel, family = family,
                         modelDir = modelDir, forceRefit = TRUE, verbose = FALSE)
      if (!inherits(f, "fitError") && !is.null(f$data) && "region" %in% names(f$data)) {
        fullData[[paste(mdl, sec)]] <- f$data
      }
    }

    set.seed(seed)
    winCh <- character(0); winSpec <- character(0)
    progEvery <- max(1L, nResamples %/% 20L)   # ~5% steps
    if (isTRUE(verbose)) {
      say("stage ", stg, ": resample 0/", nResamples, " (0%)")
      utils::flush.console()
    }
    for (r in seq_len(nResamples)) {
      if (isTRUE(verbose) && (r %% progEvery == 0 || r == nResamples)) {
        say("stage ", stg, ": resample ", r, "/", nResamples, " (", round(100 * r / nResamples),
            "%)")
        utils::flush.console()
      }
      draw <- sample(regions, length(regions), replace = TRUE)
      rows <- list()
      for (mdl in pool) for (sec in sectors) {
        d <- fullData[[paste(mdl, sec)]]; if (is.null(d)) next
        bootDf <- do.call(rbind, lapply(draw, function(rg) d[d$region == rg, , drop = FALSE]))
        if (is.null(bootDf) || !nrow(bootDf)) next
        if ("regionFE" %in% names(bootDf)) bootDf$regionFE <- droplevels(factor(bootDf$regionFE))
        rf <- .fitSpecModel(specByName[[mdl]], sec, stg, bootDf, family = family,
                            modelDir = modelDir, verbose = FALSE, prepared = TRUE)
        if (inherits(rf, "fitError") || is.null(rf$model)) next
        rows[[length(rows) + 1L]] <- tryCatch(.channelFitMetrics(rf, specByName[[mdl]], sec, stg),
                                              error = function(e) NULL)
      }
      rows <- Filter(Negate(is.null), rows)
      if (!length(rows)) next
      tab <- do.call(rbind, rows)
      mmr <- tryCatch(computeMaximinScore(tab[, intersect(mmCols, names(tab)), drop = FALSE]),
                      error = function(e) NULL)
      if (is.null(mmr)) next
      top <- mmr[mmr$gatePass %in% TRUE, , drop = FALSE]
      if (!nrow(top)) next
      winCh <- c(winCh, chOf(top$model[1])); winSpec <- c(winSpec, top$model[1])
    }

    if (!length(winCh)) { say("stage ", stg, ": no successful resample (refits failed); skipping"); next }
    chTab <- sort(table(winCh) / length(winCh), decreasing = TRUE)
    # per-channel-token frequency (split the winning channel set on "|")
    toks <- unlist(strsplit(winCh, "|", fixed = TRUE))
    tokTab <- if (length(toks)) sort(table(toks) / length(winCh), decreasing = TRUE) else NULL
    out[[stg]] <- list(
      nEffective = length(winCh),
      channelSetFreq = chTab,
      channelTokenFreq = tokTab,
      specFreq = if (detail == "full") sort(table(winSpec) / length(winSpec), decreasing = TRUE) else NULL,
      poolSize = length(pool))
    say("stage ", stg, ": top channel set ", names(chTab)[1], " in ",
        round(100 * chTab[[1]]), "% of ", length(winCh), " resamples")
  }

  res <- list(stages = out, nResamples = nResamples, detail = detail, topK = topK,
              specNames = stats::setNames(
                vapply(c("Adoption", "Stringency"), function(s) {
                  m <- sweep$maximin[[s]]; if (is.null(m)) NA_character_ else
                    m$model[m$gatePass %in% TRUE][1] }, character(1)),
                c("Adoption", "Stringency")),
              generated = Sys.time())
  saveRDS(res, file.path(groupDir, "selection-bootstrap.rds"))
  .recordStep(groupDir, group, "selection-bootstrap", t0, metrics = list(
    nResamples = nResamples, detail = detail, topK = topK))
  say("done -> ", file.path(groupDir, "selection-bootstrap.rds"))
  invisible(res)
}
# nolint end
