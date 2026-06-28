# nolint start
# Internal: content-addressed key for a per-(spec x sector x stage) bootstrap cache file (ADR 0034).
# Keyed on the FIT-DETERMINING config + sector + stage + panel fingerprint + seed; excludes
# nResamples (so a smaller cache is found and extended), detail (summary-only) and the pfm version
# (cleared by hand on code changes).
#' @keywords internal
.bootCacheKey <- function(cfg, sector, stage, panelHash, seed) {
  fitFields <- cfg[intersect(names(cfg), c(
    "actorPowerDrivers", "actorPowerIndex", "instQualityDrivers", "controlDrivers",
    "regionMappingFixedEffects", "useMundlak", "panelTransform", "includeLagged",
    "includeLaggedECP", "nickellCorrection", "logisticTimeTrend", "gdpGovInteraction",
    "ridgeInteractions", "interactRegionFE", "priceLink", "priceCeilingMax", "stringencyOnly"))]
  substr(digest::digest(list(fitFields, sector, stage, panelHash, seed), algo = "sha256"), 1, 16)
}

# Internal: crash-safe save (temp + rename) so an interrupted multi-hour run never leaves a
# half-written cache file.
#' @keywords internal
.bootAtomicSave <- function(obj, path) {
  tmp <- paste0(path, ".tmp", Sys.getpid())
  saveRDS(obj, tmp)
  file.rename(tmp, path)
}

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
  panelHash <- substr(digest::digest(panel, algo = "sha256"), 1, 16)
  cacheDir <- if (!is.null(modelDir)) file.path(modelDir, "boot-cache") else NULL
  if (!is.null(cacheDir)) dir.create(cacheDir, showWarnings = FALSE, recursive = TRUE)
  # Deterministic region draws (seed + the panel-pinned region list): draw r is identical every run
  # (the fits use no RNG), so cached per-spec resample rows stay valid and extend cleanly (ADR 0034).
  set.seed(seed)
  draws <- lapply(seq_len(nResamples), function(i) sample(regions, length(regions), replace = TRUE))
  validCols <- c("resample", mmCols)

  out <- list()
  for (stg in intersect(c("Adoption", "Stringency"), unique(sweep$results$stage))) {
    mm <- sweep$maximin[[stg]]
    pool <- utils::head(mm$model[mm$gatePass %in% TRUE], topK)
    pool <- pool[!is.na(pool) & pool %in% names(specByName)]
    if (length(pool) < 2) { say("stage ", stg, ": pool < 2, skipping"); next }
    say("stage ", stg, ": ", length(pool), " candidate(s) x ", nResamples, " resamples ...")

    # Full-fit $data computed lazily and only for specs that still need (re)computation.
    fullData <- new.env(parent = emptyenv())
    getFullData <- function(mdl, sec) {
      k <- paste(mdl, sec)
      if (!is.null(fullData[[k]])) return(fullData[[k]])
      f <- .fitSpecModel(specByName[[mdl]], sec, stg, panel, family = family,
                         modelDir = modelDir, forceRefit = TRUE, verbose = FALSE)
      d <- if (!inherits(f, "fitError") && !is.null(f$data) && "region" %in% names(f$data)) f$data else NA
      fullData[[k]] <- d; d
    }

    # Per (spec, sector): reuse cached resample rows; compute/extend only what's missing (ADR 0034).
    specRows <- list(); nHit <- 0L; nNew <- 0L; nExt <- 0L
    done <- 0L; total <- length(pool) * length(sectors)
    progEvery <- max(1L, total %/% 20L)
    if (isTRUE(verbose)) { say("stage ", stg, ": spec-sector 0/", total, " (0%)"); utils::flush.console() }
    for (mdl in pool) for (sec in sectors) {
      done <- done + 1L
      # Progress line parsed by pfm::runStatus for the live model-bootstrap bar (the refactor replaced
      # the old per-resample counter with this per-spec one; ADR 0034).
      if (isTRUE(verbose) && (done %% progEvery == 0 || done == total)) {
        say("stage ", stg, ": spec-sector ", done, "/", total, " (", round(100 * done / total), "%)")
        utils::flush.console()
      }
      key <- .bootCacheKey(specByName[[mdl]], sec, stg, panelHash, seed)
      cf <- if (!is.null(cacheDir)) file.path(cacheDir, paste0("boot_", stg, "_", sec, "_", key, ".rds")) else NULL
      cached <- if (!is.null(cf) && file.exists(cf)) tryCatch(readRDS(cf), error = function(e) NULL) else NULL
      if (!(is.data.frame(cached) && all(validCols %in% names(cached)))) cached <- NULL  # stale schema -> drop
      nHave <- if (is.null(cached)) 0L else nrow(cached)
      if (nHave >= nResamples) {                       # full hit / truncate: no fitting
        specRows[[paste(mdl, sec)]] <- cached[cached$resample <= nResamples, , drop = FALSE]
        nHit <- nHit + 1L
        next
      }
      d <- getFullData(mdl, sec)
      if (!is.data.frame(d)) next                      # full fit failed -> skip this spec
      newRows <- lapply(seq.int(nHave + 1L, nResamples), function(r) {
        bootDf <- do.call(rbind, lapply(draws[[r]], function(rg) d[d$region == rg, , drop = FALSE]))
        if (is.null(bootDf) || !nrow(bootDf)) return(NULL)
        if ("regionFE" %in% names(bootDf)) bootDf$regionFE <- droplevels(factor(bootDf$regionFE))
        rf <- .fitSpecModel(specByName[[mdl]], sec, stg, bootDf, family = family,
                            modelDir = modelDir, verbose = FALSE, prepared = TRUE)
        if (inherits(rf, "fitError") || is.null(rf$model)) return(NULL)
        m <- tryCatch(.channelFitMetrics(rf, specByName[[mdl]], sec, stg), error = function(e) NULL)
        if (!is.null(m)) m$resample <- r
        m
      })
      newDf <- do.call(rbind, Filter(Negate(is.null), newRows))
      allDf <- if (is.null(cached)) newDf else if (is.null(newDf)) cached else {
        common <- intersect(names(cached), names(newDf))   # tolerate an evolved diagnostic schema
        rbind(cached[, common, drop = FALSE], newDf[, common, drop = FALSE])
      }
      if (is.null(allDf) || !nrow(allDf)) next
      if (!is.null(cf)) tryCatch(.bootAtomicSave(allDf, cf), error = function(e) NULL)
      specRows[[paste(mdl, sec)]] <- allDf[allDf$resample <= nResamples, , drop = FALSE]
      if (nHave > 0L) nExt <- nExt + 1L else nNew <- nNew + 1L
    }
    say("stage ", stg, ": cache ", nHit, " hit / ", nNew, " new / ", nExt, " extended (of ",
        length(pool) * length(sectors), " spec-sectors)")

    # Rank per resample from the assembled rows -> winning channel set.
    winCh <- character(0); winSpec <- character(0)
    for (r in seq_len(nResamples)) {
      rows <- lapply(specRows, function(df) { rr <- df[df$resample == r, , drop = FALSE]
        if (nrow(rr)) rr else NULL })
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
