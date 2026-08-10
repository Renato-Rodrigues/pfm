# nolint start
# Internal: content-addressed key for a per-(spec x sector) PSM bootstrap cache file
# (the ADR 0034 pattern, PSM variant). Keyed on the PSM fit-determining config fields +
# sector + panel fingerprint + seed; excludes nResamples (so a smaller cache is found
# and extended) and the pfm version (cleared by hand on code changes). The "psm" tag
# and the includeLaggedPS/indexMax fields keep the keys disjoint from the price-model
# bootstrap cache in the same boot-cache/ folder.
#' @keywords internal
.psmBootCacheKey <- function(cfg, sector, panelHash, seed) {
  fitFields <- cfg[intersect(names(cfg), c(
    "actorPowerDrivers", "actorPowerIndex", "instQualityDrivers", "controlDrivers",
    "regionMappingFixedEffects", "useMundlak", "includeLaggedPS", "logisticTimeTrend",
    "gdpGovInteraction", "interactRegionFE", "indexMax"))]
  substr(digest::digest(list("psm", fitFields, sector, panelHash, seed), algo = "sha256"), 1, 16)
}

#' Selection-uncertainty bootstrap for the Policy Stringency Model (Tournament v2)
#'
#' @description
#' The PSM counterpart of \code{\link{runSelectionBootstrap}} (ADR 0025/0034), run
#' under the Tournament v2 selection semantics (ADR 0039). For each resample the
#' panel's outcome-supported regions (countries carrying the CAPMF index at the
#' deployed resolution — the coverage set, not the full 249-country map) are drawn
#' with replacement, the gate-passing candidate pool is refit on the resampled design
#' (via the \code{prepared = TRUE} entry of
#' \code{\link{estimatePolicyStringencyModel}} — duplicated region rows add weight
#' to their block, no FE relabel needed), and the pool is re-ranked by
#' \code{\link{computeMaximinScore}} with the same knobs the sweep used
#' (worse-sector deltaR2 ranking, Green tier gate, trend-dominance gate 0.9,
#' inference-fragility preference). Records, per resample: the winning spec, its
#' channel set and actor-power form, and the DEPLOYED spec's gate-pass status and
#' rank — the publication exhibit for how stable the specification search is.
#'
#' Two winners are recorded per resample. The \emph{unconditional} winner is the
#' top gate-passer of the resampled maximin ranking. The \emph{sanity-conditional}
#' winner additionally excludes the specs that the ORIGINAL run's Projection
#' Sanity walk rejected (e.g. the scenario-blind composite-AP specs, ADR 0039) —
#' the responsiveness verdict is a property of the spec's projection under fixed
#' scenario panels, not of the estimation sample, so it is applied as a fixed
#' filter rather than recomputed per resample. Pool members the original walk
#' never reached carry no verdict and are retained in both rankings.
#'
#' Writes \code{selection-bootstrap.rds} to the Run-Group. Resamples are cached
#' per (spec x sector) under \code{<modelDir>/boot-cache/} (crash-safe, extended
#' on larger \code{nResamples}; the draws are deterministic in \code{seed} + the
#' panel's region list, so cached rows stay valid across runs).
#'
#' @param group Character. PSM Run-Group name (must contain \code{sweep.rds} from
#'   \code{\link{runPSMSweep}}).
#' @param resultsDir,modelDir Results Root / Fit Cache. Defaults from options.
#' @param cachefolder Character or NULL. madrat data cache (set in-session when supplied).
#' @param panelData Optional pre-built historical panel (magpie), as trained. When
#'   NULL it is resolved in order: \code{<group>/data/panelDataHistorical.rds},
#'   the manifest's \code{panel_hash} via \code{\link{loadTrainingPanel}} (the
#'   sweep's exact Training Panel — the offline path), then a fresh
#'   \code{\link{panelDataHistorical}} build.
#' @param y,outputRegionMappingFile Panel build args (only used for the fresh-build
#'   fallback).
#' @param nResamples Integer. Number of bootstrap resamples. Default \code{200}.
#' @param topK Integer. Size of the candidate pool (top gate-passing maximin specs;
#'   the deployed spec is always included). Default \code{40}.
#' @param seed Integer. RNG seed for the region draws. Default \code{1}.
#' @param nearTieEps,feParsimonyWeight,dropIdleControls,softVifGate,trendDominanceGate,deltaR2Max,inferenceTGate,rankBy,tierGate
#'   Maximin knobs forwarded to \code{\link{computeMaximinScore}}; defaults mirror
#'   \code{\link{runPSMSweep}} — pass the same overrides the sweep ran with.
#' @param verbose Logical. Default \code{TRUE}.
#' @return Invisibly, the bootstrap summary list (also saved as
#'   \code{selection-bootstrap.rds}), or \code{NULL} when skipped.
#' @seealso \code{\link{runSelectionBootstrap}}, \code{\link{computeMaximinScore}},
#'   \code{\link{runPSMSweep}}, ADR 0025, ADR 0039
#' @export
#' @author Renato Rodrigues
runPSMSelectionBootstrap <- function(group,
                                     resultsDir = getOption("pfm.resultsDir", "output"),
                                     modelDir = getOption("pfm.modelDir", "output"),
                                     cachefolder = NULL,
                                     panelData = NULL, y = 2000:2022,
                                     outputRegionMappingFile = "regionmapping_54.csv",
                                     nResamples = 200L, topK = 40L, seed = 1L,
                                     nearTieEps = 0.025, feParsimonyWeight = 0,
                                     dropIdleControls = TRUE, softVifGate = 6,
                                     trendDominanceGate = 0.9, deltaR2Max = 1,
                                     inferenceTGate = 2.33,
                                     rankBy = "worseDeltaR2", tierGate = "Green",
                                     verbose = TRUE) {
  groupDir <- .resolveGroupDir(group, resultsDir, modelDir, cachefolder)
  say <- function(...) if (isTRUE(verbose)) message("[psm-boot:", group, "] ", ...)
  t0 <- Sys.time()
  sectors <- c("Bulk", "Diffuse")
  stg <- "PolicyStringency"

  sweepPath <- file.path(groupDir, "sweep.rds")
  if (!file.exists(sweepPath)) {
    .recordStep(groupDir, group, "selection-bootstrap", t0, status = "skipped",
                metrics = list(reason = "no sweep.rds (run runPSMSweep first)"))
    return(invisible(NULL))
  }
  sweep <- readRDS(sweepPath)
  mm <- sweep$maximin[[stg]]
  if (is.null(mm)) {
    .recordStep(groupDir, group, "selection-bootstrap", t0, status = "skipped",
                metrics = list(reason = "sweep.rds has no PolicyStringency maximin (not a PSM group)"))
    return(invisible(NULL))
  }

  # ── Specs: reuse the sweep's normalised, psmSpecs-adapted grid ────────────────
  specs <- sweep$specs
  if (is.null(specs)) {
    if (!requireNamespace("yaml", quietly = TRUE)) stop("The 'yaml' package is required.")
    cfgFile <- list.files(groupDir, pattern = "^channels-.*\\.yml$", full.names = TRUE)
    if (length(cfgFile) == 0) stop("No specs in sweep.rds and no channels-*.yml in ",
                                   groupDir, call. = FALSE)
    specs <- psmSpecs(yaml::read_yaml(cfgFile[[1]]), verbose = FALSE)
  }
  specByName <- stats::setNames(specs, vapply(specs, `[[`, character(1), "name"))

  # ── Candidate pool: top gate-passers + always the deployed spec ──────────────
  deployed <- sweep$selected[[stg]] %||% NA_character_
  pool <- utils::head(mm$model[mm$gatePass %in% TRUE], topK)
  if (!is.na(deployed) && !(deployed %in% pool)) pool <- c(pool, deployed)
  pool <- pool[!is.na(pool) & pool %in% names(specByName)]
  if (length(pool) < 2) {
    .recordStep(groupDir, group, "selection-bootstrap", t0, status = "skipped",
                metrics = list(reason = paste0("candidate pool < 2 (", length(pool),
                                               " gate-passing spec(s))")))
    return(invisible(NULL))
  }

  # Fixed sanity filter from the ORIGINAL walk (deterministic per spec): walked and
  # not passed (severe flags, incl. scenarioBlind, or not evaluable) => excluded
  # from the conditional ranking. Unwalked specs carry no verdict.
  trace <- sweep$sanity[[stg]]$trace
  sanityRejected <- if (is.data.frame(trace) && all(c("model", "pass") %in% names(trace))) {
    unique(trace$model[!(trace$pass %in% TRUE)])
  } else character(0)

  # ── Historical panel, as trained ──────────────────────────────────────────────
  panel <- panelData
  if (is.null(panel)) {
    p <- file.path(groupDir, "data", "panelDataHistorical.rds")
    if (file.exists(p)) panel <- tryCatch(readRDS(p), error = function(e) NULL)
    if (is.list(panel) && !is.null(panel$data)) panel <- panel$data
  }
  if (is.null(panel)) {
    hash <- tryCatch(jsonlite::read_json(file.path(groupDir, "manifest.json"))$panel_hash,
                     error = function(e) NULL)
    if (!is.null(hash)) {
      panel <- loadTrainingPanel(hash, modelDir)
      if (!is.null(panel)) say("Training Panel loaded from the Fit Cache (hash ", hash, ").")
    }
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
    .recordStep(groupDir, group, "selection-bootstrap", t0, status = "failed",
                metrics = list(reason = "historical panel unavailable"))
    return(invisible(NULL))
  }
  if (magclass::is.magpie(panel) &&
        "GDP per Capita" %in% magclass::getNames(panel) &&
        !"GDP per Capita Sq" %in% magclass::getNames(panel)) {
    panel <- magclass::mbind(
      panel, magclass::setNames(panel[, , "GDP per Capita"]^2, "GDP per Capita Sq")
    )
  }

  chOf <- function(m) vapply(strsplit(as.character(m), " ", fixed = TRUE),
                             function(x) if (length(x) >= 2) x[[2]] else "", character(1))
  apOf <- function(m) vapply(strsplit(as.character(m), " ", fixed = TRUE),
                             function(x) if (length(x) >= 3) x[[3]] else "", character(1))
  mmCols <- c("model", "sector", "sigActorPower", "sigInstQual", "sigInteractions",
              "deltaR2Theory", "pseudoR2", "bic", "maxVIF", "converged", "usesLagged",
              "nFE", "nObs", "sigControl", "nControl", "trendShare", "minSigTheoryT")
  # Draw universe = outcome-supported regions only. The country panel maps ~249
  # ISO3 regions but only the CAPMF coverage set (~46) carries the outcome; drawing
  # from the full map would leave each resample's effective block count binomially
  # random (needless variance in effective sample size) without changing its mean.
  # Deterministic given the panel, so the cache key (panelHash + seed) still pins it.
  regions <- magclass::getRegions(panel)
  psVars <- intersect(paste0("Policy Stringency|", sectors), magclass::getNames(panel))
  if (magclass::is.magpie(panel) && length(psVars)) {
    hasOutcome <- vapply(regions, function(rg) {
      any(is.finite(as.vector(panel[rg, , psVars])))
    }, logical(1))
    if (any(hasOutcome)) regions <- regions[hasOutcome]
  }
  panelHash <- substr(digest::digest(panel, algo = "sha256"), 1, 16)
  cacheDir <- if (!is.null(modelDir)) file.path(modelDir, "boot-cache") else NULL
  if (!is.null(cacheDir)) dir.create(cacheDir, showWarnings = FALSE, recursive = TRUE)
  # Deterministic region draws (seed + the panel-pinned region list): draw r is identical
  # every run (the fits use no RNG), so cached per-spec resample rows extend cleanly (ADR 0034).
  set.seed(seed)
  draws <- lapply(seq_len(nResamples), function(i) sample(regions, length(regions), replace = TRUE))
  validCols <- c("resample", mmCols)

  say(length(pool), " candidate(s) x ", nResamples, " resamples | ",
      length(regions), " region blocks",
      if (length(sanityRejected)) paste0(" | sanity-rejected filter: ",
                                         length(sanityRejected), " spec(s)") else "",
      " | deployed: ", if (is.na(deployed)) "(none)" else deployed)

  # Full-fit $data computed lazily and only for specs that still need (re)computation.
  fullData <- new.env(parent = emptyenv())
  getFullData <- function(mdl, sec) {
    k <- paste(mdl, sec)
    if (!is.null(fullData[[k]])) return(fullData[[k]])
    f <- .fitSpecModel(specByName[[mdl]], sec, stg, panel,
                       modelDir = modelDir, forceRefit = TRUE, verbose = FALSE)
    d <- if (!inherits(f, "fitError") && !is.null(f$data) && "region" %in% names(f$data)) f$data else NA
    fullData[[k]] <- d
    d
  }

  # Per (spec, sector): reuse cached resample rows; compute/extend only what's missing (ADR 0034).
  specRows <- list(); nHit <- 0L; nNew <- 0L; nExt <- 0L
  done <- 0L; total <- length(pool) * length(sectors)
  progEvery <- max(1L, total %/% 20L)
  if (isTRUE(verbose)) { say("spec-sector 0/", total, " (0%)"); utils::flush.console() }
  for (mdl in pool) for (sec in sectors) {
    done <- done + 1L
    if (isTRUE(verbose) && (done %% progEvery == 0 || done == total)) {
      say("spec-sector ", done, "/", total, " (", round(100 * done / total), "%)")
      utils::flush.console()
    }
    key <- .psmBootCacheKey(specByName[[mdl]], sec, panelHash, seed)
    cf <- if (!is.null(cacheDir)) file.path(cacheDir, paste0("psmboot_", sec, "_", key, ".rds")) else NULL
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
      rf <- .fitSpecModel(specByName[[mdl]], sec, stg, bootDf,
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
  say("cache ", nHit, " hit / ", nNew, " new / ", nExt, " extended (of ", total, " spec-sectors)")

  # ── Re-rank per resample under the v2 knobs ───────────────────────────────────
  perResample <- vector("list", nResamples)
  for (r in seq_len(nResamples)) {
    rows <- lapply(specRows, function(df) {
      rr <- df[df$resample == r, , drop = FALSE]
      if (nrow(rr)) rr else NULL
    })
    rows <- Filter(Negate(is.null), rows)
    if (!length(rows)) next
    tab <- do.call(rbind, rows)
    mmr <- tryCatch(
      computeMaximinScore(tab[, intersect(mmCols, names(tab)), drop = FALSE],
                          nearTieEps = nearTieEps, feParsimonyWeight = feParsimonyWeight,
                          dropIdleControls = dropIdleControls, softVifGate = softVifGate,
                          trendDominanceGate = trendDominanceGate, deltaR2Max = deltaR2Max,
                          inferenceTGate = inferenceTGate, rankBy = rankBy, tierGate = tierGate),
      error = function(e) NULL)
    if (is.null(mmr)) next
    top <- mmr[mmr$gatePass %in% TRUE, , drop = FALSE]
    winner <- if (nrow(top)) top$model[1] else NA_character_
    condPool <- top$model[!(top$model %in% sanityRejected)]
    winnerCond <- if (length(condPool)) condPool[1] else NA_character_
    depPass <- !is.na(deployed) && deployed %in% top$model
    perResample[[r]] <- data.frame(
      resample = r, winner = winner, winnerConditional = winnerCond,
      nGatePass = nrow(top), deployedGatePass = depPass,
      deployedRank = if (depPass) match(deployed, top$model) else NA_integer_,
      stringsAsFactors = FALSE)
  }
  perResample <- do.call(rbind, Filter(Negate(is.null), perResample))
  if (is.null(perResample) || !nrow(perResample)) {
    .recordStep(groupDir, group, "selection-bootstrap", t0, status = "failed",
                metrics = list(reason = "no successful resample (refits failed)"))
    return(invisible(NULL))
  }

  freq <- function(x) {
    x <- x[!is.na(x)]
    if (!length(x)) return(NULL)
    sort(table(x) / length(x), decreasing = TRUE)
  }
  nEff <- nrow(perResample)
  win <- perResample$winner
  winCond <- perResample$winnerConditional
  toks <- unlist(strsplit(chOf(win[!is.na(win)]), "|", fixed = TRUE))
  res <- list(
    stage = stg,
    deployed = deployed,
    deployedChannelSet = if (!is.na(deployed)) chOf(deployed) else NA_character_,
    nResamples = nResamples, nEffective = nEff,
    specFreq = freq(win),
    channelSetFreq = freq(chOf(win[!is.na(win)])),
    channelTokenFreq = if (length(toks)) sort(table(toks) / sum(!is.na(win)), decreasing = TRUE) else NULL,
    apFormFreq = freq(apOf(win[!is.na(win)])),
    specFreqConditional = freq(winCond),
    channelSetFreqConditional = freq(chOf(winCond[!is.na(winCond)])),
    apFormFreqConditional = freq(apOf(winCond[!is.na(winCond)])),
    deployedWinShare = if (!is.na(deployed)) mean(win %in% deployed) else NA_real_,
    deployedWinShareConditional = if (!is.na(deployed)) mean(winCond %in% deployed) else NA_real_,
    deployedGatePassShare = if (!is.na(deployed)) mean(perResample$deployedGatePass) else NA_real_,
    deployedRankQuantiles = if (any(!is.na(perResample$deployedRank))) {
      stats::quantile(perResample$deployedRank, c(0.05, 0.25, 0.5, 0.75, 0.95), na.rm = TRUE)
    } else NULL,
    gateEmptyShare = mean(perResample$nGatePass == 0),
    sanityRejected = sanityRejected,
    perResample = perResample,
    pool = pool, poolSize = length(pool), topK = topK, seed = seed,
    knobs = list(nearTieEps = nearTieEps, feParsimonyWeight = feParsimonyWeight,
                 dropIdleControls = dropIdleControls, softVifGate = softVifGate,
                 trendDominanceGate = trendDominanceGate, deltaR2Max = deltaR2Max,
                 inferenceTGate = inferenceTGate, rankBy = rankBy, tierGate = tierGate),
    generated = Sys.time())
  saveRDS(res, file.path(groupDir, "selection-bootstrap.rds"))
  .recordStep(groupDir, group, "selection-bootstrap", t0, metrics = list(
    nResamples = nResamples, nEffective = nEff, topK = topK,
    deployedWinShare = round(res$deployedWinShare, 3),
    deployedWinShareConditional = round(res$deployedWinShareConditional, 3),
    deployedGatePassShare = round(res$deployedGatePassShare, 3),
    topChannelSet = if (length(res$channelSetFreq)) names(res$channelSetFreq)[1] else NA_character_,
    topChannelSetShare = if (length(res$channelSetFreq)) round(res$channelSetFreq[[1]], 3) else NA_real_))
  say("winner channel set: ", if (length(res$channelSetFreq)) paste0(
        names(res$channelSetFreq)[1], " in ", round(100 * res$channelSetFreq[[1]]), "% of ",
        nEff, " resamples") else "(none)",
      " | deployed win share ", round(100 * (res$deployedWinShare %||% NA_real_)),
      "% (conditional ", round(100 * (res$deployedWinShareConditional %||% NA_real_)), "%)")
  say("done -> ", file.path(groupDir, "selection-bootstrap.rds"))
  invisible(res)
}
# nolint end
