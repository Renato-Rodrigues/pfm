# nolint start
#' Fit a single sweep specification (one sector × stage)
#'
#' @description
#' The unit of work for the model sweep (ADR 0019). Fits one configuration for one
#' sector and stage, returns the metrics row and coefficient rows the selection needs, and
#' (when \code{modelDir} is set) persists the fit to the Fit Cache as \code{{id}.rds}
#' \emph{without} touching \code{index.json} — so it is safe to call concurrently from parallel
#' workers. The master rebuilds the index once after the sweep
#' (\code{\link{rebuildPFMModelIndex}}). The same function backs both the sequential and the
#' parallel execution paths.
#'
#' @param cfg A normalised spec (named list) from the sweep config.
#' @param sector Character. \code{"Bulk"} or \code{"Diffuse"}.
#' @param stage Character. \code{"Adoption"} or \code{"Stringency"}.
#' @param panelData Prepared historical panel (magpie).
#' @param family Character. Stringency GLM family. Default \code{"gaussian"}.
#' @param modelDir Character or NULL. Fit Cache root.
#' @param forceRefit Logical. Ignore any cached fit and re-estimate. Default \code{FALSE}.
#' @param verbose Logical. Default \code{FALSE}.
#'
#' @return A list with \code{metrics} (one-row data.frame), \code{coefRows} (data.frame or
#'   NULL), \code{failed} (logical), and \code{errMsg} (character or NULL).
#' @seealso \code{\link{runFitGrid}}, ADR 0019
#' @export
#' @author Renato Rodrigues
# Internal: fit a single spec and return the FIT object (with $model, $coeftest, $data, $formula),
# or a `fitError` structure on failure. The cfg -> estimate*Model() argument mapping lives here so
# both fitOneSpec() (metrics wrapper) and runSelectionBootstrap() (needs $data to resample) reuse it.
#' @keywords internal
.fitSpecModel <- function(cfg, sector, stage, panelData, family = "gaussian",
                          modelDir = NULL, forceRefit = FALSE, verbose = FALSE, prepared = FALSE) {
  tryCatch({
    if (stage == "PolicyStringency") {
      # PSM (ADR 0036): single-stage bounded index, satP selection engine only.
      estimatePolicyStringencyModel(
        data = panelData, sector = sector, estimator = "satP",
        indexMax = cfg$indexMax %||% 10,
        actorPowerDrivers = cfg$actorPowerDrivers,
        actorPowerIndex = cfg$actorPowerIndex,
        instQualityDrivers = cfg$instQualityDrivers,
        controlDrivers = cfg$controlDrivers,
        includeLaggedPS = isTRUE(cfg$includeLaggedPS),
        interactRegionFE = cfg$interactRegionFE,
        regionMappingFixedEffects = cfg$regionMappingFixedEffects,
        useMundlak = cfg$useMundlak,
        gdpGovInteraction = cfg$gdpGovInteraction,
        logisticTimeTrend = cfg$logisticTimeTrend,
        apTransform = cfg$apTransform %||% "linear",
        modelDir = modelDir, updateIndex = FALSE, ignoreCache = forceRefit,
        verbose = verbose, prepared = prepared
      )
    } else if (stage == "Adoption") {
      estimateAdoptionModel(
        data = panelData, sector = sector,
        actorPowerDrivers = cfg$actorPowerDrivers,
        actorPowerIndex = cfg$actorPowerIndex,
        instQualityDrivers = cfg$instQualityDrivers,
        controlDrivers = cfg$controlDrivers,
        includeLaggedAdoption = cfg$includeLagged,
        interactRegionFE = cfg$interactRegionFE,
        regionMappingFixedEffects = cfg$regionMappingFixedEffects,
        useMundlak = cfg$useMundlak,
        gdpGovInteraction = cfg$gdpGovInteraction,
        logisticTimeTrend = cfg$logisticTimeTrend,
        ridgeInteractions = cfg$ridgeInteractions,
        panelTransform = cfg$panelTransform,
        modelDir = modelDir, updateIndex = FALSE, ignoreCache = forceRefit,
        verbose = verbose, compute = c(ame = FALSE, predictedProbs = FALSE),
        prepared = prepared
      )
    } else {
      estimatePriceStringencyModel(
        data = panelData, sector = sector, family = family,
        actorPowerDrivers = cfg$actorPowerDrivers,
        actorPowerIndex = cfg$actorPowerIndex,
        instQualityDrivers = cfg$instQualityDrivers,
        controlDrivers = cfg$controlDrivers,
        includeLaggedECP = isTRUE(cfg$includeLaggedECP) || isTRUE(cfg$includeLagged),
        interactRegionFE = cfg$interactRegionFE,
        regionMappingFixedEffects = cfg$regionMappingFixedEffects,
        useMundlak = cfg$useMundlak,
        gdpGovInteraction = cfg$gdpGovInteraction,
        logisticTimeTrend = cfg$logisticTimeTrend,
        ridgeInteractions = cfg$ridgeInteractions,
        panelTransform = cfg$panelTransform,
        nickellCorrection = isTRUE(cfg$nickellCorrection),
        priceLink = cfg$priceLink %||% "log1p",
        priceCeilingMax = cfg$priceCeilingMax %||% 1000,
        modelDir = modelDir, updateIndex = FALSE, ignoreCache = forceRefit,
        verbose = verbose, prepared = prepared
      )
    }
  }, error = function(e) structure(list(.err = conditionMessage(e)), class = "fitError"))
}

fitOneSpec <- function(cfg, sector, stage, panelData, family = "gaussian",
                       modelDir = NULL, forceRefit = FALSE, verbose = FALSE,
                       prepared = FALSE) {
  fit <- .fitSpecModel(cfg, sector, stage, panelData, family, modelDir, forceRefit, verbose, prepared)
  errMsg <- if (inherits(fit, "fitError")) fit$.err else NULL
  if (inherits(fit, "fitError")) fit <- NULL

  metrics <- .channelFitMetrics(fit, cfg, sector, stage)
  coefRows <- NULL
  if (!is.null(fit) && !is.null(fit$coeftest)) {
    ct <- as.data.frame(unclass(fit$coeftest))
    names(ct) <- c("estimate", "stdError", "zValue", "pValue")
    ct$term <- rownames(fit$coeftest)
    ct$model <- cfg$name
    ct$sector <- sector
    ct$stage <- stage
    rownames(ct) <- NULL
    coefRows <- ct
  }
  list(metrics = metrics, coefRows = coefRows,
       failed = is.null(fit) || is.null(fit$model), errMsg = errMsg)
}

#' Fit a full sweep grid (specs × sectors × stages), optionally in parallel
#'
#' @description
#' Flattens the specification grid into independent jobs and maps \code{\link{fitOneSpec}}
#' over them — sequentially (\code{nCores = 1}) or in parallel via \pkg{future.apply}
#' (\code{plan(multisession)} on Windows, \code{plan(multicore)} on Unix). Workers persist
#' their own \code{{id}.rds} without touching the index; the master rebuilds \code{index.json}
#' once afterwards (ADR 0019). Returns the assembled metrics and coefficient tables.
#'
#' @param specs List of normalised specs.
#' @param sectors Character vector (e.g. \code{c("Bulk","Diffuse")}).
#' @param stages Character vector (e.g. \code{c("Adoption","Stringency")}).
#' @param panelData Prepared historical panel (magpie).
#' @param family Character. Stringency GLM family. Default \code{"gaussian"}.
#' @param modelDir Character or NULL. Fit Cache root.
#' @param nCores Integer. \code{1} (default) runs sequentially; \code{> 1} uses
#'   \pkg{future.apply} if installed (else falls back to sequential with a warning).
#' @param forceRefit Logical. Ignore cached fits and re-estimate every spec. Default
#'   \code{FALSE} (resume: cached fits are loaded, only missing ones computed).
#' @param verbose Logical. Default \code{TRUE}.
#' @param say Optional logging function \code{function(...)}; defaults to a \code{[fits]} prefix.
#'
#' @return A list: \code{results} (per-fit metrics data.frame), \code{coefficients}
#'   (rbind of coef rows or NULL), \code{nJobs}, \code{nNew} (newly written fits),
#'   \code{nFailed}.
#' @seealso \code{\link{fitOneSpec}}, \code{\link{rebuildPFMModelIndex}}, ADR 0019
#' @export
#' @author Renato Rodrigues
runFitGrid <- function(specs, sectors, stages, panelData, family = "gaussian",
                       modelDir = NULL, nCores = 1L, forceRefit = FALSE,
                       verbose = TRUE, say = NULL) {
  if (is.null(say)) say <- function(...) if (isTRUE(verbose)) message("[fits] ", ...)

  jobs <- list()
  for (i in seq_along(specs)) {
    # Saturating-price twins are stringency-only (ADR 0028): priceLink does not affect the
    # adoption fit, so skip the Adoption jobs for a twin (avoids duplicate adoption rows).
    stringencyOnly <- isTRUE(specs[[i]]$stringencyOnly)
    for (stg in stages) {
      if (stringencyOnly && identical(stg, "Adoption")) next
      for (sec in sectors) {
        jobs[[length(jobs) + 1L]] <- list(cfg = specs[[i]], sector = sec, stage = stg)
      }
    }
  }
  total <- length(jobs)
  countRds <- function() if (!is.null(modelDir)) {
    length(list.files(file.path(modelDir, "models"), pattern = "\\.rds$"))
  } else 0L
  nBefore <- countRds()

  workerSetup <- .captureWorkerSetup(modelDir)
  fitFun <- function(job) {
    .applyWorkerSetup(workerSetup)
    fitOneSpec(job$cfg, job$sector, job$stage, panelData = panelData,
               family = family, modelDir = modelDir, forceRefit = forceRefit, verbose = FALSE)
  }

  mapper <- .resolveSweepMapper(nCores, total, say)
  out <- mapper(jobs, fitFun)

  results <- do.call(rbind, lapply(out, `[[`, "metrics"))
  rownames(results) <- NULL
  coefList <- Filter(Negate(is.null), lapply(out, `[[`, "coefRows"))
  coefficients <- if (length(coefList) > 0) do.call(rbind, coefList) else NULL
  nFailed <- sum(vapply(out, function(o) isTRUE(o$failed), logical(1)))

  # Master writes the index exactly once (ADR 0019: avoids the parallel race and the
  # O(n^2) per-save churn).
  if (!is.null(modelDir)) rebuildPFMModelIndex(modelDir)
  nNew <- max(0L, countRds() - nBefore)
  say(total, " jobs: ", nNew, " newly fit, ~", max(0L, total - nNew - nFailed),
      " from cache, ", nFailed, " failed.")

  list(results = results, coefficients = coefficients,
       nJobs = total, nNew = nNew, nFailed = nFailed)
}

# Internal: choose the map function. Sequential lapply for nCores <= 1 or when future.apply
# is unavailable; otherwise a future.apply backend with a platform-appropriate plan.
#' @keywords internal
.resolveSweepMapper <- function(nCores, total, say) {
  seqMapper <- function(X, FUN) {
    n <- length(X)
    res <- vector("list", n)
    for (i in seq_len(n)) {
      res[[i]] <- FUN(X[[i]])
      if (i %% 20 == 0 || i == n) say("  ", i, "/", n)
    }
    res
  }
  if (is.null(nCores) || nCores <= 1) return(seqMapper)
  if (!requireNamespace("future", quietly = TRUE) ||
      !requireNamespace("future.apply", quietly = TRUE)) {
    warning("runFitGrid: nCores > 1 but 'future'/'future.apply' not installed; ",
            "running sequentially.", call. = FALSE)
    return(seqMapper)
  }
  workers <- as.integer(min(nCores, max(1L, total)))
  oplan <- future::plan()
  if (identical(.Platform$OS.type, "windows")) {
    future::plan(future::multisession, workers = workers)
  } else {
    future::plan(future::multicore, workers = workers)
  }
  say("parallel backend: ", class(future::plan())[[1]], " x ", workers, " worker(s)")
  function(X, FUN) {
    on.exit(future::plan(oplan), add = TRUE)
    future.apply::future_lapply(
      X, FUN, future.seed = TRUE,
      future.packages = c("pfm", "magclass", "madrat")
    )
  }
}

# Internal: snapshot the madrat config + modelDir the workers need (fresh PSOCK processes
# carry none of the master's session state — the same cachefolder/mappingfolder/forcecache
# gotcha that bit the offline panel rebuild; ADR 0019).
#' @keywords internal
.captureWorkerSetup <- function(modelDir) {
  keys <- c("cachefolder", "mappingfolder", "sourcefolder",
            "regionmapping", "extramappings", "forcecache")
  cfg <- tryCatch(madrat::getConfig(), error = function(e) NULL)
  vals <- if (!is.null(cfg)) cfg[keys] else stats::setNames(vector("list", length(keys)), keys)
  names(vals) <- keys
  list(madrat = vals, modelDir = modelDir)
}

# Internal: apply the captured setup once per worker (idempotent via an option flag).
#' @keywords internal
.applyWorkerSetup <- function(setup) {
  if (isTRUE(getOption("pfm.workerInitDone"))) return(invisible(NULL))
  if (!is.null(setup$modelDir)) options(pfm.modelDir = setup$modelDir)
  args <- Filter(function(v) !is.null(v) && length(v) > 0, setup$madrat)
  if (length(args) > 0) tryCatch(do.call(madrat::setConfig, args), error = function(e) NULL)
  options(pfm.workerInitDone = TRUE)
  invisible(NULL)
}
# nolint end
