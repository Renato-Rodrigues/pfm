# nolint start
#' Inference artifact for the deployed spec (pfm-inference)
#'
#' @description
#' Refits the deployed specification per sector and persists the two inference quantities the
#' paper quotes but no Run-Group step ever wrote:
#'
#' \itemize{
#'   \item \strong{wild-cluster bootstrap-\eqn{t} \eqn{p}-values} (Rademacher, clustered on
#'     country) from \code{\link{computeWildClusterBootstrap}} — with 48 clusters the
#'     asymptotic cluster-robust \eqn{p} is the weaker statement, and this is what
#'     \code{MODEL.md} §2.5 is supposed to print;
#'   \item \strong{average marginal effects} on the natural index scale, which
#'     \code{fitAndDiagnose()} already computes inside the fit and which was being discarded
#'     with it.
#' }
#'
#' Writes \code{<group>/inference.rds}. Report-only — it never changes the deliverable
#' (ADR 0037).
#'
#' @section Why this step exists:
#' Both quantities were produced once, by hand, for Run-Group \code{v1}, and then went stale
#' without anything noticing: \code{MODEL.md} §2.5 carried \code{v1} \eqn{p_{wild}} and AME
#' columns against \code{v4} coefficients for weeks, because nothing recomputed them and
#' nothing checked. A number the paper quotes must be produced by a step, not by a session.
#' \code{TODO.md} item 14h.
#'
#' @inheritParams runPFMInfluence
#' @param B Bootstrap replications. Default 999.
#' @param seed Bootstrap seed, so the artifact is reproducible.
#' @return Invisibly, the artifact list, or \code{NULL} when skipped.
#' @export
#' @author Renato Rodrigues
runPFMInference <- function(group,
                            resultsDir = getOption("pfm.resultsDir", "output"),
                            modelDir = getOption("pfm.modelDir", "output"),
                            cachefolder = NULL, panelData = NULL,
                            y = 2000:2022,
                            outputRegionMappingFile = "regionmapping_54.csv",
                            indexMax = 10, B = 999, seed = 42,
                            verbose = TRUE) {
  groupDir <- .resolveGroupDir(group, resultsDir, modelDir, cachefolder)
  say <- function(...) if (isTRUE(verbose)) message("[PFM-INFERENCE:", group, "] ", ...)
  t0 <- Sys.time()

  selPath <- .pfmSelectedModels(groupDir)
  if (!file.exists(selPath)) {
    .recordStep(groupDir, group, "inference", t0, status = "skipped",
                metrics = list(reason = "no selected-models-pfm.yml (run runPFMSweep first)"))
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
      panelDataHistorical(aggregate = TRUE, y = y,
                          outputRegionMappingFile = outputRegionMappingFile,
                          includePolicyStringency = TRUE),
      error = function(e) NULL
    )
  }
  if (is.null(panel)) {
    .recordStep(groupDir, group, "inference", t0, status = "failed",
                metrics = list(reason = "historical panel unavailable"))
    return(invisible(NULL))
  }

  out <- list(spec = NULL, B = B, seed = seed, bySector = list())
  stepMetrics <- list()
  for (sec in c("Bulk", "Diffuse")) {
    hit <- Filter(function(x) identical(x$model_type, paste0("PolicyStringency: ", sec)), sel)
    if (length(hit) == 0) next
    cfg <- norm(hit[[1]])
    out$spec <- out$spec %||% cfg$name
    say("deployed satP refit (", sec, ") ...")
    fit <- tryCatch(
      do.call(estimatePolicyStringencyModel, c(
        list(data = panel, sector = sec, estimator = "satP", indexMax = indexMax,
             modelDir = NULL, verbose = FALSE),
        .pfmSpecArgs(cfg))),
      error = function(e) { say("  ", sec, " refit failed: ", conditionMessage(e)); NULL }
    )
    if (is.null(fit)) next

    wcb <- tryCatch(computeWildClusterBootstrap(fit, B = B, seed = seed),
                    error = function(e) {
                      say("  ", sec, " bootstrap failed: ", conditionMessage(e)); NULL
                    })

    # fitAndDiagnose() computes the AME inside the fit; it was being dropped with the fit.
    ame <- fit$ame

    # One table, so a reader never has to join two artifacts to quote one row. The
    # asymptotic p stays beside the bootstrap p on purpose - the gap between them IS the
    # small-G warning, and hiding the weaker number hides the warning with it.
    tab <- NULL
    if (!is.null(wcb)) {
      tab <- wcb
      if (!is.null(ame) && "term" %in% names(ame)) {
        j <- match(tab$term, ame$term)
        tab$ame <- ame$ame[j]
        tab$ameSE <- ame$se[j]
      }
      # `fit$coeftest` is a `coeftest` object. as.data.frame() on one renames the columns
      # to `x.Estimate` and DROPS the rownames, so the join silently matched nothing and
      # the column came back empty - caught 2026-09-15. Index the matrix directly.
      ct <- tryCatch(unclass(fit$coeftest), error = function(e) NULL)
      if (is.matrix(ct) && ncol(ct) >= 4 && !is.null(rownames(ct))) {
        tab$pAsymptotic <- ct[match(tab$term, rownames(ct)), 4]
      }
    }

    out$bySector[[sec]] <- list(table = tab, wcb = wcb, ame = ame,
                                nClusters = length(unique(fit$data$region)))
    if (!is.null(tab)) {
      nDisagree <- sum(is.finite(tab$pWild) & is.finite(tab$pAsymptotic) &
                         ((tab$pWild < 0.05) != (tab$pAsymptotic < 0.05)))
      stepMetrics[[paste0("signFlips.", sec)]] <- nDisagree
      say("  ", sec, ": ", nDisagree, " term(s) where the bootstrap and asymptotic p ",
          "disagree at alpha = 0.05")
    }
  }
  if (!length(out$bySector)) {
    .recordStep(groupDir, group, "inference", t0, status = "failed",
                metrics = list(reason = "no sector produced inference"))
    return(invisible(NULL))
  }
  saveRDS(out, file.path(groupDir, "inference.rds"))
  .recordStep(groupDir, group, "inference", t0, metrics = c(list(spec = out$spec), stepMetrics))
  say("Saved ", file.path(groupDir, "inference.rds"))
  invisible(out)
}
# nolint end
