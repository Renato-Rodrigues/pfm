# nolint start
#' Leave-one-cluster-out influence diagnostics for marginally (non-)significant terms
#'
#' @description
#' The post-selection half of ADR 0037: explains \emph{which countries/regions make a
#' theory term marginally (non-)significant} instead of pressuring selection with
#' p-values. For a satP-engine fit, every cluster (region/country) is dropped in turn
#' and the model refit analytically (OLS on the model matrix with CR1 clustered SEs and
#' the Cameron-Gelbach-Miller small-G adjustment - the same convention as
#' \code{\link{computeWildClusterBootstrap}} and the suite's \code{vcovCL} p-values;
#' p from a t distribution with G-1 df). Reported per tracked term:
#' \itemize{
#'   \item the full \strong{coefficient / p-value path} across folds;
#'   \item \strong{pivotal clusters} - folds where the term crosses \code{alpha} in
#'     either direction (\code{"gain"}: significant only without that cluster;
#'     \code{"loss"}: significance depends on that cluster);
#'   \item a per-fold \strong{DFBETA} (\code{dBeta} = fold beta minus full beta) and the
#'     \strong{beta-vs-SE decomposition} of the t change (is marginality coefficient
#'     shrinkage or SE inflation?);
#'   \item optionally a \strong{targeted wild-cluster bootstrap} re-run at the pivotal
#'     folds only (never all folds - Webb/Rademacher inference at G-1 clusters is noisy
#'     and 50x the compute buys little; ADR 0037).
#' }
#' A fold that empties a region-FE level refits without that dummy (the empty-level
#' singularity gotcha); a term dropped by the fold is returned as \code{"fold-failed"}.
#'
#' @param fit A satP-engine fit result (list with \code{formula}, \code{data};
#'   gaussian identity on the logit-transformed index).
#' @param terms Character vector of model-matrix term names to track. Default
#'   \code{NULL} = all non-intercept, non-region-FE terms.
#' @param alpha Numeric. Significance threshold defining a pivotal crossing.
#'   Default \code{0.05}.
#' @param wcb Logical. Re-run the wild-cluster bootstrap at pivotal folds.
#'   Default \code{TRUE}.
#' @param wcbB Integer. Bootstrap replications for the targeted re-runs.
#' @param seed Integer. RNG seed for the targeted bootstrap.
#' @param verbose Logical.
#'
#' @return A list:
#'   \describe{
#'     \item{byTerm}{One row per tracked term: full-sample \code{estimate, se, t, p},
#'       the fold p range (\code{pMin, pMax}), \code{nPivotal} and
#'       \code{pivotalClusters}, the \code{topInfluencer} (largest |dBeta|) and the
#'       \code{tChangeDriver} decomposition (\code{"coefficient"} / \code{"se"}) at the
#'       fold that moves p farthest.}
#'     \item{path}{Long data.frame term x cluster: fold \code{estimate, se, t, p},
#'       \code{dBeta}, \code{seRatio}, \code{crossing}, \code{pWild} (targeted folds only).}
#'     \item{alpha, clusters, nClusters}{Inputs echoed for the report.}
#'   }
#'
#' @seealso \code{\link{computeWildClusterBootstrap}}, \code{\link{runPSMInfluence}},
#'   ADR 0037
#' @importFrom stats model.matrix as.formula complete.cases var pt
#' @export
#' @author Renato Rodrigues
computeInfluenceDiagnostics <- function(fit, terms = NULL, alpha = 0.05,
                                        wcb = TRUE, wcbB = 999, seed = 42,
                                        verbose = TRUE) {
  say <- function(...) if (isTRUE(verbose)) message("[influence] ", ...)
  df <- fit$data
  fml <- stats::as.formula(fit$formula)
  vars <- intersect(all.vars(fml), colnames(df))
  df <- df[stats::complete.cases(df[, vars, drop = FALSE]), , drop = FALSE]
  if (!"region" %in% colnames(df)) stop("computeInfluenceDiagnostics: fit$data lacks 'region'.")

  # Analytic CR1 cluster-robust refit on a data subset. Rebuilds the model matrix from
  # the subset (droplevels first) so an FE level emptied by the fold drops its dummy
  # instead of producing a singular crossprod (the documented empty-level gotcha).
  clusterFit <- function(d) {
    if ("regionFE" %in% colnames(d) && is.factor(d$regionFE)) d$regionFE <- droplevels(d$regionFE)
    mm <- stats::model.matrix(fml, data = d)
    keep <- colnames(mm) == "(Intercept)" |
      apply(mm, 2, function(col) isTRUE(stats::var(col) > 0))
    mm <- mm[, keep, drop = FALSE]
    yv <- d$ecp[match(rownames(mm), rownames(d))]
    g <- as.character(d$region[match(rownames(mm), rownames(d))])
    G <- length(unique(g)); n <- nrow(mm); k <- ncol(mm)
    xtxInv <- solve(crossprod(mm))
    b <- as.numeric(xtxInv %*% crossprod(mm, yv)); names(b) <- colnames(mm)
    res <- yv - as.numeric(mm %*% b)
    us <- rowsum(mm * res, g)
    adj <- (G / (G - 1)) * ((n - 1) / max(n - k, 1))
    se <- sqrt(pmax(diag(xtxInv %*% crossprod(us) %*% xtxInv) * adj, 0))
    names(se) <- colnames(mm)
    tv <- b / se
    list(b = b, se = se, t = tv, p = 2 * stats::pt(-abs(tv), df = max(G - 1, 1)), G = G, n = n)
  }

  full <- clusterFit(df)
  allTerms <- names(full$b)
  if (is.null(terms)) terms <- allTerms[!grepl("^\\(Intercept\\)$|^regionFE", allTerms)]
  terms <- intersect(terms, allTerms)
  if (length(terms) == 0) stop("computeInfluenceDiagnostics: no tracked term is in the model.")

  clusters <- sort(unique(as.character(df$region)))
  say(length(clusters), " clusters x ", length(terms), " terms ...")
  rows <- vector("list", length(clusters) * length(terms)); i <- 0L
  for (cl in clusters) {
    fold <- tryCatch(clusterFit(df[df$region != cl, , drop = FALSE]), error = function(e) NULL)
    for (tm in terms) {
      i <- i + 1L
      fp <- full$p[[tm]]
      if (is.null(fold) || !tm %in% names(fold$b) || !is.finite(fold$se[[tm]]) ||
            fold$se[[tm]] <= 0) {
        rows[[i]] <- data.frame(term = tm, cluster = cl, estimate = NA_real_, se = NA_real_,
                                t = NA_real_, p = NA_real_, dBeta = NA_real_,
                                seRatio = NA_real_, crossing = "fold-failed",
                                stringsAsFactors = FALSE)
        next
      }
      pG <- fold$p[[tm]]
      crossing <- if (fp >= alpha && pG < alpha) "gain"
      else if (fp < alpha && pG >= alpha) "loss"
      else "none"
      rows[[i]] <- data.frame(
        term = tm, cluster = cl,
        estimate = fold$b[[tm]], se = fold$se[[tm]], t = fold$t[[tm]], p = pG,
        dBeta = fold$b[[tm]] - full$b[[tm]],
        seRatio = fold$se[[tm]] / full$se[[tm]],
        crossing = crossing, stringsAsFactors = FALSE
      )
    }
  }
  path <- do.call(rbind, rows)
  rownames(path) <- NULL

  # Targeted wild-cluster bootstrap: only the folds where an analytic crossing occurred.
  path$pWild <- NA_real_
  if (isTRUE(wcb)) {
    pivotalFolds <- unique(path$cluster[path$crossing %in% c("gain", "loss")])
    if (length(pivotalFolds) > 0) say("targeted WCB at ", length(pivotalFolds), " pivotal fold(s) ...")
    for (cl in pivotalFolds) {
      wres <- tryCatch(
        computeWildClusterBootstrap(
          list(formula = fit$formula, data = df[df$region != cl, , drop = FALSE]),
          B = wcbB, seed = seed
        ),
        error = function(e) { say("  WCB fold ", cl, " failed: ", conditionMessage(e)); NULL }
      )
      if (is.null(wres)) next
      idx <- path$cluster == cl & path$term %in% wres$term
      path$pWild[idx] <- wres$pWild[match(path$term[idx], wres$term)]
    }
  }

  byTerm <- do.call(rbind, lapply(terms, function(tm) {
    sub <- path[path$term == tm & is.finite(path$p), , drop = FALSE]
    fp <- full$p[[tm]]
    piv <- sub$cluster[sub$crossing %in% c("gain", "loss")]
    topInf <- if (nrow(sub)) sub$cluster[which.max(abs(sub$dBeta))] else NA_character_
    # beta-vs-SE decomposition of the t change at the fold moving p farthest:
    #   dT = (b_g - b)/se_g  (coefficient component)  +  b*(1/se_g - 1/se)  (SE component)
    driver <- NA_character_
    if (nrow(sub)) {
      w <- sub[which.max(abs(sub$p - fp)), ]
      dtBeta <- (w$estimate - full$b[[tm]]) / w$se
      dtSE <- full$b[[tm]] * (1 / w$se - 1 / full$se[[tm]])
      driver <- if (abs(dtBeta) >= abs(dtSE)) "coefficient" else "se"
    }
    data.frame(
      term = tm, estimate = full$b[[tm]], se = full$se[[tm]], t = full$t[[tm]], p = fp,
      pMin = if (nrow(sub)) min(sub$p) else NA_real_,
      pMax = if (nrow(sub)) max(sub$p) else NA_real_,
      nPivotal = length(piv),
      pivotalClusters = paste(piv, collapse = ", "),
      topInfluencer = topInf, tChangeDriver = driver,
      stringsAsFactors = FALSE
    )
  }))
  rownames(byTerm) <- NULL

  list(byTerm = byTerm, path = path, alpha = alpha,
       clusters = clusters, nClusters = length(clusters))
}

#' Influence-diagnostics Run-Group step (psm-influence)
#'
#' @description
#' Runs \code{\link{computeInfluenceDiagnostics}} on the deployed spec
#' (\code{selected-models-psm.yml}) for both sectors, tracking the \emph{theory}
#' terms (Actor Power mains/index, Institutional Quality channels, and their
#' interactions). Writes \code{<group>/influence.rds}. Report-only - it never
#' changes the deliverable (ADR 0037).
#'
#' @inheritParams runPSMIV
#' @param alpha,wcbB Forwarded to \code{\link{computeInfluenceDiagnostics}}.
#' @return Invisibly, the artifact list, or \code{NULL} when skipped.
#' @export
#' @author Renato Rodrigues
runPSMInfluence <- function(group,
                            resultsDir = getOption("pfm.resultsDir", "output"),
                            modelDir = getOption("pfm.modelDir", "output"),
                            cachefolder = NULL, panelData = NULL,
                            y = 2000:2022,
                            outputRegionMappingFile = "regionmapping_54.csv",
                            indexMax = 10, alpha = 0.05, wcbB = 999,
                            verbose = TRUE) {
  groupDir <- .resolveGroupDir(group, resultsDir, modelDir, cachefolder)
  say <- function(...) if (isTRUE(verbose)) message("[PSM-INFLUENCE:", group, "] ", ...)
  t0 <- Sys.time()
  selPath <- file.path(groupDir, "selected-models-psm.yml")
  if (!file.exists(selPath)) {
    .recordStep(groupDir, group, "influence", t0, status = "skipped",
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
      panelDataHistorical(aggregate = TRUE, y = y,
                          outputRegionMappingFile = outputRegionMappingFile,
                          includePolicyStringency = TRUE),
      error = function(e) NULL
    )
  }
  if (is.null(panel)) {
    .recordStep(groupDir, group, "influence", t0, status = "failed",
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
    say("deployed satP refit (", sec, ") ...")
    fit <- tryCatch(
      estimatePolicyStringencyModel(
        data = panel, sector = sec, estimator = "satP", indexMax = indexMax,
        actorPowerDrivers = cfg$actorPowerDrivers, actorPowerIndex = cfg$actorPowerIndex,
        instQualityDrivers = cfg$instQualityDrivers, controlDrivers = cfg$controlDrivers,
        regionMappingFixedEffects = cfg$regionMappingFixedEffects,
        logisticTimeTrend = isTRUE(cfg$logisticTimeTrend),
        interactRegionFE = isTRUE(cfg$interactRegionFE),
        useMundlak = isTRUE(cfg$useMundlak),
        gdpGovInteraction = isTRUE(cfg$gdpGovInteraction),
        includeLaggedPS = isTRUE(cfg$includeLaggedPS),
        modelDir = NULL, verbose = FALSE
      ),
      error = function(e) { say("  ", sec, " refit failed: ", conditionMessage(e)); NULL }
    )
    if (is.null(fit)) next
    # theory terms: any model-matrix term touching an AP/IQ variable (mains + interactions)
    thVars <- make.names(c(cfg$actorPowerDrivers, cfg$actorPowerIndex, cfg$instQualityDrivers))
    mmTerms <- rownames(fit$coeftest)
    thTerms <- mmTerms[!grepl("^\\(Intercept\\)$|^regionFE", mmTerms) &
                         vapply(mmTerms, function(nm)
                           any(vapply(thVars, function(p) grepl(p, nm, fixed = TRUE),
                                      logical(1))), logical(1))]
    inf <- tryCatch(
      computeInfluenceDiagnostics(fit, terms = thTerms, alpha = alpha, wcbB = wcbB,
                                  verbose = verbose),
      error = function(e) { say("  ", sec, " influence failed: ", conditionMessage(e)); NULL }
    )
    if (is.null(inf)) next
    out$bySector[[sec]] <- inf
    nPiv <- sum(inf$byTerm$nPivotal)
    stepMetrics[[paste0("pivotal.", sec)]] <- nPiv
    marg <- inf$byTerm$term[inf$byTerm$p >= alpha & inf$byTerm$pMin < alpha]
    if (length(marg)) {
      say("  ", sec, " marginal terms rescued by dropping one cluster: ",
          paste(marg, collapse = ", "))
    }
    say("  ", sec, ": ", nPiv, " pivotal (term x cluster) crossings")
  }
  if (!length(out$bySector)) {
    .recordStep(groupDir, group, "influence", t0, status = "failed",
                metrics = list(reason = "no sector produced influence diagnostics"))
    return(invisible(NULL))
  }
  saveRDS(out, file.path(groupDir, "influence.rds"))
  .recordStep(groupDir, group, "influence", t0, metrics = c(list(spec = out$spec), stepMetrics))
  say("Saved ", file.path(groupDir, "influence.rds"))
  invisible(out)
}
# nolint end
