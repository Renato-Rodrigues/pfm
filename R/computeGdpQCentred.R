# nolint start
# Package-level environment for shared mutable state across function calls.
# Populated by panelDataHistorical (PCA rotation, GDP quartile breaks)
# and consumed by panelDataScenario to ensure consistent transformations.
.pfm_env <- new.env(parent = emptyenv())

#' Within-income-quartile de-meaned GDP per Capita
#'
#' Internal helper to calculate the within-income-quartile de-meaned GDP per Capita.
#'
#' Fit/apply semantics (Scenario Scope decision, 2026-06-12): with
#' \code{storeBreaks = TRUE} (historical panel) the quartile breaks, the
#' quartile-group means, AND the per-region group assignments are computed from
#' the supplied data and cached in \code{.pfm_env}. With \code{storeBreaks = FALSE}
#' and a cached fit present (scenario panel), all three are reused — a region's
#' scenario value reads "GDP relative to its historical income-class average",
#' continuous at the historical/scenario seam. Regions absent from the historical
#' fit are assigned via the stored breaks on their scenario mean.
#'
#' @param gdpPCNorm magpie with one variable (log-normalized GDP per Capita between 0 and 1)
#' @param storeBreaks if TRUE, fits and stores quartile breaks, group means and
#'   region assignments in .pfm_env for scenario reuse
#'
#' @return magpie with same dims as gdpPCNorm, variable renamed "GDP per Capita (Q-centred)"
#' @keywords internal
computeGdpQCentred <- function(gdpPCNorm, storeBreaks = FALSE) {
  arr3d <- as.array(gdpPCNorm)                   # [nReg, nYr, 1]
  arr   <- arr3d[, , 1L, drop = FALSE]           # keep matrix form [nReg, nYr]
  dim(arr) <- c(dim(arr3d)[1L], dim(arr3d)[2L])  # ensure 2D
  rownames(arr) <- magclass::getRegions(gdpPCNorm)

  meanGdpPC <- rowMeans(arr, na.rm = TRUE)

  applyStored <- !storeBreaks && !is.null(.pfm_env$gdppc_q_fit)
  if (applyStored) {
    # ── Apply mode: everything frozen from the historical fit ─────────────────
    fit <- .pfm_env$gdppc_q_fit
    qBreaks <- fit$breaks
    qMeans  <- fit$means
    qGroup  <- fit$group[names(meanGdpPC)]
    # Regions absent from the historical fit: assign via stored breaks
    missingRegion <- is.na(qGroup)
    if (any(missingRegion)) {
      qGroup[missingRegion] <- findInterval(meanGdpPC[missingRegion], qBreaks)
    }
  } else {
    # ── Fit mode ───────────────────────────────────────────────────────────────
    qBreaks <- stats::quantile(meanGdpPC, probs = c(0, 0.25, 0.5, 0.75, 1.0), na.rm = TRUE)
    qBreaks[c(1L, 5L)] <- c(-Inf, Inf)
    qGroup <- findInterval(meanGdpPC, qBreaks)                # integer 1-4
    qMeans <- tapply(meanGdpPC, qGroup, mean, na.rm = TRUE)   # named "1".."4"
    if (storeBreaks) {
      .pfm_env$gdppc_q_fit <- list(
        breaks = qBreaks,
        means  = qMeans,
        group  = stats::setNames(qGroup, names(meanGdpPC))
      )
      # kept for backward compatibility with older readers of .pfm_env
      .pfm_env$gdppc_q_breaks <- qBreaks
    }
  }

  arrOut <- arr
  for (q in seq_len(4L)) {
    inQ <- which(qGroup == q)
    qm  <- unname(qMeans[as.character(q)])
    if (length(inQ) > 0L && !is.na(qm)) arrOut[inQ, ] <- arr[inQ, , drop = FALSE] - qm
  }

  # Use gdpPCNorm as template (preserves magpie metadata) then overwrite values + name
  out_mag <- gdpPCNorm
  out_mag[, , ] <- arrOut
  magclass::getNames(out_mag) <- "GDP per Capita (Q-centred)"
  return(out_mag)
}
# nolint end
