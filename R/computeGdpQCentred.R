# Package-level environment for shared mutable state across function calls.
# Populated by panelDataHistorical (PCA rotation, GDP quartile breaks)
# and consumed by panelDataScenario to ensure consistent transformations.
.pfm_env <- new.env(parent = emptyenv())

#' Within-income-quartile de-meaned GDP per Capita
#'
#' Internal helper to calculate the within-income-quartile de-meaned GDP per Capita.
#'
#' @param gdpPCNorm magpie with one variable (log-normalized GDP per Capita \code{[0, 1]})
#' @param storeBreaks if TRUE, stores quartile breaks in .pfm_env for scenario reuse
#'
#' @return magpie with same dims as gdpPCNorm, variable renamed "GDP per Capita (Q-centred)"
#' @keywords internal
computeGdpQCentred <- function(gdpPCNorm, storeBreaks = FALSE) {
  arr3d <- as.array(gdpPCNorm)                   # [nReg, nYr, 1]
  arr   <- arr3d[, , 1L, drop = FALSE]           # keep matrix form [nReg, nYr]
  dim(arr) <- c(dim(arr3d)[1L], dim(arr3d)[2L])  # ensure 2D
  rownames(arr) <- magclass::getRegions(gdpPCNorm)

  meanGdpPC <- rowMeans(arr, na.rm = TRUE)

  # Quartile breaks — use stored historical breaks for scenario consistency
  if (!storeBreaks && !is.null(.pfm_env$gdppc_q_breaks)) {
    qBreaks <- .pfm_env$gdppc_q_breaks
  } else {
    qBreaks <- stats::quantile(meanGdpPC, probs = c(0, 0.25, 0.5, 0.75, 1.0), na.rm = TRUE)
    qBreaks[c(1L, 5L)] <- c(-Inf, Inf)
    if (storeBreaks) .pfm_env$gdppc_q_breaks <- qBreaks
  }

  qGroup <- findInterval(meanGdpPC, qBreaks)              # integer 1-4
  qMeans <- tapply(meanGdpPC, qGroup, mean, na.rm = TRUE) # named "1".."4"

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
