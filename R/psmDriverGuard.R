# nolint start
#' Driver-support guard for bounded-index projections (R3, 2026-07-06)
#'
#' @description
#' The first PSM real-data run showed that scenario drivers leave their training
#' support by several standard deviations (REMIND decarbonization pushes the
#' actor-power axis far beyond anything observed 2000-2022), and that fitted
#' interaction terms then dominate the projection with politically implausible
#' results (see docs/psm-nature-readiness-assessment.md, Sections 4.1 and 10).
#' The guard winsorizes each standardized base driver at its training-support
#' range at projection time and recomputes the interaction columns from the
#' guarded factors. This is a \emph{disclosed regressor-support restriction} -
#' the bounded-response philosophy of ADR 0036 is untouched (nothing clamps the
#' outcome; the design matrix simply never leaves the support the coefficients
#' were estimated on). The share of guarded drivers that were out of support is
#' returned per row (\code{driverOutOfSupport}) - the extrapolation audit of the
#' readiness assessment (Tier-1 test 2) rides on the same computation.
#'
#' \code{.driverSupportRanges} extracts the per-driver [min, max] (standardized
#' scale) from a prepared training data.frame: numeric base-driver columns only -
#' the outcome, its lags, the time trends, region FE, interaction (`_x_`) and
#' Mundlak (`_grp_mean`) columns are excluded (interactions are recomputed from
#' guarded factors, never guarded directly).
#'
#' @param df A prepared panel data.frame (fit: training rows; guard: scenario rows).
#' @param ranges Named list, column -> c(min, max), as returned by
#'   \code{.driverSupportRanges} (stored on fitted models as
#'   \code{transforms$driverRanges}).
#'
#' @return \code{.driverSupportRanges}: named list of length-2 numeric vectors.
#'   \code{.psmDriverGuard}: list with \code{df} (guarded, interactions
#'   recomputed) and \code{outOfSupport} (numeric per-row share of guarded
#'   drivers outside support, computed BEFORE clamping).
#'
#' @keywords internal
#' @author Renato Rodrigues
.driverSupportRanges <- function(df) {
  excl <- c("region", "year", "ecp", "lagged_ecp", "lagged_adoption",
            "timeTrend", "logisticTimeTrend", "regionFE")
  cols <- setdiff(colnames(df), excl)
  cols <- cols[!grepl("_x_|_grp_mean$", cols)]
  cols <- cols[vapply(df[cols], is.numeric, logical(1))]
  ranges <- lapply(cols, function(cl) {
    v <- df[[cl]]
    v <- v[is.finite(v)]
    if (length(v) == 0) return(NULL)
    c(min = min(v), max = max(v))
  })
  names(ranges) <- cols
  ranges[!vapply(ranges, is.null, logical(1))]
}

# Winsorize prepared scenario rows at the training support and recompute the
# interaction columns from the guarded factors; see the block above.
.psmDriverGuard <- function(df, ranges) {
  guardCols <- intersect(names(ranges), colnames(df))
  outCount <- rep(0L, nrow(df))
  if (length(guardCols) == 0) {
    return(list(df = df, outOfSupport = rep(NA_real_, nrow(df))))
  }
  for (cl in guardCols) {
    lo <- ranges[[cl]][["min"]]
    hi <- ranges[[cl]][["max"]]
    v <- df[[cl]]
    outside <- is.finite(v) & (v < lo | v > hi)
    outCount <- outCount + as.integer(outside)
    df[[cl]] <- pmin(pmax(v, lo), hi)
  }
  # Recompute interaction columns from the guarded factors (they were built from
  # unguarded values in preparePanelData).
  intCols <- grep("_x_", colnames(df), value = TRUE)
  for (ic in intCols) {
    parts <- strsplit(ic, "_x_", fixed = TRUE)[[1]]
    if (length(parts) == 2 && all(parts %in% colnames(df))) {
      df[[ic]] <- df[[parts[1]]] * df[[parts[2]]]
    }
  }
  list(df = df, outOfSupport = outCount / length(guardCols))
}

# Spread of the estimated region-FE coefficients (the reference level enters as 0).
# Used to inflate the projection interval for out-of-coverage rows, which inherit
# the reference-group fixed effect and therefore carry the full between-FE
# uncertainty on top of the coefficient uncertainty (R4a, 2026-07-06).
.regionFESpread <- function(beta) {
  fe <- beta[grepl("^regionFE", names(beta))]
  if (length(fe) == 0) return(0)
  stats::sd(c(0, as.numeric(fe)))
}
# nolint end
