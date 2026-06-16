#' @title applyPanelTransform
#' @description Applies a first-difference Panel Transform to a prepared panel
#' data.frame (output of \code{preparePanelData}). Implements the Panel Transform
#' axis decided in ADR 0005:
#' \describe{
#'   \item{\code{"levels"}}{No-op. The data.frame is returned unchanged.}
#'   \item{\code{"hybridFD"}}{The headline first-difference variant. The outcome and
#'     the fast-moving Actor Power terms are differenced within region between
#'     consecutive years; Institutional Quality (and control) variables stay in
#'     levels, because slow-moving V-Dem indices cannot be identified in
#'     differences. Interaction columns are recomputed from the transformed
#'     values, so \code{AP_x_IQ} becomes \eqn{\Delta AP \times IQ}.}
#'   \item{\code{"pureFD"}}{Everything differenced — Actor Power, Institutional
#'     Quality, and control variables. Retained to document empirically that pure
#'     differencing extinguishes the institutional channels.}
#' }
#'
#' Stage semantics under any FD transform:
#' \describe{
#'   \item{adoption}{Becomes a discrete-time hazard (onset) model: the sample is
#'     restricted to the at-risk set (region-years whose \emph{previous} year was
#'     not adopted, i.e. previous ECP <= 0). The \code{adoption} column then
#'     encodes onset — P(adopt this year | not yet adopted). Time-trend columns
#'     are kept in levels (they act as the baseline hazard).}
#'   \item{stringency}{The dependent variable becomes the within-spell change:
#'     \eqn{\Delta\log(1 + ECP)} when \code{logTransform = TRUE}, else
#'     \eqn{\Delta ECP}. Only consecutive year pairs with positive ECP in both
#'     years (continuing adopter spells) are retained. Region fixed effects are
#'     differenced out by construction and must be suppressed by the caller.}
#' }
#'
#' Differenced values replace the original columns under the same column names,
#' so model formulas, Term Group classification, and group-contribution
#' diagnostics work unchanged; the transform is recorded in the
#' \code{"panelTransform"} attribute of the returned data.frame.
#'
#' @param df Data.frame. A prepared panel (output of \code{preparePanelData});
#'   for \code{stage = "adoption"} it must already contain the \code{adoption}
#'   column.
#' @param panelTransform Character. One of \code{"levels"}, \code{"hybridFD"},
#'   \code{"pureFD"}.
#' @param stage Character. \code{"adoption"} or \code{"stringency"}.
#' @param actorPowerDrivers Character vector or NULL. Actor Power main-effect names.
#' @param actorPowerIndex Character vector or NULL. Actor Power interaction-source names.
#' @param instQualityDrivers Character vector or NULL. Institutional Quality names
#'   (differenced only under \code{"pureFD"}).
#' @param controlDrivers Character vector or NULL. Control names (differenced only
#'   under \code{"pureFD"}).
#' @param logTransform Logical. Stringency only: difference \code{log(1 + ECP)}
#'   instead of raw ECP. Default: \code{TRUE}.
#' @param verbose Logical. Print a summary of the transformation. Default: \code{TRUE}.
#'
#' @return The transformed data.frame, with attribute \code{"panelTransform"} set.
#'
#' @seealso ADR 0005 (first-difference support), \code{\link{preparePanelData}}
#'
#' @export
#' @author Renato Rodrigues
applyPanelTransform <- function(df, panelTransform = c("levels", "hybridFD", "pureFD"),
                                stage = c("adoption", "stringency"),
                                actorPowerDrivers = NULL, actorPowerIndex = NULL,
                                instQualityDrivers = NULL, controlDrivers = NULL,
                                logTransform = TRUE, verbose = TRUE) {
  panelTransform <- match.arg(panelTransform)
  stage <- match.arg(stage)
  if (panelTransform == "levels") {
    attr(df, "panelTransform") <- "levels"
    return(df)
  }

  if (any(grepl("_grp_mean$", colnames(df)))) {
    stop("applyPanelTransform: Mundlak group-mean columns detected. ",
         "useMundlak is incompatible with first-difference transforms (ADR 0005).")
  }
  if (!all(c("region", "year", "ecp") %in% colnames(df))) {
    stop("applyPanelTransform: df must contain 'region', 'year' and 'ecp' columns.")
  }

  nInput <- nrow(df)

  # ── Columns to difference ────────────────────────────────────────────────────
  bookkeeping <- c("lagged_ecp", "lagged_adoption", "timeTrend", "logisticTimeTrend",
                   "ecp", "adoption", "region", "year", "regionFE")
  apCols <- intersect(make.names(unique(c(actorPowerDrivers, actorPowerIndex))),
                      colnames(df))
  diffCols <- setdiff(apCols, bookkeeping)
  if (panelTransform == "pureFD") {
    iqCols  <- intersect(make.names(unique(instQualityDrivers)), colnames(df))
    ctlCols <- intersect(make.names(unique(controlDrivers)), colnames(df))
    diffCols <- unique(c(diffCols, setdiff(c(iqCols, ctlCols), bookkeeping)))
  }
  if (length(diffCols) == 0) {
    stop("applyPanelTransform: no driver columns found to difference.")
  }

  # ── Previous-year row matching within region ────────────────────────────────
  yr <- as.integer(round(df$year))
  ord <- order(df$region, yr)
  df <- df[ord, , drop = FALSE]
  yr <- yr[ord]
  key <- paste(df$region, yr, sep = "\r")
  prevIdx <- match(paste(df$region, yr - 1L, sep = "\r"), key)
  hasPrev <- !is.na(prevIdx)

  # Previous-year ECP level (needed before df$ecp is overwritten)
  prevEcp <- rep(NA_real_, nrow(df))
  prevEcp[hasPrev] <- df$ecp[prevIdx[hasPrev]]

  # ── Difference the driver columns (same names, transformed content) ─────────
  for (cl in diffCols) {
    newVal <- rep(NA_real_, nrow(df))
    newVal[hasPrev] <- df[[cl]][hasPrev] - df[[cl]][prevIdx[hasPrev]]
    df[[cl]] <- newVal
  }

  # ── Stage-specific outcome and sample construction ──────────────────────────
  if (stage == "adoption") {
    # Discrete-time hazard: at-risk set = previous year observed and not adopted.
    keep <- hasPrev & !is.na(prevEcp) & prevEcp <= 0
    df <- df[keep, , drop = FALSE]
    # Current adoption status within the at-risk set IS the onset indicator.
    df$adoption <- as.integer(df$ecp > 0)
  } else {
    # Within-spell price change: both years must be adopting (ECP > 0).
    curEcp <- df$ecp
    valid <- hasPrev & !is.na(prevEcp) & prevEcp > 0 & !is.na(curEcp) & curEcp > 0
    delta <- rep(NA_real_, nrow(df))
    if (isTRUE(logTransform)) {
      delta[valid] <- log1p(curEcp[valid]) - log1p(prevEcp[valid])
    } else {
      delta[valid] <- curEcp[valid] - prevEcp[valid]
    }
    df <- df[valid, , drop = FALSE]
    df$ecp <- delta[valid]
  }

  # ── Recompute interaction columns from transformed values ───────────────────
  # hybridFD: AP_x_IQ becomes deltaAP x IQ(level); pureFD: deltaAP x deltaIQ.
  intCols <- grep("_x_", colnames(df), fixed = TRUE, value = TRUE)
  for (ic in intCols) {
    parts <- strsplit(ic, "_x_", fixed = TRUE)[[1]]
    if (length(parts) == 2 && all(parts %in% colnames(df))) {
      df[[ic]] <- df[[parts[1]]] * df[[parts[2]]]
    }
  }

  if (isTRUE(verbose)) {
    message("  [", panelTransform, "] ", stage,
            if (stage == "adoption") " hazard (onset) sample: " else " within-spell differences: ",
            nrow(df), " of ", nInput, " rows retained; differenced: ",
            paste(diffCols, collapse = ", "))
  }

  attr(df, "panelTransform") <- panelTransform
  df
}
