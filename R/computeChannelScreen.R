#' @title computeChannelScreen
#' @description Stage-0 correlation pre-screen of the guided model-selection
#' algorithm (ADR 0004). Operates on a prepared panel data.frame and produces,
#' without fitting any model:
#' \enumerate{
#'   \item a pairwise correlation table over all Institutional Quality Channel
#'     candidates (plus optional Actor Power / control variables), flagging pairs
#'     whose absolute correlation exceeds \code{corThreshold} — such channel pairs
#'     must never co-enter one specification;
#'   \item a ranking of the candidates within each multi-candidate channel slot by
#'     the absolute \emph{partial} correlation with the dependent variable,
#'     controlling for each Government Effectiveness slot candidate in turn
#'     (so "best accountability index" has a data-driven definition before any
#'     model is fit).
#' }
#'
#' @param df Data.frame. A prepared panel (e.g. from \code{preparePanelData})
#'   containing the dependent variable and all candidate columns. Variable names
#'   may be given in original form (\code{"Rule of Law (VDem)"}) or in safe
#'   \code{make.names} form; both are matched.
#' @param depVar Character. Dependent-variable column: \code{"adoption"} for the
#'   adoption stage (point-biserial correlations) or \code{"ecp"} for stringency.
#' @param channelCandidates Named list of character vectors — one element per
#'   Institutional Quality Channel slot. Default: the canonical slots decided
#'   2026-06-12 (govEff / ruleOfLaw / accountability).
#' @param otherVars Character vector or NULL. Additional variables (Actor Power
#'   terms, controls) included in the pairwise table but not in slot rankings.
#' @param corThreshold Numeric. Absolute pairwise correlation above which two
#'   channel candidates are forbidden from co-entering a spec. Default: \code{0.8}.
#' @param method Character. Correlation method: \code{"pearson"} (default) or
#'   \code{"spearman"}.
#'
#' @return A list:
#'   \describe{
#'     \item{pairs}{Data.frame \code{var1, var2, r, n, forbidden} for all variable
#'       pairs in the screen.}
#'     \item{forbiddenPairs}{Subset of \code{pairs} where both variables are channel
#'       candidates and \code{|r| > corThreshold}.}
#'     \item{slotRankings}{Data.frame \code{slot, candidate, controllingFor,
#'       partialR, n} — partial correlation of each candidate (from multi-candidate
#'       slots) with \code{depVar}, controlling for each govEff candidate in turn.
#'       \code{controllingFor = "(none)"} rows give the raw correlation.}
#'     \item{bestPerSlot}{Named character vector: per multi-candidate slot, the
#'       candidate with the highest mean \code{|partialR|} across controls.}
#'     \item{corThreshold, depVar, method}{Echoed inputs.}
#'   }
#'
#' @importFrom stats complete.cases cor
#'
#' @export
#' @author Renato Rodrigues
computeChannelScreen <- function(df, depVar = "adoption",
                                 channelCandidates = list(
                                   govEff = c(
                                     "State Capacity PC1 (VDem)",
                                     "State Capacity PC2 (VDem)",
                                     "Government Effectiveness (WGI)"
                                   ),
                                   ruleOfLaw = "Rule of Law (VDem)",
                                   accountability = c(
                                     "Vertical Accountability (VDem)",
                                     "Horizontal Accountability (VDem)",
                                     "Diagonal Accountability (VDem)"
                                   )
                                 ),
                                 otherVars = NULL,
                                 corThreshold = 0.8,
                                 method = "pearson") {
  stopifnot(is.data.frame(df))

  # Match a display name to an existing df column (raw or make.names form).
  resolveCol <- function(nm) {
    if (nm %in% colnames(df)) return(nm)
    safe <- make.names(nm)
    if (safe %in% colnames(df)) return(safe)
    NA_character_
  }

  depCol <- resolveCol(depVar)
  if (is.na(depCol)) stop("computeChannelScreen: dependent variable '", depVar, "' not found in df.")

  # Resolve candidates, dropping (with a message) those absent from the panel.
  resolved <- lapply(channelCandidates, function(v) {
    cols <- vapply(v, resolveCol, character(1))
    if (any(is.na(cols))) {
      message("  [screen] not in panel, skipped: ", paste(v[is.na(cols)], collapse = ", "))
    }
    stats::setNames(cols[!is.na(cols)], v[!is.na(cols)])
  })
  channelCols <- unlist(resolved, use.names = TRUE)
  if (length(channelCols) < 2) stop("computeChannelScreen: fewer than 2 channel candidates found in df.")

  otherCols <- character(0)
  if (!is.null(otherVars)) {
    oc <- vapply(otherVars, resolveCol, character(1))
    otherCols <- stats::setNames(oc[!is.na(oc)], otherVars[!is.na(oc)])
  }

  displayName <- function(col) {
    allCols <- c(channelCols, otherCols)
    hits <- names(allCols)[allCols == col]
    if (length(hits) > 0) hits[1] else col
  }

  # ── 1. Pairwise correlation table ───────────────────────────────────────────
  screenCols <- unique(c(channelCols, otherCols))
  combos <- utils::combn(screenCols, 2, simplify = FALSE)
  pairs <- do.call(rbind, lapply(combos, function(pr) {
    cc <- stats::complete.cases(df[, pr, drop = FALSE])
    n <- sum(cc)
    r <- if (n >= 3) stats::cor(df[cc, pr[1]], df[cc, pr[2]], method = method) else NA_real_
    data.frame(
      var1 = displayName(pr[1]), var2 = displayName(pr[2]),
      r = r, n = n,
      forbidden = isTRUE(abs(r) > corThreshold) &&
        pr[1] %in% channelCols && pr[2] %in% channelCols,
      stringsAsFactors = FALSE
    )
  }))
  pairs <- pairs[order(-abs(pairs$r)), , drop = FALSE]
  rownames(pairs) <- NULL
  forbiddenPairs <- pairs[pairs$forbidden, , drop = FALSE]

  # ── 2. Partial-correlation slot rankings ────────────────────────────────────
  # Residualise x and y on the control columns via QR, then correlate residuals.
  partialCor <- function(xCol, controlCols) {
    use <- unique(c(xCol, depCol, controlCols))
    cc <- stats::complete.cases(df[, use, drop = FALSE])
    n <- sum(cc)
    if (n < length(controlCols) + 4) return(c(r = NA_real_, n = n))
    if (length(controlCols) == 0) {
      return(c(r = stats::cor(df[cc, xCol], df[cc, depCol], method = method), n = n))
    }
    cm <- cbind(`(Intercept)` = 1, as.matrix(df[cc, controlCols, drop = FALSE]))
    qrC <- qr(cm)
    resX <- qr.resid(qrC, df[cc, xCol])
    resY <- qr.resid(qrC, df[cc, depCol])
    c(r = stats::cor(resX, resY, method = method), n = n)
  }

  govEffCols <- resolved[["govEff"]] %||% character(0)
  multiSlots <- names(resolved)[vapply(resolved, length, integer(1)) > 1]
  rankRows <- list()
  for (slot in multiSlots) {
    for (i in seq_along(resolved[[slot]])) {
      cand <- resolved[[slot]][i]
      controlSets <- c(list("(none)" = character(0)))
      if (slot != "govEff" && length(govEffCols) > 0) {
        controlSets <- c(controlSets, as.list(stats::setNames(govEffCols, names(govEffCols))))
      }
      for (j in seq_along(controlSets)) {
        pc <- partialCor(cand, unlist(controlSets[[j]], use.names = FALSE))
        rankRows[[length(rankRows) + 1]] <- data.frame(
          slot = slot, candidate = names(cand),
          controllingFor = names(controlSets)[j],
          partialR = unname(pc["r"]), n = unname(pc["n"]),
          stringsAsFactors = FALSE
        )
      }
    }
  }
  slotRankings <- if (length(rankRows) > 0) do.call(rbind, rankRows) else
    data.frame(slot = character(0), candidate = character(0),
               controllingFor = character(0), partialR = numeric(0), n = integer(0))

  # ── 3. Best candidate per multi-candidate slot (mean |partialR|, controls only) ──
  bestPerSlot <- character(0)
  for (slot in multiSlots) {
    sub <- slotRankings[slotRankings$slot == slot &
                          slotRankings$controllingFor != "(none)", , drop = FALSE]
    if (nrow(sub) == 0) {
      sub <- slotRankings[slotRankings$slot == slot, , drop = FALSE]
    }
    if (nrow(sub) > 0) {
      agg <- tapply(abs(sub$partialR), sub$candidate, mean, na.rm = TRUE)
      bestPerSlot[slot] <- names(agg)[which.max(agg)]
    }
  }

  list(
    pairs = pairs,
    forbiddenPairs = forbiddenPairs,
    slotRankings = slotRankings,
    bestPerSlot = bestPerSlot,
    corThreshold = corThreshold,
    depVar = depVar,
    method = method
  )
}
