#' @title computeSeamDiagnostics
#' @description Builds the Seam-Continuity Table: for every variable present in
#' both the historical and the scenario panel and every region, the jump between
#' the last non-NA historical value and the first non-NA scenario value,
#' normalized by the variable's historical standard deviation (across all
#' regions and years). The panel-data report's primary instrument for catching
#' historical/scenario misalignment (e.g. a frozen-reference bug in a derived
#' variable shows up as a block of large same-signed jumps).
#'
#' @param histMag \code{magpie}. Historical panel (e.g. from \code{panelDataHistorical}).
#' @param scenMag \code{magpie}. Scenario panel (e.g. from \code{panelDataScenario}).
#' @param variables Character vector or NULL. Variables to check; default: all
#'   variables present in both panels.
#' @param flagThreshold Numeric. |normalized jump| above which a row is flagged.
#'   Default: \code{0.5} (half a historical standard deviation).
#'
#' @return Data.frame, sorted by |normJump| descending:
#'   \code{variable, region, histYear, scenYear, histValue, scenValue, jump,
#'   normJump, flagged}.
#'
#' @importFrom stats sd
#'
#' @export
#' @author Renato Rodrigues
computeSeamDiagnostics <- function(histMag, scenMag, variables = NULL,
                                   flagThreshold = 0.5) {
  histVars <- magclass::getNames(histMag)
  scenVars <- magclass::getNames(scenMag)
  shared <- intersect(histVars, scenVars)
  if (!is.null(variables)) shared <- intersect(shared, variables)
  if (length(shared) == 0) stop("computeSeamDiagnostics: no shared variables between panels.")

  regions <- intersect(magclass::getRegions(histMag), magclass::getRegions(scenMag))
  histYears <- sort(magclass::getYears(histMag, as.integer = TRUE))
  scenYears <- sort(magclass::getYears(scenMag, as.integer = TRUE))

  # Anchor year for the continuity test. When the panels overlap in time (the
  # scenario panel commonly republishes recent years, e.g. 2005-2020, alongside
  # future years), comparing last-historical against first-scenario would span a
  # multi-year gap AND a source change — not a continuity test. Use the last
  # COMMON year as a same-year anchor; only fall back to the gap comparison when
  # the panels do not overlap at all.
  commonYears <- intersect(histYears, scenYears)
  overlap <- length(commonYears) > 0
  anchorYear <- if (overlap) max(commonYears) else NA_integer_

  rows <- list()
  for (v in shared) {
    hArr <- as.array(histMag[regions, , v])[, , 1, drop = FALSE]
    sArr <- as.array(scenMag[regions, , v])[, , 1, drop = FALSE]
    dim(hArr) <- dim(hArr)[1:2]
    dim(sArr) <- dim(sArr)[1:2]
    rownames(hArr) <- regions
    rownames(sArr) <- regions
    colnames(hArr) <- histYears
    colnames(sArr) <- scenYears

    histSd <- stats::sd(hArr, na.rm = TRUE)

    for (r in regions) {
      hVals <- hArr[r, ]
      sVals <- sArr[r, ]
      if (overlap) {
        # Same-year continuity test at the anchor year.
        hv <- hVals[as.character(anchorYear)]
        sv <- sVals[as.character(anchorYear)]
        if (!is.finite(hv) || !is.finite(sv)) next
        hYr <- anchorYear
        sYr <- anchorYear
        jump <- sv - hv
      } else {
        hIdx <- which(is.finite(hVals))
        sIdx <- which(is.finite(sVals))
        if (length(hIdx) == 0 || length(sIdx) == 0) next
        hYr <- histYears[max(hIdx)]
        sYr <- scenYears[min(sIdx)]
        hv <- hVals[max(hIdx)]
        sv <- sVals[min(sIdx)]
        jump <- sv - hv
      }
      normJump <- if (is.finite(histSd) && histSd > 0) jump / histSd else NA_real_
      rows[[length(rows) + 1L]] <- data.frame(
        variable = v, region = r,
        histYear = hYr, scenYear = sYr,
        histValue = unname(hv), scenValue = unname(sv),
        jump = unname(jump), normJump = unname(normJump),
        flagged = is.finite(normJump) && abs(normJump) > flagThreshold,
        stringsAsFactors = FALSE
      )
    }
  }
  out <- do.call(rbind, rows)
  out <- out[order(-abs(out$normJump)), , drop = FALSE]
  rownames(out) <- NULL
  out
}
