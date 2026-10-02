# nolint start
#' A Run-Group's panel definition
#'
#' @description
#' Which years the historical panel spans, whether it is smoothed, and which edition of the IEA
#' World Energy Balances its energy drivers come from, and whether geothermal counts in the clean
#' baseload control (\code{geothermal}; design note 0005 A4). Every step that builds a historical or a
#' scenario panel must use the definition the group was fitted on: a group fitted on annual data
#' but projected (or coupled inside REMIND) on smoothed data would run to completion and be
#' wrong, with nothing to say so.
#'
#' The definition is resolved once per run by \code{\link{pfmRun}} and held in the
#' \code{pfm.panel} option, from which \code{\link{panelDataHistorical}} and
#' \code{\link{panelDataScenario}} take their defaults. The sweep records it in the group's
#' \code{manifest.json} (\code{panel}); every later step, the REMIND export and
#' \code{\link{iterativePFM}} read it from there.
#'
#' Resolution order for a group (\code{.pfmPanelDefForGroup}):
#' \enumerate{
#'   \item the \code{panel} recorded in the group's \code{manifest.json};
#'   \item the legacy definition, for a group that was swept before the record existed
#'     (\code{v5} and earlier: a \code{panel_hash} but no \code{panel});
#'   \item \code{config.yml}'s \code{panel} block, with the group's override, for a new group;
#'   \item the legacy definition.
#' }
#' Editing \code{config.yml} therefore never changes a group that has already been swept.
#'
#' @param def A panel definition (\code{firstYear}, \code{lastYear}, \code{movingAverage},
#'   \code{ieaVersion}, \code{geothermal}).
#' @return \code{pfmPanelDef()}: the active definition (the \code{pfm.panel} option, else the
#'   legacy one).
#' @export
#' @author Renato Rodrigues
pfmPanelDef <- function() {
  .pfmPanelDefNormalise(getOption("pfm.panel", .pfmPanelDefLegacy()))
}

# The definition every Run-Group up to v5 was built on: 2000-2022, a centred 5-year moving
# average, the 2024 IEA edition (madrat ieaVersion "default", complete to 2022), and hydro + nuclear
# only in the clean-baseload control.
#' @keywords internal
.pfmPanelDefLegacy <- function() {
  list(firstYear = 2000L, lastYear = 2022L, movingAverage = 5L, ieaVersion = "default",
       geothermal = FALSE)
}

# Accepts the config form (years = c(first, last) or a full vector) and the manifest form
# (firstYear / lastYear); fills gaps from the legacy definition and validates.
#' @keywords internal
.pfmPanelDefNormalise <- function(def) {
  base <- .pfmPanelDefLegacy()
  if (is.null(def)) return(base)
  if (!is.null(def$years)) {
    yrs <- as.integer(unlist(def$years))
    def$firstYear <- min(yrs)
    def$lastYear <- max(yrs)
    def$years <- NULL
  }
  out <- utils::modifyList(base, def[intersect(names(def), names(base))])
  out$firstYear <- as.integer(out$firstYear)
  out$lastYear <- as.integer(out$lastYear)
  out$movingAverage <- as.integer(out$movingAverage %||% 1L)
  out$ieaVersion <- as.character(out$ieaVersion)
  out$geothermal <- isTRUE(as.logical(out$geothermal))
  if (out$lastYear < out$firstYear) {
    stop("panel definition: lastYear (", out$lastYear, ") < firstYear (", out$firstYear, ")",
         call. = FALSE)
  }
  if (out$movingAverage < 1L) {
    stop("panel definition: movingAverage must be >= 1 (1 = annual values)", call. = FALSE)
  }
  if (!out$ieaVersion %in% c("default", "latest")) {
    stop("panel definition: ieaVersion must be 'default' (IEA 2024 edition, data to 2022) or ",
         "'latest' (2025 edition, data to 2023), got '", out$ieaVersion, "'", call. = FALSE)
  }
  out
}

# The panel years of a definition.
#' @keywords internal
.pfmPanelYears <- function(def = pfmPanelDef()) {
  seq.int(def$firstYear, def$lastYear)
}

# The moving-average window as panelDataHistorical() takes it: NULL = annual values.
#' @keywords internal
.pfmPanelMA <- function(def = pfmPanelDef()) {
  if (def$movingAverage <= 1L) NULL else def$movingAverage
}

# One line for logs and plans.
#' @keywords internal
.pfmPanelDefLabel <- function(def) {
  paste0(def$firstYear, "-", def$lastYear, ", ",
         if (def$movingAverage <= 1L) "annual values" else paste0(def$movingAverage, "-year moving average"),
         ", IEA ", if (identical(def$ieaVersion, "latest")) "2025 edition" else "2024 edition",
         if (isTRUE(def$geothermal)) ", geothermal in the baseload control" else "")
}

# The definition for a group, in the resolution order documented above, with where it came from.
#' @param groupDir Run-Group directory (may not exist yet).
#' @param configPanel The resolved \code{panel} block of config.yml for this group, or NULL.
#' @keywords internal
.pfmPanelDefForGroup <- function(groupDir, configPanel = NULL) {
  mf <- file.path(groupDir, "manifest.json")
  man <- if (file.exists(mf)) tryCatch(jsonlite::fromJSON(mf, simplifyVector = FALSE),
                                       error = function(e) NULL) else NULL
  if (!is.null(man[["panel"]])) {
    return(structure(.pfmPanelDefNormalise(man[["panel"]]), source = "manifest"))
  }
  if (!is.null(man[["panel_hash"]])) {
    return(structure(.pfmPanelDefLegacy(), source = "legacy (swept before the panel record)"))
  }
  if (!is.null(configPanel)) {
    return(structure(.pfmPanelDefNormalise(configPanel), source = "config"))
  }
  structure(.pfmPanelDefLegacy(), source = "legacy (no config panel block)")
}
# nolint end
