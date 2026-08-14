# nolint start
#' Resolve a region mapping, preferring the configured mappingfolder
#'
#' @description
#' Delegates to \code{\link[mrpfm]{toolPFMMapping}}. The mappings and the resolution
#' logic live in \pkg{mrpfm} — the data layer, which \pkg{pfm} depends on — so there
#' is exactly one copy of each mapping and one search order for the whole stack.
#'
#' This wrapper exists so pfm's own call sites read naturally and do not each have to
#' name the data package. See \code{\link[mrpfm]{toolPFMMapping}} for the search order
#' and for why it is closed rather than deferring to madrat's package search.
#'
#' @param name Mapping file name, e.g. \code{"regionmapping_21_EU11.csv"}.
#' @param type madrat mapping type; \code{"regional"} for region mappings.
#' @param verbose Logical. Report when the bundled fallback is used.
#'
#' @return The mapping data.frame.
#' @seealso \code{\link[mrpfm]{toolPFMMapping}}
#' @author Renato Rodrigues
#' @export
pfmGetMapping <- function(name, type = "regional", verbose = TRUE) {
  mrpfm::toolPFMMapping(name, type = type, verbose = verbose)
}
# nolint end
