# nolint start
#' @title resolveRegionMapping
#' @description Resolves the `outputRegionMappingFile` sentinel value `"country"`
#' to the in-code country-identity mapping (`RegionCode == CountryCode`), generated
#' on demand by [`mrpfm::toolCountryIdentityMapping()`]. Any other value is
#' returned unchanged, so every existing call site keeps working. With the
#' identity mapping, `aggregate = TRUE` aggregates each country to itself and the
#' whole pipeline runs at country resolution - no hand-made mapping file needed.
#'
#' The panel builders combine this with an internal global-config scope
#' (see `.scopeRegionmapping`): `mrpfm::calcPolicyStringency()`'s coverage filter
#' reads the GLOBAL `madrat::getConfig("regionmapping")`, not the mapping passed
#' to `calcOutput()`, so without the scope a country-level run silently deletes
#' covered countries (Turkey, Greece, Chile, Korea, ...) through whole-region
#' coverage exclusion.
#'
#' @param outputRegionMappingFile character; a region mapping file name or the
#'   sentinel `"country"`.
#' @return The concrete mapping file name (character).
#' @author Renato Rodrigues
#' @seealso [`mrpfm::toolCountryIdentityMapping()`]
#' @export
#'
resolveRegionMapping <- function(outputRegionMappingFile) {
  if (identical(outputRegionMappingFile, "country")) {
    return(mrpfm::toolCountryIdentityMapping())
  }
  outputRegionMappingFile
}

# Internal: temporarily point the global madrat regionmapping config at `mapping`.
# Returns a restore function the caller must register with `on.exit(..., add = TRUE)`.
# Only used on the "country" sentinel path - existing call paths never touch the
# global config, so their behaviour (and madrat cache keys) stay byte-identical.
#' @keywords internal
.scopeRegionmapping <- function(mapping) {
  old <- tryCatch(madrat::getConfig("regionmapping"), error = function(e) NULL)
  if (is.null(old) || identical(old, mapping)) {
    return(function() invisible(NULL))
  }
  suppressMessages(madrat::setConfig(regionmapping = mapping))
  function() suppressMessages(madrat::setConfig(regionmapping = old))
}
# nolint end
