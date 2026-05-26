#' Calculated drivers
#'
#' @param data A [`magpie`][magclass::magclass] object.
#' @return A [`magpie`][magclass::magclass] object with calculated drivers
#' @author Renato Rodrigues
#'
#' @importFrom magclass getNames<- getYears getRegions new.magpie ndata clean_magpie
#'
#' @export
#'
iamCalculatedDrivers <- function(data) {
  # add calculated drivers
  driverList <- c(
    "Coal primary energy share", "Oil/Gas primary energy share",
    "Fossil share in Industry", "VRE share", "Electrification"
  )
  result <- new.magpie(
    cells_and_regions = getRegions(data), # nolint: undesirable_function_linter.
    years = getYears(data), names = driverList, fill = 0
  )
  # Safe division helper
  safe_div <- function(num, den) {
    val <- num / den
    val[!is.finite(val)] <- 0
    return(val)
  }
  
  result[, , "Coal primary energy share"] <-
    safe_div(data[, , "pecoal"], data[, , "petotal"])
  result[, , "Oil/Gas primary energy share"] <-
    safe_div(data[, , "pegas"] + data[, , "peoil"], data[, , "petotal"])
  result[, , "Fossil share in Industry"] <-
    safe_div(data[, , "fe_indst_fossil"], data[, , "fe_indst"])
  result[, , "VRE share"] <-
    safe_div(data[, , "wind"] + data[, , "solar"], data[, , "seel"])
  result[, , "Electrification"] <-
    safe_div(data[, , "fe_seel"], data[, , "fe_total"])

  return(result)
}
