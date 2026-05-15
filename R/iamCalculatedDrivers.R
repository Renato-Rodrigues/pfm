#' Calculated drivers
#'
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
    cells_and_regions = getRegions(data), years = getYears(data), names = driverList, fill = 0
  )
  result[, , "Coal primary energy share"] <-
    data[, , "pecoal"] / data[, , "petotal"]
  result[, , "Oil/Gas primary energy share"] <-
    (data[, , "pegas"] + data[, , "peoil"]) / data[, , "petotal"]
  result[, , "Fossil share in Industry"] <-
    data[, , "fe_indst_fossil"] / data[, , "fe_indst"]
  result[, , "VRE share"] <-
    (data[, , "wind"] + data[, , "solar"]) / data[, , "seel"]
  result[, , "Electrification"] <-
    data[, , "fe_seel"] / data[, , "fe_total"]

  return(result)
}
