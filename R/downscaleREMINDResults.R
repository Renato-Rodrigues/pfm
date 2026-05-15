#' Downscale REMIND data
#'
#' Reads REMIND variables from a GDX file, downscales to country level and aggregates to alternative region mapping.
#'
#' @param aggregate boolean, if true aggregates to region mapping defined at outputRegionMappingFile
#' @param gdxFile string with path to the GDX file
#' @param gdxRegionMappingFile string with path to remind gdx mapping file
#' @param outputRegionMappingFile string with path to output mapping file
#'
#' @return A [`magpie`][magclass::magclass] object with downscaled REMIND data.
#' @author Renato Rodrigues
#'
#' @importFrom magclass getNames<- getYears getRegions new.magpie ndata
#' @importFrom gdx readGDX
#'
#' @export
#'
downscaleREMINDResults <- function(gdxFile = "fulldata.gdx", aggregate = FALSE,
                                   gdxRegionMappingFile = "regionmappingH12.csv",
                                   outputRegionMappingFile = "regionmappingH12.csv") {
  # REMIND mapping file
  remindMappingFile <- toolGetMapping(gdxRegionMappingFile, type = "regional", where = "mappingfolder")

  # Years
  yearsList <- c(seq(2005, 2060, 5), seq(2070, 2110, 10), 2130, 2150)

  # Read remindData from gdx
  peVars <- c("pecoal", "peoil", "pegas", "pewin", "pesol", "peur", "pehyd", "pegeo", "petotal")
  seVars <- c("wind", "solar", "seel")
  feVars <- c("fe_indst_fossil", "fe_indst", "fe_seel", "fe_total")
  vars <- c(peVars, seVars, feVars)

  # --- Primary energy
  prodPe <- gdx::readGDX(gdxFile, "vm_prodPe", field = "l", react = "silent", restore_zeros = FALSE)[, yearsList, ]
  remindData <- new.magpie(cells_and_regions = getRegions(prodPe), years = yearsList, names = vars, fill = 0)
  remindData[, , peVars[!peVars == "petotal"]] <- prodPe[, , peVars[!peVars == "petotal"]]
  remindData[, , "petotal"] <- setNames(dimSums(prodPe, dim = 3), "petotal")

  # --- Secondary energy
  prodSe <- gdx::readGDX(gdxFile, "vm_prodSe", field = "l", react = "silent", restore_zeros = FALSE)[, yearsList, ]
  remindData[, , "wind"] <- dimSums(prodSe[, , c("windon", "windoff")], dim = 3, na.rm = TRUE)
  remindData[, , "solar"] <- dimSums(prodSe[, , c("spv", "csp")], dim = 3, na.rm = TRUE)
  remindData[, , "seel"] <- dimSums(prodSe[, , "seel"], dim = 3, na.rm = TRUE)

  # --- Final energy
  demFeSector <- gdx::readGDX(gdxFile, "vm_demFeSector",
    field = "l",
    react = "silent", restore_zeros = FALSE
  )[, yearsList, ]
  remindData[, , "fe_indst_fossil"] <- dimSums(demFeSector[, , "indst"][, , c("seliqfos", "segafos", "sesofos")],
    dim = 3, na.rm = TRUE
  )
  remindData[, , "fe_indst"] <- dimSums(demFeSector[, , "indst"], dim = 3, na.rm = TRUE)
  remindData[, , "fe_seel"] <- dimSums(demFeSector[, , "seel"], dim = 3, na.rm = TRUE)
  remindData[, , "fe_total"] <- dimSums(demFeSector, dim = 3, na.rm = TRUE)

  # Conversion weights
  weightRemindData <- new.magpie(
    cells_and_regions = remindMappingFile$CountryCode,
    years = getYears(remindData), names = vars, fill = 0
  )

  # Fetch historical data outputs
  histData <- iamHistoricalData()
  weightRemindData <- toolTimeInterpolation(histData, interpolatedYears = yearsList)
  # Remove negative values (EST has negative values for pecoal)
  weightRemindData[weightRemindData < 0] <- 0

  # output object
  out <- toolAggregate(
    x = remindData, weight = weightRemindData,
    rel = remindMappingFile, from = "RegionCode", to = "CountryCode", zeroWeight = "setNA"
  )
  out[is.na(out)] <- 0
  out <- toolCountryFill(out, fill = 0)

  if (aggregate) {
    outMappingFile <- toolGetMapping(outputRegionMappingFile, type = "regional", where = "mappingfolder")
    out <- toolAggregate(
      x = out, rel = outMappingFile,
      from = "CountryCode", to = "RegionCode", zeroWeight = "setNA"
    )
  }

  return(out)
}
