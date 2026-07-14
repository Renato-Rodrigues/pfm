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
#' @importFrom magclass getItems getYears new.magpie dimSums setNames
#' @importFrom gdx readGDX
#'
#' @export
#'
downscaleREMINDResults <- function(gdxFile = "fulldata.gdx", aggregate = FALSE,
                                   gdxRegionMappingFile = "regionmappingH12.csv",
                                   outputRegionMappingFile = "regionmappingH12.csv") {
  outputRegionMappingFile <- resolveRegionMapping(outputRegionMappingFile)
  # REMIND mapping file
  remindMappingFile <- toolGetMapping(gdxRegionMappingFile, type = "regional", where = "mappingfolder")

  # Years
  yearsList <- c(seq(2005, 2060, 5), seq(2070, 2110, 10), 2130, 2150)

  # Read remindData from gdx
  peVars <- c("pecoal", "peoil", "pegas", "pewin", "pesol", "peur", "pehyd", "pegeo", "petotal")
  seVars <- c("wind", "solar", "seel")
  feVars <- c("fe_indst_fossil", "fe_indst", "fe_seel", "fe_total", "fe_liqbio_tran", "fe_liqtran")
  vars <- c(peVars, seVars, feVars)

  # --- Primary energy
  prodPe <- gdx::readGDX(gdxFile, "vm_prodPe", field = "l", react = "silent", restore_zeros = FALSE)[, yearsList, ]
  remindData <- new.magpie(cells_and_regions = getItems(prodPe, dim = 1), years = yearsList, names = vars, fill = 0)
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

  # Transport liquid biofuels — use vm_demFeSector_afterTax (seliqbio in trans, all fuel types + markets)
  demFeSectorTax <- gdx::readGDX(gdxFile, "vm_demFeSector_afterTax",
    field = "l", react = "silent", restore_zeros = FALSE
  )[, yearsList, ]
  remindData[, , "fe_liqbio_tran"] <- dimSums(demFeSectorTax[, , "trans"][, , "seliqbio"], dim = 3, na.rm = TRUE)
  feLiqfosRemind <- dimSums(demFeSectorTax[, , "trans"][, , "seliqfos"], dim = 3, na.rm = TRUE)
  remindData[, , "fe_liqtran"] <- remindData[, , "fe_liqbio_tran"] + feLiqfosRemind

  # Historical country data used as IPF prior (country universe from the same mapping as the GDX)
  histData <- iamHistoricalData(gdxRegionMappingFile = gdxRegionMappingFile)
  histData[histData < 0] <- 0

  # IPF group specification: each group's component vars and denominator/total var.
  # IPF balances historical country fuel-mix structure against H12 REMIND totals,
  # guaranteeing share consistency (numerator <= denominator) at every country.
  groups <- list(
    pe       = list(vars = c("pecoal", "peoil", "pegas", "pewin", "pesol", "peur", "pehyd", "pegeo"),
                    denom = "petotal"),
    se       = list(vars = c("wind", "solar"),
                    denom = "seel"),
    fe_indst = list(vars = c("fe_indst_fossil"),
                    denom = "fe_indst"),
    fe_total   = list(vars = c("fe_seel"),
                      denom = "fe_total"),
    fe_tran_liq = list(vars = c("fe_liqbio_tran"),
                       denom = "fe_liqtran")
  )

  out <- mrpfm::toolIPFDownscale(
    prior   = histData,
    remind  = remindData,
    groups  = groups,
    mapping = remindMappingFile
  )
  out[is.na(out)] <- 0
  out <- toolCountryFill(out, fill = 0)

  if (aggregate) {
    outMappingFile <- toolGetMapping(outputRegionMappingFile, type = "regional", where = "mappingfolder")
    out <- toolAggregate(
      x = out, rel = outMappingFile,
      from = "CountryCode", to = "RegionCode", zeroWeight = "setNA"
    )
    out[is.na(out)] <- 0
  }

  return(out)
}
