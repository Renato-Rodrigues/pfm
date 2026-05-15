#' Get historical REMIND data
#'
#' Gets historical REMIND variables from IEA and Ember.
#'
#' @param aggregate boolean, if true aggregates to region mapping defined at outputRegionMappingFile
#' @param outputRegionMappingFile string with path to output mapping file
#'
#' @return A list with historical [`magpie`][magclass::magclass] objects.
#' @author Renato Rodrigues
#'
#' @importFrom magclass getNames<- getYears getRegions new.magpie ndata
#' @importFrom madrat calcOutput toolAggregate toolGetMapping
#'
#' @export
#'
iamHistoricalData <- function(aggregate = FALSE, outputRegionMappingFile = "regionmappingH12.csv") {
  peVars <- c("pecoal", "peoil", "pegas", "pewin", "pesol", "peur", "pehyd", "pegeo", "petotal")
  seVars <- c("wind", "solar", "seel")
  feVars <- c("fe_indst_fossil", "fe_indst", "fe_seel", "fe_total")
  vars <- c(peVars, seVars, feVars)

  # --- Primary energy
  mappingHistPe <- tibble::tribble(
    ~histPe, ~remind,
    "PE|Coal (EJ/yr)", "pecoal",
    "PE|Oil (EJ/yr)", "peoil",
    "PE|Gas (EJ/yr)", "pegas",
    "PE|Wind|Electricity (EJ/yr)", "pewin",
    "PE|Solar|Electricity (EJ/yr)", "pesol",
    "PE|Uranium|Electricity (EJ/yr)", "peur",
    "PE|Hydro|Electricity (EJ/yr)", "pehyd",
    "PE|Geothermal|Electricity (EJ/yr)", "pegeo",
    "PE (EJ/yr)", "petotal"
  )
  histPe <- calcOutput("PE",
    subtype = "IEA", ieaVersion = "latest",
    aggregate = FALSE, warnNA = FALSE
  )[, , mappingHistPe$histPe] %>%
    toolAggregate(rel = mappingHistPe, dim = 3.1, from = "histPe", to = "remind")

  # --- Secondary energy
  mappingEmber <- tibble::tribble(
    ~ember, ~remind,
    "SE|Electricity|Solar (EJ/yr)", "solar",
    "SE|Electricity|Wind (EJ/yr)", "wind",
    "SE|Electricity (EJ/yr)", "seel"
  )
  genEmber <- calcOutput("Ember", subtype = "generation", aggregate = FALSE)[, , mappingEmber$ember] %>%
    toolAggregate(rel = mappingEmber, dim = 3.1, from = "ember", to = "remind") * 1e-3

  # --- Final energy
  mappingHistFe <- tibble::tribble(
    ~histFe, ~remind,
    "FE|Industry|Liquids|Fossil (EJ/yr)", "fe_indst_fossil",
    "FE|Industry|Gases|Fossil (EJ/yr)", "fe_indst_fossil",
    "FE|Industry|Solids|Fossil (EJ/yr)", "fe_indst_fossil",
    "FE|Industry (EJ/yr)", "fe_indst",
    "FE|Electricity (EJ/yr)", "fe_seel",
    "FE (EJ/yr)", "fe_total"
  )
  histFe <- calcOutput("FE",
    source = "IEA", ieaVersion = "latest",
    aggregate = FALSE, warnNA = FALSE
  )[, , mappingHistFe$histFe] %>%
    toolAggregate(rel = mappingHistFe, dim = 3.1, from = "histFe", to = "remind")

  # hist
  histYears <- sort(unique(c(
    getYears(histPe, as.integer = TRUE),
    getYears(genEmber, as.integer = TRUE),
    getYears(histFe, as.integer = TRUE)
  )))

  countries <- toolGetMapping("regionmappingH12.csv", type = "regional", where = "mappingfolder")$CountryCode
  histData <- new.magpie(cells_and_regions = countries, years = histYears, names = vars)
  histData[, getYears(histPe), peVars] <- histPe[, getYears(histPe), peVars]
  histData[, getYears(genEmber), seVars] <- genEmber[, getYears(genEmber), seVars]
  histData[, getYears(histFe), feVars] <- histFe[, getYears(histFe), feVars]

  if (aggregate) {
    outMappingFile <- toolGetMapping(outputRegionMappingFile, type = "regional", where = "mappingfolder")
    histData <- toolAggregate(
      x = histData, rel = outMappingFile,
      from = "CountryCode", to = "RegionCode", zeroWeight = "setNA"
    )
  }

  return(histData)
}
