#' @title toolScenarioPanelData
#' @description calculates the scenario panel data output
#'
#' @param gdxFile gdx file
#' @param aggregate boolean to aggregate
#' @param gdxRegionMappingFile mapping file for gdx regions
#' @param outputRegionMappingFile mapping file for output regions
#' @param y years to be calculated
#' @param coeff list of coefficients for actor power index calculation
#'
#' @return Returns the combined magpie object for scenario data
#' @author Renato Rodrigues
#'
#' @importFrom magclass mbind setNames add_dimension getYears
#' @importFrom madrat calcOutput toolGetMapping
#' @export
#'
panelDataScenario <- function(gdxFile = "fulldata.gdx", aggregate = TRUE,
                              y = c(seq(2005, 2060, 5), seq(2070, 2110, 10), 2130, 2150),
                              gdxRegionMappingFile = "regionmappingH12.csv",
                              outputRegionMappingFile = "regionmappingH12.csv",
                              coeff = list(
                                bulk = list(
                                  actor_power = list(innov = 1, incumb = 1),
                                  innovators_power = list(vre = 1, elec = 0.6),
                                  incumbents_power = list(coal = 1, oilgas = 1, fossilInd = 0.5)
                                ),
                                diffuse = list(
                                  actor_power = list(innov = 1, incumb = 1),
                                  innovators_power = list(vre = 0.5, elec = 1),
                                  incumbents_power = list(coal = 0.2, oilgas = 0.2, fossilInd = 1)
                                )
                              )) {
  out <- NULL

  # Carbon Price
  # TODO: read from REMIND to compare results

  # Actor Power Index
  modelDownscale <- downscaleREMINDResults(
    gdxFile = gdxFile, aggregate = aggregate,
    gdxRegionMappingFile = gdxRegionMappingFile,
    outputRegionMappingFile = outputRegionMappingFile
  )
  modelCalculatedDrivers <- iamCalculatedDrivers(modelDownscale)
  modelAPI <- actorPowerIndex(modelCalculatedDrivers, coeff)
  out <- mbind(out, modelAPI[, y, c(
    "Actor Power Index|Bulk", "Actor Power Index|Diffuse",
    "Incumbent Power|Bulk", "Incumbent Power|Diffuse"
  )])
  # Actor Power Index Drivers
  out <- mbind(out, modelCalculatedDrivers[, y, ])

  # Institution Quality Drivers
  sspExt <- calcOutput("SSPextensions",
    subtype = "drivers_SSP2",
    aggregate = aggregate, regionmapping = outputRegionMappingFile
  )
  # wgi - simple assumtpion - keep it constant
  wgi <- calcOutput("WGIindicator", aggregate = aggregate, regionmapping = outputRegionMappingFile)
  wgiInt <- toolTimeInterpolation(wgi, y)
  wgiNorm <- toolNormalize(wgiInt, targetRange = c(0, 1))
  out <- mbind(
    out,
    wgiNorm[, y, "Voice and Accountability"],
    wgiNorm[, y, "Political Stability"],
    wgiNorm[, y, "Regulatory Quality"],
    setNames(sspExt[, y, "SSP2.Rule-of-Law Index"], "Rule of Law"),
    setNames(sspExt[, y, "SSP2.Governance Index|Government Effectiveness"], "Government Effectiveness"),
    setNames(sspExt[, y, "SSP2.Governance Index|Control of Corruption"], "Control of Corruption")
  )

  # Control Variables
  # GDP per capita
  pop <- calcOutput("Population",
    scenario = c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5"),
    aggregate = aggregate, regionmapping = outputRegionMappingFile
  )
  gdp <- calcOutput("GDP",
    scenario = c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5"),
    aggregate = aggregate, regionmapping = outputRegionMappingFile
  )
  gdpPerCapita <- magclass::collapseNames(
    gdp[, intersect(getYears(pop), getYears(gdp)), ] /
      pop[, intersect(getYears(pop), getYears(gdp)), ]
  )
  popNorm <- toolNormalize(pop, minVal = 0, maxVal = 1500) # 1.5 billion normalized to 1
  gdpNorm <- toolNormalize(gdp, minVal = 0, maxVal = 30000000) # 30 trillion normalized to 1
  gdpPerCapitaNorm <- toolNormalize(gdpPerCapita, minVal = 0, maxVal = 150000) # 150k normalized to 1
  # Land area
  landArea <- new.magpie(getRegions(pop), y, "LandArea", fill = NA)
  landArea[, y, ] <- calcOutput("FAOLandArea", aggregate = aggregate, regionmapping = outputRegionMappingFile)
  landAreaNorm <- toolNormalize(landArea, minVal = 0, maxVal = 1500000) # 1.5 million 1000 ha normalized to 1
  # 1.5 million 1000 ha = 15 million square kilometers =~ Largest country in the World (Russia)
  # IEA energy intensity
  energyIntensity <- modelDownscale[, y, "fe_total"] * 31.536 / (gdp[, y, ] / 1e6) # (EJ / million US$)
  energyIntensityNorm <- toolNormalize(energyIntensity, minVal = 0, maxVal = 600) # 600 normalized to 1

  out <- mbind(
    out,
    setNames(popNorm[, y, "SSP2"], "Population"),
    setNames(gdpNorm[, y, "SSP2"], "GDP"),
    setNames(gdpPerCapitaNorm[, y, "SSP2"], "GDP per Capita"),
    setNames(landAreaNorm[, y, ], "Land Area"),
    setNames(sspExt[, y, "SSP2.Population|Urban [Share]"] / 100, "Urban Population Share"),
    setNames(sspExt[, y, "SSP2.Gini Income Inequality Coefficient"] / 100, "Gini Income Inequality Coefficient"),
    setNames(sspExt[, y, "SSP2.Gender Inequality Index"], "Gender Inequality Index"),
    setNames(energyIntensityNorm[, y, "SSP2"], "Energy Intensity")
  )

  return(out)
}
