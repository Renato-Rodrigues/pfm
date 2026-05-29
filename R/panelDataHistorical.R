#' @title panelDataHistorical
#' @description calculates the historical panel data output
#'
#' @param aggregate boolean to aggregate
#' @param outputRegionMappingFile mapping file for output regions
#' @param y years to be calculated
#' @param coeff list of coefficients for actor power index calculation
#'
#' @return Returns the combined magpie object for historical data
#' @author Renato Rodrigues
#'
#' @importFrom magclass mbind setNames add_dimension getYears time_interpolate
#' @importFrom madrat calcOutput toolGetMapping
#' @export
#'
panelDataHistorical <- function(aggregate = TRUE,
                                y = 2000:2022,
                                outputRegionMappingFile = "regionmappingH12.csv",
                                movingAverage = NULL,
                                coeff = list(
                                  bulk = list(
                                    actor_power = list(innov = 1, incumb = 1),
                                    innovators_power = list(vre = 1, elec = 0.6),
                                    incumbents_power = list(coal = 1, oilgas = 1, fossilInd = 0.5)
                                  ),
                                  diffuse = list(
                                    actor_power = list(innov = 1, incumb = 1),
                                    innovators_power = list(vre = 0.5, elec = 1, biofuel = 0.4),
                                    incumbents_power = list(coal = 0.2, oilgas = 0.2, fossilInd = 1)
                                  )
                                )) {
  out <- NULL

  # Carbon Price
  cp <- calcOutput("CarbonPrice",
    subtype = "effectivePrice",
    aggregate = aggregate, regionmapping = outputRegionMappingFile
  )
  out <- mbind(
    out,
    setNames(cp[, y, "bulk"], "Effective Carbon Price|Bulk"),
    setNames(cp[, y, "diffuse"], "Effective Carbon Price|Diffuse")
  )

  # Actor Power Index
  histData <- iamHistoricalData(aggregate = aggregate, outputRegionMappingFile = outputRegionMappingFile)
  histCalculatedDrivers <- iamCalculatedDrivers(histData)
  histAPI <- actorPowerIndex(histCalculatedDrivers, coeff = coeff)
  out <- mbind(out, histAPI[, y, c(
    "Actor Power Index|Bulk", "Actor Power Index|Diffuse",
    "Innovator Power|Bulk", "Innovator Power|Diffuse",
    "Incumbent Power|Bulk", "Incumbent Power|Diffuse"
  )])
  # Actor Power Index Drivers
  out <- mbind(out, histCalculatedDrivers[, y, ])

  # Institution Quality Drivers
  wgi <- calcOutput("WGIindicator", aggregate = aggregate, regionmapping = outputRegionMappingFile)
  if (!any(grepl("\\(WGI\\)", magclass::getNames(wgi)))) {
    magclass::getNames(wgi) <- paste0(magclass::getNames(wgi), " (WGI)")
  }
  wgiInt <- toolTimeInterpolation(wgi, y)
  wgiInt <- mrpfm::toolImputeMedians(wgiInt)

  # Calculate dynamic global country-level baseline bounds
  wgiCnt <- calcOutput("WGIindicator", aggregate = FALSE)
  if (!any(grepl("\\(WGI\\)", magclass::getNames(wgiCnt)))) {
    magclass::getNames(wgiCnt) <- paste0(magclass::getNames(wgiCnt), " (WGI)")
  }
  wgiMin <- apply(wgiCnt, 3, min, na.rm = TRUE)
  wgiMax <- apply(wgiCnt, 3, max, na.rm = TRUE)

  wgiNorm <- toolNormalize(wgiInt, minVal = wgiMin, maxVal = wgiMax, targetRange = c(0, 1))
  out <- mbind(out, wgiNorm[, y, ])

  # V-Dem governance indicators (rule of law and accountability)
  vdem <- calcOutput("VDem", aggregate = aggregate, regionmapping = outputRegionMappingFile)
  vdemInt <- toolTimeInterpolation(vdem, y)
  # Guard against regions that survive aggregation with < 2 valid data points
  # (toolTimeInterpolation skips those, leaving NAs that drop rows in preparePanelData).
  vdemInt <- mrpfm::toolImputeMedians(vdemInt)

  # Calculate dynamic global country-level baseline bounds
  vdemCnt <- calcOutput("VDem", aggregate = FALSE)
  vdemMin <- apply(vdemCnt, 3, min, na.rm = TRUE)
  vdemMax <- apply(vdemCnt, 3, max, na.rm = TRUE)

  vdemNorm <- toolNormalize(vdemInt, minVal = vdemMin, maxVal = vdemMax, targetRange = c(0, 1))
  out <- mbind(out, vdemNorm[, y, ])

  # Control Variables
  # GDP per capita
  pop <- calcOutput("PopulationPast", aggregate = aggregate, regionmapping = outputRegionMappingFile)
  gdp <- calcOutput("GDPPast", aggregate = aggregate, regionmapping = outputRegionMappingFile)
  gdpPerCapita <- magclass::collapseNames(
    gdp[, intersect(getYears(pop), getYears(gdp)), ] /
      pop[, intersect(getYears(pop), getYears(gdp)), ]
  )
  popNorm <- toolNormalize(pop, minVal = 0, maxVal = 1500) # 1.5 billion normalized to 1
  gdpNorm <- toolNormalize(gdp, minVal = 0, maxVal = 30000000) # 30 trillion normalized to 1
  gdpPerCapitaNorm <- toolNormalize(gdpPerCapita, minVal = 0, maxVal = 150000) # 150k normalized to 1
  # Land area
  landArea <- new.magpie(getRegions(pop), y, "LandArea", fill = NA) # nolint: undesirable_function_linter.
  landArea[, y, ] <- calcOutput("FAOLandArea", aggregate = aggregate, regionmapping = outputRegionMappingFile)
  landAreaNorm <- toolNormalize(landArea, minVal = 0, maxVal = 1500000) # 1.5 million 1000 ha normalized to 1
  # 1.5 million 1000 ha = 15 million square kilometers =~ Largest country in the World (Russia)

  # SSP extensions
  sspExt <- calcOutput("SSPextensions",
    subtype = "drivers_SSP2",
    aggregate = aggregate, regionmapping = outputRegionMappingFile
  )
  # IEA energy intensity
  energyIntensity <- histData[, y, "fe_total"] * 31.536 / (gdp[, y, ] / 1e6) # (EJ / million US$)
  energyIntensityNorm <- toolNormalize(energyIntensity, minVal = 0, maxVal = 600) # 600 normalized to 1

  out <- mbind(
    out,
    setNames(popNorm[, y, ], "Population"),
    setNames(gdpNorm[, y, ], "GDP"),
    setNames(gdpPerCapitaNorm[, y, ], "GDP per Capita"),
    setNames(landAreaNorm[, y, ], "Land Area"),
    setNames(sspExt[, y, "SSP2.Population|Urban [Share]"] / 100, "Urban Population Share"),
    setNames(sspExt[, y, "SSP2.Gini Income Inequality Coefficient"] / 100, "Gini Income Inequality Coefficient"),
    setNames(sspExt[, y, "SSP2.Gender Inequality Index"], "Gender Inequality Index"),
    setNames(energyIntensityNorm[, y, ], "Energy Intensity")
  )

  if (!is.null(movingAverage) && is.numeric(movingAverage) && movingAverage > 1) {
    if (dim(out)[2] > 1) {
      # Apply centered moving average correction along the year dimension
      # It smoothly shrinks the window at the boundaries to prevent NA losses
      sma <- function(v, k) {
        if (all(is.na(v))) return(v)
        half <- floor(k / 2)
        n <- length(v)
        sapply(seq_len(n), function(i) mean(v[max(1, i - half):min(n, i + half)], na.rm = TRUE))
      }
      smoothedOut <- out
      smoothedOut[, , ] <- aperm(apply(as.array(out), c(1, 3), sma, k = movingAverage), c(2, 1, 3))
      out <- smoothedOut
    }
  }

  return(out)
}
