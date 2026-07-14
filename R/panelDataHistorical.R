# nolint start
#' @title panelDataHistorical
#' @description calculates the historical panel data output
#'
#' @param aggregate boolean to aggregate
#' @param outputRegionMappingFile mapping file for output regions, or the sentinel
#'   `"country"` for a country-resolution run (identity mapping generated in code by
#'   [`mrpfm::toolCountryIdentityMapping()`]; the global madrat regionmapping config is
#'   scoped around the data calls so the CAPMF coverage filter sees the right mapping)
#' @param y years to be calculated
#' @param coeff list of coefficients for actor power index calculation
#' @param includePolicyStringency logical; if TRUE, adds the CAPMF-based Policy
#'   Stringency Model outcomes ("Policy Stringency|Bulk", "Policy Stringency|Diffuse"
#'   and, when available, "Policy Stringency|Composite"; ADR 0036). Default FALSE so
#'   the carbon-price panel (and its end-year rule) is unchanged unless the PSM
#'   explicitly asks for it. Years the CAPMF source does not reach are NA-filled;
#'   preparePanelData later drops rows with a missing outcome.
#' @param psSectorResolution character; `"two"` (Bulk/Diffuse, default) or `"four"`
#'   (Electricity/Industry/Buildings/Transport) CAPMF sector outcomes.
#' @param psWeighting how CAPMF sectors are aggregated into Bulk/Diffuse: `"equal"`
#'   (default), `"ghg"` (EDGAR sectoral emissions), `"gdp"` (OECD value added by
#'   activity), `"fe"` (final-energy activity proxy), or explicit weights (a named
#'   numeric vector or a per-cell magpie), forwarded to
#'   [mrpfm::calcPolicyStringency()] — the T2 aggregation-sensitivity axis.
#'
#' @return Returns the combined magpie object for historical data
#' @author Renato Rodrigues
#'
#' @importFrom magclass mbind setNames add_dimension getYears time_interpolate new.magpie getItems getNames
#' @importFrom madrat calcOutput toolGetMapping
#' @export
#'
panelDataHistorical <- function(aggregate = TRUE,
                                y = 2000:2022,
                                outputRegionMappingFile = "regionmappingH12.csv",
                                movingAverage = 5,
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
                                ),
                                includePolicyStringency = FALSE,
                                psSectorResolution = "two",
                                psWeighting = "equal") {
  psSectorResolution <- match.arg(psSectorResolution, c("two", "four"))
  # "country" sentinel: generate the identity mapping in code and scope the global
  # madrat regionmapping config for the duration (the calcPolicyStringency coverage
  # filter reads the global config, not the regionmapping= argument).
  countryLevel <- identical(outputRegionMappingFile, "country")
  outputRegionMappingFile <- resolveRegionMapping(outputRegionMappingFile)
  if (countryLevel) {
    restoreRegionmapping <- .scopeRegionmapping(outputRegionMappingFile)
    on.exit(restoreRegionmapping(), add = TRUE)
  }
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

  # Policy Stringency (PSM outcomes, ADR 0036)
  if (isTRUE(includePolicyStringency)) {
    ps <- calcOutput("PolicyStringency",
      aggregate = aggregate, regionmapping = outputRegionMappingFile,
      sectorResolution = psSectorResolution, weighting = psWeighting
    )
    psVars <- if (identical(psSectorResolution, "four")) {
      c(Electricity = "Policy Stringency|Electricity", Industry = "Policy Stringency|Industry",
        Buildings = "Policy Stringency|Buildings", Transport = "Policy Stringency|Transport")
    } else {
      c(bulk = "Policy Stringency|Bulk", diffuse = "Policy Stringency|Diffuse",
        composite = "Policy Stringency|Composite")
    }
    psVars <- psVars[names(psVars) %in% getNames(ps)]
    psY <- intersect(y, getYears(ps, as.integer = TRUE))
    psFull <- new.magpie(getItems(ps, dim = 1), y, unname(psVars), fill = NA)
    for (nm in names(psVars)) {
      psFull[, psY, psVars[[nm]]] <- ps[, psY, nm]
    }
    out <- mbind(out, psFull)
  }

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

  # V-Dem state-capacity indicators (replace WGI Government Effectiveness)
  # Three variables are "bad when high" (corruption/neopatrimonialism) and are
  # inverted BEFORE normalisation so that the final scale is 0 = worst, 1 = best:
  #   Executive Corruption     → Policy Implementation (VDem)
  #   Political Corruption     → Absence of Corruption (VDem)
  #   Neopatrimonialism        → Meritocracy Index (VDem)
  scRaw <- calcOutput("VDem", subtype = "stateCapacity",
                      aggregate = aggregate, regionmapping = outputRegionMappingFile)
  scInt <- toolTimeInterpolation(scRaw, y)
  scInt <- mrpfm::toolImputeMedians(scInt)

  # Invert "bad when high" variables before normalisation
  invertMap <- c(
    "Executive Corruption (VDem)" = "Policy Implementation (VDem)",
    "Political Corruption (VDem)" = "Absence of Corruption (VDem)",
    "Neopatrimonialism (VDem)"    = "Meritocracy Index (VDem)"
  )
  for (rawName in names(invertMap)) {
    if (rawName %in% magclass::getNames(scInt)) {
      scInt[, , rawName] <- 1 - scInt[, , rawName]
    }
  }
  magclass::getNames(scInt) <- ifelse(
    magclass::getNames(scInt) %in% names(invertMap),
    invertMap[magclass::getNames(scInt)],
    magclass::getNames(scInt)
  )

  # Country-level bounds for normalisation (after inversion)
  scCnt <- calcOutput("VDem", subtype = "stateCapacity", aggregate = FALSE)
  for (rawName in names(invertMap)) {
    if (rawName %in% magclass::getNames(scCnt)) {
      scCnt[, , rawName] <- 1 - scCnt[, , rawName]
    }
  }
  magclass::getNames(scCnt) <- ifelse(
    magclass::getNames(scCnt) %in% names(invertMap),
    invertMap[magclass::getNames(scCnt)],
    magclass::getNames(scCnt)
  )
  scMin <- apply(scCnt, 3, min, na.rm = TRUE)
  scMax <- apply(scCnt, 3, max, na.rm = TRUE)

  scNorm <- toolNormalize(scInt, minVal = scMin, maxVal = scMax, targetRange = c(0, 1))
  out <- mbind(out, scNorm[, y, ])

  # V-Dem state-capacity PCA: fit and cache rotation in .pfm_env$sc_pca_rotation
  scPC <- computeVDemStateCapacityPC(scNorm[, y, ])
  if (!is.null(scPC)) out <- mbind(out, scPC)

  # Control Variables — SSP2 GDP/Population (mrdrivers). These return a single harmonized
  # series spanning history + projection (1960-2150, yearly), which gives recent historical
  # years (incl. 2024) that the *Past variants lacked. Restrict to the historical/training
  # window so the log min/max normalization bounds (reused as the projection clamp) are not
  # distorted by projected future values.
  .histEnd <- max(as.integer(gsub("y", "", as.character(y))))
  pop <- magclass::collapseNames(calcOutput("Population", scenario = "SSP2",
    aggregate = aggregate, regionmapping = outputRegionMappingFile))
  gdp <- magclass::collapseNames(calcOutput("GDP", scenario = "SSP2", average2020 = FALSE,
    aggregate = aggregate, regionmapping = outputRegionMappingFile))
  pop <- pop[, getYears(pop, as.integer = TRUE) <= .histEnd, ]
  gdp <- gdp[, getYears(gdp, as.integer = TRUE) <= .histEnd, ]
  gdpPerCapita <- magclass::collapseNames(
    gdp[, intersect(getYears(pop), getYears(gdp)), ] /
      pop[, intersect(getYears(pop), getYears(gdp)), ]
  )
  # Log-scale bounds from historical regional data (bounds must be regional, not country-level,
  # because pop/GDP/area are additive: a region's total exceeds any individual country)
  popLogMin       <- min(log(pop), na.rm = TRUE)
  popLogMax       <- max(log(pop), na.rm = TRUE)
  gdpLogMin       <- min(log(gdp), na.rm = TRUE)
  gdpLogMax       <- max(log(gdp), na.rm = TRUE)
  gdpPCLogMin     <- min(log(gdpPerCapita), na.rm = TRUE)
  gdpPCLogMax     <- max(log(gdpPerCapita), na.rm = TRUE)
  popNorm          <- toolNormalize(log(pop), minVal = popLogMin, maxVal = popLogMax, targetRange = c(0, 1))
  gdpNorm          <- toolNormalize(log(gdp), minVal = gdpLogMin, maxVal = gdpLogMax, targetRange = c(0, 1))
  gdpPerCapitaNorm <- toolNormalize(log(gdpPerCapita), minVal = gdpPCLogMin, maxVal = gdpPCLogMax, targetRange = c(0, 1))

  # GDP per Capita (Q-centred): within-income-quartile de-meaned.
  # Removes the cross-quartile income level correlated with governance indicators while
  # preserving within-quartile variation. Quartile breaks stored for scenario reuse.
  gdpPCQCentred <- computeGdpQCentred(gdpPerCapitaNorm[, y, ], storeBreaks = TRUE)

  # Land area
  landArea <- new.magpie(getRegions(pop), y, "LandArea", fill = NA) # nolint: undesirable_function_linter.
  landArea[, y, ] <- calcOutput("FAOLandArea", aggregate = aggregate, regionmapping = outputRegionMappingFile)
  landAreaLogMin  <- min(log(landArea), na.rm = TRUE)
  landAreaLogMax  <- max(log(landArea), na.rm = TRUE)
  landAreaNorm    <- toolNormalize(log(landArea), minVal = landAreaLogMin, maxVal = landAreaLogMax, targetRange = c(0, 1))

  # SSP extensions
  sspExt <- calcOutput("SSPextensions",
    subtype = "drivers_SSP2",
    aggregate = aggregate, regionmapping = outputRegionMappingFile
  )
  # IEA energy intensity — log1p for robustness against near-zero values;
  # fixed ceiling matches historical assumption, consistent with scenario panel
  energyIntensity <- histData[, y, "fe_total"] * 31.536 / (gdp[, y, ] / 1e6) # (EJ / million US$)
  energyIntensityNorm <- toolNormalize(log1p(energyIntensity), minVal = 0, maxVal = log1p(600), targetRange = c(0, 1))

  out <- mbind(
    out,
    setNames(popNorm[, y, ], "Population"),
    setNames(gdpNorm[, y, ], "GDP"),
    setNames(gdpPerCapitaNorm[, y, ], "GDP per Capita"),
    setNames(landAreaNorm[, y, ], "Land Area"),
    setNames(sspExt[, y, "SSP2.Population|Urban [Share]"] / 100, "Urban Population Share"),
    setNames(sspExt[, y, "SSP2.Gini Income Inequality Coefficient"] / 100, "Gini Income Inequality Coefficient"),
    setNames(sspExt[, y, "SSP2.Gender Inequality Index"], "Gender Inequality Index"),
    setNames(energyIntensityNorm[, y, ], "Energy Intensity"),
    setNames(gdpPCQCentred[, y, ],       "GDP per Capita (Q-centred)")
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
# nolint end
