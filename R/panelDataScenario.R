# nolint start
#' @title toolScenarioPanelData
#' @description calculates the scenario panel data output
#'
#' @param gdxFile gdx file
#' @param aggregate boolean to aggregate
#' @param gdxRegionMappingFile mapping file for gdx regions
#' @param outputRegionMappingFile mapping file for output regions, or the sentinel
#'   `"country"` (see [`panelDataHistorical()`]; projection/REMIND coupling normally
#'   stays at REMIND resolution - the sentinel here is for symmetry, not the default)
#' @param y years to be calculated
#' @param histYears,movingAverage,ieaVersion,geothermal the historical panel the scenario is
#'   harmonised to: its years, moving-average window, IEA edition and clean-baseload definition
#'   (\code{geothermal} also applies to the scenario's own control). Default: the active panel definition
#'   (\code{\link{pfmPanelDef}}), which must be the one the Run-Group was fitted on.
#' @param ssp \code{"SSP1"} ... \code{"SSP5"}: the SSP of GDP, population and the SSP-extension
#'   series (Government Effectiveness, Control of Corruption, Rule of Law, urban share, Gini,
#'   gender inequality). Must match the REMIND run's \code{cm_GDPpopScen}. Default \code{"SSP2"}.
#' @param institutions how institution series without an SSP projection are projected:
#'   \code{"storyline"} (default; for SSP2 identical to \code{"convergence"}), \code{"convergence"}
#'   or \code{"hold"}; see
#'   \code{\link{pfmInstitutionProjection}}.
#' @param histCache Path of a file caching the harmonisation's historical panel, or \code{NULL}
#'   (default: none). The historical panel does not depend on the gdx, so a coupled run, which
#'   calls this once per REMIND iteration, builds it once (design note 0005 E17/F7, about a third
#'   of each call after the 2026-10-02 speed-ups). The file also carries the state that
#'   \code{panelDataHistorical} leaves for the scenario panel (the GDP-per-capita quartile fit and
#'   the V-Dem state-capacity rotation), and a key of every argument that shapes the panel; a
#'   file with another key is rebuilt.
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
                              harmonizeScenario = TRUE,
                              movingAverage = .pfmPanelMA(),
                              histYears = .pfmPanelYears(),
                              ieaVersion = pfmPanelDef()$ieaVersion,
                              geothermal = pfmPanelDef()$geothermal,
                              ssp = "SSP2",
                              institutions = "storyline",
                              histCache = NULL,
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
                              harmonizeScenarioYear = 2040) {
  if (!ssp %in% paste0("SSP", 1:5)) stop("panelDataScenario: unknown ssp '", ssp, "'", call. = FALSE)
  institutions <- match.arg(institutions, c("storyline", "convergence", "hold"))
  # "country" sentinel: see panelDataHistorical (identity mapping + scoped config)
  countryLevel <- identical(outputRegionMappingFile, "country")
  outputRegionMappingFile <- resolveRegionMapping(outputRegionMappingFile)
  if (countryLevel) {
    restoreRegionmapping <- .scopeRegionmapping(outputRegionMappingFile)
    on.exit(restoreRegionmapping(), add = TRUE)
  }
  out <- NULL

  # Carbon Price
  # Read from REMIND to compare results

  # Actor Power Index
  modelDownscale <- downscaleREMINDResults(
    gdxFile = gdxFile, aggregate = aggregate,
    gdxRegionMappingFile = gdxRegionMappingFile,
    outputRegionMappingFile = outputRegionMappingFile
  )
  modelCalculatedDrivers <- iamCalculatedDrivers(modelDownscale, geothermal = geothermal)
  # See the note in panelDataHistorical(): the RAW population series, not the
  # normalised driver. Fetched once here and reused for the controls below, so the
  # two can never diverge. The uncollapsed object is kept because the control block
  # slices it by [, , ssp].
  .popRaw <- calcOutput("Population", scenario = ssp,
    aggregate = aggregate, regionmapping = outputRegionMappingFile
  )
  # vm_prodPe is TWa; the historical panel is EJ. Unconverted, every per-capita driver fell
  # to 1/31.5 of its historical scale once the harmonisation offset faded (PITFALLS 28).
  modelAPI <- actorPowerIndex(modelCalculatedDrivers, coeff = coeff,
                              energyPerCapita = .energyPerCapita(
                                modelDownscale, magclass::collapseNames(.popRaw),
                                peUnit = "TWa"))
  out <- mbind(out, modelAPI[, y, c(
    "Actor Power Index|Bulk", "Actor Power Index|Diffuse",
    "Innovator Power|Bulk", "Innovator Power|Diffuse",
    "Incumbent Power|Bulk", "Incumbent Power|Diffuse",
    "Innovator Power pc|Bulk", "Innovator Power pc|Diffuse",
    "Incumbent Power pc|Bulk", "Incumbent Power pc|Diffuse"
  )])
  # Actor Power Index Drivers
  out <- mbind(out, modelCalculatedDrivers[, y, ])

  # Institution Quality Drivers
  sspExt <- calcOutput("SSPextensions",
    subtype = paste0("drivers_", ssp),
    aggregate = aggregate, regionmapping = outputRegionMappingFile
  )
  # Voice and Accountability, Political Stability, Regulatory Quality: no SSP projection exists;
  # projected by the declared `institutions` rule (pfmInstitutionProjection).
  wgi <- calcOutput("WGIindicator", aggregate = aggregate, regionmapping = outputRegionMappingFile)
  if (!any(grepl("\\(WGI\\)", magclass::getNames(wgi)))) {
    magclass::getNames(wgi) <- paste0(magclass::getNames(wgi), " (WGI)")
  }
  wgiInt <- .pfmProjectInstitutions(wgi, y, institutions, ssp)
  wgiInt <- mrpfm::toolImputeMedians(wgiInt)

  # Calculate dynamic global country-level baseline bounds (historical observations only)
  wgiCnt <- calcOutput("WGIindicator", aggregate = FALSE)
  if (!any(grepl("\\(WGI\\)", magclass::getNames(wgiCnt)))) {
    magclass::getNames(wgiCnt) <- paste0(magclass::getNames(wgiCnt), " (WGI)")
  }
  wgiMin <- apply(wgiCnt, 3, min, na.rm = TRUE)
  wgiMax <- apply(wgiCnt, 3, max, na.rm = TRUE)

  wgiNorm <- toolNormalize(wgiInt, minVal = wgiMin, maxVal = wgiMax, targetRange = c(0, 1))

  # Rule of Law, Government Effectiveness, Control of Corruption — SSP extensions projections,
  # normalized to 0-1 using country-level min/max over historical years only (matching WGI bound computation)
  sspGovVars <- paste0(ssp, c(".Rule-of-Law Index", ".Governance Index|Government Effectiveness",
                              ".Governance Index|Control of Corruption"))
  sspExtCnt <- calcOutput("SSPextensions", subtype = paste0("drivers_", ssp), aggregate = FALSE)
  wgiHistYears <- magclass::getYears(wgiCnt)
  sspHistYears <- intersect(wgiHistYears, magclass::getYears(sspExtCnt))
  sspGovMin <- apply(sspExtCnt[, sspHistYears, sspGovVars], 3, min, na.rm = TRUE)
  sspGovMax <- apply(sspExtCnt[, sspHistYears, sspGovVars], 3, max, na.rm = TRUE)
  sspGovNorm <- toolNormalize(sspExt[, y, sspGovVars],
    minVal = sspGovMin, maxVal = sspGovMax, targetRange = c(0, 1), clamp = TRUE)

  out <- mbind(
    out,
    wgiNorm[, y, "Voice and Accountability (WGI)"],
    wgiNorm[, y, "Political Stability (WGI)"],
    wgiNorm[, y, "Regulatory Quality (WGI)"],
    magclass::setNames(sspGovNorm[, , sspGovVars[1]], "Rule of Law (WGI)"),
    magclass::setNames(sspGovNorm[, , sspGovVars[2]], "Government Effectiveness (WGI)"),
    magclass::setNames(sspGovNorm[, , sspGovVars[3]], "Control of Corruption (WGI)")
  )

  # V-Dem governance indicators: no SSP projection exists; the declared `institutions` rule.
  vdem <- calcOutput("VDem", aggregate = aggregate, regionmapping = outputRegionMappingFile)
  vdemInt <- .pfmProjectInstitutions(vdem, y, institutions, ssp)
  vdemInt <- mrpfm::toolImputeMedians(vdemInt)

  # Calculate dynamic global country-level baseline bounds
  vdemCnt <- calcOutput("VDem", aggregate = FALSE)
  vdemMin <- apply(vdemCnt, 3, min, na.rm = TRUE)
  vdemMax <- apply(vdemCnt, 3, max, na.rm = TRUE)

  vdemNorm <- toolNormalize(vdemInt, minVal = vdemMin, maxVal = vdemMax, targetRange = c(0, 1))
  out <- mbind(out, vdemNorm[, y, ])

  # V-Dem state-capacity indicators: the same `institutions` rule as the accountability indicators
  scRaw <- calcOutput("VDem", subtype = "stateCapacity",
                      aggregate = aggregate, regionmapping = outputRegionMappingFile)
  scInt <- .pfmProjectInstitutions(scRaw, y, institutions, ssp)
  scInt <- mrpfm::toolImputeMedians(scInt)

  # Invert "bad when high" variables before normalisation (same map as panelDataHistorical)
  invertMapSc <- c(
    "Executive Corruption (VDem)" = "Policy Implementation (VDem)",
    "Political Corruption (VDem)" = "Absence of Corruption (VDem)",
    "Neopatrimonialism (VDem)"    = "Meritocracy Index (VDem)"
  )
  for (rawName in names(invertMapSc)) {
    if (rawName %in% magclass::getNames(scInt)) {
      scInt[, , rawName] <- 1 - scInt[, , rawName]
    }
  }
  magclass::getNames(scInt) <- ifelse(
    magclass::getNames(scInt) %in% names(invertMapSc),
    invertMapSc[magclass::getNames(scInt)],
    magclass::getNames(scInt)
  )

  scCnt <- calcOutput("VDem", subtype = "stateCapacity", aggregate = FALSE)
  for (rawName in names(invertMapSc)) {
    if (rawName %in% magclass::getNames(scCnt)) {
      scCnt[, , rawName] <- 1 - scCnt[, , rawName]
    }
  }
  magclass::getNames(scCnt) <- ifelse(
    magclass::getNames(scCnt) %in% names(invertMapSc),
    invertMapSc[magclass::getNames(scCnt)],
    magclass::getNames(scCnt)
  )
  scMin <- apply(scCnt, 3, min, na.rm = TRUE)
  scMax <- apply(scCnt, 3, max, na.rm = TRUE)

  scNorm <- toolNormalize(scInt, minVal = scMin, maxVal = scMax, targetRange = c(0, 1))
  out <- mbind(out, scNorm[, y, ])
  # PCA will be added after harmonization (rotation populated by panelDataHistorical call below)

  # Control Variables
  # GDP/Population of the run's SSP (mrdrivers): one harmonized series (history + projection,
  # 1960-2150; the SSPs are identical until 2029)
  # used for BOTH the future trajectory and the historical normalization reference, so training
  # and scenario share a single source. The scenario name is kept for the downstream [,,ssp] slices.
  pop <- .popRaw          # same series the actor-power scaler used, fetched above
  gdp <- calcOutput("GDP", scenario = ssp, average2020 = FALSE,
    aggregate = aggregate, regionmapping = outputRegionMappingFile
  )
  gdpPerCapita <- gdp[, intersect(getYears(pop), getYears(gdp)), ] /
    pop[, intersect(getYears(pop), getYears(gdp)), ]
  # Log-scale bounds from the HISTORICAL window of the same SSP2 series (anchor = WGI horizon),
  # so the model receives inputs on the same scale it was trained on and future values clamp to
  # the historical max. (For an exact training/scenario seam, train the historical panel to the
  # same horizon; the bounds are a linear transform so model tiers/significance are unaffected.)
  .histEnd <- max(getYears(wgi, as.integer = TRUE))
  popH  <- pop[, getYears(pop, as.integer = TRUE) <= .histEnd, ]
  gdpH  <- gdp[, getYears(gdp, as.integer = TRUE) <= .histEnd, ]
  gdpPCHist <- gdpH[, intersect(getYears(gdpH), getYears(popH)), ] /
    popH[, intersect(getYears(gdpH), getYears(popH)), ]
  popLogMin   <- min(log(popH), na.rm = TRUE)
  popLogMax   <- max(log(popH), na.rm = TRUE)
  gdpLogMin   <- min(log(gdpH), na.rm = TRUE)
  gdpLogMax   <- max(log(gdpH), na.rm = TRUE)
  gdpPCLogMin <- min(log(gdpPCHist), na.rm = TRUE)
  gdpPCLogMax <- max(log(gdpPCHist), na.rm = TRUE)

  popNorm          <- toolNormalize(log(pop), minVal = popLogMin, maxVal = popLogMax, targetRange = c(0, 1), clamp = TRUE)
  gdpNorm          <- toolNormalize(log(gdp), minVal = gdpLogMin, maxVal = gdpLogMax, targetRange = c(0, 1), clamp = TRUE)
  gdpPerCapitaNorm <- toolNormalize(log(gdpPerCapita), minVal = gdpPCLogMin, maxVal = gdpPCLogMax, targetRange = c(0, 1), clamp = TRUE)
  # GDP Q-centred will be added after harmonization (uses historical quartile breaks)

  # Land area — constant; bounds from the same aggregated snapshot used in both panels
  landArea <- new.magpie(getRegions(pop), y, "LandArea", fill = NA) # nolint: undesirable_function_linter.
  landArea[, y, ] <- calcOutput("FAOLandArea", aggregate = aggregate, regionmapping = outputRegionMappingFile)
  landAreaLogMin <- min(log(landArea), na.rm = TRUE)
  landAreaLogMax <- max(log(landArea), na.rm = TRUE)
  landAreaNorm   <- toolNormalize(log(landArea), minVal = landAreaLogMin, maxVal = landAreaLogMax, targetRange = c(0, 1))
  # IEA energy intensity — log1p; fixed ceiling consistent with historical panel;
  # future intensity only falls so upper breach is not a concern
  energyIntensity <- setNames(
    magclass::collapseNames(modelDownscale[, y, "fe_total"]) * 31.536 /
      (magclass::collapseNames(gdp[, y, ]) / 1e6), ssp) # (EJ / million US$)
  energyIntensityNorm <- toolNormalize(log1p(energyIntensity), minVal = 0, maxVal = log1p(600), targetRange = c(0, 1))

  out <- mbind(
    out,
    setNames(popNorm[, y, ssp], "Population"),
    setNames(gdpNorm[, y, ssp], "GDP"),
    setNames(gdpPerCapitaNorm[, y, ssp], "GDP per Capita"),
    setNames(landAreaNorm[, y, ], "Land Area"),
    setNames(sspExt[, y, paste0(ssp, ".Population|Urban [Share]")] / 100, "Urban Population Share"),
    setNames(sspExt[, y, paste0(ssp, ".Gini Income Inequality Coefficient")] / 100, "Gini Income Inequality Coefficient"),
    setNames(sspExt[, y, paste0(ssp, ".Gender Inequality Index")], "Gender Inequality Index"),
    setNames(energyIntensityNorm[, y, ssp], "Energy Intensity")
  )
  # GDP Q-centred placeholder — populated after harmonization so quartile breaks are available

  if (harmonizeScenario) {
    histArgs <- list(aggregate = aggregate, y = histYears,
                     outputRegionMappingFile = outputRegionMappingFile,
                     movingAverage = movingAverage, ieaVersion = ieaVersion,
                     geothermal = geothermal, coeff = coeff)
    histPanel <- .pfmHistHarmonisationPanel(histArgs, histCache)

    # Use the latest historical year as the harmonization anchor so that recent
    # changes (e.g. a Rule of Law recovery in 2022) are reflected in the scenario.
    # If that year is not a native scenario timestep (REMIND runs in 5-yr steps),
    # linearly interpolate the scenario variables to that year for the offset only.
    stitchYear <- max(magclass::getYears(histPanel, as.integer = TRUE))
    scenYears  <- magclass::getYears(out, as.integer = TRUE)

    if (!stitchYear %in% scenYears) {
      outAtStitch <- magclass::time_interpolate(out, stitchYear, extrapolation_type = "linear")
    } else {
      outAtStitch <- out
    }

    # Exclude variables already perfectly anchored by their own projection method
    constants <- c("Voice and Accountability (WGI)", "Political Stability (WGI)",
                   "Regulatory Quality (WGI)",
                   magclass::getNames(vdemNorm),
                   magclass::getNames(scNorm))

    varsToHarmonize <- intersect(magclass::getNames(out), magclass::getNames(histPanel))
    varsToHarmonize <- setdiff(varsToHarmonize, constants)

    for (v in varsToHarmonize) {
      offset <- magclass::setYears(histPanel[, stitchYear, v], NULL) -
                magclass::setYears(outAtStitch[, stitchYear, v], NULL)

      for (year_val in scenYears) {
        if (year_val <= stitchYear) {
          weight <- 1.0
        } else if (year_val >= harmonizeScenarioYear) {
          weight <- 0.0
        } else {
          weight <- (harmonizeScenarioYear - year_val) / (harmonizeScenarioYear - stitchYear)
        }
        out[, year_val, v] <- out[, year_val, v] + offset * weight
      }
    }
  }

  # ── Post-harmonization additions (need historical PCA rotation + quartile breaks) ──
  # V-Dem state-capacity PCA: apply the historical rotation cached by the
  # panelDataHistorical() call inside the harmonization block above.
  storedRot <- .pfm_env$sc_pca_rotation
  scPC_scen <- computeVDemStateCapacityPC(
    scNorm[, y, ],
    rotation = storedRot   # NULL triggers fit mode if historical call didn't run
  )
  if (!is.null(scPC_scen)) out <- mbind(out, scPC_scen)

  # GDP per Capita (Q-centred): apply historical quartile breaks stored by panelDataHistorical
  gdpPCScen <- setNames(gdpPerCapitaNorm[, y, ssp], "GDP per Capita")
  gdpPCQCentred_scen <- computeGdpQCentred(gdpPCScen, storeBreaks = FALSE)
  if (!is.null(gdpPCQCentred_scen)) out <- mbind(out, gdpPCQCentred_scen)

  return(out)
}
# The harmonisation's historical panel, from `cache` when it holds one built with the same
# arguments (and pfm version), else built and written there. The .pfm_env state the historical
# call leaves for the scenario panel travels with it.
#' @keywords internal
.pfmHistHarmonisationPanel <- function(histArgs, cache = NULL) {
  stateKeys <- c("gdppc_q_fit", "gdppc_q_breaks", "sc_pca_rotation")
  key <- digest::digest(list(histArgs, as.character(utils::packageVersion("pfm"))), algo = "sha256")
  if (!is.null(cache) && file.exists(cache)) {
    hit <- tryCatch(readRDS(cache), error = function(e) NULL)
    if (identical(hit$key, key)) {
      for (k in stateKeys) assign(k, hit$state[[k]], envir = .pfm_env)
      return(hit$panel)
    }
  }
  panel <- do.call(panelDataHistorical, histArgs)
  if (!is.null(cache)) {
    state <- stats::setNames(lapply(stateKeys, function(k) .pfm_env[[k]]), stateKeys)
    dir.create(dirname(cache), showWarnings = FALSE, recursive = TRUE)
    tryCatch(saveRDS(list(key = key, panel = panel, state = state), cache),
             error = function(e) warning("histCache not written (", conditionMessage(e), ")", call. = FALSE))
  }
  panel
}
# nolint end
