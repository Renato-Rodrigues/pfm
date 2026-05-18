#' Prepare panel data.frame from magpie object
#'
#' Converts a magpie object from panelDataHistorical into a flat
#' data.frame suitable for regression, including the dependent variable (ECP),
#' all requested predictors, a linear time trend, region fixed-effect labels,
#' and pre-computed interaction columns (each actorPowerIndex × each inst. quality driver).
#'
#' @param data magpie object
#' @param sector character, "Bulk" or "Diffuse"
#' @param actorPowerDrivers character vector or NULL
#' @param actorPowerIndex character or NULL
#' @param instQualityDrivers character vector
#' @param controlDrivers character vector
#' @param regionMappingFixedEffects character, mapping file name
#' @param lag integer. Time lag for independent variables (drivers).
#'   If \code{lag > 0}, drivers at time \code{t-lag} are used to predict the
#'   dependent variable at time \code{t}. Default: \code{1}.
#'
#' @return data.frame with columns: region, year, timeTrend, regionFE,
#'   ecp, plus one column per driver (safe R-named), plus
#'   <actorPowerIndex>_x_<driver> interaction columns.
#'
#' @importFrom magclass getNames getRegions getYears
#' @importFrom madrat toolGetMapping
#'
#' @keywords internal
#'
#' @export
#'
#' @author Renato Rodrigues
preparePanelData <- function(data, sector, actorPowerDrivers, # nolint: cyclocomp_linter.
                             actorPowerIndex, instQualityDrivers,
                             controlDrivers, regionMappingFixedEffects,
                             lag = 1) {
  # If data is already a data.frame, assume it is already prepared and return it.
  if (is.data.frame(data)) {
    return(data)
  }

  regions <- magclass::getRegions(data) # nolint: undesirable_function_linter.
  years <- magclass::getYears(data, as.integer = TRUE)

  # Dependent variable name
  ecpName <- paste0("Effective Carbon Price|", sector)

  # Actor Power Index name (sector-qualified in the data)
  apiName <- if (!is.null(actorPowerIndex)) {
    paste0(actorPowerIndex, "|", sector)
  } else {
    NULL
  }

  # Collect all predictor names we need from the magpie object
  allVarsNeeded <- c(
    apiName, actorPowerDrivers,
    instQualityDrivers, controlDrivers
  )

  availableVars <- magclass::getNames(data)
  hasEcp <- ecpName %in% availableVars

  # Verify all requested predictor variables exist (excluding internally computed lags)
  missing <- setdiff(allVarsNeeded, availableVars)
  missing <- setdiff(missing, c("lagged_ecp", "lagged_adoption"))
  if (length(missing) > 0) {
    stop(
      "The following variables are missing from the data: ",
      paste(missing, collapse = ", ")
    )
  }

  # --- Build flat data.frame row by row (region x year) ---
  rows <- list()
  idx <- 1
  for (r in regions) {
    for (yi in seq_along(years)) {
      row <- list()
      row$region <- r
      row$year <- years[yi]
      row$timeTrend <- yi # linear time trend (1, 2, 3, ...)

      # Dependent variable
      if (hasEcp) {
        val <- as.numeric(data[r, years[yi], ecpName])
        row$ecp <- if (is.finite(val)) val else NA_real_
      } else {
        row$ecp <- NA_real_
      }

      # Fetch driver values from the lagged year index (yi - lag)
      yiLag <- yi - lag

      # Compute lagged dependent variables
      if (hasEcp) {
        valLag <- if (yiLag >= 1) as.numeric(data[r, years[yiLag], ecpName]) else NA_real_
        row$lagged_ecp <- if (is.finite(valLag)) valLag else NA_real_
        row$lagged_adoption <- if (is.finite(valLag)) as.integer(valLag > 0) else NA_integer_
      } else {
        row$lagged_ecp <- NA_real_
        row$lagged_adoption <- NA_integer_
      }

      # Actor Power Index
      if (!is.null(apiName)) {
        for (i in seq_along(actorPowerIndex)) {
          valI <- if (yiLag >= 1) as.numeric(data[r, years[yiLag], apiName[i]]) else NA_real_
          row[[make.names(actorPowerIndex[i])]] <- if (is.finite(valI)) valI else NA_real_
        }
      }

      # All other drivers
      cleanDrivers <- setdiff(
        c(actorPowerDrivers, instQualityDrivers, controlDrivers),
        c("lagged_ecp", "lagged_adoption")
      )
      for (v in cleanDrivers) {
        safeName <- make.names(v)
        val <- if (yiLag >= 1) as.numeric(data[r, years[yiLag], v]) else NA_real_
        row[[safeName]] <- if (is.finite(val)) val else NA_real_
      }

      rows[[idx]] <- row
      idx <- idx + 1
    }
  }

  df <- do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE))

  # --- Add region fixed effects ---
  if (!is.null(regionMappingFixedEffects)) {
    mapping <- madrat::toolGetMapping(regionMappingFixedEffects,
      type = "regional",
      where = "mappingfolder"
    )
    regionLookup <- stats::setNames(mapping$RegionCode, mapping$CountryCode)
    # If data regions are already region codes, use directly as FE grouping
    regFE <- if (all(df$region %in% mapping$RegionCode)) {
      df$region
    } else {
      regionLookup[df$region]
    }

    # --- Robust Standardization of Region Labels ---
    # 1. Trim and handle NAs
    regFE <- trimws(as.character(regFE))
    regFE[is.na(regFE) | regFE == "" | regFE == "NA"] <- "Other"

    # Create factor and set "Other" as the reference level
    # This ensures EU and OECD have coefficients, while "Other" is the baseline.
    df$regionFE <- as.factor(regFE)
    if ("Other" %in% levels(df$regionFE)) {
      df$regionFE <- stats::relevel(df$regionFE, ref = "Other")
    }
  }

  # --- Remove rows with NA in dependent variable (only if ECP was provided) ---
  if (hasEcp) {
    df <- df[!is.na(df$ecp), , drop = FALSE]
  }

  # --- Pre-compute interaction columns: each apiIndex × each instQuality driver ---
  if (!is.null(actorPowerIndex) && !is.null(instQualityDrivers)) {
    for (v in instQualityDrivers) {
      for (api in actorPowerIndex) {
        intNameSpec <- paste0(make.names(api), "_x_", make.names(v))
        df[[intNameSpec]] <- df[[make.names(api)]] * df[[make.names(v)]]
      }
    }
  }

  return(df)
}
