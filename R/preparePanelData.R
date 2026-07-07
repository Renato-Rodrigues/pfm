# nolint start
#' Prepare panel data.frame from magpie object
#'
#' Converts a magpie object from panelDataHistorical into a flat
#' data.frame suitable for regression, including the dependent variable
#' (\code{outcomeVar}, default the Effective Carbon Price),
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
#' @param useMundlak Logical. If \code{TRUE}, applies the Mundlak (1978) correction:
#'   computes within-region means of all theory and control variables and appends
#'   them as \code{<var>_grp_mean} columns. Region fixed-effect dummies are
#'   suppressed — \code{regionMappingFixedEffects} is ignored. Default: \code{FALSE}.
#' @param driverScaling Named list or \code{NULL}. Per-variable standardization
#'   constants \code{c(mean, sd)} for the driver columns (and hence the interaction
#'   factors). \code{NULL} (fit mode) computes them from this data and returns them
#'   via the \code{"driverScaling"} attribute. Supply the stored training-data
#'   values (apply mode) when building a scenario panel so historical and projected
#'   panels share one frozen reference. Standardization is fit-, prediction- and
#'   significance-neutral (a linear reparameterization) but improves interaction
#'   conditioning and makes driver coefficients comparable in per-SD units
#'   (de-scale to natural units by dividing a coefficient by the driver's stored sd).
#' @param trendMidpoint,trendSteepness Numeric. Shape of the single common
#'   logistic time trend (bounded in \code{[0, 1]}, no rescale). Defaults \code{2030} and
#'   \code{0.08}: flat toe before 2000, meaningful rise through history, saturating
#'   near 1 by ~2060. Keep identical across fit and projection.
#' @param outcomeVar Character. Base name of the dependent variable; the
#'   sector-qualified column \code{"<outcomeVar>|<sector>"} is read from the data.
#'   Default \code{"Effective Carbon Price"} (the carbon-price hurdle model);
#'   the Policy Stringency Model passes \code{"Policy Stringency"} (ADR 0036).
#'   Whatever the outcome, its panel column is named \code{ecp} (and its lag
#'   \code{lagged_ecp}) for historical reasons, so all downstream estimation
#'   machinery works unchanged.
#' @param trendFreezeYear Integer or \code{NULL}. If set, the logistic time trend
#'   is held flat at its value in this year for all later years (ADR 0010): the
#'   trend regressor uses \code{min(year, trendFreezeYear)}. Used only when
#'   preparing a \emph{scenario} panel (set to the last historical year, e.g. 2022)
#'   so the trend is not extrapolated out of sample and stops dominating
#'   projections. \code{NULL} (default, used for the historical fit) applies no
#'   freeze, leaving the in-sample design unchanged.
#'
#' @return data.frame with columns: region, year, timeTrend, regionFE (unless
#'   \code{useMundlak = TRUE}), ecp, plus one column per driver (safe R-named),
#'   \code{<var>_grp_mean} columns (when \code{useMundlak = TRUE}), and
#'   <actorPowerIndex>_x_<driver> interaction columns (from standardized factors).
#'   Driver columns are standardized in place; carries a \code{"driverScaling"}
#'   attribute (the per-variable mean/sd used).
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
                             lag = 1, useMundlak = FALSE,
                             gdpGovInteraction = FALSE,
                             driverScaling = NULL,
                             trendMidpoint = 2030, trendSteepness = 0.08,
                             trendFreezeYear = NULL,
                             outcomeVar = "Effective Carbon Price") {
  # If data is already a data.frame, assume it is already prepared and return it.
  if (is.data.frame(data)) {
    return(data)
  }

  # ADR 0011: derived control columns, computed on the fly from base variables so
  # every caller — historical AND scenario panels — has them without separate
  # augmentation. Projection-safe transforms only: square of the (bounded) GDP-Q
  # quantile transform, log GDP per capita (+ its square), and log Population
  # (raw population is hugely right-skewed). Added only when requested as a
  # control and not already present; a missing base variable is skipped silently
  # (the usual "missing predictor" check below then reports it). Done BEFORE the
  # array conversion so the new columns are indexable in data_arr.
  .logFloor <- function(v) { v[v < 1e-6] <- 1e-6; log(v) }
  .derive <- list(
    "GDP per Capita (Q-centred) Sq" = function(d) d[, , "GDP per Capita (Q-centred)"]^2,
    "GDP per Capita Sq"             = function(d) d[, , "GDP per Capita"]^2,
    "GDP per Capita (log)"          = function(d) .logFloor(d[, , "GDP per Capita"]),
    "GDP per Capita (log) Sq"       = function(d) .logFloor(d[, , "GDP per Capita"])^2,
    "Population (log)"              = function(d) .logFloor(d[, , "Population"])
  )
  for (nm in setdiff(intersect(controlDrivers, names(.derive)), magclass::getNames(data))) {
    val <- tryCatch(.derive[[nm]](data), error = function(e) NULL)
    if (!is.null(val)) data <- magclass::mbind(data, magclass::setNames(val, nm))
  }

  # Convert magpie object to standard 3D array for extreme speedup in indexing
  data_arr <- as.array(data)

  regions <- magclass::getRegions(data) # nolint: undesirable_function_linter.
  years <- magclass::getYears(data, as.integer = TRUE)

  # Dependent variable name (the panel column is named `ecp` whatever the outcome)
  ecpName <- paste0(outcomeVar, "|", sector)

  # Actor Power Index name (sector-qualified in the data)
  apiName <- if (!is.null(actorPowerIndex)) {
    paste0(actorPowerIndex, "|", sector)
  } else {
    NULL
  }

  known_indices <- c("Actor Power Index", "Innovator Power", "Incumbent Power")

  # Function to get the correct magpie name for a driver
  getMagpieName <- function(name, sector) {
    if (name %in% known_indices) paste0(name, "|", sector) else name
  }

  # Collect all predictor names we need from the magpie object
  allVarsNeeded <- c(
    apiName, 
    sapply(actorPowerDrivers, getMagpieName, sector = sector),
    instQualityDrivers, controlDrivers
  )

  availableVars <- magclass::getNames(data)
  hasEcp <- ecpName %in% availableVars

  # Verify all requested predictor variables exist (excluding internally computed
  # lags and the internally derived EU Membership dummy)
  missing <- setdiff(allVarsNeeded, availableVars)
  missing <- setdiff(missing, c("lagged_ecp", "lagged_adoption", "EU Membership"))
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
      # Calendar-anchored time trend so the value is consistent across separate
      # magpie objects (historical vs. scenario). yi would restart from 1 for
      # each new dataset; year-1999 gives a continuous sequence (2000→1,
      # 2001→2, …, 2022→23, 2025→26, …) regardless of dataset boundaries.
      row$timeTrend <- years[yi] - 1999L

      # Logistic (S-curve) time trend — a single common, bounded, SATURATING curve
      # (revised 2026-06-14). Raw logistic in [0, 1], NO affine rescale: midpoint
      # `trendMidpoint`, steepness `trendSteepness`. The defaults (2030, 0.08) place
      # the flat toe BEFORE the historical window (~0.04 in 1990, ~0.08 in 2000),
      # give a meaningful historical rise (~0.08 -> ~0.35 over 2000-2022), and let
      # the curve SATURATE near 1 within the projection horizon (~0.92 by 2060,
      # ~1.0 by 2100). Because it stays in [0, 1] it cannot drive an unbounded
      # extrapolation (the old rescaled curve reached ~6 by 2080, half of the
      # stringency price explosion). One common curve is shared across all
      # scenarios: scenario differences flow through the scenario-specific actor
      # power and institutional drivers, not the trend, so "ambition" is not
      # double-counted. See CONTEXT.md "Logistic Time Trend".
      # ADR 0010: out-of-sample the trend is FROZEN at its last-historical value.
      # Time effects cannot be extrapolated, so for projection years (year >
      # trendFreezeYear) we hold the regressor flat at the freeze-year value; the
      # historical fit is unchanged (freeze is NULL or beyond the sample there).
      # This stops the rising curve from dominating projections (it was the bulk of
      # the §5.4 early-onset and the §7.1 stringency explosion). Drivers carry the
      # future differentiation.
      trendYr <- if (!is.null(trendFreezeYear)) min(years[yi], trendFreezeYear) else years[yi]
      row$logisticTimeTrend <- 1.0 / (1.0 + exp(-trendSteepness * (trendYr - trendMidpoint)))

      # Dependent variable
      if (hasEcp) {
        val <- data_arr[r, yi, ecpName]
        row$ecp <- if (is.finite(val)) val else NA_real_
      } else {
        row$ecp <- NA_real_
      }

      # Fetch driver values from the lagged year index (yi - lag)
      yiLag <- yi - lag

      # Compute lagged dependent variables
      if (hasEcp) {
        valLag <- if (yiLag >= 1) data_arr[r, yiLag, ecpName] else NA_real_
        row$lagged_ecp <- if (is.finite(valLag)) valLag else NA_real_
        row$lagged_adoption <- if (is.finite(valLag)) as.integer(valLag > 0) else NA_integer_
      } else {
        row$lagged_ecp <- NA_real_
        row$lagged_adoption <- NA_integer_
      }

      # Actor Power Index
      if (!is.null(apiName)) {
        for (i in seq_along(actorPowerIndex)) {
          valI <- if (yiLag >= 1) data_arr[r, yiLag, apiName[i]] else NA_real_
          row[[make.names(actorPowerIndex[i])]] <- if (is.finite(valI)) valI else NA_real_
        }
      }

      # All other drivers
      cleanDrivers <- setdiff(
        c(actorPowerDrivers, instQualityDrivers, controlDrivers),
        c("lagged_ecp", "lagged_adoption", "EU Membership")
      )
      # Do not process variables again if they were already processed via actorPowerIndex
      cleanDrivers <- setdiff(cleanDrivers, actorPowerIndex)

      for (v in cleanDrivers) {
        safeName <- make.names(v)
        vMagpie <- getMagpieName(v, sector)
        val <- if (yiLag >= 1) data_arr[r, yiLag, vMagpie] else NA_real_
        row[[safeName]] <- if (is.finite(val)) val else NA_real_
      }

      rows[[idx]] <- row
      idx <- idx + 1
    }
  }
  
  if (length(rows) == 0) {
    df <- data.frame()
  } else {
    col_names <- names(rows[[1]])
    cols <- lapply(col_names, function(nm) {
      sapply(rows, `[[`, nm)
    })
    names(cols) <- col_names
    df <- as.data.frame(cols, stringsAsFactors = FALSE)
  }

  # --- EU Membership (R7, 2026-07-06): time-varying accession dummy -----------
  # A policy-diffusion regressor replacing part of what the atheoretical trend
  # absorbs (the EU acquis mechanically ratchets member stringency). Derived from
  # region + year, so it is projection-safe by construction (frozen at its last
  # value under any scenario). Only UNAMBIGUOUS R54 codes carry a default
  # accession year; multi-country aggregates (NES_EU, ECS, ECE_other, NEN_other,
  # ...) are deliberately uncoded (0) until their composition is verified — a
  # documented caveat, not a hidden assumption. GBR exits in 2020 (Brexit).
  # Lagged like every other driver (membership at year - lag).
  if ("EU Membership" %in% controlDrivers) {
    euAccession <- c(
      DEU = 1958, FRA = 1958, ITA = 1958, NLD = 1958, BELUX = 1958,
      IRL = 1973, DNK = 1973, GBR = 1973,
      GRC = 1981, ESP = 1986, PRT = 1986,
      AUT = 1995, SWE = 1995, FIN = 1995, POL = 2004
    )
    accYr <- euAccession[df$region]
    memberYr <- df$year - lag
    eu <- as.numeric(!is.na(accYr) & memberYr >= accYr)
    eu[df$region == "GBR" & memberYr >= 2020] <- 0
    df[[make.names("EU Membership")]] <- eu
  }

  # --- Mundlak correction: within-region means of theory & control variables ---
  if (isTRUE(useMundlak)) {
    mundlak_safe <- unique(c(
      make.names(actorPowerIndex),
      make.names(instQualityDrivers),
      make.names(controlDrivers)
    ))
    mundlak_safe <- setdiff(mundlak_safe,
                            c("lagged_ecp", "lagged_adoption", "timeTrend", "ecp"))
    mundlak_safe <- intersect(mundlak_safe, colnames(df))
    if (length(mundlak_safe) > 0) {
      grp_means <- aggregate(
        df[, mundlak_safe, drop = FALSE],
        by = list(region = df$region),
        FUN = mean, na.rm = TRUE
      )
      colnames(grp_means)[-1] <- paste0(colnames(grp_means)[-1], "_grp_mean")
      df <- merge(df, grp_means, by = "region", all.x = TRUE)
    }
  }

  # --- Add region fixed effects (skipped when useMundlak = TRUE) ---
  if (!is.null(regionMappingFixedEffects) && !isTRUE(useMundlak)) {
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

  # --- Standardize driver columns (center + scale), FROZEN reference ----------
  # Driver main effects and interaction factors are standardized to (x-mean)/sd.
  # Properties: (i) it is a linear reparameterization, so fitted values, deviance,
  # AIC, every coefficient's significance, and the theory tiers / Group
  # Contributions (beta*x is invariant) are ALL unchanged; (ii) the centring part
  # removes the mechanical main-effect/interaction collinearity behind the
  # stringency price explosion; (iii) the scaling part puts every driver
  # coefficient in the same "per standard deviation" units, so magnitudes are
  # directly comparable (de-scale to natural units by dividing by the stored sd).
  # Mean & sd are FROZEN: fit mode (driverScaling = NULL) computes them here and
  # returns them via the "driverScaling" attribute; apply mode (scenario
  # projection) passes the stored training values so historical and scenario
  # panels share one reference — the freeze discipline of GDP-Q and the PCA.
  # Excludes the outcome, its lags, the time trends and region FE. Negative-valued
  # drivers (e.g. Actor Power Index in ~[-0.8, 0.1]) standardize cleanly; a
  # near-constant column (sd ~ 0) is left unscaled to avoid a division blow-up.
  scaleExcl  <- c("ecp", "lagged_ecp", "lagged_adoption", "timeTrend",
                  "logisticTimeTrend", "regionFE")
  scaleVars  <- setdiff(make.names(unique(c(actorPowerDrivers, actorPowerIndex,
                                            instQualityDrivers, controlDrivers))),
                        scaleExcl)
  scaleVars  <- intersect(scaleVars, colnames(df))
  scaleVars  <- scaleVars[!grepl("_grp_mean$", scaleVars)]
  scaling    <- if (is.null(driverScaling)) list() else driverScaling
  for (col in scaleVars) {
    if (!is.null(driverScaling) && !is.null(driverScaling[[col]])) {
      m <- driverScaling[[col]][["mean"]]; s <- driverScaling[[col]][["sd"]]
    } else {
      m <- mean(df[[col]], na.rm = TRUE)
      s <- stats::sd(df[[col]], na.rm = TRUE)
      if (!is.finite(s) || s < 1e-8) s <- 1
      scaling[[col]] <- c(mean = m, sd = s)
    }
    df[[col]] <- (df[[col]] - m) / s
  }

  # --- Pre-compute interaction columns (from STANDARDIZED factors) -----------
  if (!is.null(actorPowerIndex) && !is.null(instQualityDrivers)) {
    for (v in instQualityDrivers) {
      for (api in actorPowerIndex) {
        a <- make.names(api); b <- make.names(v)
        df[[paste0(a, "_x_", b)]] <- df[[a]] * df[[b]]
      }
    }
  }

  # --- Pre-compute GDP × IQ interaction columns (when requested) ---
  # GDP per Capita is standardized above only if it is among the driver lists;
  # whichever scale it is on, the interaction is the product of the two columns.
  if (isTRUE(gdpGovInteraction) && !is.null(instQualityDrivers)) {
    gdpSafe <- make.names("GDP per Capita")
    if (gdpSafe %in% colnames(df)) {
      for (v in instQualityDrivers) {
        b <- make.names(v)
        df[[paste0(gdpSafe, "_x_", b)]] <- df[[gdpSafe]] * df[[b]]
      }
    }
  }

  attr(df, "driverScaling") <- scaling
  return(df)
}
# nolint end
