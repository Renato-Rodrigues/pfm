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
#'   logistic time trend (bounded in \code{[0, 1]}, no rescale). Defaults
#'   \code{2010} and \code{0.20}: the inflection sits INSIDE the estimation
#'   window, so the S-curve's shape is identified rather than asserted, and the
#'   curve is ~0.92 by 2022 — leaving almost nothing for the projection to
#'   extrapolate. Keep identical across fit and projection (the fit stores them;
#'   see \code{\link{estimatePolicyStringencyModel}}).
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
#' @param excludeCountries ISO3 codes dropped from the panel before estimation.
#'   Defaults to \code{getOption("pfm.excludeCountries", "EST")}. Estonia's upstream
#'   \code{PE|Coal} is negative (derived coal gases and coke are booked as primary coal,
#'   and Estonia consumes them without producing any), so the clamp turns its coal share
#'   into 0.0 - a \emph{clean} reading for one of Europe's most carbon-intensive systems.
#'   A wrong-but-plausible value biases the fit invisibly; a dropped country is visible
#'   in the sample size. Set to \code{character(0)} once the upstream issue is resolved -
#'   see \code{docs/reference/psm-pecoal-estonia-issue.md}.
#' @param apTransform Character. Functional form of the actor-power drivers
#'   (ADR 0040). \code{"linear"} (default) uses the raw share. \code{"saturating"}
#'   applies the parameter-free diminishing-returns map \code{x / (x + xBar)} with
#'   \code{xBar} the training median, so the marginal political effect of the
#'   energy transition flattens instead of extrapolating linearly into a region no
#'   country has occupied. Applies only to strictly-positive actor-power columns
#'   (the composite Actor Power Index is a difference and is left linear).
#'   \strong{Fit mode only}: \code{xBar} is frozen into the \code{"driverScaling"}
#'   attribute as the \code{sat} element, so apply-mode callers reproduce the
#'   transform by passing the stored \code{driverScaling} and need not set this
#'   argument.
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
                             trendMidpoint = 2010, trendSteepness = 0.20,
                             trendFreezeYear = NULL,
                             outcomeVar = "Effective Carbon Price",
                             apTransform = "linear",
                             excludeCountries = getOption("pfm.excludeCountries",
                                                          "EST")) {
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

  # Sector-qualified in the data as "<name>|<sector>". The per-capita variants
  # (ADR 0040 follow-up) carry the same sector suffix, so they must resolve the
  # same way -- omitting them made every `*pc` spec fail with "missing from the
  # data" even though the column was present.
  known_indices <- c("Actor Power Index", "Innovator Power", "Incumbent Power")
  known_indices <- c(known_indices, paste(known_indices, "pc"))

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
  # lags and the internally derived context dummies, ADR 0038)
  missing <- setdiff(allVarsNeeded, availableVars)
  missing <- setdiff(missing, c("lagged_ecp", "lagged_adoption",
                                "EU Membership", "Transition Economy"))
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
      # (revised 2026-06-14; RE-PARAMETERIZED 2026-08-22). Raw logistic in [0, 1],
      # NO affine rescale: midpoint `trendMidpoint`, steepness `trendSteepness`.
      #
      # The defaults are (2010, 0.20): the inflection sits INSIDE the 2000-2022
      # estimation window and the curve spans 0.14 -> 0.92 across it. The previous
      # defaults (2030, 0.08) put the midpoint eight years BEYOND the last data
      # year, so the whole fit happened on the convex toe (0.09 -> 0.35, sd 0.078)
      # and the saturation was asserted rather than estimated. Two measured
      # consequences of the old choice (analysis/trendParameterisationGrid.R):
      #   * the projection lever beta*(x_2100 - x_2022) was 6.69 in Bulk, enough on
      #     its own to move the 2100 ceiling between 4.1 and 10.0 depending purely
      #     on whether the trend was frozen. Under (2010, 0.20) the lever is 0.29
      #     and the frozen/unfrozen ceilings are 4.11 vs 4.82 — the freeze stops
      #     being load-bearing.
      #   * the old setting fit WORSE: frontier logLik -1379 (Bulk) / -647
      #     (Diffuse) against -1319 / -559 here, at identical parameter count.
      # beta*sd is near-invariant across parameterizations (0.80 -> 0.89 Bulk), so
      # this changes what the trend does OUT of sample, not its in-sample role.
      # Do not push the midpoint earlier still: (2005, 0.25) fits better again but
      # drives frontier gamma to exactly 1.0000, the boundary degeneracy.
      #
      # Because it stays in [0, 1] it cannot drive an unbounded extrapolation (the
      # old rescaled curve reached ~6 by 2080, half of the stringency price
      # explosion). One common curve is shared across all scenarios: scenario
      # differences flow through the scenario-specific actor power and
      # institutional drivers, not the trend, so "ambition" is not double-counted.
      # See CONTEXT.md "Logistic Time Trend".
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
        c("lagged_ecp", "lagged_adoption", "EU Membership", "Transition Economy")
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

  # --- countries excluded from estimation (docs/reference/psm-pecoal-estonia-issue.md) ---
  # EST by default: its PE|Coal is NEGATIVE upstream (derived coal gases and coke are
  # counted as primary coal, and Estonia consumes them without producing any), so the
  # clamp turns its coal share into 0.0 - reading as a CLEAN system for one of Europe's
  # most carbon-intensive. A wrong-but-plausible value biases the fit while looking
  # fine; a dropped country is at least visible in the sample size.
  # Reverse with options(pfm.excludeCountries = character(0)) once upstream is fixed.
  if (length(excludeCountries)) {
    drop <- as.character(df$region) %in% excludeCountries
    if (any(drop)) {
      message("[preparePanelData] excluding ", sum(drop), " row(s) for ",
              paste(intersect(excludeCountries, unique(as.character(df$region))),
                    collapse = ", "),
              " - see docs/reference/psm-pecoal-estonia-issue.md")
      df <- df[!drop, , drop = FALSE]
    }
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
      # R54 single-country / unambiguous aggregate codes (BELUX both 1958;
      # ECE_other = CZE+EST+LTU+LVA+SVK, all 2004)
      DEU = 1958, FRA = 1958, ITA = 1958, NLD = 1958, BELUX = 1958,
      IRL = 1973, DNK = 1973, GBR = 1973,
      GRC = 1981, ESP = 1986, PRT = 1986,
      AUT = 1995, SWE = 1995, FIN = 1995, POL = 2004, ECE_other = 2004,
      # ISO3 codes for the country-resolution run (ADR 0038 / country sentinel);
      # single-country R54 codes above already coincide with ISO3
      BEL = 1958, LUX = 1958,
      CYP = 2004, CZE = 2004, EST = 2004, HUN = 2004, LTU = 2004, LVA = 2004,
      MLT = 2004, SVK = 2004, SVN = 2004,
      BGR = 2007, ROU = 2007, HRV = 2013
    )
    accYr <- euAccession[df$region]
    memberYr <- df$year - lag
    eu <- as.numeric(!is.na(accYr) & memberYr >= accYr)
    eu[df$region == "GBR" & memberYr >= 2020] <- 0
    df[[make.names("EU Membership")]] <- eu
  }

  # --- Transition Economy (ADR 0038, 2026-07-13): static post-communist dummy --
  # The second von Dulong context control (Armingeon & Careja transition list).
  # Time-invariant, so trivially projection-safe; under region FE it is identified
  # only where a block mixes transition and non-transition members (and at country
  # resolution). Same deliberate-uncoded rule as the EU dummy: only unambiguous
  # R54 aggregates are coded (ECS = BGR+HRV+HUN+ROU+SVN and ECE_other are all
  # post-communist; mixed aggregates stay 0 until composition is verified).
  if ("Transition Economy" %in% controlDrivers) {
    transitionCodes <- c(
      # ISO3 (Armingeon & Careja 2008: CEE + former Soviet Union)
      "ALB", "ARM", "AZE", "BLR", "BIH", "BGR", "HRV", "CZE", "EST", "GEO",
      "HUN", "KAZ", "KGZ", "LVA", "LTU", "MKD", "MDA", "MNE", "POL", "ROU",
      "RUS", "SRB", "SVK", "SVN", "TJK", "TKM", "UKR", "UZB",
      # unambiguous R54 aggregates
      "ECS", "ECE_other"
    )
    df[[make.names("Transition Economy")]] <- as.numeric(df$region %in% transitionCodes)
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
  if (identical(regionMappingFixedEffects, "unit") && !isTRUE(useMundlak)) {
    # "unit" sentinel (2026-07-13): every row-unit is its own FE block — the TWFE /
    # unit-FE baseline rung. Resolution-agnostic: 25 dummies at R54, ~50 at country
    # resolution. Never a swept candidate (ADR 0011); estimator-agreement rung only.
    df$regionFE <- factor(df$region)
  } else if (!is.null(regionMappingFixedEffects) && !isTRUE(useMundlak)) {
    mapping <- pfmGetMapping(regionMappingFixedEffects, type = "regional")
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
  scaleExcl  <- c("ecp", "lagged_ecp", "lagged_adoption", "regionFE")
  scaleVars  <- setdiff(make.names(unique(c(actorPowerDrivers, actorPowerIndex,
                                            instQualityDrivers, controlDrivers))),
                        scaleExcl)
  scaleVars  <- intersect(scaleVars, colnames(df))
  scaleVars  <- scaleVars[!grepl("_grp_mean$", scaleVars)]

  # The time trends are NOT driver-list members, so they have to be added here
  # explicitly (2026-08-22). Before this they were the only regressors left on a
  # raw scale while every driver was per-SD, which made the coefficient table
  # unreadable: the logistic trend's raw beta of 10.28 against drivers near 0.26
  # invited the conclusion that it dwarfed them, when beta*sd puts it at 0.80
  # against 0.26 — the largest single term, but smaller than the political block
  # combined (1.24). Standardizing is a linear reparameterization: fitted values,
  # logLik, frontier gamma and every z-statistic are unchanged; only the units of
  # the reported coefficient move. The trend enters as a pure main effect
  # (buildModelFormula never interacts it), so centring introduces no
  # interaction-interpretation change. See computeComparableCoefficients().
  scaleVars <- c(scaleVars, intersect(c("timeTrend", "logisticTimeTrend"), colnames(df)))

  # --- Saturating actor-power transform (ADR 0040), applied BEFORE scaling -----
  # The actor-power drivers are energy-system shares whose HISTORICAL range is a
  # small part of the range any transition scenario visits (Bulk innovator power
  # spans 0.03-0.43 in training, 95th pct 0.23, and REMIND takes it to 0.58-0.68).
  # A model linear in the share extrapolates its slope - including the negative
  # interaction slopes - far outside the identified region, which is the diagnosed
  # cause of the declining projections and the perverse ceiling feedback (see
  # docs/psm-ceiling-feedback-diagnosis.md). The saturating form applies
  #     xTilde = x / (x + xBar),   xBar = training MEDIAN of x,
  # a parameter-free, strictly increasing map onto (0, 1) with diminishing
  # returns: the marginal effect at projection levels is ~19x smaller than at the
  # historical median, so extrapolation is bounded instead of linear. Substantive
  # reading: political power saturates in economic weight - the first tenth of
  # renewables builds a constituency, the eighth tenth adds little that is new.
  #
  # An S-curve (plogis) was tested and REJECTED: it saturates every country to
  # 1.000 by 2050, which re-creates the scenario-blindness the responsiveness gate
  # exists to catch. The hyperbolic form keeps a live scenario signal.
  #
  # xBar is FROZEN exactly like mean/sd: it is stored INSIDE the driverScaling
  # entry as the `sat` element, so every existing apply-mode caller (which already
  # passes driverScaling = fit$driverScaling) reproduces the transform with no
  # signature change. Applied only to actor-power columns that are strictly
  # positive - the composite Actor Power Index is a DIFFERENCE (innovator minus
  # incumbent, ~[-0.8, 0.1]) for which "diminishing returns in a share" is not
  # defined, so it is left linear and flagged with sat = NA.
  apTransform <- match.arg(apTransform, c("linear", "saturating"))
  apVars <- intersect(make.names(unique(c(actorPowerDrivers, actorPowerIndex))), scaleVars)
  satOf <- function(col) {
    if (!is.null(driverScaling) && !is.null(driverScaling[[col]])) {
      s <- driverScaling[[col]]
      return(if ("sat" %in% names(s)) unname(s[["sat"]]) else NA_real_)
    }
    if (!identical(apTransform, "saturating")) return(NA_real_)
    v <- df[[col]]
    if (any(v < 0, na.rm = TRUE)) {
      warning("preparePanelData: apTransform = 'saturating' skipped for '", col,
              "' (column takes negative values; the saturating form is defined ",
              "for shares). Column left linear.", call. = FALSE)
      return(NA_real_)
    }
    md <- stats::median(v, na.rm = TRUE)
    if (!is.finite(md) || md <= 0) return(NA_real_)
    md
  }
  satPars <- stats::setNames(vapply(apVars, satOf, numeric(1)), apVars)
  for (col in apVars) {
    sp <- satPars[[col]]
    if (is.finite(sp)) df[[col]] <- df[[col]] / (df[[col]] + sp)
  }

  scaling    <- if (is.null(driverScaling)) list() else driverScaling
  # Apply mode with an INCOMPLETE reference is a silent-corruption hazard, not a
  # recoverable one: falling through to the else-branch would recompute mean/sd
  # from the SCENARIO panel (2005-2150), so the projection design would sit on a
  # different scale from the coefficients and the ceiling would be quietly wrong.
  # This bites hardest for artifacts written before a column joined scaleVars —
  # exactly the trend's situation after 2026-08-22 — so refuse instead.
  if (!is.null(driverScaling) && length(driverScaling)) {
    stale <- setdiff(scaleVars, names(driverScaling))
    if (length(stale)) {
      stop("preparePanelData: driverScaling has no entry for ",
           paste(stale, collapse = ", "),
           ". This reference predates those columns being standardized — refit the ",
           "model rather than projecting with a mixed-scale design.")
    }
  }
  for (col in scaleVars) {
    if (!is.null(driverScaling) && !is.null(driverScaling[[col]])) {
      m <- driverScaling[[col]][["mean"]]; s <- driverScaling[[col]][["sd"]]
    } else {
      m <- mean(df[[col]], na.rm = TRUE)
      s <- stats::sd(df[[col]], na.rm = TRUE)
      if (!is.finite(s) || s < 1e-8) s <- 1
      scaling[[col]] <- c(mean = m, sd = s)
      if (col %in% apVars) scaling[[col]] <- c(scaling[[col]], sat = satPars[[col]])
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
