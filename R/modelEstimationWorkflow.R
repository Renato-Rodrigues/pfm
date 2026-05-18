#' @title modelEstimationWorkflow
#' @description Runs the full Political Feasibility Module (PFM) estimation
#' workflow. Loads historical panel data, then estimates the two-stage Hurdle
#' model (Adoption Logit + Price Stringency GLM) for each sector.
#'
#' @param aggregate Boolean. If TRUE, aggregates to region mapping.
#'   Default: \code{TRUE}.
#' @param y Numeric vector of years. Default: \code{2000:2022}.
#' @param outputRegionMappingFile Character. Region mapping file for data
#'   aggregation. Default: \code{"regionmappingH12.csv"}.
#' @param coeff List of coefficients for actor power index calculation.
#'   See \code{panelDataHistorical} for the default structure.
#' @param sectors Character vector of sectors to estimate.
#'   Default: \code{c("Bulk", "Diffuse")}.
#' @param family Character. GLM family for the stringency model:
#'   \code{"Gamma"} or \code{"gaussian"}. Default: \code{"Gamma"}.
#' @param actorPowerDrivers Character vector of individual Actor Power driver
#'   names. Only used as main effects if \code{actorPowerIndex} is \code{NULL}.
#' @param actorPowerIndex Character or NULL. Name of the Actor Power Index
#'   variable. If provided, it is used as the sole Actor Power main effect
#'   and for interaction terms.
#' @param instQualityDrivers Character vector of Institutional Quality
#'   indicator names.
#' @param controlDrivers Character vector of control variable names.
#' @param regionMappingFixedEffects Character or NULL. Region mapping file for fixed effects.
#'   If \code{NULL}, region fixed effects are omitted.
#'   Default: \code{"regionmappingH12.csv"}.
#' @param timeTrend Logical. If \code{TRUE} (default), adds a linear time trend.
#' @param useFirth Logical. If \code{TRUE} (default), uses bias-reduced
#'   estimation: Firth's penalized likelihood for the adoption stage (Logit)
#'   and Firth-type bias reduction (via \code{brglm2}) for the stringency stage (GLM).
#' @param logTransform Logical. If \code{TRUE}, the stringency-stage dependent
#'   variable is transformed to \code{log(1 + ECP)}. Default: \code{TRUE}.
#' @param lag Integer. Time lag for drivers in years. Default: \code{1}.
#' @param includeLaggedAdoption Logical. If \code{TRUE}, includes the lagged adoption status.
#'   Default: \code{FALSE}.
#' @param includeLaggedECP Logical. If \code{TRUE}, includes the lagged carbon price.
#'   Default: \code{FALSE}.
#' @param panelData Optional \code{magpie} object or \code{data.frame}. If provided,
#'   skips loading and processing historical data via \code{panelDataHistorical}.
#'
#' @return A list containing:
#'   \describe{
#'     \item{models}{A named list (by sector) where each element contains:
#'       \itemize{
#'         \item{\code{adoption}: Result list from \code{estimateAdoptionModel}.}
#'         \item{\code{stringency}: Result list from \code{estimatePriceStringencyModel}.}
#'       }}
#'     \item{coefficients}{A data frame containing combined robust coefficients for all estimated models,
#'       including columns for \code{sector}, \code{stage} (Adoption/Stringency), \code{term},
#'       \code{Estimate}, \code{Std. Error}, \code{t value}, and \code{Pr(>|t|)}.}
#'     \item{model_stats}{A data frame containing key statistics for all estimated models,
#'       including columns for \code{sector}, \code{stage} (Adoption/Stringency),
#'       \code{Observations}, \code{AIC}, and \code{Converged}.}
#'   }
#'
#' @author Renato Rodrigues
#'
#' @export
#'

modelEstimationWorkflow <- function(
    aggregate = TRUE,
    y = 2000:2022,
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
    ),
    sectors = c("Bulk", "Diffuse"),
    family = "Gamma",
    actorPowerDrivers = c(
      "VRE share", "Electrification",
      "Coal primary energy share", "Oil/Gas primary energy share",
      "Fossil share in Industry"
    ),
    actorPowerIndex = "Actor Power Index",
    instQualityDrivers = c(
      "Government Effectiveness", "Control of Corruption",
      "Voice and Accountability", "Political Stability", "Regulatory Quality", "Rule of Law"
    ),
    controlDrivers = c(
      "Population", "GDP per Capita", "Land Area",
      "Urban Population Share",
      "Gini Income Inequality Coefficient",
      "Gender Inequality Index", "Energy Intensity"
    ),
    regionMappingFixedEffects = "regionmappingH12.csv",
    timeTrend = TRUE,
    useFirth = TRUE,
    logTransform = TRUE,
    lag = 1,
    includeLaggedAdoption = FALSE,
    includeLaggedECP = FALSE,
    panelData = NULL) {
  # --- 1. Load data ---
  if (is.null(panelData)) {
    message("Loading historical panel data...")
    data <- panelDataHistorical(
      aggregate = aggregate,
      y = y,
      outputRegionMappingFile = outputRegionMappingFile,
      coeff = coeff
    )
  } else {
    data <- panelData
  }
  message("Data loaded: ", paste(dim(data), collapse = " x "))

  # --- 2. Estimate models for each sector ---
  allModels <- list()
  allCoefficients <- list()
  allModelStats <- list()

  for (s in sectors) {
    message("\n=== Sector: ", s, " ===")

    # Stage 1: Adoption
    message("  Stage 1: Estimating Adoption (Logit)...")
    adoptionResult <- estimateAdoptionModel(
      data = data,
      sector = s,
      actorPowerDrivers = actorPowerDrivers,
      actorPowerIndex = actorPowerIndex,
      instQualityDrivers = instQualityDrivers,
      controlDrivers = controlDrivers,
      regionMappingFixedEffects = regionMappingFixedEffects,
      timeTrend = timeTrend,
      useFirth = useFirth,
      lag = lag,
      includeLaggedAdoption = includeLaggedAdoption
    )
    message("  Stage 1 complete. Converged: ", adoptionResult$model$converged)

    # Stage 2: Stringency
    message("  Stage 2: Estimating Stringency (GLM, family = ", family, ")...")
    stringencyResult <- estimatePriceStringencyModel(
      data = data,
      sector = s,
      family = family,
      actorPowerDrivers = actorPowerDrivers,
      actorPowerIndex = actorPowerIndex,
      instQualityDrivers = instQualityDrivers,
      controlDrivers = controlDrivers,
      regionMappingFixedEffects = regionMappingFixedEffects,
      timeTrend = timeTrend,
      logTransform = logTransform,
      lag = lag,
      useFirth = useFirth,
      includeLaggedECP = includeLaggedECP
    )
    message("  Stage 2 complete. Converged: ", stringencyResult$model$converged)

    allModels[[s]] <- list(
      adoption   = adoptionResult,
      stringency = stringencyResult,
      summary    = paste0(
        "=== ", s, " Sector Equations ===\n\n",
        "ADOPTION STAGE:\n",
        formatModelEquation(
          adoptionCoeffs, actorPowerIndex, actorPowerDrivers, instQualityDrivers, controlDrivers, includeLegend = FALSE
        ),
        "\n\nSTRINGENCY STAGE:\n",
        formatModelEquation(
          stringencyCoeffs, actorPowerIndex, actorPowerDrivers, instQualityDrivers, controlDrivers, includeLegend = TRUE
        )
      )
    )

    # Collect coefficients
    adoptionCoeffs <- .coeftestToDataFrame(adoptionResult$coeftest)
    adoptionCoeffs$sector <- s
    adoptionCoeffs$stage <- "Adoption"

    stringencyCoeffs <- .coeftestToDataFrame(stringencyResult$coeftest)
    stringencyCoeffs$sector <- s
    stringencyCoeffs$stage <- "Stringency"

    allCoefficients[[s]] <- rbind(adoptionCoeffs, stringencyCoeffs)

    # Collect model stats
    # AIC for logistf needs manual calculation if fit$aic is NULL
    getAIC <- function(m) {
      if (inherits(m, "logistf")) {
        return(-2 * m$loglik["full"] + 2 * length(m$coefficients))
      }
      return(m$aic)
    }
    getNobs <- function(m) {
      if (inherits(m, "logistf")) return(m$n)
      if (inherits(m, "glm")) return(length(m$y))
      return(NA)
    }

    adoptionStats <- data.frame(
      sector = s,
      stage = "Adoption",
      Observations = getNobs(adoptionResult$model),
      AIC = round(getAIC(adoptionResult$model), 2),
      Converged = adoptionResult$model$converged,
      Equation = formatModelEquation(
        coefficients = adoptionCoeffs,
        actorPowerIndex = actorPowerIndex,
        actorPowerDrivers = actorPowerDrivers,
        instQualityDrivers = instQualityDrivers,
        controlDrivers = controlDrivers
      )
    )

    stringencyStats <- data.frame(
      sector = s,
      stage = "Stringency",
      Observations = getNobs(stringencyResult$model),
      AIC = round(getAIC(stringencyResult$model), 2),
      Converged = stringencyResult$model$converged,
      Equation = formatModelEquation(
        coefficients = stringencyCoeffs,
        actorPowerIndex = actorPowerIndex,
        actorPowerDrivers = actorPowerDrivers,
        instQualityDrivers = instQualityDrivers,
        controlDrivers = controlDrivers
      )
    )

    allModelStats[[s]] <- rbind(adoptionStats, stringencyStats)
  }

  # Combine all coefficients and model stats into single data frames
  combinedCoefficients <- do.call(rbind, allCoefficients)
  combinedModelStats <- do.call(rbind, allModelStats)
  rownames(combinedCoefficients) <- NULL
  rownames(combinedModelStats) <- NULL

  # --- 3. Full Workflow Summary ---
  fullWorkflowSummary <- ""
  for (s in sectors) {
    fullWorkflowSummary <- paste0(fullWorkflowSummary, "Sector: ", s, "\n")
    fullWorkflowSummary <- paste0(fullWorkflowSummary, "Equation:\n")
    # Adoption
    fullWorkflowSummary <- paste0(fullWorkflowSummary, formatModelEquation(
      coefficients = .coeftestToDataFrame(allModels[[s]]$adoption$coeftest),
      actorPowerIndex = actorPowerIndex,
      actorPowerDrivers = actorPowerDrivers,
      instQualityDrivers = instQualityDrivers,
      controlDrivers = controlDrivers,
      includeLegend = FALSE,
      prefix = "  adoption ~ "
    ), "\n\n")

    # Stringency
    fullWorkflowSummary <- paste0(fullWorkflowSummary, formatModelEquation(
      coefficients = .coeftestToDataFrame(allModels[[s]]$stringency$coeftest),
      actorPowerIndex = actorPowerIndex,
      actorPowerDrivers = actorPowerDrivers,
      instQualityDrivers = instQualityDrivers,
      controlDrivers = controlDrivers,
      includeLegend = FALSE,
      prefix = "  stringency ~ "
    ), "\n\n")
  }

  fullWorkflowSummary <- paste0(
    fullWorkflowSummary,
    "---\nSignificance Legend:\n*** p < 0.01\n** p < 0.05\n* p < 0.1\n"
  )

  # --- 4. Print summaries ---
  message("\n", paste(rep("=", 60), collapse = ""))
  message("MODEL ESTIMATION WORKFLOW COMPLETE")
  message(paste(rep("=", 60), collapse = ""))
  message("\n--- Combined Model Statistics ---")
  print(combinedModelStats)

  return(list(
    models = allModels,
    coefficients = combinedCoefficients,
    model_stats = combinedModelStats,
    workflowSummary = fullWorkflowSummary
  ))
}

# Convert a coeftest object to a tidy data.frame.
# Column names from as.data.frame(coeftest) are: Estimate, Std. Error, z value, Pr(>|z|)
# @keywords internal
.coeftestToDataFrame <- function(ct) {
  df <- as.data.frame(unclass(ct))
  df$term <- rownames(df)
  rownames(df) <- NULL
  names(df) <- c("estimate", "stdError", "zValue", "pValue", "term")
  df$signif <- ifelse(df$pValue < 0.001, "***",
    ifelse(df$pValue < 0.01, "**",
      ifelse(df$pValue < 0.05, "*",
        ifelse(df$pValue < 0.1, ".", "")
      )
    )
  )
  df <- df[, c("term", "estimate", "stdError", "zValue", "pValue", "signif")]
  return(df)
}
