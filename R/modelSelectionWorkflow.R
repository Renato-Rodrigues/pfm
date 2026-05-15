#' @title modelSelectionWorkflow
#' @description Runs backward structured model selection for all combinations of
#' sectors and stages (Adoption / Stringency). This is a convenience wrapper
#' around \code{\link{modelSelection}}.
#'
#' @param aggregate Boolean. If TRUE, aggregates to region mapping.
#' @param y Numeric vector of years. Default: \code{2000:2022}.
#' @param outputRegionMappingFile Character. Region mapping file for data
#'   aggregation. Default: \code{"regionmappingH12.csv"}.
#' @param coeff List of coefficients for actor power index calculation.
#' @param sectors Character vector. Default: \code{c("Bulk", "Diffuse")}.
#' @param stages Character vector. Default: \code{c("adoption", "stringency")}.
#' @param family Character. GLM family for stringency models.
#' @param actorPowerDrivers Character vector of individual Actor Power driver
#'   names. Only used as main effects if \code{actorPowerIndex} is \code{NULL}.
#' @param actorPowerIndex Character or NULL. Name of the Actor Power Index
#'   variable. If provided, it is used as the sole Actor Power main effect
#'   and for interaction terms.
#' @param instQualityDrivers Character vector of Institutional Quality driver
#'   names.
#' @param controlDrivers Character vector of control variable names.
#' @param regionMappingFixedEffects Character or NULL. Region mapping file for fixed
#'   effects. If \code{NULL}, region fixed effects are omitted.
#'   Default: \code{"regionmappingH12.csv"}.
#' @param testMode Character. \code{"incremental"} or \code{"combinations"}. Default: \code{"incremental"}.
#' @param useFirth Logical. If \code{TRUE} (default), uses Firth's penalized
#'   likelihood logistic regression for the adoption stage.
#' @param stabilityShift Numeric. Small constant added to the dependent variable
#'   (e.g., \code{0.1}) to stabilize Gamma regression in the stringency stage.
#'   Default: \code{0}.
#' @param logTransform Logical. If \code{TRUE}, the stringency-stage dependent
#'   variable is transformed to \code{log(1 + ECP)}. Default: \code{TRUE}.
#' @param includeLaggedECP Logical. If \code{TRUE}, adds the lagged ECP
#'   (\code{lagged_ecp}) as an additional predictor in the stringency stage.
#'   Requires the panel data to include \code{lagged_ecp} (computed by
#'   \code{preparePanelData} when \code{lag >= 1}). Default: \code{FALSE}.
#' @param panelData Optional \code{magpie} object or \code{data.frame}. If provided,
#'   skips loading and processing historical data via \code{panelDataHistorical}.
#'
#' @return A list with:
#'   \describe{
#'     \item{selections}{Named list (e.g. \code{Bulk_adoption}) each containing
#'       the full result from \code{modelSelection}.}
#'     \item{bestModels}{data.frame summary of the best model per combo:
#'       \code{sector}, \code{stage}, \code{bestStep}, \code{nPredictors},
#'       \code{aic}, \code{bic}, \code{converged}.}
#'     \item{selectionPaths}{Combined data.frame of all selection paths across
#'       combos, with \code{sector} and \code{stage} columns.}
#'     \item{bestCoefficients}{Combined tidy coefficients data.frame from the
#'       best model of each combo.}
#'     \item{coeffSummaryTable}{A list of wide-format summary tables (pvalue,
#'       estimate, stdError, statistic) pivoting their values across all
#'       evaluated sector/stage combos.}
#'     \item{groupAnalysisTable}{A consolidated table reporting group-level
#'       contributions, ANOVA results, and predictor counts across all models.}
#'   }
#'
#' @author Renato Rodrigues
#'
#' @importFrom dplyr %>% mutate select arrange filter
#' @importFrom tidyr pivot_wider
#'
#' @export
#'
modelSelectionWorkflow <- function(
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
    stages = c("adoption", "stringency"),
    family = "Gamma",
    actorPowerDrivers = c(
      "VRE share", "Electrification",
      "Coal primary energy share", "Oil/Gas primary energy share",
      "Fossil share in Industry"
    ),
    actorPowerIndex = c("Actor Power Index", "Incumbent Power"),
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
    criterion = "AIC",
    testMode = "incremental",
    useFirth = TRUE,
    stabilityShift = 0,
    logTransform = TRUE,
    includeLaggedECP = FALSE,
    panelData = NULL) {
  # --- 1. Load data once ---
  if (is.null(panelData)) {
    message("Loading historical panel data...")
    panelData <- panelDataHistorical(
      aggregate = aggregate,
      y = y,
      outputRegionMappingFile = outputRegionMappingFile,
      coeff = coeff
    )
  }
  message("Data loaded: ", paste(dim(panelData), collapse = " x "))

  # --- 2. Run model selection for each sector x stage combo ---
  selections <- list()
  bestModelRows <- list()
  pathRows <- list()
  bestCoefRows <- list()
  groupAnalysisRows <- list()

  for (sec in sectors) {
    for (stg in stages) {
      comboName <- paste(sec, stg, sep = "_")
      message("\n", paste(rep("=", 60), collapse = ""))
      message("MODEL SELECTION: ", sec, " / ", stg)
      message(paste(rep("=", 60), collapse = ""))

      sel <- modelSelection(
        data = panelData,
        sector = sec,
        stage = stg,
        family = family,
        testMode = testMode,
        actorPowerDrivers = actorPowerDrivers,
        actorPowerIndex = actorPowerIndex,
        instQualityDrivers = instQualityDrivers,
        controlDrivers = if (stg == "stringency" && isTRUE(includeLaggedECP))
          c(controlDrivers, "lagged_ecp") else controlDrivers,
        regionMappingFixedEffects = regionMappingFixedEffects,
        regionFEMode = "block",
        timeTrend = timeTrend,
        criterion = criterion,
        useFirth = useFirth,
        stabilityShift = stabilityShift
      )
      selections[[comboName]] <- sel

      # Best model summary row
      best <- sel$bestModel
      bestPath <- sel$selectionPath[sel$bestStep, ]
      bestModelRows[[comboName]] <- data.frame(
        sector = sec,
        stage = stg,
        bestStep = sel$bestStep,
        description = bestPath$description,
        formula = bestPath$formula,
        nPredictors = bestPath$nPredictors,
        nSignificant = bestPath$nSignificant,
        aic = bestPath$aic,
        bic = bestPath$bic,
        aicc = bestPath$aicc,
        hqic = bestPath$hqic,
        loglik = bestPath$loglik,
        pseudoR2 = bestPath$pseudoR2,
        maxVIF = bestPath$maxVIF,
        converged = isTRUE(best$converged),
        stringsAsFactors = FALSE
      )

      # Selection path with sector/stage labels
      sp <- sel$selectionPath
      sp$sector <- sec
      sp$stage <- stg
      pathRows[[comboName]] <- sp

      # Best model coefficients
      bestCoef <- sel$allCoefficients[
        sel$allCoefficients$step == sel$bestStep, ,
        drop = FALSE
      ]
      bestCoef$sector <- sec
      bestCoef$stage <- stg
      bestCoefRows[[comboName]] <- bestCoef

      # Group analysis results
      ga <- sel$groupAnalysis
      for (gn in names(ga)) {
        res <- ga[[gn]]
        groupAnalysisRows[[length(groupAnalysisRows) + 1]] <- data.frame(
          sector = sec,
          stage = stg,
          group = gn,
          predictors = res$nPred,
          significantPredictors = res$nSig,
          chisq = res$chisq,
          pValue = res$pval,
          deltaR2 = res$deltaR2,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  # --- 3. Combine outputs ---
  bestModels <- do.call(rbind, bestModelRows)
  selectionPaths <- do.call(rbind, pathRows)
  bestCoefficients <- do.call(rbind, bestCoefRows)
  rownames(bestModels) <- NULL
  rownames(selectionPaths) <- NULL
  rownames(bestCoefficients) <- NULL

  # --- 4. Summary Tables ---
  # Consolidate all best coefficients into a formatted group-based summary table
  baseCoeffs <- bestCoefficients %>%
    dplyr::mutate(
      group = dplyr::case_when(
        .data$term == "(Intercept)" ~ "Intercept",
        .data$term %in% make.names(actorPowerIndex) ~ "Actor Power Index",
        .data$term %in% make.names(actorPowerDrivers) ~ "Actor Power",
        .data$term %in% make.names(instQualityDrivers) ~ "Institutional Quality",
        .data$term %in% make.names(controlDrivers) ~ "Control",
        .data$term == "timeTrend" ~ "Time Trend",
        grepl("regionFE", .data$term) ~ "Region Fixed Effects",
        TRUE ~ "Interaction Term"
      ),
      type = paste0(.data$sector, "_", .data$stage)
    )

  pivotCoeffs <- function(df, valCol, rnd = 3) {
    df %>%
      dplyr::mutate(val = round(.data[[valCol]], rnd)) %>%
      dplyr::select(.data$group, driver = .data$term, .data$val, .data$type) %>%
      tidyr::pivot_wider(names_from = .data$type, values_from = .data$val) %>%
      dplyr::arrange(match(.data$group, c(
        "Intercept", "Actor Power Index", "Actor Power", "Institutional Quality",
        "Interaction Term", "Control", "Time Trend", "Region Fixed Effects"
      ))) %>%
      dplyr::relocate("group", "driver", dplyr::any_of(c("Bulk_adoption", "Diffuse_adoption", "Bulk_stringency", "Diffuse_stringency")))
  }

  coeffSummaryTable <- list(
    pvalue    = pivotCoeffs(baseCoeffs, "pValue"),
    estimate  = pivotCoeffs(baseCoeffs, "estimate"),
    stdError  = pivotCoeffs(baseCoeffs, "stdError"),
    statistic = pivotCoeffs(baseCoeffs, "statistic")
  )

  # --- 5. Group Analysis Table ---
  groupAnalysisTable <- do.call(rbind, groupAnalysisRows)
  groupAnalysisTable <- groupAnalysisTable %>%
    dplyr::mutate(
      `ANOVA (Deviance) Chi-Sq` = round(.data$chisq, 2),
      `ANOVA (Deviance) p-value` = ifelse(is.na(.data$pValue), "",
        paste0(round(.data$pValue, 3), ifelse(.data$pValue < 0.05, " (Significant at p < 0.05)", " (Non-Significant)"))
      ),
      `Pseudo-R2 contribution` = paste0(round(.data$deltaR2 * 100, 2), "%")
    ) %>%
    dplyr::select(.data$sector, .data$stage, .data$group, .data$predictors,
      `significant predictors` = .data$significantPredictors,
      `ANOVA (Deviance) Chi-Sq`, `ANOVA (Deviance) p-value`, `Pseudo-R2 contribution`
    ) %>%
    relocate(.data$stage, .data$group) %>%
    dplyr::arrange(
      match(.data$stage, c("adoption", "stringency")),
      match(.data$group, c(
        "Actor Power Index", "Actor Power", "Institutional Quality",
        "Interaction Term", "Control", "Time Trend", "Region Fixed Effects"
      ))
    )

  # --- 6. Full Workflow Summary ---
  fullWorkflowSummary <- ""
  for (sec in sectors) {
    fullWorkflowSummary <- paste0(fullWorkflowSummary, "Sector: ", sec, "\n")
    fullWorkflowSummary <- paste0(fullWorkflowSummary, "Equation:\n")
    
    for (stg in stages) {
      comboName <- paste(sec, stg, sep = "_")
      if (comboName %in% names(selections)) {
        sel <- selections[[comboName]]
        eq <- formatModelEquation(
          coefficients = coeftestToDataFrame(sel$bestModel$coeftest),
          actorPowerIndex = actorPowerIndex,
          actorPowerDrivers = actorPowerDrivers,
          instQualityDrivers = instQualityDrivers,
          controlDrivers = controlDrivers,
          includeLegend = FALSE,
          prefix = paste0("  ", stg, " ~ ")
        )
        fullWorkflowSummary <- paste0(fullWorkflowSummary, eq, "\n\n")
      }
    }
  }
  
  fullWorkflowSummary <- paste0(
    fullWorkflowSummary,
    "---\nSignificance Legend:\n*** p < 0.01\n** p < 0.05\n* p < 0.1\n"
  )

  # --- 7. Print summary ---
  message("\n", paste(rep("=", 60), collapse = ""))
  message("MODEL SELECTION WORKFLOW COMPLETE")
  message(paste(rep("=", 60), collapse = ""))
  message("\nBest models by ", criterion, ":")
  print(bestModels)

  invisible(list(
    selections = selections,
    bestModels = bestModels,
    selectionPaths = selectionPaths,
    bestCoefficients = bestCoefficients,
    coeffSummaryTable = coeffSummaryTable,
    groupAnalysisTable = groupAnalysisTable,
    workflowSummary = fullWorkflowSummary
  ))
}
