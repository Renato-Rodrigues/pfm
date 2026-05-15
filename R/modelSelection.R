#' @title modelSelection
#' @description Performs structured model selection incrementally or combinatorially.
#'
#' @param data A \code{magpie} object produced by \code{panelDataHistorical}.
#' @param sector Character. \code{"Bulk"} or \code{"Diffuse"}.
#' @param stage Character. \code{"adoption"} or \code{"stringency"}.
#' @param family Character. GLM family for stringency.
#' @param testMode Character. \code{"incremental"} or \code{"combinations"}.
#' @param actorPowerDrivers Character vector of Actor Power driver names.
#' @param actorPowerIndex Character vector of Actor Power Index names.
#' @param instQualityDrivers Character vector of Institutional Quality drivers.
#' @param controlDrivers Character vector of control variable names.
#' @param regionMappingFixedEffects Character or NULL. Region mapping file.
#' @param timeTrend Logical. Add a linear time trend (can be applied from start). Default: \code{TRUE}.
#' @param useFirth Logical. If \code{TRUE}, uses bias-reduced estimation (Firth-type) for both adoption (Logit) and stringency (GLM) stages.
#' @param criterion Character. \code{"AIC"}, \code{"BIC"}, \code{"AICc"},
#'   \code{"HQIC"}, or \code{"pseudoR2"} for best model.
#' @param regionFEMode Character. \code{"block"} or \code{"individual"} for fixed effects.
#' @param stabilityShift Numeric. Small constant added to the dependent variable
#'   (e.g., \code{0.1}) to stabilize Gamma regression in the stringency stage.
#'   Default: \code{0}.
#' @param logTransform Logical. If \code{TRUE}, the stringency-stage dependent
#'   variable is transformed to \code{log(1 + ECP)}. This is useful when
#'   using a gaussian family with an identity link (OLS on log-prices).
#'   Ignored for the adoption stage. Default: \code{TRUE}.
#' @param lag Integer. Time lag for drivers in years. Default: \code{1}.
#'
#' @return A list containing:
#'   \describe{
#'     \item{\code{bestModel}}{List object with the results and diagnostics for the overall best selected model.}
#'     \item{\code{bestStep}}{Numeric index for the best model in the selection path.}
#'     \item{\code{selectionPath}}{A \code{data.frame} logging the sequence of models tested. It contains:
#'       \itemize{
#'         \item{\code{step}:} {Numeric index of the evaluated model.}
#'         \item{\code{phase}:} {Phase of the model selection (e.g., "Phase 1: IQ Incremental").}
#'         \item{\code{description}:} {Short summary of the variables added or tested.}
#'         \item{\code{nPredictors}:} {Total number of parameters estimated (k), including the intercept.}
#'         \item{\code{aic}:} {Akaike Information Criterion score. It estimates the relative quality of statistical
#'           models by evaluating the trade-off between the goodness of fit of the model and its complexity.
#'           Lower is better.}
#'         \item{\code{bic}:} {Bayesian Information Criterion score. Similar to AIC, but it applies a heavier penalty
#'           for models with more parameters, strongly guarding against overfitting. Lower is better.}
#'         \item{\code{aicc}:} {Corrected Akaike Information Criterion score. A version of AIC tailored for smaller
#'           sample sizes. Lower is better.}
#'         \item{\code{hqic}:} {Hannan-Quinn Information Criterion. An alternative to AIC and BIC with a mathematically
#'           intermediate penalty for complexity. Lower is better.}
#'         \item{\code{loglik}:} {Log-likelihood of the model. It measures how likely the observed data is given the
#'           estimated model parameters. Higher values (closer to zero in negative logs) indicate a better fit.}
#'         \item{\code{pseudoR2}:} {McFadden's Pseudo R-squared. Defined as 1 - (Log-likelihood of the full model /
#'           Log-likelihood of the null model). A value between 0.2 and 0.4 often indicates an excellent model fit.}
#'         \item{\code{nSignificant}:} {Number of predictors with p-values < 0.05.}
#'         \item{\code{kOverN}:} {Ratio of estimated parameters (k) to the number of observations (N).}
#'         \item{\code{overfitting}:} {Logical indicating potential overfitting. It is set to \code{TRUE} if the
#'           \code{kOverN} ratio is greater than 0.1 (i.e., less than 10 observations per parameter).}
#'         \item{\code{separation}:} {Logical indicating quasi-complete separation issues. It is set to \code{TRUE}
#'           if any absolute coefficient is exceptionally large (> 10) or, for logistic regression, if predicted
#'           probabilities are extremely close to 0 or 1.}
#'         \item{\code{converged}:} {Logical indicating if the maximum likelihood estimation converged.}
#'         \item{\code{formula}:} {Character string of the exact regression formula evaluated.}
#'       }
#'     }
#'     \item{\code{allModels}}{List containing all \code{fitAndDiagnose} results for evaluated models.}
#'     \item{\code{allCoefficients}}{A \code{data.frame} containing the \code{coeftest} coefficients for all steps.
#'       It contains:
#'       \itemize{
#'         \item{\code{term}:} {The name of the independent variable/predictor.}
#'         \item{\code{estimate}:} {The estimated regression coefficient, representing the strength and
#'             direction of the relationship.}
#'         \item{\code{stdError}:} {The standard error of the estimate, representing the precision of the coefficient.}
#'         \item{\code{statistic}:} {The t-statistic or z-statistic (estimate divided by standard error).}
#'         \item{\code{pValue}:} {The probability that the observed statistical value occurred by chance
#'           (assuming the true coefficient was 0).}
#'       }
#'     }
#'     \item{\code{recommendation}}{A character string synthesizing the final best model recommendation.}
#'     \item{\code{evalLogs}}{A character vector recording every message output during the evaluation sequence,
#'       specifically noting instances where different Selection Criteria conflict over which model is best.}
#'     \item{\code{groupAnalysis}}{A structured list reporting drop-one-group variance partitioning for the final model.
#'       Details the exact incremental \code{pseudoR2} contribution per predictor block alongside mathematical
#'       \code{ANOVA} Deviance test statistics (\code{Chi-Sq}) and probability metrics (\code{p-value}) to
#'       confirm the formal statistical significance of each extracted group.}
#'     \item{\code{sector}, \code{stage}, \code{testMode}}{Tracking arguments for the selection workflow.}
#'   }
#' @author Renato Rodrigues
#'
#' @importFrom stats glm binomial Gamma gaussian as.formula BIC logLik
#' @importFrom lmtest coeftest
#' @importFrom sandwich vcovCL
#' @importFrom logistf logistf
#'
#' @export
modelSelection <- function(
    data,
    sector = "Bulk",
    stage = "adoption",
    family = "Gamma",
    testMode = "incremental",
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
    baselineIQ = list(adoption = "Government Effectiveness", stringency = "Government Effectiveness"),
    timeTrend = TRUE,
    useFirth = TRUE,
    criterion = "AIC",
    regionFEMode = "block",
    stabilityShift = 0,
    logTransform = TRUE,
    lag = 1) {
  # --- 1. Argument validation ---
  stage <- match.arg(stage, c("adoption", "stringency"))
  testMode <- match.arg(testMode, c("incremental", "combinations"))
  criterion <- match.arg(criterion, c("AIC", "BIC", "AICc", "HQIC", "pseudoR2"))
  regionFEMode <- match.arg(regionFEMode, c("block", "individual"))

  .msg <- function(...) {
    base::message(...)
    flush.console()
  }

  # --- 2. Data preparation ---
  df <- preparePanelData(
    data = data,
    sector = sector,
    actorPowerDrivers = actorPowerDrivers,
    actorPowerIndex = actorPowerIndex,
    instQualityDrivers = instQualityDrivers,
    controlDrivers = controlDrivers,
    regionMappingFixedEffects = regionMappingFixedEffects,
    lag = lag
  )

  # --- 3. Define dependent variable and filter data ---
  if (stage == "adoption") {
    df$adoption <- as.integer(df$ecp > 0)
    depVar <- "adoption"
  } else {
    df <- df[df$ecp > 0, , drop = FALSE]
    if (stabilityShift > 0) {
      df$ecp <- df$ecp + stabilityShift
      .msg("Numerical stability shift applied: +", stabilityShift)
    }
    if (isTRUE(logTransform)) {
      df$ecp <- log(1 + df$ecp)
      .msg("Log-transform applied: ecp -> log(1 + ecp)")
    }
    depVar <- "ecp"
  }

  # --- 4. Compute null model log-likelihood ---
  n <- nrow(df)
  .msg("Data prepared: ", n, " observations")

  nullLoglik <- computeNullLoglik(df, depVar, stage, family, useFirth)

  stepsList <- list()
  stepCounter <- 1
  evalLogs <- character()

  .compareModels <- function(newRes, bestRes, criterion, logs) {
    if (!is.null(newRes$rejectionReason)) {
      msg <- paste0("         [Rejected] ", newRes$rejectionReason)
      .msg(msg)
      logs <- c(logs, msg)
      stepStr <- if (is.null(bestRes)) "0" else bestRes$step
      selMsg <- paste0("         Selected model: keep previously selected model (step ", stepStr, ").")
      .msg(selMsg)
      logs <- c(logs, selMsg)
      return(list(improves = FALSE, logs = logs, decision = paste0("Rejected: ", newRes$rejectionReason)))
    }

    if (isTRUE(newRes$highVIF)) {
      flaggedStr <- paste(newRes$vifFlagged, collapse = ", ")
      msg <- paste0("         [Rejected] High VIF detected: ", flaggedStr, " (Max VIF: ", round(newRes$maxVIF, 2), ")")
      .msg(msg)
      logs <- c(logs, msg)
      stepStr <- if (is.null(bestRes)) "0" else bestRes$step
      selMsg <- paste0("         Selected model: keep previously selected model (step ", stepStr, ").")
      .msg(selMsg)
      logs <- c(logs, selMsg)
      return(list(improves = FALSE, logs = logs, decision = "Rejected: High VIF"))
    }

    if (isTRUE(newRes$separation)) {
      msg <- "         [Rejected] Quasi-complete separation detected."
      .msg(msg)
      logs <- c(logs, msg)
      stepStr <- if (is.null(bestRes)) "0" else bestRes$step
      selMsg <- paste0("         Selected model: keep previously selected model (step ", stepStr, ").")
      .msg(selMsg)
      logs <- c(logs, selMsg)
      return(list(improves = FALSE, logs = logs, decision = "Rejected: Separation"))
    }

    if (is.null(bestRes)) {
      deltaMsg <- paste0("         Sequential Pseudo-R2 (vs Base): +", round(newRes$pseudoR2 * 100, 2), "%")
      .msg(deltaMsg)
      logs <- c(logs, deltaMsg)

      selMsg <- paste0("         Selected model: replace with current model (step ", newRes$step, ").")
      .msg(selMsg)
      logs <- c(logs, selMsg)
      return(list(improves = TRUE, logs = logs, decision = "Base Selection"))
    }

    critNew <- newRes[[tolower(criterion)]]
    critBest <- bestRes[[tolower(criterion)]]
    improves <- if (criterion == "pseudoR2") (critNew > critBest) else (critNew < critBest)

    checkMetric <- function(name, newV, bestV, lowerIsBetter = TRUE) {
      res <- list(new = NULL, best = NULL)
      if (lowerIsBetter) {
        if (newV < bestV) {
          res$new <- paste0(name, " (", round(newV, 2), " < ", round(bestV, 2), ")")
        } else if (newV > bestV) res$best <- paste0(name, " (", round(bestV, 2), " < ", round(newV, 2), ")")
      } else {
        if (newV > bestV) {
          res$new <- paste0(name, " (", round(newV, 4), " > ", round(bestV, 4), ")")
        } else if (newV < bestV) res$best <- paste0(name, " (", round(bestV, 4), " > ", round(newV, 4), ")")
      }
      res
    }

    metrics <- list(
      checkMetric("AIC", newRes$aic, bestRes$aic),
      checkMetric("BIC", newRes$bic, bestRes$bic),
      checkMetric("AICc", newRes$aicc, bestRes$aicc),
      checkMetric("HQIC", newRes$hqic, bestRes$hqic),
      checkMetric("pseudoR2", newRes$pseudoR2, bestRes$pseudoR2, lowerIsBetter = FALSE)
    )

    betterNew <- unlist(lapply(metrics, function(x) x$new))
    betterBest <- unlist(lapply(metrics, function(x) x$best))

    if (length(betterNew) > 0 && length(betterBest) > 0) {
      msg <- paste0(
        "         [Conflict] New model is better on: ", paste(betterNew, collapse = ", "),
        ". Previous was better on: ", paste(betterBest, collapse = ", "), "."
      )
      .msg(msg)
      logs <- c(logs, msg)
    }

    deltaR2 <- (newRes$pseudoR2 - bestRes$pseudoR2) * 100
    deltaMsg <- paste0(
      "         Sequential Pseudo-R2 (vs current selected): ",
      if (deltaR2 > 0) "+" else "", round(deltaR2, 2), "% ",
      "(", round(bestRes$pseudoR2, 4), " -> ", round(newRes$pseudoR2, 4), ")"
    )
    .msg(deltaMsg)
    logs <- c(logs, deltaMsg)

    if (improves) {
      selMsg <- paste0("         Selected model: replace with current model (step ", newRes$step, ").")
      decision <- paste0("Replaced Step ", bestRes$step)
    } else {
      selMsg <- paste0("         Selected model: keep previously selected model (step ", bestRes$step, ").")
      decision <- paste0("Kept Step ", bestRes$step)
    }
    .msg(selMsg)
    logs <- c(logs, selMsg)

    list(improves = improves, logs = logs, decision = decision)
  }

  .fitStep <- function(api, iq, ap, ctrl, feLevels, phaseDesc, shortDesc) {
    fml <- makeFormula(depVar, api, iq, ap, ctrl, feLevels, timeTrend)
    # Note: preparePanelData was already called with 'lag' during data prep in modelSelection.
    # However, if fitStep needs to re-prepare data (it doesn't, it uses 'df'), we are fine.
    # But wait, does fitStep need the lag? No, it uses 'df' which is already lagged.
    fit <- fitAndDiagnose(fml, df, depVar, stage, family, useFirth, nullLoglik, n)
    fit$phase <- phaseDesc
    if (isTRUE(fit$maxitWarning)) {
      shortDesc <- paste0(shortDesc, " [Max Iterations Exceeded]")
    }
    fit$description <- shortDesc
    fit$api <- api
    fit$iq <- iq
    fit$ap <- ap
    fit$ctrl <- ctrl
    fit$feLevels <- feLevels
    return(fit)
  }

  .logStep <- function(res, idx, criterion) {
    res$step <- idx
    cTest <- coeftestToDataFrame(res$coeftest)
    cTest$step <- idx
    pathRow <- data.frame(
      step = idx, phase = res$phase, description = res$description,
      nPredictors = res$nPredictors, aic = round(res$aic, 2), bic = round(res$bic, 2),
      aicc = round(res$aicc, 2), hqic = round(res$hqic, 2),
      loglik = round(res$loglik, 2), pseudoR2 = round(res$pseudoR2, 4),
      nSignificant = res$nSignificant, kOverN = round(res$kOverN, 4),
      overfitting = res$overfitting, separation = res$separation, converged = res$converged,
      maxVIF = round(res$maxVIF, 2), highVIF = res$highVIF, decision = NA_character_,
      formula = paste(deparse(res$formula, width.cutoff = 500), collapse = " "),
      formatted_formula = formatModelEquation(
        coefficients = cTest,
        actorPowerIndex = actorPowerIndex,
        actorPowerDrivers = actorPowerDrivers,
        instQualityDrivers = instQualityDrivers,
        controlDrivers = controlDrivers
      ),
      stringsAsFactors = FALSE
    )

    critValue <- round(res[[tolower(criterion)]], 4)
    fmlStr <- paste(deparse(res$formula, width.cutoff = 500), collapse = " ")
    msg <- paste0(
      "Step ", idx, ": ", res$description, "\n",
      "         Model: ", fmlStr, "\n",
      "         Result: ", criterion, ": ", critValue
    )
    .msg(msg)

    list(model = res, cTest = cTest, pathRow = pathRow, msg = msg)
  }

  # --- Helper for generating subset combinations (non-empty subsets of vectors)
  .generateSubsets <- function(baseList, addedItem = NULL) {
    if (is.null(addedItem)) {
      if (length(baseList) == 0) {
        return(list())
      }
      combos <- list()
      for (i in seq_along(baseList)) {
        combsOfI <- combn(baseList, i, simplify = FALSE)
        combos <- c(combos, combsOfI)
      }
      return(combos)
    } else {
      # Return subsets of baseList U {addedItem} that CONTAIN addedItem
      if (length(baseList) == 0) {
        return(list(addedItem))
      }
      combos <- list(addedItem)
      for (i in 1:length(baseList)) {
        combsOfI <- combn(baseList, i, simplify = FALSE)
        for (comb in combsOfI) combos <- c(combos, list(c(comb, addedItem)))
      }
      return(combos)
    }
  }

  # --- 5. Run selection workflow ---
  grandBestModel <- NULL

  if (testMode == "incremental") { # Represents the sequential 4-phase mode expected

    # === PHASE 1: Baseline Model ===
    p1Msg <- "--- Phase 1: Establish Baseline Model ---"
    .msg(p1Msg)
    evalLogs <- c(evalLogs, p1Msg)

    baseIQ <- baselineIQ[[stage]]
    baseCtrl <- controlDrivers[1]

    for (api in actorPowerIndex) {
      # Step A (No FE)
      descA <- paste0("Base: API=", api, " (No FE)")
      resA <- .fitStep(api, baseIQ, NULL, baseCtrl, NULL, "Phase 1: Baseline", descA)
      stepOutA <- .logStep(resA, stepCounter, criterion)
      stepsList[[stepCounter]] <- stepOutA
      evalLogs <- c(evalLogs, stepOutA$msg)
      stepCounter <- stepCounter + 1
      compA <- .compareModels(stepOutA$model, grandBestModel, criterion, evalLogs)
      evalLogs <- compA$logs
      stepsList[[stepCounter - 1]]$pathRow$decision <- compA$decision
      if (compA$improves) grandBestModel <- stepOutA$model

      # Step B (With FE block)
      descB <- paste0("Base: API=", api, " (+ regionFE)")
      feArgB <- if (!is.null(regionMappingFixedEffects) && regionFEMode == "block") "regionFE" else NULL
      if (!is.null(feArgB)) {
        resB <- .fitStep(api, baseIQ, NULL, baseCtrl, "regionFE", "Phase 1: Baseline", descB)
        stepOutB <- .logStep(resB, stepCounter, criterion)
        stepsList[[stepCounter]] <- stepOutB
        evalLogs <- c(evalLogs, stepOutB$msg)
        stepCounter <- stepCounter + 1
        compB <- .compareModels(stepOutB$model, grandBestModel, criterion, evalLogs)
        evalLogs <- compB$logs
        stepsList[[stepCounter - 1]]$pathRow$decision <- compB$decision
        if (compB$improves) grandBestModel <- stepOutB$model
      }
    }

    if (is.null(grandBestModel) && length(stepsList) > 0) {
      validSteps <- Filter(function(x) isTRUE(x$pathRow$phase == "Phase 1: Baseline"), stepsList)
      if (length(validSteps) > 0) {
        crits <- vapply(validSteps, function(x) x$model[[tolower(criterion)]], FUN.VALUE = numeric(1))
        bestIdx <- if (criterion == "pseudoR2") which.max(crits) else which.min(crits)
        grandBestModel <- validSteps[[bestIdx]]$model
        msgFb <- "         [Warning] All baseline models rejected. Forced fallback to best available Phase 1 model."
        .msg(msgFb)
        evalLogs <- c(evalLogs, msgFb)
      }
    }

    # === PHASE 2: Institutional Quality Drivers ===
    p2Msg <- "--- Phase 2: Select Additional IQ Drivers ---"
    .msg(p2Msg)
    evalLogs <- c(evalLogs, p2Msg)

    phaseBestModel <- grandBestModel
    remainIq <- setdiff(instQualityDrivers, baseIQ)

    for (drv in remainIq) {
      curIq <- phaseBestModel[["iq"]]
      candSubsets <- .generateSubsets(curIq, drv)

      stepBestModel <- phaseBestModel
      for (iqSet in candSubsets) {
        desc <- paste0("Test IQ: [", paste(iqSet, collapse = ", "), "]")
        res <- .fitStep(phaseBestModel[["api"]], iqSet, phaseBestModel[["ap"]], phaseBestModel[["ctrl"]], phaseBestModel[["feLevels"]], "Phase 2: IQ Selection", desc)
        stepOut <- .logStep(res, stepCounter, criterion)
        stepsList[[stepCounter]] <- stepOut
        evalLogs <- c(evalLogs, stepOut$msg)
        stepCounter <- stepCounter + 1
        comp <- .compareModels(stepOut$model, stepBestModel, criterion, evalLogs)
        evalLogs <- comp$logs
        stepsList[[stepCounter - 1]]$pathRow$decision <- comp$decision
        if (comp$improves) stepBestModel <- stepOut$model
      }
      phaseBestModel <- stepBestModel
    }
    grandBestModel <- phaseBestModel

    # === PHASE 3: Actor Power Drivers ===
    p3Msg <- "--- Phase 3: Select AP Drivers ---"
    .msg(p3Msg)
    evalLogs <- c(evalLogs, p3Msg)

    phaseBestModel <- grandBestModel

    for (drv in actorPowerDrivers) {
      curAp <- phaseBestModel[["ap"]]
      candSubsets <- .generateSubsets(curAp, drv)

      stepBestModel <- phaseBestModel
      for (apSet in candSubsets) {
        desc <- paste0("Test AP Combo: [", paste(apSet, collapse = ", "), "]")
        res <- .fitStep(phaseBestModel[["api"]], phaseBestModel[["iq"]], apSet, phaseBestModel[["ctrl"]], phaseBestModel[["feLevels"]], "Phase 3: AP Combos", desc)
        stepOut <- .logStep(res, stepCounter, criterion)
        stepsList[[stepCounter]] <- stepOut
        evalLogs <- c(evalLogs, stepOut$msg)
        stepCounter <- stepCounter + 1
        comp <- .compareModels(stepOut$model, stepBestModel, criterion, evalLogs)
        evalLogs <- comp$logs
        stepsList[[stepCounter - 1]]$pathRow$decision <- comp$decision
        if (comp$improves) stepBestModel <- stepOut$model
      }
      phaseBestModel <- stepBestModel
    }
    grandBestModel <- phaseBestModel

    # === PHASE 4: Additional Controls ===
    p4Msg <- "--- Phase 4: Additional Controls ---"
    .msg(p4Msg)
    evalLogs <- c(evalLogs, p4Msg)

    phaseBestModel <- grandBestModel
    remainCtrl <- setdiff(controlDrivers, baseCtrl)

    for (drv in remainCtrl) {
      curCtrl <- phaseBestModel[["ctrl"]]
      candSubsets <- .generateSubsets(curCtrl, drv)

      stepBestModel <- phaseBestModel
      for (ctrlSet in candSubsets) {
        desc <- paste0("Test Ctrl: [", paste(ctrlSet, collapse = ", "), "]")
        res <- .fitStep(phaseBestModel[["api"]], phaseBestModel[["iq"]], phaseBestModel[["ap"]], ctrlSet, phaseBestModel[["feLevels"]], "Phase 4: Ctrl Selection", desc)
        stepOut <- .logStep(res, stepCounter, criterion)
        stepsList[[stepCounter]] <- stepOut
        evalLogs <- c(evalLogs, stepOut$msg)
        stepCounter <- stepCounter + 1
        comp <- .compareModels(stepOut$model, stepBestModel, criterion, evalLogs)
        evalLogs <- comp$logs
        stepsList[[stepCounter - 1]]$pathRow$decision <- comp$decision
        if (comp$improves) stepBestModel <- stepOut$model
      }
      phaseBestModel <- stepBestModel
    }
    grandBestModel <- phaseBestModel
  } else { # testMode == "combinations"
    # === PHASE 1, 2, 3 Combined ===
    pCmbMsg <- "--- Combinations Mode: Phase 1, 2, 3 Combined ---"
    .msg(pCmbMsg)
    evalLogs <- c(evalLogs, pCmbMsg)

    baseCtrl <- controlDrivers[1]
    allIqCombos <- .generateSubsets(instQualityDrivers, NULL)

    allApConfigs <- list()
    for (api in actorPowerIndex) allApConfigs <- c(allApConfigs, list(list(type = "api", val = api)))
    apSubsets <- .generateSubsets(actorPowerDrivers, NULL)
    for (ap in apSubsets) allApConfigs <- c(allApConfigs, list(list(type = "ap", val = ap)))

    for (iqSet in allIqCombos) {
      for (apCfg in allApConfigs) {
        apiArg <- if (apCfg$type == "api") apCfg$val else NULL
        apArg <- if (apCfg$type == "ap") apCfg$val else NULL
        feArg <- if (!is.null(regionMappingFixedEffects) && regionFEMode == "block") "regionFE" else NULL

        desc <- paste0("Combos: IQ[", paste(iqSet, collapse = ","), "] ", apCfg$type, "[", paste(apCfg$val, collapse = ","), "]")
        res <- .fitStep(apiArg, iqSet, apArg, baseCtrl, feArg, "Combos P1-3", desc)
        stepOut <- .logStep(res, stepCounter, criterion)
        stepsList[[stepCounter]] <- stepOut
        evalLogs <- c(evalLogs, stepOut$msg)
        stepCounter <- stepCounter + 1
        comp <- .compareModels(stepOut$model, grandBestModel, criterion, evalLogs)
        evalLogs <- comp$logs
        stepsList[[stepCounter - 1]]$pathRow$decision <- comp$decision
        if (comp$improves) grandBestModel <- stepOut$model
      }
    }

    # === PHASE 4: Controls ===
    p4Msg <- "--- Combinations Mode: Phase 4 Controls ---"
    .msg(p4Msg)
    evalLogs <- c(evalLogs, p4Msg)
    phaseBestModel <- grandBestModel
    remainCtrl <- setdiff(controlDrivers, baseCtrl)

    for (drv in remainCtrl) {
      curCtrl <- phaseBestModel[["ctrl"]]
      candSubsets <- .generateSubsets(curCtrl, drv)

      stepBestModel <- phaseBestModel
      for (ctrlSet in candSubsets) {
        desc <- paste0("Test Ctrl: [", paste(ctrlSet, collapse = ", "), "]")
        res <- .fitStep(phaseBestModel[["api"]], phaseBestModel[["iq"]], phaseBestModel[["ap"]], ctrlSet, phaseBestModel[["feLevels"]], "Phase 4: Ctrl Selection", desc)
        stepOut <- .logStep(res, stepCounter, criterion)
        stepsList[[stepCounter]] <- stepOut
        evalLogs <- c(evalLogs, stepOut$msg)
        stepCounter <- stepCounter + 1
        comp <- .compareModels(stepOut$model, stepBestModel, criterion, evalLogs)
        evalLogs <- comp$logs
        stepsList[[stepCounter - 1]]$pathRow$decision <- comp$decision
        if (comp$improves) stepBestModel <- stepOut$model
      }
      phaseBestModel <- stepBestModel
    }
    grandBestModel <- phaseBestModel
  }

  allModelsList <- lapply(stepsList, function(x) x$model)
  selectionPath <- do.call(rbind, lapply(stepsList, function(x) x$pathRow))
  allCoefficients <- do.call(rbind, lapply(stepsList, function(x) x$cTest))
  rownames(selectionPath) <- NULL
  rownames(allCoefficients) <- NULL

  criterionValues <- vapply(allModelsList, function(m) m[[tolower(criterion)]], numeric(1))

  hasSeparation <- vapply(allModelsList, function(m) isTRUE(m$separation), logical(1))
  hasHighVif <- vapply(allModelsList, function(m) isTRUE(m$highVIF), logical(1))
  validIdx <- which(!hasSeparation & !hasHighVif)
  if (length(validIdx) == 0) {
    if (length(allModelsList) > 0) {
      bestStep <- if (criterion == "pseudoR2") which.max(criterionValues) else which.min(criterionValues)
    } else {
      bestStep <- NA
    }
    warning("All models show signs of quasi-complete separation or high VIF.")
  } else {
    validCrits <- criterionValues[validIdx]
    bestValidIdx <- if (criterion == "pseudoR2") which.max(validCrits) else which.min(validCrits)
    bestStep <- validIdx[bestValidIdx]
  }

  bestGlobalModel <- allModelsList[[bestStep]]

  msg1 <- paste0("\n", paste(rep("=", 60), collapse = ""))
  msg2 <- "--- Step 5: Overall Best Model Selection ---"
  msg3 <- paste("Selected API: ", bestGlobalModel$api)
  msg4 <- paste(rep("=", 60), collapse = "")
  .msg(msg1)
  .msg(msg2)
  .msg(msg3)
  .msg(msg4)
  evalLogs <- c(evalLogs, msg1, msg2, msg3, msg4)

  recommendation <- buildRecommendation(selectionPath, bestStep, criterion, allModelsList)
  .msg("\n", recommendation)
  evalLogs <- c(evalLogs, "", recommendation)

  groupAnalysis <- performGroupAnalysis(bestGlobalModel, df, depVar, stage, family, useFirth, nullLoglik, n, timeTrend, .msg)

  return(list(
    bestModel       = bestGlobalModel,
    bestEquation    = formatModelEquation(
      coefficients    = coeftestToDataFrame(bestGlobalModel$coeftest),
      actorPowerIndex = actorPowerIndex,
      actorPowerDrivers = actorPowerDrivers,
      instQualityDrivers = instQualityDrivers,
      controlDrivers = controlDrivers,
      includeLegend   = TRUE
    ),
    bestStep        = bestStep,
    selectionPath   = selectionPath,
    allModels       = allModelsList,
    allCoefficients = allCoefficients,
    recommendation  = recommendation,
    evalLogs        = evalLogs,
    groupAnalysis   = groupAnalysis,
    sector          = sector,
    stage           = stage,
    testMode        = testMode
  ))
}
