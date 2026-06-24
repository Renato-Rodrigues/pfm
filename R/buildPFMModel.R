# nolint start
#' Assemble a PFMModel from a fit result and its training context
#'
#' Accepts either a \code{\link{fitAndDiagnose}} result list (which already
#' carries all diagnostic fields) or the bare fit objects from
#' \code{\link{estimateAdoptionModel}} / \code{\link{estimatePriceStringencyModel}}
#' (where diagnostics are computed here). In both cases the training data and
#' context arguments are required.
#'
#' @param fit List. Output of \code{fitAndDiagnose}, or a list with at minimum
#'   \code{model}, \code{coeftest}, \code{vcov}, and \code{formula}.
#' @param training_data data.frame. The df actually passed to glm/logistf.
#' @param sector Character. \code{"Bulk"} or \code{"Diffuse"}.
#' @param stage Character. \code{"adoption"} or \code{"stringency"}.
#' @param family Character. \code{"logistf"}, \code{"Gamma"}, or \code{"gaussian"}.
#' @param useFirth Logical.
#' @param label Character. Optional human label stored in the manifest.
#'
#' @return A \code{\link{PFMModel}} object.
#'
#' @importFrom stats cor fitted terms
#' @importFrom utils packageVersion
#' @keywords internal
buildPFMModel <- function(fit, training_data, sector, stage, family, useFirth, label = "",
                          driverScaling = NULL, prepSpec = NULL) {

  fml <- fit$formula
  # Stringency fits sharing a formula+data can still differ by family/link
  # (Gamma-log vs gaussian-identity); key them apart so a family change does not
  # reuse a stale cached fit. Adoption (logistf) keeps the legacy key (extra = NULL).
  idExtra <- if (identical(stage, "stringency")) family else NULL
  ids <- computeModelId(fml, training_data, extra = idExtra)

  # --- Fitted values (kept as a small standalone vector; stripped from the fit) ---
  fittedVals <- if (!is.null(fit$model)) {
    tryCatch(as.numeric(fitted(fit$model)), error = function(e) numeric(0))
  } else {
    numeric(0)
  }

  # --- Training year range ---
  trainingYears <- if ("year" %in% names(training_data)) {
    as.integer(range(training_data$year, na.rm = TRUE))
  } else {
    integer(0)
  }

  correlations <- .buildCorrelations(fml, training_data)
  diag <- .extractDiagnostics(fit, training_data)

  # --- ADR 0009: frozen application transforms (self-contained prediction) ---
  # prepSpec records the preparePanelData arguments (driver selection + flags) so
  # a loaded model can rebuild a scenario design matrix from a fresh gdx panel
  # WITHOUT the historical panel or the original cfg (REMIND iterative coupling).
  transforms <- list(
    driverScaling = driverScaling %||% attr(training_data, "driverScaling"),
    gdpQ          = .pfm_env$gdppc_q_fit,
    scPCA         = .pfm_env$sc_pca_rotation,
    family        = family,
    prepSpec      = prepSpec
  )

  # --- ADR 0009: minimal state the lag recursion / clamp need without the panel ---
  seedPrices <- if (all(c("region", "ecp") %in% names(training_data))) {
    tryCatch(tapply(training_data$ecp, training_data$region,
                    function(v) v[length(v)]), error = function(e) NULL)
  } else NULL
  feLevels <- if ("regionFE" %in% names(training_data) && is.factor(training_data$regionFE)) {
    levels(droplevels(training_data$regionFE))
  } else if (!is.null(fit$model) && !is.null(fit$model$xlevels$regionFE)) {
    fit$model$xlevels$regionFE
  } else NULL
  applyState <- list(
    seed_prices     = seedPrices,
    # Clamp anchor = in-sample OBSERVED response max (log(1+ECP) for stringency), not the
    # fitted max which an ill-conditioned GLM can inflate (2026-06-24, ADR 0023).
    insMaxResp      = {
      respVals <- if (identical(tolower(stage), "stringency") && "ecp" %in% names(training_data)) {
        training_data$ecp
      } else fittedVals
      if (length(respVals) > 0) max(respVals, na.rm = TRUE) else NA_real_
    },
    regionFE_levels = feLevels
  )

  newPFMModel(
    id             = ids[["id"]],
    id_full        = ids[["id_full"]],
    created_at     = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    label          = label,
    pfm_version    = as.character(utils::packageVersion("pfm")),
    sector         = sector,
    stage          = stage,
    formula        = fml,
    family         = family,
    training_years = trainingYears,
    useFirth       = isTRUE(useFirth),
    data_hash      = ids[["data_hash"]],
    training_panel_hash = getOption("pfm.trainingPanelHash", NA_character_),
    model          = .stripFit(fit$model),  # data-bearing slots stripped (ADR 0009)
    coeftest       = fit$coeftest,
    vcov           = fit$vcov,
    fitted_values  = fittedVals,
    correlations   = correlations,
    diagnostics    = diag,
    transforms     = transforms,
    applyState     = applyState,
    projections    = NULL
  )
}

# Build Pearson and Spearman correlation matrices for the numeric predictors
# that appear in the model formula.
.buildCorrelations <- function(fml, df) {
  termLabels <- tryCatch(attr(stats::terms(fml), "term.labels"), error = function(e) character(0))
  # Keep only simple (non-interaction) terms that are numeric columns in df
  simpleTerms <- termLabels[!grepl(":", termLabels)]
  existingTerms <- simpleTerms[simpleTerms %in% names(df)]
  numCols <- if (length(existingTerms) > 0) {
    existingTerms[vapply(df[existingTerms], is.numeric, logical(1))]
  } else {
    character(0)
  }

  if (length(numCols) < 2) {
    return(list(pearson = matrix(nrow = 0, ncol = 0), spearman = matrix(nrow = 0, ncol = 0)))
  }

  subDf <- df[, numCols, drop = FALSE]
  list(
    pearson  = cor(subDf, use = "pairwise.complete.obs", method = "pearson"),
    spearman = cor(subDf, use = "pairwise.complete.obs", method = "spearman")
  )
}

# Extract or compute diagnostics from a fit result.
# If the fit came from fitAndDiagnose, all fields are already present.
# If it came from estimateAdoptionModel/estimatePriceStringencyModel, compute the missing ones.
.extractDiagnostics <- function(fit, df) {
  # Fields present in fitAndDiagnose output
  hasFull <- all(c("aic", "bic", "aicc", "hqic", "loglik", "pseudoR2",
                   "nPredictors", "nSignificant", "kOverN",
                   "overfitting", "separation", "converged") %in% names(fit))

  nObs      <- nrow(df)
  nCountries <- if ("country" %in% names(df)) length(unique(df$country)) else
    if ("iso3c" %in% names(df)) length(unique(df$iso3c)) else NA_integer_

  if (hasFull) {
    return(list(
      aic             = fit$aic,
      bic             = fit$bic,
      aicc            = fit$aicc,
      hqic            = fit$hqic,
      loglik          = fit$loglik,
      pseudoR2        = fit$pseudoR2,
      nPredictors     = as.integer(fit$nPredictors),
      nObs            = as.integer(nObs),
      nCountries      = as.integer(nCountries),
      nSignificant    = as.integer(fit$nSignificant),
      kOverN          = fit$kOverN,
      overfitting     = isTRUE(fit$overfitting),
      separation      = isTRUE(fit$separation),
      highZ           = isTRUE(fit$highZ),
      maxAbsZ         = fit$maxAbsZ %||% NA_real_,
      converged       = isTRUE(fit$converged),
      maxitWarning    = isTRUE(fit$maxitWarning),
      rejectionReason = fit$rejectionReason %||% NA_character_,
      vif             = list(
        values  = fit$vifRaw %||% numeric(0),
        maxVIF  = fit$maxVIF %||% NA_real_,
        highVIF = isTRUE(fit$highVIF),
        flagged = fit$vifFlagged %||% character(0)
      )
    ))
  }

  # Bare fit from estimateAdoptionModel / estimatePriceStringencyModel
  m   <- fit$model
  fml <- fit$formula
  k   <- if (!is.null(m)) length(stats::coef(m)) else NA_integer_

  loglik <- tryCatch({
    if (inherits(m, "logistf")) as.numeric(m$loglik["full"]) else as.numeric(stats::logLik(m))
  }, error = function(e) NA_real_)

  aic <- tryCatch({
    if (inherits(m, "logistf")) -2 * loglik + 2 * k else m$aic
  }, error = function(e) NA_real_)

  bic <- tryCatch(BIC(m), error = function(e) NA_real_)
  aicc <- if (!is.na(aic) && !is.na(k) && !is.na(nObs)) {
    aic + (2 * k * (k + 1)) / max(nObs - k - 1, 1)
  } else NA_real_
  hqic <- if (!is.na(loglik) && !is.na(k) && !is.na(nObs)) {
    -2 * loglik + 2 * k * log(log(nObs))
  } else NA_real_

  converged <- tryCatch({
    if (inherits(m, "logistf")) !is.null(m$coefficients) && !any(is.na(m$coefficients))
    else isTRUE(m$converged)
  }, error = function(e) NA)

  pVals <- tryCatch(fit$coeftest[, 4], error = function(e) numeric(0))
  nSig  <- sum(pVals < 0.05, na.rm = TRUE)
  kOverN <- if (!is.na(k) && nObs > 0) k / nObs else NA_real_

  vifRes <- tryCatch(computeVIF(data = df, formula = fml), error = function(e) NULL)

  list(
    aic             = aic,
    bic             = bic,
    aicc            = aicc,
    hqic            = hqic,
    loglik          = loglik,
    pseudoR2        = NA_real_,
    nPredictors     = as.integer(k),
    nObs            = as.integer(nObs),
    nCountries      = as.integer(nCountries),
    nSignificant    = as.integer(nSig),
    kOverN          = kOverN,
    overfitting     = if (!is.na(kOverN)) kOverN > 0.1 else NA,
    separation      = NA,
    highZ           = NA,
    maxAbsZ         = NA_real_,
    converged       = converged,
    maxitWarning    = FALSE,
    rejectionReason = NA_character_,
    vif             = list(
      values  = if (!is.null(vifRes)) vifRes$values  else numeric(0),
      maxVIF  = if (!is.null(vifRes)) vifRes$maxVIF  else NA_real_,
      highVIF = if (!is.null(vifRes)) vifRes$highVIF else NA,
      flagged = if (!is.null(vifRes)) vifRes$flagged else character(0)
    )
  )
}

# Null-coalescing helper (local to this file; pfm-reports also defines one)
`%||%` <- function(x, y) if (!is.null(x)) x else y
# nolint end
