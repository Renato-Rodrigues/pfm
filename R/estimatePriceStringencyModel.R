#' @title estimatePriceStringencyModel
#' @description Estimates the Stage 2 Price Stringency (GLM Model) of the
#' two-stage Hurdle model for carbon pricing. For each sector, a GLM with a
#' log link estimates the carbon price level conditional on adoption (ECP > 0).
#'
#' Robustness safeguards include a linear time trend, region fixed effects
#' (at a configurable spatial resolution), and clustered standard errors by
#' region.
#'
#' @param data A \code{magpie} object or a \code{data.frame}. If a \code{data.frame}
#'   is provided, it is assumed to be already prepared and the internal
#'   call to \code{preparePanelData} is skipped.
#' @param sector Character. The sector to estimate: \code{"Bulk"} or
#'   \code{"Diffuse"}. Default: \code{"Bulk"}.
#' @param family Character. GLM family to use: \code{"Gamma"} or
#'   \code{"gaussian"}. Both use a log link.
#'   \describe{
#'     \item{Gamma}{Natural for positive, right-skewed data where variance
#'       scales with the mean. Recommended for carbon prices.}
#'     \item{gaussian}{Assumes constant variance. More familiar but less
#'       suited for skewed, heteroskedastic price data.}
#'   }
#'   Default: \code{"Gamma"}.
#' @param actorPowerDrivers Character vector of individual Actor Power driver
#'   names. If \code{actorPowerIndex} is in this list, it takes priority as
#'   the sole Actor Power main effect. Otherwise, all drivers in this list
#'   are included as individual main effects.
#' @param actorPowerIndex Character or NULL. Name of the Actor Power Index
#'   variable. If provided, it is used for interaction terms. It only acts
#'   as a main effect if its name is also in \code{actorPowerDrivers}.
#'   Set to \code{NULL} to exclude the interaction.
#' @param instQualityDrivers Character vector of Institutional Quality
#'   indicator names.
#' @param controlDrivers Character vector of control variable names.
#' @param regionMappingFixedEffects Character or NULL. Region mapping file for fixed effects.
#'   If \code{NULL}, region fixed effects are omitted.
#'   Default: \code{"regionmappingH12.csv"}.
#' @param timeTrend Logical. If \code{TRUE} (default), adds a linear time trend
#'   to the model.
#' @param logTransform Logical. If \code{TRUE}, the dependent variable is
#'   transformed to \code{log(1 + ECP)}. Default: \code{TRUE}.
#' @param lag Integer. Time lag for drivers in years. Default: \code{1}.
#' @param useFirth Logical. If \code{TRUE}, uses Firth-type bias reduction
#'   (via \code{brglm2::brglmFit}) for the GLM estimation. Default: \code{FALSE}.
#' @param includeLaggedECP Logical. If \code{TRUE}, includes the lagged
#'   carbon price (\code{lagged_ecp}) as a predictor. Default: \code{FALSE}.
#' @param modelDir Character or NULL. Directory for saving/loading \code{PFMModel}
#'   files. Defaults to \code{getOption("pfm.modelDir", NULL)}. Set to \code{NULL}
#'   to disable persistence.
#' @param label Character. Optional human label stored in the saved model manifest.
#' @param verbose Logical. If \code{TRUE} (default), prints progress messages when
#'   loading from cache or estimating.
#'
#' @return A list with elements:
#'   \describe{
#'     \item{model}{The fitted \code{glm} object.}
#'     \item{coeftest}{Robust coefficient table with clustered SE.}
#'     \item{vcov}{The clustered variance-covariance matrix.}
#'     \item{sector}{The sector estimated.}
#'     \item{family}{The GLM family used.}
#'     \item{formula}{The formula used.}
#'   }
#'
#' @author Renato Rodrigues
#'
#' @importFrom stats glm Gamma gaussian as.formula
#' @importFrom lmtest coeftest
#' @importFrom sandwich vcovCL
#'
#' @export
#'
estimatePriceStringencyModel <- function(
    data,
    sector = "Bulk",
    family = "Gamma",
    actorPowerDrivers = c(
      "VRE share", "Electrification",
      "Coal primary energy share", "Oil/Gas primary energy share",
      "Fossil share in Industry"
    ),
    actorPowerIndex = "Actor Power Index",
    instQualityDrivers = c(
      "Government Effectiveness (WGI)", "Control of Corruption (WGI)",
      "Voice and Accountability (WGI)", "Political Stability (WGI)", "Regulatory Quality (WGI)", "Rule of Law (WGI)"
    ),
    controlDrivers = c(
      "Population", "GDP per Capita", "Land Area",
      "Urban Population Share",
      "Gini Income Inequality Coefficient",
      "Gender Inequality Index", "Energy Intensity"
    ),
    regionMappingFixedEffects = "regionmappingH12.csv",
    timeTrend = TRUE,
    logTransform = TRUE,
    lag = 1,
    useFirth = FALSE,
    includeLaggedECP = FALSE,
    interactRegionFE = FALSE,
    modelDir = getOption("pfm.modelDir", NULL),
    label = "",
    verbose = TRUE) {
  # --- 1. Prepare data.frame ---
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

  # --- 2. Subset to positive prices ---
  df <- df[df$ecp > 0, , drop = FALSE]

  # --- 2b. Optional log-transform ---
  if (isTRUE(logTransform)) {
    df$ecp <- log(1 + df$ecp)
    if (isTRUE(verbose)) {
      message("Log-transform applied: ecp -> log(1 + ecp)")
    }
  }

  if (nrow(df) < 5) {
    warning(
      "Only ", nrow(df),
      " observations with positive carbon prices for sector '",
      sector, "'. Model may be unreliable."
    )
  }

  if (isTRUE(includeLaggedECP)) {
    # If the dependent variable is log-transformed, we should log-transform the lagged predictor as well
    if (isTRUE(logTransform)) {
      df$lagged_ecp <- log(1 + df$lagged_ecp)
    }
    controlDrivers <- c(controlDrivers, "lagged_ecp")
  }

  # --- 2c. Cache check: return saved model if formula + data unchanged ---
  # (formula built after data prep so we check after steps 1-2b)
  fml <- buildModelFormula(
    depVar = "ecp",
    actorPowerDrivers = actorPowerDrivers,
    actorPowerIndex = actorPowerIndex,
    instQualityDrivers = instQualityDrivers,
    controlDrivers = controlDrivers,
    regionMappingFixedEffects = regionMappingFixedEffects,
    timeTrend = timeTrend,
    interactRegionFE = interactRegionFE
  )

  if (!is.null(modelDir)) {
    ids <- computeModelId(fml, df)
    cachedPath <- file.path(modelDir, paste0(ids[["id"]], ".rds"))
    if (file.exists(cachedPath)) {
      if (isTRUE(verbose)) {
        message("  [cache hit] Loading stringency model ", ids[["id"]], " from disk.")
      }
      cached <- loadPFMModel(ids[["id"]], modelDir)
      return(list(
        model    = cached$model,
        coeftest = cached$coeftest,
        vcov     = cached$vcov,
        sector   = sector,
        family   = family,
        formula  = fml
      ))
    }
  }

  # --- 4. Choose GLM family ---
  if (isTRUE(verbose)) {
    message("  [running] Estimating stringency model (", sector, " sector)...")
  }
  if (family == "Gamma") {
    glmFamily <- Gamma(link = "log")
  } else if (family == "gaussian") {
    glmFamily <- gaussian(link = "log")
  } else {
    stop("Unsupported family '", family, "'. Use 'Gamma' or 'gaussian'.")
  }

  # --- 5. Estimate GLM ---
  if (isTRUE(useFirth)) {
    if (!requireNamespace("brglm2", quietly = TRUE)) {
      stop("Package 'brglm2' is required for bias-reduced estimation. Please install it.")
    }
    fit <- stats::glm(fml, data = df, family = glmFamily, method = brglm2::brglmFit, control = list(maxit = 1000))
  } else {
    fit <- stats::glm(fml, data = df, family = glmFamily, control = list(maxit = 1000))
    # If the standard GLM failed to converge, try providing robust starting values
    if (!fit$converged) {
      if (isTRUE(verbose)) message("  [fallback] GLM failed to converge. Attempting robust starting values...")
      # Fit a simple gaussian model on log-scale of the dependent variable to get starting coefficients
      dep_var <- as.character(fml[[2]])
      df_init <- df
      # Avoid log(0) or log(negative) if somehow present
      df_init[[dep_var]] <- log(abs(df_init[[dep_var]]) + 1e-6)
      init_fit <- stats::lm(fml, data = df_init)
      
      # Re-attempt GLM with these starting values
      fit2 <- tryCatch({
        stats::glm(fml, data = df, family = glmFamily, control = list(maxit = 1000), start = coef(init_fit))
      }, error = function(e) fit, warning = function(w) fit)
      
      if (isTRUE(fit2$converged)) {
        fit <- fit2
        if (isTRUE(verbose)) message("  [fallback] Successfully converged with robust starting values!")
      }
    }
  }

  # --- 6. Clustered SE by region ---
  vcovClust <- sandwich::vcovCL(fit, cluster = df$regionFE, type = "HC1")
  robustTest <- lmtest::coeftest(fit, vcov. = vcovClust)

  result <- list(
    model    = fit,
    coeftest = robustTest,
    vcov     = vcovClust,
    sector   = sector,
    family   = family,
    formula  = fml
  )

  # --- 7. Save PFMModel if modelDir is configured ---
  if (!is.null(modelDir)) {
    pfmModel <- buildPFMModel(
      fit           = result,
      training_data = df,
      sector        = sector,
      stage         = "stringency",
      family        = family,
      useFirth      = useFirth,
      label         = label
    )
    savePFMModel(pfmModel, modelDir)
    if (isTRUE(verbose)) {
      message("  [saved] Stringency model ", pfmModel$id, " -> ", modelDir)
    }
  }

  result
}
