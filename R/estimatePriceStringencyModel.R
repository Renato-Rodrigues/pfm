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
    logTransform = TRUE,
    lag = 1,
    useFirth = FALSE,
    includeLaggedECP = FALSE) {
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
    message("Log-transform applied: ecp -> log(1 + ecp)")
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

  # --- 3. Build formula ---
  fml <- buildModelFormula(
    depVar = "ecp",
    actorPowerDrivers = actorPowerDrivers,
    actorPowerIndex = actorPowerIndex,
    instQualityDrivers = instQualityDrivers,
    controlDrivers = controlDrivers,
    regionMappingFixedEffects = regionMappingFixedEffects,
    timeTrend = timeTrend
  )

  # --- 4. Choose GLM family ---
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
    fit <- stats::glm(fml, data = df, family = glmFamily, method = brglm2::brglmFit)
  } else {
    fit <- stats::glm(fml, data = df, family = glmFamily)
  }

  # --- 6. Clustered SE by region ---
  vcovClust <- sandwich::vcovCL(fit, cluster = df$regionFE, type = "HC1")
  robustTest <- lmtest::coeftest(fit, vcov. = vcovClust)

  return(list(
    model    = fit,
    coeftest = robustTest,
    vcov     = vcovClust,
    sector   = sector,
    family   = family,
    formula  = fml
  ))
}
