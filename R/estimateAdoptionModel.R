#' @title estimateAdoptionModel
#' @description Estimates the Stage 1 Adoption Probability (Logit Model) of
#' the two-stage Hurdle model for carbon pricing. For each sector, a logistic
#' regression estimates whether a region adopts a carbon price in a given year.
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
#' @param actorPowerDrivers Character vector of individual Actor Power driver
#'   names. If \code{actorPowerIndex} is in this list, it takes priority as
#'   the sole Actor Power main effect. Otherwise, all drivers in this list
#'   are included as individual main effects.
#' @param actorPowerIndex Character or NULL. Name of the Actor Power Index
#'   variable. If provided, it is used for interaction terms. It only acts
#'   as a main effect if its name is also in \code{actorPowerDrivers}.
#'   \code{NULL} to exclude the interaction.
#' @param regionMappingFixedEffects Character or NULL. Region mapping file used to define the
#'   fixed-effects grouping. If \code{NULL}, region fixed effects are omitted.
#'   Default: \code{"regionmappingH12.csv"}.
#' @param timeTrend Logical. If \code{TRUE} (default), adds a linear time trend
#'   to the model.
#' @param useFirth Logical. If \code{TRUE} (default), uses Firth's penalized
#'   likelihood logistic regression (\code{logistf}) to handle perfect
#'   separation. Recommended for the adoption stage.
#' @param lag Integer. Time lag for drivers in years. Default: \code{1}.
#' @param includeLaggedAdoption Logical. If \code{TRUE}, includes the lagged
#'   adoption status (\code{adoption_lagged}) as a predictor. Default: \code{FALSE}.
#'
#' @return A list with elements:
#'   \describe{
#'     \item{model}{The fitted model object (either \code{glm} or \code{logistf}).}
#'     \item{coeftest}{Robust coefficient table (clustered SE for GLM, Firth
#'       standard errors for logistf).}
#'     \item{vcov}{The variance-covariance matrix.}
#'     \item{sector}{The sector estimated.}
#'     \item{formula}{The formula used.}
#'   }
#'
#' @author Renato Rodrigues
#'
#' @importFrom stats glm binomial as.formula
#' @importFrom lmtest coeftest
#' @importFrom sandwich vcovCL
#' @importFrom logistf logistf
#'
#' @export
#'
estimateAdoptionModel <- function(
    data,
    sector = "Bulk",
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
    lag = 1,
    includeLaggedAdoption = FALSE) {
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

  # --- 2. Create binary dependent variable ---
  df$adoption <- as.integer(df$ecp > 0)

  if (isTRUE(includeLaggedAdoption)) {
    controlDrivers <- c(controlDrivers, "lagged_adoption")
  }

  # --- 3. Build formula ---
  fml <- buildModelFormula(
    depVar = "adoption",
    actorPowerDrivers = actorPowerDrivers,
    actorPowerIndex = actorPowerIndex,
    instQualityDrivers = instQualityDrivers,
    controlDrivers = controlDrivers,
    regionMappingFixedEffects = regionMappingFixedEffects,
    timeTrend = timeTrend
  )

  # --- 4. Estimate model ---
  if (isTRUE(useFirth)) {
    # Firth's penalized likelihood logistic regression
    fit <- logistf::logistf(fml, data = df)

    # Construct a coeftest-like matrix.
    # logistf uses Profile Likelihood for p-values (prob) and standard errors (var)
    robustTest <- cbind(
      fit$coefficients,
      sqrt(diag(fit$var)),
      fit$coefficients / sqrt(diag(fit$var)),
      fit$prob
    )
    colnames(robustTest) <- c("Estimate", "Std. Error", "z value", "Pr(>|z|)")
    vcovMat <- fit$var
    # Add converged flag for consistency
    fit$converged <- !is.null(fit$coefficients) && !any(is.na(fit$coefficients))
  } else {
    # Standard logistic regression
    fit <- glm(fml, data = df, family = binomial(link = "logit"))

    # Clustered SE by region (if regionFE exists)
    clusterVar <- if ("regionFE" %in% names(df)) df$regionFE else NULL
    vcovMat <- sandwich::vcovCL(fit, cluster = clusterVar, type = "HC1")
    robustTest <- lmtest::coeftest(fit, vcov. = vcovMat)
  }

  return(list(
    model    = fit,
    coeftest = robustTest,
    vcov     = vcovMat,
    sector   = sector,
    formula  = fml
  ))
}
