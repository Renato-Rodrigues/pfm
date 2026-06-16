#' @title computeDeltaR2Theory
#' @description Computes \eqn{\Delta R^2}(theory): the McFadden / deviance
#' pseudo-R-squared of the full model minus that of a baseline refit with all
#' theory terms removed (Actor Power mains, Institutional Quality mains, and
#' every \code{_x_} interaction). This is the canonical implementation of the
#' quantity previously computed report-locally in model-selection.Rmd; unlike
#' the report version it is family-aware, so FD-transformed stringency models
#' (gaussian identity, ADR 0005) use a matching baseline family.
#'
#' @param fit Result list from \code{estimateAdoptionModel} /
#'   \code{estimatePriceStringencyModel} (needs \code{$model}, \code{$formula},
#'   \code{$data}).
#' @param actorPowerDrivers Character vector or NULL.
#' @param actorPowerIndex Character vector or NULL.
#' @param instQualityDrivers Character vector or NULL.
#' @param stage Character. \code{"adoption"} or \code{"stringency"}.
#'
#' @return Numeric scalar (rounded to 3 decimals), or \code{NA_real_} when the
#'   baseline cannot be fit.
#'
#' @importFrom stats glm binomial terms as.formula family deviance
#'
#' @export
#' @author Renato Rodrigues
computeDeltaR2Theory <- function(fit, actorPowerDrivers = NULL, actorPowerIndex = NULL,
                                 instQualityDrivers = NULL,
                                 stage = c("adoption", "stringency")) {
  stage <- match.arg(stage)
  if (is.null(fit) || is.null(fit$model) || is.null(fit$formula) || is.null(fit$data)) {
    return(NA_real_)
  }
  m <- fit$model
  df <- fit$data

  # Full-model pseudo-R2
  r2Full <- if (inherits(m, "logistf")) {
    llF <- m$loglik["full"]
    llN <- m$loglik["null"]
    if (is.null(llF) || is.null(llN) || !is.finite(llF) || !is.finite(llN) || llN == 0) {
      return(NA_real_)
    }
    1 - llF / llN
  } else if (!is.null(m$deviance) && !is.null(m$null.deviance) &&
               is.finite(m$null.deviance) && m$null.deviance > 0) {
    1 - m$deviance / m$null.deviance
  } else {
    return(NA_real_)
  }

  # Strip theory terms: AP mains, IQ mains, and any _x_ interaction
  theorySafe <- make.names(unique(c(actorPowerDrivers, actorPowerIndex, instQualityDrivers)))
  allTerms <- attr(stats::terms(fit$formula), "term.labels")
  isTheory <- vapply(allTerms, function(tm) {
    grepl("_x_", tm, fixed = TRUE) || tm %in% theorySafe
  }, logical(1))
  baseTerms <- allTerms[!isTheory]
  if (length(baseTerms) == 0) baseTerms <- "1"
  depVar <- as.character(fit$formula[[2]])
  fmlBase <- tryCatch(
    stats::as.formula(paste(depVar, "~", paste(baseTerms, collapse = " + "))),
    error = function(e) NULL
  )
  if (is.null(fmlBase)) return(NA_real_)

  # Baseline family: match the fitted model (handles Gamma log, gaussian log,
  # and the FD gaussian identity); adoption baselines are plain logits.
  fam <- if (stage == "adoption") {
    stats::binomial(link = "logit")
  } else if (inherits(m, "glm")) {
    stats::family(m)
  } else {
    stats::Gamma(link = "log")
  }

  baseFit <- tryCatch(
    suppressWarnings(stats::glm(fmlBase, data = df, family = fam,
                                control = list(maxit = 500))),
    error = function(e) NULL
  )
  if (is.null(baseFit) || !is.finite(baseFit$null.deviance) || baseFit$null.deviance <= 0) {
    return(NA_real_)
  }
  r2Base <- 1 - baseFit$deviance / baseFit$null.deviance
  if (!is.finite(r2Full) || !is.finite(r2Base)) return(NA_real_)
  round(as.numeric(r2Full - r2Base), 3)
}
