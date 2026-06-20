# nolint start
#' @title computePredictedProbabilities
#' @description Computes Predicted Probability Profiles (margins plots data) for a
#'   fitted adoption logit model. For each variable in \code{sweepVars}, the variable
#'   is swept from its observed minimum to its maximum across \code{nGrid} equally-spaced
#'   points while all other predictors (including the intercept) are held at their
#'   sample means. Delta-method 95% confidence bounds are included.
#'
#'   This answers: "How does Pr(Adoption) change as variable X increases from its
#'   minimum to its maximum, holding all other variables at their means?"
#'
#' @param fit List. Output of \code{fitAndDiagnose} or \code{estimateAdoptionModel}.
#'   Must contain \code{$model} and \code{$vcov}.
#' @param df Data.frame or NULL. Used to compute variable ranges and means. If NULL,
#'   the model's own stored training data (\code{m$model}) is used.
#' @param sweepVars Character vector or NULL. Names of model-matrix columns to sweep.
#'   Accepts either original variable names or their \code{make.names()} versions.
#'   If NULL, auto-detected as all non-dummy theory-group variables (Actor Power +
#'   Inst. Quality) present in the model formula.
#' @param actorPowerDrivers Character vector or NULL.
#' @param actorPowerIndex Character or NULL.
#' @param instQualityDrivers Character vector or NULL.
#' @param controlDrivers Character vector or NULL.
#' @param nGrid Integer. Number of grid points per sweep. Default: \code{50}.
#'
#' @return A named list of \code{data.frame}s, one per sweep variable (names are the
#'   model-matrix column names). Each data frame has columns:
#'   \describe{
#'     \item{variable}{Model-matrix column name being swept.}
#'     \item{x}{The value of the sweep variable at this grid point.}
#'     \item{prob}{Predicted Pr(Adoption) at this grid point.}
#'     \item{lower}{Lower 95% confidence bound (clamped to 0--1).}
#'     \item{upper}{Upper 95% confidence bound (clamped to 0--1).}
#'   }
#'   Returns \code{NULL} when no suitable sweep variables are found or the model
#'   is unavailable.
#'
#' @author Renato Rodrigues
#' @export
#'
#' @importFrom stats coef model.matrix plogis formula
computePredictedProbabilities <- function(fit,
                                           df                 = NULL,
                                           sweepVars          = NULL,
                                           actorPowerDrivers  = NULL,
                                           actorPowerIndex    = NULL,
                                           instQualityDrivers = NULL,
                                           controlDrivers     = NULL,
                                           nGrid              = 50) {
  m       <- if (is.list(fit) && !is.null(fit$model)) fit$model else fit
  vcovMat <- if (is.list(fit) && !is.null(fit$vcov)) fit$vcov else NULL
  if (is.null(m)) return(NULL)

  data_src <- if (!is.null(df)) {
    df
  } else if (!is.null(m$model)) {
    m$model
  } else {
    NULL
  }
  if (is.null(data_src)) return(NULL)

  fml <- if (inherits(m, "logistf")) m$formula else stats::formula(m)
  mm  <- tryCatch(
    stats::model.matrix(fml, data = data_src),
    error = function(e) NULL
  )
  if (is.null(mm) || nrow(mm) == 0) return(NULL)

  beta   <- stats::coef(m)
  common <- intersect(colnames(mm), names(beta))
  if (length(common) == 0) return(NULL)
  mm   <- mm[, common, drop = FALSE]
  beta <- beta[common]

  # Auto-detect sweep variables: theory-group predictors that are continuous
  # (not intercept, not dummies, not time trend, not lagged adoption)
  non_sweep_pattern <- "^regionFE|^\\(Intercept\\)|^timeTrend$|adoption_lagged|lagged_adoption"
  if (is.null(sweepVars)) {
    groups    <- classifyTermGroups(
      common, actorPowerDrivers, actorPowerIndex, instQualityDrivers, controlDrivers
    )
    sweepVars <- common[
      groups %in% c("Actor Power", "Inst. Quality") &
      !grepl(non_sweep_pattern, common)
    ]
  } else {
    # Accept original names or make.names() versions; match against model-matrix columns
    safe_sweep  <- make.names(sweepVars)
    resolved    <- character(0)
    for (i in seq_along(sweepVars)) {
      if (sweepVars[i] %in% common) {
        resolved <- c(resolved, sweepVars[i])
      } else if (safe_sweep[i] %in% common) {
        resolved <- c(resolved, safe_sweep[i])
      }
    }
    sweepVars <- resolved
  }
  if (length(sweepVars) == 0) return(NULL)

  # Hold-at-means profile (mean of every model-matrix column)
  x_bar <- colMeans(mm, na.rm = TRUE)

  vcov_sub <- NULL
  if (!is.null(vcovMat)) {
    vcov_sub <- tryCatch(
      vcovMat[common, common, drop = FALSE],
      error = function(e) NULL
    )
  }

  results <- lapply(sweepVars, function(var) {
    col_vals  <- mm[, var]
    sweep_seq <- seq(min(col_vals, na.rm = TRUE), max(col_vals, na.rm = TRUE),
                     length.out = nGrid)

    rows <- lapply(sweep_seq, function(v) {
      x_new      <- x_bar
      x_new[var] <- v
      eta        <- sum(x_new * beta)
      prob       <- stats::plogis(eta)

      se <- NA_real_
      if (!is.null(vcov_sub)) {
        # Delta-method: Var(prob) ≈ g'Vg, where g = dprob/dbeta = pi*(1-pi)*x_new
        g  <- prob * (1 - prob) * x_new
        se <- sqrt(max(0, as.numeric(t(g) %*% vcov_sub %*% g)))
      }

      data.frame(
        variable = var,
        x        = v,
        prob     = prob,
        lower    = max(0, prob - 1.96 * se),
        upper    = min(1, prob + 1.96 * se),
        stringsAsFactors = FALSE
      )
    })
    do.call(rbind, rows)
  })
  names(results) <- sweepVars
  Filter(Negate(is.null), results)
}
# nolint end
