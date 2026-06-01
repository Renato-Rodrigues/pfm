#' @title computeGroupContributions
#' @description Computes mean beta-times-x contributions per Term Group across all
#'   observations, along with Theory Score and Theory Fraction.
#'
#'   This is the library counterpart of the inline group-contribution logic previously
#'   embedded in \code{add_contributions()} in \file{model-selection.Rmd}.
#'   It is cheap to compute and is always called by \code{fitAndDiagnose} and
#'   \code{estimateAdoptionModel} regardless of the \code{compute} flag.
#'
#' @param fit List. Output of \code{fitAndDiagnose} or \code{estimateAdoptionModel},
#'   containing at minimum a \code{$model} element (a \code{logistf} or \code{glm} object).
#'   A raw \code{logistf}/\code{glm} object is also accepted.
#' @param df Data.frame or NULL. If supplied, used as the data source for computing
#'   the model matrix. If NULL, the model's own stored training data (\code{m$model}) is used.
#' @param actorPowerDrivers Character vector or NULL. Original (un-safe-named) Actor Power
#'   driver names passed to \code{classifyTermGroups}.
#' @param actorPowerIndex Character or NULL.
#' @param instQualityDrivers Character vector or NULL.
#' @param controlDrivers Character vector or NULL.
#'
#' @return A named list with one numeric entry per Term Group (mean beta*x contribution
#'   averaged across all observations), plus \code{"Total η"}, \code{"Mean P"},
#'   \code{"Theory Score"}, and \code{"Theory Frac."}. Returns \code{NULL} when the
#'   model is unavailable or the model matrix cannot be built.
#'
#' @author Renato Rodrigues
#' @export
#'
#' @importFrom stats coef model.matrix plogis
computeGroupContributions <- function(fit,
                                      df                 = NULL,
                                      actorPowerDrivers  = NULL,
                                      actorPowerIndex    = NULL,
                                      instQualityDrivers = NULL,
                                      controlDrivers     = NULL) {
  m <- if (is.list(fit) && !is.null(fit$model)) fit$model else fit
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

  contrib_mat <- sweep(mm, 2, beta, `*`)
  groups      <- classifyTermGroups(
    common, actorPowerDrivers, actorPowerIndex, instQualityDrivers, controlDrivers
  )

  group_levels <- c("Intercept", "Actor Power", "Inst. Quality", "Interaction",
                    "Controls", "Time Trend", "Path Dep.", "Region FE", "Other")

  result <- list()
  for (g in group_levels) {
    cols <- which(groups == g)
    result[[g]] <- if (length(cols) > 0)
      round(mean(rowSums(contrib_mat[, cols, drop = FALSE]), na.rm = TRUE), 3)
    else
      NA_real_
  }

  eta <- rowSums(contrib_mat)
  result[["Total η"]] <- round(mean(eta, na.rm = TRUE), 3)
  result[["Mean P"]]       <- round(mean(stats::plogis(eta), na.rm = TRUE), 3)

  theory_names  <- c("Actor Power", "Inst. Quality", "Interaction")
  non_int_names <- c("Actor Power", "Inst. Quality", "Interaction",
                     "Controls", "Time Trend", "Path Dep.", "Region FE", "Other")
  safe_val <- function(x) if (!is.null(x) && length(x) == 1 && !is.na(x)) x else 0

  theory_score <- sum(vapply(theory_names,  function(g) safe_val(result[[g]]), numeric(1)))
  abs_denom    <- sum(vapply(non_int_names, function(g) abs(safe_val(result[[g]])), numeric(1)))

  result[["Theory Score"]] <- round(theory_score, 3)
  result[["Theory Frac."]] <- if (abs_denom > 0)
    round(abs(theory_score) / abs_denom, 3)
  else
    NA_real_

  result
}
