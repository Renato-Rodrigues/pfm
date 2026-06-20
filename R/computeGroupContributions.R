# nolint start
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

  dataSrc <- if (!is.null(df)) {
    df
  } else if (!is.null(m$model)) {
    m$model
  } else {
    NULL
  }
  if (is.null(dataSrc)) return(NULL)

  fml <- if (inherits(m, "logistf")) m$formula else stats::formula(m)
  mm  <- tryCatch(
    stats::model.matrix(fml, data = dataSrc),
    error = function(e) NULL
  )
  if (is.null(mm) || nrow(mm) == 0) return(NULL)

  beta   <- stats::coef(m)
  common <- intersect(colnames(mm), names(beta))
  if (length(common) == 0) return(NULL)
  mm   <- mm[, common, drop = FALSE]
  beta <- beta[common]

  contribMat <- sweep(mm, 2, beta, `*`)
  groups      <- classifyTermGroups(
    common, actorPowerDrivers, actorPowerIndex, instQualityDrivers, controlDrivers
  )

  groupLevels <- c("Intercept", "Actor Power", "Inst. Quality", "Interaction",
                    "Controls", "Time Trend", "Path Dep.", "Region FE", "Other")

  result <- list()
  for (g in groupLevels) {
    cols <- which(groups == g)
    result[[g]] <- if (length(cols) > 0)
      round(mean(rowSums(contribMat[, cols, drop = FALSE]), na.rm = TRUE), 3)
    else
      NA_real_
  }

  eta <- rowSums(contribMat)
  result[["Total η"]] <- round(mean(eta, na.rm = TRUE), 3)
  result[["Mean P"]]       <- round(mean(stats::plogis(eta), na.rm = TRUE), 3)

  theoryNames  <- c("Actor Power", "Inst. Quality", "Interaction")
  nonIntNames <- c("Actor Power", "Inst. Quality", "Interaction",
                     "Controls", "Time Trend", "Path Dep.", "Region FE", "Other")
  safeVal <- function(x) if (!is.null(x) && length(x) == 1 && !is.na(x)) x else 0

  theoryScore <- sum(vapply(theoryNames,  function(g) safeVal(result[[g]]), numeric(1)))
  absDenom    <- sum(vapply(nonIntNames, function(g) abs(safeVal(result[[g]])), numeric(1)))

  result[["Theory Score"]] <- round(theoryScore, 3)
  result[["Theory Frac."]] <- if (absDenom > 0)
    round(abs(theoryScore) / absDenom, 3)
  else
    NA_real_

  result
}
# nolint end
