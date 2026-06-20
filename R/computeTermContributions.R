# nolint start
#' @title computeTermContributions
#' @description Computes per-observation, per-term beta-times-x log-odds contributions
#'   for a fitted adoption model applied to a supplied data frame. Handles regionFE
#'   dummy encoding manually so that single-country subsets (where factor levels are
#'   incomplete) work correctly.
#'
#'   This is the library counterpart of \code{build_contributions()} + \code{to_long()} +
#'   \code{to_grouped()} previously embedded in \file{country-adoption.Rmd}.
#'
#' @param fit List. Output of \code{estimateAdoptionModel} or \code{fitAndDiagnose},
#'   containing at minimum a \code{$model} element.
#' @param df Data.frame. Panel data to evaluate. Must contain \code{region}, \code{year},
#'   \code{ecp}, \code{regionFE} (if the model uses region fixed effects), and all
#'   formula predictor columns. Typically the prepared panel data filtered to a region
#'   of interest. Use \code{fit$data} from \code{estimateAdoptionModel} as the source.
#' @param actorPowerDrivers Character vector or NULL.
#' @param actorPowerIndex Character or NULL.
#' @param instQualityDrivers Character vector or NULL.
#' @param controlDrivers Character vector or NULL.
#' @param aggregate Logical. If \code{TRUE}, contributions are summed within each
#'   Term Group per (year, region) combination and a \code{term_group} column is
#'   returned instead of \code{term_raw}. Default: \code{FALSE}.
#'
#' @return A tidy \code{data.frame} in long format. When \code{aggregate = FALSE}:
#'   columns are \code{year, region, ecp, adoption, eta, prob, term_raw, contribution, term_group}.
#'   When \code{aggregate = TRUE}: columns are \code{year, region, ecp, adoption, eta, prob,
#'   term_group, contribution} with \code{term_group} as an ordered factor using the
#'   canonical Term Group order. Returns \code{NULL} when no valid rows exist.
#'
#' @author Renato Rodrigues
#' @export
#'
#' @importFrom stats coef complete.cases formula plogis
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr group_by summarise ungroup mutate all_of .data
computeTermContributions <- function(fit,
                                     df,
                                     actorPowerDrivers  = NULL,
                                     actorPowerIndex    = NULL,
                                     instQualityDrivers = NULL,
                                     controlDrivers     = NULL,
                                     aggregate          = FALSE) {
  m <- if (is.list(fit) && !is.null(fit$model)) fit$model else fit
  if (is.null(m) || !is.data.frame(df) || nrow(df) == 0) return(NULL)

  fml    <- if (inherits(m, "logistf")) m$formula else stats::formula(m)
  needed <- intersect(all.vars(fml), colnames(df))
  dfOk  <- df[stats::complete.cases(df[, needed, drop = FALSE]), , drop = FALSE]
  if (nrow(dfOk) == 0) return(NULL)

  beta        <- stats::coef(m)
  n           <- nrow(dfOk)
  contribMat <- matrix(0, nrow = n, ncol = length(beta),
                        dimnames = list(NULL, names(beta)))

  for (termName in names(beta)) {
    b <- beta[[termName]]
    if (termName == "(Intercept)") {
      contribMat[, termName] <- b
    } else if (grepl("^regionFE", termName)) {
      # Factor dummy: decode from the coefficient name, not model.matrix,
      # so single-country subsets with incomplete factor levels work correctly.
      feLevel <- sub("^regionFE", "", termName)
      contribMat[, termName] <- b * as.integer(
        as.character(dfOk$regionFE) == feLevel
      )
    } else if (termName %in% colnames(dfOk)) {
      contribMat[, termName] <- b * dfOk[[termName]]
    }
    # Terms absent from dfOk remain 0 (already initialised)
  }

  dfOut          <- as.data.frame(contribMat, stringsAsFactors = FALSE)
  dfOut$year     <- dfOk$year
  dfOut$region   <- if ("region" %in% names(dfOk)) dfOk$region else NA_character_
  dfOut$ecp      <- dfOk$ecp
  dfOut$adoption <- as.integer(dfOk$ecp > 0)
  dfOut$eta      <- rowSums(contribMat)
  dfOut$prob     <- stats::plogis(dfOut$eta)

  metaCols <- c("year", "region", "ecp", "adoption", "eta", "prob")
  termCols <- setdiff(names(dfOut), metaCols)

  long <- tidyr::pivot_longer(
    dfOut,
    cols      = dplyr::all_of(termCols),
    names_to  = "term_raw",
    values_to = "contribution"
  )
  long$term_group <- classifyTermGroups(
    long$term_raw,
    actorPowerDrivers, actorPowerIndex, instQualityDrivers, controlDrivers
  )

  if (isTRUE(aggregate)) {
    canonicalOrder <- c("Intercept", "Actor Power", "Inst. Quality", "Interaction",
                         "Controls", "Time Trend", "Path Dep.", "Region FE", "Other")
    long <- long |>
      dplyr::group_by(
        .data$year, .data$region, .data$ecp, .data$adoption,
        .data$eta, .data$prob, .data$term_group
      ) |>
      dplyr::summarise(contribution = sum(.data$contribution), .groups = "drop") |>
      dplyr::mutate(
        term_group = factor(.data$term_group, levels = canonicalOrder)
      )
  }

  as.data.frame(long, stringsAsFactors = FALSE)
}
# nolint end
