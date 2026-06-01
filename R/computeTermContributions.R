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
  df_ok  <- df[stats::complete.cases(df[, needed, drop = FALSE]), , drop = FALSE]
  if (nrow(df_ok) == 0) return(NULL)

  beta        <- stats::coef(m)
  n           <- nrow(df_ok)
  contrib_mat <- matrix(0, nrow = n, ncol = length(beta),
                        dimnames = list(NULL, names(beta)))

  for (term_name in names(beta)) {
    b <- beta[[term_name]]
    if (term_name == "(Intercept)") {
      contrib_mat[, term_name] <- b
    } else if (grepl("^regionFE", term_name)) {
      # Factor dummy: decode from the coefficient name, not model.matrix,
      # so single-country subsets with incomplete factor levels work correctly.
      fe_level <- sub("^regionFE", "", term_name)
      contrib_mat[, term_name] <- b * as.integer(
        as.character(df_ok$regionFE) == fe_level
      )
    } else if (term_name %in% colnames(df_ok)) {
      contrib_mat[, term_name] <- b * df_ok[[term_name]]
    }
    # Terms absent from df_ok remain 0 (already initialised)
  }

  df_out          <- as.data.frame(contrib_mat, stringsAsFactors = FALSE)
  df_out$year     <- df_ok$year
  df_out$region   <- if ("region" %in% names(df_ok)) df_ok$region else NA_character_
  df_out$ecp      <- df_ok$ecp
  df_out$adoption <- as.integer(df_ok$ecp > 0)
  df_out$eta      <- rowSums(contrib_mat)
  df_out$prob     <- stats::plogis(df_out$eta)

  meta_cols <- c("year", "region", "ecp", "adoption", "eta", "prob")
  term_cols <- setdiff(names(df_out), meta_cols)

  long <- tidyr::pivot_longer(
    df_out,
    cols      = dplyr::all_of(term_cols),
    names_to  = "term_raw",
    values_to = "contribution"
  )
  long$term_group <- classifyTermGroups(
    long$term_raw,
    actorPowerDrivers, actorPowerIndex, instQualityDrivers, controlDrivers
  )

  if (isTRUE(aggregate)) {
    canonical_order <- c("Intercept", "Actor Power", "Inst. Quality", "Interaction",
                         "Controls", "Time Trend", "Path Dep.", "Region FE", "Other")
    long <- long %>%
      dplyr::group_by(
        .data$year, .data$region, .data$ecp, .data$adoption,
        .data$eta, .data$prob, .data$term_group
      ) %>%
      dplyr::summarise(contribution = sum(.data$contribution), .groups = "drop") %>%
      dplyr::mutate(
        term_group = factor(.data$term_group, levels = canonical_order)
      )
  }

  as.data.frame(long, stringsAsFactors = FALSE)
}
