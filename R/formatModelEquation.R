#' @title formatModelEquation
#' @description Formats a model's coefficients into a readable equation string,
#' grouping variables by category (Actor, Inst, Controls, etc.) and including
#' significance stars.
#'
#' @param coefficients A data frame with columns \code{term}, \code{estimate}, and \code{pValue}.
#' @param actorPowerIndex Character or NULL.
#' @param actorPowerDrivers Character vector.
#' @param instQualityDrivers Character vector.
#' @param controlDrivers Character vector.
#' @param includeLegend Logical. If \code{TRUE}, appends a significance legend to the output.
#' @param prefix Character. Optional prefix for the first line (e.g., "  adoption ~ ").
#'
#' @return A character string containing the formatted equation.
#'
#' @importFrom dplyr mutate case_when group_by summarise
#' @importFrom rlang .data
#' @importFrom stringr str_pad
#' @export
formatModelEquation <- function(coefficients, actorPowerIndex = NULL, actorPowerDrivers = NULL,
                                instQualityDrivers = NULL, controlDrivers = NULL,
                                includeLegend = FALSE, prefix = "") {
  if (nrow(coefficients) == 0) return("")

  # Calculate indentation for multi-line support
  indent <- stringr::str_pad("", nchar(prefix), "right")

  # Helper for significance stars
  getStars <- function(p) {
    if (is.na(p)) return("")
    if (p < 0.01) return("***")
    if (p < 0.05) return("**")
    if (p < 0.1) return("*")
    return("")
  }

  # Helper for formatting individual terms
  formatTerm <- function(estimate, term, pValue, isIntercept = FALSE) {
    stars <- getStars(pValue)
    val   <- abs(round(estimate, 3))
    sign  <- if (estimate >= 0) "+ " else "- "
    if (isIntercept) {
      return(paste0(if (estimate < 0) "- " else "", val, stars))
    } else {
      # Clean up term name if it's safe R-named but needs to look pretty
      cleanTerm <- gsub("\\.", " ", term)
      return(paste0(sign, val, stars, "[", cleanTerm, "]"))
    }
  }

  # Ensure names are matched against what's in the coefficients
  # In pfm, we often use make.names() for predictors.
  safeAPI <- if (!is.null(actorPowerIndex)) make.names(actorPowerIndex) else NULL # nolint: object_usage_linter.
  safeAPD <- if (!is.null(actorPowerDrivers)) make.names(actorPowerDrivers) else NULL # nolint: object_usage_linter.
  safeIQD <- if (!is.null(instQualityDrivers)) make.names(instQualityDrivers) else NULL # nolint: object_usage_linter.
  safeCD  <- if (!is.null(controlDrivers)) make.names(controlDrivers) else NULL # nolint: object_usage_linter.

  # Grouping logic
  # Note: Use unquoted column names for dplyr
  df <- coefficients |>
    dplyr::mutate(
      group = dplyr::case_when(
        .data$term == "(Intercept)" ~ "1_intercept",
        .data$term == safeAPI | .data$term %in% safeAPD ~ "2_actor",
        .data$term %in% safeIQD ~ "3_inst",
        grepl("_x_", .data$term) ~ "4_interaction",
        .data$term %in% safeCD ~ "5_controls",
        .data$term == "timeTrend" ~ "6_time",
        grepl("regionFE", .data$term) ~ "7_fixed_effects",
        TRUE ~ "5_controls"
      ),
      term_txt = mapply( # nolint: undesirable_function_linter.
        formatTerm, .data$estimate, .data$term, .data$pValue, .data$term == "(Intercept)"
      )
    ) |>
    dplyr::group_by(.data$group) |>
    dplyr::summarise(group_txt = paste(.data$term_txt, collapse = " "), .groups = "drop")

  # Build the final string
  eqParts <- stats::setNames(df$group_txt, df$group)

  # Initialize the variable
  out <- prefix

  # Intercept line
  if ("1_intercept" %in% names(eqParts)) {
    out <- paste0(out, eqParts["1_intercept"])
  }

  orderedGroups <- c("2_actor", "3_inst", "4_interaction", "5_controls", "6_time", "7_fixed_effects")
  for (g in orderedGroups) {
    if (g %in% names(eqParts)) {
      # Add a newline and indentation for each group
      out <- paste0(out, "\n", indent, eqParts[g])
    }
  }

  # Add Legend if requested
  if (isTRUE(includeLegend)) {
    legendText <- paste0(
      "\n---\n",
      "Significance Legend:\n",
      "*** p < 0.01\n",
      "** p < 0.05\n",
      "* p < 0.1\n"
    )
    out <- paste0(out, legendText)
  }

  return(out)
}
