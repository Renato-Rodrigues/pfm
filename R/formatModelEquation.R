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
#' @importFrom stringr str_pad
#' @export
formatModelEquation <- function(coefficients, actorPowerIndex = NULL, actorPowerDrivers = NULL,
                                instQualityDrivers = NULL, controlDrivers = NULL,
                                includeLegend = FALSE, prefix = "") {
  if (nrow(coefficients) == 0) return("")

  # Calculate indentation for multi-line support
  indent <- stringr::str_pad("", nchar(prefix), "right")

  # Helper for significance stars
  get_stars <- function(p) {
    if (is.na(p)) return("")
    if (p < 0.01) return("***")
    if (p < 0.05) return("**")
    if (p < 0.1)  return("*")
    return("")
  }

  # Helper for formatting individual terms
  format_term <- function(estimate, term, pValue, is_intercept = FALSE) {
    stars <- get_stars(pValue)
    val   <- abs(round(estimate, 3))
    sign  <- if (estimate >= 0) "+ " else "- "
    if (is_intercept) {
      return(paste0(if (estimate < 0) "- " else "", val, stars))
    } else {
      # Clean up term name if it's safe R-named but needs to look pretty
      clean_term <- gsub("\\.", " ", term)
      return(paste0(sign, val, stars, "[", clean_term, "]"))
    }
  }

  # Ensure names are matched against what's in the coefficients
  # In pfm, we often use make.names() for predictors.
  safeAPI <- if (!is.null(actorPowerIndex)) make.names(actorPowerIndex) else NULL
  safeAPD <- if (!is.null(actorPowerDrivers)) make.names(actorPowerDrivers) else NULL
  safeIQD <- if (!is.null(instQualityDrivers)) make.names(instQualityDrivers) else NULL
  safeCD  <- if (!is.null(controlDrivers)) make.names(controlDrivers) else NULL

  # Grouping logic
  # Note: Use unquoted column names for dplyr
  df <- coefficients %>%
    dplyr::mutate(
      group = dplyr::case_when(
        term == "(Intercept)" ~ "1_intercept",
        term == safeAPI | term %in% safeAPD ~ "2_actor",
        term %in% safeIQD ~ "3_inst",
        grepl("_x_", term) ~ "4_interaction",
        term %in% safeCD ~ "5_controls",
        term == "timeTrend" ~ "6_time",
        grepl("regionFE", term) ~ "7_fixed_effects",
        TRUE ~ "5_controls"
      ),
      term_txt = mapply(format_term, estimate, term, pValue, term == "(Intercept)")
    ) %>%
    dplyr::group_by(group) %>%
    dplyr::summarise(group_txt = paste(term_txt, collapse = " "), .groups = "drop")

  # Build the final string
  eq_parts <- stats::setNames(df$group_txt, df$group)
  
  # Initialize the variable
  out <- prefix
  
  # Intercept line
  if ("1_intercept" %in% names(eq_parts)) {
    out <- paste0(out, eq_parts["1_intercept"])
  }
  
  ordered_groups <- c("2_actor", "3_inst", "4_interaction", "5_controls", "6_time", "7_fixed_effects")
  for (g in ordered_groups) {
    if (g %in% names(eq_parts)) {
      # Add a newline and indentation for each group
      out <- paste0(out, "\n", indent, eq_parts[g])
    }
  }
  
  # Add Legend if requested
  if (isTRUE(includeLegend)) {
    legend_text <- paste0(
      "\n---\n",
      "Significance Legend:\n",
      "*** p < 0.01\n",
      "** p < 0.05\n",
      "* p < 0.1\n"
    )
    out <- paste0(out, legend_text)
  }
  
  return(out)
}
