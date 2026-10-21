# The interaction weight and its scope (design note 0007, O10; A3 and A7 of section 8).
#
# A weight is named by its panel variable, optionally followed by "@<scope>":
#   "Liberal Democracy (VDem)"         every actor-power x institution interaction is weighted (A3)
#   "Liberal Democracy (VDem)@accRoL"  only the accountability and rule-of-law interactions are weighted;
#                                      the state-capability interactions act in every regime (A7)
# The scope travels inside the name, so every caller that passes the weight (spec field, fit-cache key,
# frozen driverScaling) carries it unchanged. preparePanelData writes the weight to the column
# `interactionWeight__` and, for a scoped weight, the scope's position in .pfmIwScopes to
# `interactionWeightScope__` (one value, repeated: it survives row subsets and rbind, unlike an attribute).

# Scopes: a regex on the institution's column name (make.names form) selecting the weighted interactions.
.pfmIwScopes <- c(accRoL = "Accountability|Rule\\.of\\.Law")

# Split a weight name into its panel variable and its scope (NULL = every interaction).
.pfmIwParse <- function(w) {
  if (is.null(w)) return(list(variable = NULL, scope = NULL))
  parts <- strsplit(w, "@", fixed = TRUE)[[1]]
  scope <- if (length(parts) > 1) parts[2] else NULL
  if (!is.null(scope) && !scope %in% names(.pfmIwScopes)) {
    stop("interactionWeight '", w, "': unknown scope '", scope, "' (known: ",
         paste(names(.pfmIwScopes), collapse = ", "), ")", call. = FALSE)
  }
  list(variable = parts[1], scope = scope)
}

# The weight that multiplies the interaction of an actor-power column with institution column `inst`
# in frame `df`: the weight column when the interaction is in scope, else 1.
.pfmIwFactor <- function(df, inst) {
  if (!"interactionWeight__" %in% colnames(df)) return(1)
  w <- df[["interactionWeight__"]]
  if ("interactionWeightScope__" %in% colnames(df)) {
    code <- df[["interactionWeightScope__"]][1]
    if (is.finite(code) && code >= 1 && !grepl(.pfmIwScopes[[code]], inst)) return(1)
  }
  w
}
