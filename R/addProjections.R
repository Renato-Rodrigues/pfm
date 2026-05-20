#' Add scenario projections to a saved PFMModel
#'
#' Loads the saved \code{PFMModel} by ID, replaces the \code{projections} field
#' with \code{projections}, and re-saves the model to disk. The \code{index.json}
#' entry is updated to reflect \code{has_projections = TRUE}.
#'
#' Projections are not computed at fit time because they require REMIND scenario
#' data (\code{fulldata.gdx}) that is not available during model estimation.
#' Call this function after running \code{\link{panelDataScenario}} and generating
#' predictions with the fitted model.
#'
#' @param id Character. Short (12-char) or full SHA-256 ID of the saved model.
#' @param projections List. Scenario projection results to attach. Structure is
#'   caller-defined; the dashboard and REMIND export layer both accept any list.
#'   Recommended fields:
#'   \describe{
#'     \item{bulk_adoption}{data.frame: region, year, estimate, ci_lo, ci_hi.}
#'     \item{bulk_stringency}{data.frame: region, year, estimate, ci_lo, ci_hi.}
#'     \item{diffuse_adoption}{data.frame: region, year, estimate, ci_lo, ci_hi.}
#'     \item{diffuse_stringency}{data.frame: region, year, estimate, ci_lo, ci_hi.}
#'     \item{scenario_data_hash}{Character. SHA-256 of the scenario panel data used.}
#'     \item{gdx_path}{Character. Path to the fulldata.gdx used.}
#'   }
#' @param dir Character. Directory containing the saved model.
#'   Defaults to \code{getOption("pfm.modelDir")}.
#'
#' @return The updated \code{PFMModel} invisibly.
#' @export
addProjections <- function(id, projections, dir = getOption("pfm.modelDir")) {
  model <- loadPFMModel(id, dir)
  model$projections <- projections
  savePFMModel(model, dir)
  invisible(model)
}
