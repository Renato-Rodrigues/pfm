#' Save, load, and list PFMModel objects
#'
#' @description
#' Three functions form the persistence layer for fitted pfm models:
#'
#' - \code{savePFMModel}: writes \code{{id}.rds} and updates \code{index.json}.
#' - \code{loadPFMModel}: loads a model by short or full ID.
#' - \code{listPFMModels}: returns the index as a \code{data.frame}.
#'
#' The storage directory is controlled by \code{getOption("pfm.modelDir")}.
#' If that option is not set, a directory must be supplied explicitly.
#'
#' @name pfm-persistence
NULL

#' Save a PFMModel to disk
#'
#' Writes \code{{id}.rds} into \code{dir} and adds or updates the corresponding
#' entry in \code{dir/index.json}. Silently overwrites if the ID already exists
#' (i.e. re-saving after \code{\link{addProjections}}).
#'
#' @param model A \code{PFMModel} object.
#' @param dir Character. Directory to write to. Defaults to \code{getOption("pfm.modelDir")}.
#'
#' @importFrom jsonlite fromJSON write_json
#' @return \code{model} invisibly.
#' @export
savePFMModel <- function(model, dir = getOption("pfm.modelDir")) {
  stopifnot(inherits(model, "PFMModel"))
  if (is.null(dir)) stop("Supply 'dir' or set options(pfm.modelDir = '...')", call. = FALSE)
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)

  rdsPath <- file.path(dir, paste0(model$id, ".rds"))
  saveRDS(model, file = rdsPath)

  .updatePFMModelIndex(model, dir)
  invisible(model)
}

#' Load a PFMModel from disk
#'
#' @param id Character. Short (12-char) or full SHA-256 ID.
#' @param dir Character. Directory to search. Defaults to \code{getOption("pfm.modelDir")}.
#'
#' @return A \code{PFMModel} object.
#' @export
loadPFMModel <- function(id, dir = getOption("pfm.modelDir")) {
  if (is.null(dir)) stop("Supply 'dir' or set options(pfm.modelDir = '...')", call. = FALSE)
  shortId <- substr(id, 1, 12)
  rdsPath <- file.path(dir, paste0(shortId, ".rds"))
  if (!file.exists(rdsPath)) {
    stop("No PFMModel found with id '", shortId, "' in '", dir, "'", call. = FALSE)
  }
  readRDS(rdsPath)
}

#' List all saved PFMModels
#'
#' Reads \code{dir/index.json} and returns its contents as a \code{data.frame}.
#' Does not deserialise any \code{.rds} files, so this is fast even with many
#' saved models.
#'
#' @param dir Character. Directory to search. Defaults to \code{getOption("pfm.modelDir")}.
#'
#' @return A \code{data.frame} with one row per saved model. Returns an empty
#'   \code{data.frame} if no models have been saved yet.
#' @export
listPFMModels <- function(dir = getOption("pfm.modelDir")) {
  if (is.null(dir)) stop("Supply 'dir' or set options(pfm.modelDir = '...')", call. = FALSE)
  idxPath <- file.path(dir, "index.json")
  if (!file.exists(idxPath)) return(data.frame())
  idx <- jsonlite::fromJSON(idxPath, simplifyDataFrame = TRUE)
  if (length(idx) == 0) return(data.frame())
  idx
}

# Internal: add or update one entry in index.json.
.updatePFMModelIndex <- function(model, dir) {
  idxPath <- file.path(dir, "index.json")

  entry <- data.frame(
    id              = model$id,
    id_full         = model$id_full,
    created_at      = model$created_at,
    label           = model$label,
    pfm_version     = model$pfm_version,
    sector          = model$sector,
    stage           = model$stage,
    formula         = paste(deparse(model$formula, width.cutoff = 500), collapse = " "),
    family          = model$family,
    training_year_min = if (length(model$training_years) == 2) model$training_years[1] else NA_integer_,
    training_year_max = if (length(model$training_years) == 2) model$training_years[2] else NA_integer_,
    useFirth        = model$useFirth,
    data_hash       = model$data_hash,
    aic             = model$diagnostics$aic,
    bic             = model$diagnostics$bic,
    aicc            = model$diagnostics$aicc,
    pseudoR2        = model$diagnostics$pseudoR2,
    nObs            = model$diagnostics$nObs,
    nCountries      = model$diagnostics$nCountries,
    converged       = model$diagnostics$converged,
    separation      = model$diagnostics$separation,
    highVIF         = model$diagnostics$vif$highVIF,
    has_projections = !is.null(model$projections),
    stringsAsFactors = FALSE
  )

  existing <- if (file.exists(idxPath)) {
    tryCatch(jsonlite::fromJSON(idxPath, simplifyDataFrame = TRUE), error = function(e) data.frame())
  } else {
    data.frame()
  }

  if (nrow(existing) > 0 && model$id %in% existing$id) {
    existing[existing$id == model$id, ] <- entry
    updated <- existing
  } else {
    updated <- rbind(existing, entry)
  }

  jsonlite::write_json(updated, idxPath, pretty = TRUE, auto_unbox = TRUE)
  invisible(NULL)
}
