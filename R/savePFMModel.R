# nolint start
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
#' Writes \code{{id}.rds} into \code{{dir}/models/} and adds or updates the
#' corresponding entry in \code{{dir}/index.json}. Silently overwrites if the ID
#' already exists. Scenario projections are stored separately (see
#' \code{\link{saveProjection}}), never embedded here (ADR 0009).
#'
#' @param model A \code{PFMModel} object.
#' @param dir Character. Directory to write to. Defaults to \code{getOption("pfm.modelDir")}.
#' @param updateIndex Logical. When \code{TRUE} (default) the shared \code{index.json} is
#'   updated after writing \code{{id}.rds}. Parallel sweep workers pass \code{FALSE} so they
#'   only write their (unique) \code{.rds} and never race on the index; the master then calls
#'   \code{\link{rebuildPFMModelIndex}} once after the run (ADR 0019).
#'
#' @importFrom jsonlite fromJSON write_json
#' @return \code{model} invisibly.
#' @export
savePFMModel <- function(model, dir = getOption("pfm.modelDir"), updateIndex = TRUE) {
  stopifnot(inherits(model, "PFMModel"))
  if (is.null(dir)) stop("Supply 'dir' or set options(pfm.modelDir = '...')", call. = FALSE)
  modelsDir <- file.path(dir, "models")
  dir.create(modelsDir, showWarnings = FALSE, recursive = TRUE)

  saveRDS(model, file = file.path(modelsDir, paste0(model$id, ".rds")))
  if (isTRUE(updateIndex)) .updatePFMModelIndex(model, dir)
  invisible(model)
}

#' Save the shared Training Panel once (content-addressed)
#'
#' Writes the raw historical panel to \code{{dir}/panels/panel_{hash}.rds} only
#' if a file with that hash does not already exist, and sets
#' \code{options(pfm.trainingPanelHash = hash)} so that Fitted Models saved during
#' the same session reference it (ADR 0009). Returns the hash invisibly.
#'
#' @param panel A magpie object (the raw historical panel) or data.frame.
#' @param dir Character. Cache root. Defaults to \code{getOption("pfm.modelDir")}.
#' @importFrom digest digest
#' @export
saveTrainingPanel <- function(panel, dir = getOption("pfm.modelDir")) {
  if (is.null(dir)) stop("Supply 'dir' or set options(pfm.modelDir = '...')", call. = FALSE)
  hash <- digest::digest(panel, algo = "sha256")
  panelsDir <- file.path(dir, "panels")
  dir.create(panelsDir, showWarnings = FALSE, recursive = TRUE)
  path <- file.path(panelsDir, paste0("panel_", substr(hash, 1, 16), ".rds"))
  if (!file.exists(path)) saveRDS(panel, file = path)
  options(pfm.trainingPanelHash = substr(hash, 1, 16))
  invisible(substr(hash, 1, 16))
}

#' Load a shared Training Panel by hash
#' @param hash Character. The (16-char) panel hash stored on a Fitted Model.
#' @param dir Character. Cache root. Defaults to \code{getOption("pfm.modelDir")}.
#' @return The stored panel object, or \code{NULL} if not found.
#' @export
loadTrainingPanel <- function(hash, dir = getOption("pfm.modelDir")) {
  if (is.null(dir) || is.null(hash) || is.na(hash)) return(NULL)
  path <- file.path(dir, "panels", paste0("panel_", substr(hash, 1, 16), ".rds"))
  if (!file.exists(path)) return(NULL)
  readRDS(path)
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
  # ADR 0009 layout: {dir}/models/{id}.rds; fall back to the legacy flat path.
  rdsPath <- file.path(dir, "models", paste0(shortId, ".rds"))
  if (!file.exists(rdsPath)) {
    legacy <- file.path(dir, paste0(shortId, ".rds"))
    if (file.exists(legacy)) rdsPath <- legacy else
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

  # Ensure all expected columns exist in the returned data.frame to prevent downstream selection errors
  expectedCols <- c(
    "id", "id_full", "created_at", "label", "pfm_version", "sector", "stage",
    "formula", "family", "training_year_min", "training_year_max", "useFirth",
    "data_hash", "aic", "bic", "aicc", "pseudoR2", "nObs", "nCountries",
    "converged", "separation", "highVIF", "has_projections"
  )
  for (col in expectedCols) {
    if (!col %in% colnames(idx)) {
      idx[[col]] <- NA
    }
  }
  idx <- idx[, expectedCols, drop = FALSE]
  idx
}

# Internal: build the one-row index data.frame for a single PFMModel.
#' @keywords internal
.pfmModelIndexEntry <- function(model) {
  null2na <- function(val, default = NA) {
    if (is.null(val) || length(val) == 0) default else val
  }
  data.frame(
    id              = null2na(model$id, ""),
    id_full         = null2na(model$id_full, ""),
    created_at      = null2na(model$created_at, ""),
    label           = null2na(model$label, ""),
    pfm_version     = null2na(model$pfm_version, ""),
    sector          = null2na(model$sector, ""),
    stage           = null2na(model$stage, ""),
    formula         = paste(deparse(model$formula, width.cutoff = 500), collapse = " "),
    family          = null2na(model$family, ""),
    training_year_min = if (length(model$training_years) == 2) model$training_years[1] else NA_integer_,
    training_year_max = if (length(model$training_years) == 2) model$training_years[2] else NA_integer_,
    useFirth        = null2na(model$useFirth, NA),
    data_hash       = null2na(model$data_hash, ""),
    aic             = null2na(model$diagnostics$aic, NA_real_),
    bic             = null2na(model$diagnostics$bic, NA_real_),
    aicc            = null2na(model$diagnostics$aicc, NA_real_),
    pseudoR2        = null2na(model$diagnostics$pseudoR2, NA_real_),
    nObs            = null2na(model$diagnostics$nObs, NA_integer_),
    nCountries      = null2na(model$diagnostics$nCountries, NA_integer_),
    converged       = null2na(model$diagnostics$converged, NA),
    separation      = null2na(model$diagnostics$separation, NA),
    highVIF         = null2na(model$diagnostics$vif$highVIF, NA),
    has_projections = !is.null(model$projections),
    stringsAsFactors = FALSE
  )
}

.updatePFMModelIndex <- function(model, dir) {
  idxPath <- file.path(dir, "index.json")
  entry <- .pfmModelIndexEntry(model)

  existing <- if (file.exists(idxPath)) {
    tryCatch(jsonlite::fromJSON(idxPath, simplifyDataFrame = TRUE), error = function(e) data.frame())
  } else {
    data.frame()
  }

  target_cols <- colnames(entry)
  if (nrow(existing) > 0) {
    for (col in target_cols) {
      if (!col %in% colnames(existing)) {
        existing[[col]] <- NA
      }
    }
    existing <- existing[, target_cols, drop = FALSE]
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

#' Rebuild \code{index.json} by scanning the model store
#'
#' Reconstructs \code{{dir}/index.json} from every \code{{dir}/models/*.rds} fit on disk.
#' This is the repair/migration counterpart to \code{savePFMModel(updateIndex = FALSE)}:
#' parallel sweep workers write their \code{.rds} files without touching the index (ADR 0019),
#' and the index is rebuilt once afterwards. It also recovers the index after an interrupted
#' run, since the \code{.rds} fits are the source of truth. Unreadable/truncated \code{.rds}
#' files (e.g. a worker killed mid-write) are skipped with a warning.
#'
#' @param dir Character. Cache root. Defaults to \code{getOption("pfm.modelDir")}.
#' @return The rebuilt index as a \code{data.frame}, invisibly.
#' @importFrom jsonlite write_json
#' @export
rebuildPFMModelIndex <- function(dir = getOption("pfm.modelDir")) {
  if (is.null(dir)) stop("Supply 'dir' or set options(pfm.modelDir = '...')", call. = FALSE)
  modelsDir <- file.path(dir, "models")
  idxPath <- file.path(dir, "index.json")
  rds <- if (dir.exists(modelsDir)) {
    list.files(modelsDir, pattern = "\\.rds$", full.names = TRUE)
  } else {
    character(0)
  }
  entries <- list()
  nBad <- 0L
  for (f in rds) {
    m <- tryCatch(readRDS(f), error = function(e) NULL)
    if (is.null(m) || !inherits(m, "PFMModel")) {
      nBad <- nBad + 1L
      warning("rebuildPFMModelIndex: skipping unreadable/invalid fit '", basename(f), "'")
      next
    }
    entries[[length(entries) + 1L]] <- .pfmModelIndexEntry(m)
  }
  idx <- if (length(entries) > 0) do.call(rbind, entries) else data.frame()
  jsonlite::write_json(idx, idxPath, pretty = TRUE, auto_unbox = TRUE)
  if (nBad > 0) message("rebuildPFMModelIndex: ", nBad, " unreadable fit(s) skipped.")
  invisible(idx)
}
# nolint end
