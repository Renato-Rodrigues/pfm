#' Persist, load, and list scenario projections (ADR 0009)
#'
#' @description
#' Scenario projections are a \emph{disposable, gdx-dependent} artifact: a fitted
#' model plus a particular REMIND gdx yields one projection. They are kept out of
#' the (gdx-independent) Fitted Model and stored separately under
#' \code{{dir}/projections/} so that re-running a scenario, or coupling the same
#' model to a stream of updated gdx files, never rewrites the model. Persisting is
#' optional — in a REMIND iterative coupling the projection is typically fed
#' straight back without ever touching disk.
#'
#' - \code{saveProjection}: writes a projection under a caller-supplied label.
#' - \code{loadProjection}: reads it back.
#' - \code{listProjections}: lists persisted projection labels.
#'
#' @name pfm-projections
NULL

#' @param projection The projection object to persist (e.g. the data.frame from
#'   \code{\link{predictFeasibility}}, or a list of them).
#' @param label Character. Caller-supplied name for this projection (e.g. a
#'   scenario id). Used as the file stem; sanitised to \code{[A-Za-z0-9._-]}.
#' @param dir Character. Cache root. Defaults to \code{getOption("pfm.modelDir")}.
#' @param meta Optional named list of provenance (e.g. \code{gdx_path},
#'   \code{scenario_data_hash}, \code{model_ids}); stored alongside the projection.
#' @return The path written, invisibly.
#' @rdname pfm-projections
#' @export
saveProjection <- function(projection, label, dir = getOption("pfm.modelDir"), meta = NULL) {
  if (is.null(dir)) stop("Supply 'dir' or set options(pfm.modelDir = '...')", call. = FALSE)
  if (is.null(label) || !nzchar(label)) stop("A non-empty 'label' is required.", call. = FALSE)
  safe <- gsub("[^A-Za-z0-9._-]", "_", label)
  projDir <- file.path(dir, "projections")
  dir.create(projDir, showWarnings = FALSE, recursive = TRUE)
  path <- file.path(projDir, paste0(safe, ".rds"))
  saveRDS(list(label = label, created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
               meta = meta, projection = projection), file = path)
  invisible(path)
}

#' @param label Character. The label used at save time.
#' @rdname pfm-projections
#' @return For \code{loadProjection}: the stored projection (the \code{projection}
#'   element), or \code{NULL} if absent.
#' @export
loadProjection <- function(label, dir = getOption("pfm.modelDir")) {
  if (is.null(dir)) stop("Supply 'dir' or set options(pfm.modelDir = '...')", call. = FALSE)
  safe <- gsub("[^A-Za-z0-9._-]", "_", label)
  path <- file.path(dir, "projections", paste0(safe, ".rds"))
  if (!file.exists(path)) return(NULL)
  obj <- readRDS(path)
  obj$projection
}

#' @rdname pfm-projections
#' @return For \code{listProjections}: a character vector of persisted labels.
#' @export
listProjections <- function(dir = getOption("pfm.modelDir")) {
  if (is.null(dir)) stop("Supply 'dir' or set options(pfm.modelDir = '...')", call. = FALSE)
  projDir <- file.path(dir, "projections")
  if (!dir.exists(projDir)) return(character(0))
  sub("\\.rds$", "", list.files(projDir, pattern = "\\.rds$"))
}
