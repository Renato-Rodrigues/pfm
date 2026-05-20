#' Compute a deterministic cache key for a fitted model
#'
#' The ID is a SHA-256 hash of the deparsed formula string and a SHA-256 hash
#' of the training data. Two calls with identical formula and data will always
#' produce the same ID, enabling disk-based caching in \code{\link{modelSelection}}.
#'
#' @param formula Formula. The model formula.
#' @param training_data data.frame. The data passed to glm/logistf.
#'
#' @return A named character vector with:
#'   \describe{
#'     \item{id}{First 12 characters of the SHA-256 — used as the filename stem.}
#'     \item{id_full}{Full SHA-256.}
#'     \item{data_hash}{SHA-256 of \code{training_data} alone — stored in the manifest.}
#'   }
#'
#' @importFrom digest digest
#' @keywords internal
computeModelId <- function(formula, training_data) {
  fmlStr   <- paste(deparse(formula, width.cutoff = 500), collapse = " ")
  dataHash <- digest::digest(training_data, algo = "sha256")
  fullHash <- digest::digest(list(fmlStr, dataHash), algo = "sha256")
  c(id = substr(fullHash, 1, 12), id_full = fullHash, data_hash = dataHash)
}
