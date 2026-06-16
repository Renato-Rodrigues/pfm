#' Compute a deterministic cache key for a fitted model
#'
#' The ID is a SHA-256 hash of the deparsed formula string and a SHA-256 hash
#' of the training data. Two calls with identical formula and data will always
#' produce the same ID, enabling disk-based caching in \code{\link{modelSelection}}.
#'
#' @param formula Formula. The model formula.
#' @param training_data data.frame. The data passed to glm/logistf.
#' @param extra Optional. Anything that distinguishes two fits sharing the same
#'   formula and data but differing in estimation (e.g. the GLM family/link
#'   string for the stringency stage). When \code{NULL} (default) the key is
#'   identical to the legacy two-argument key, so existing caches (and the
#'   adoption stage, which passes nothing) are unaffected. When non-\code{NULL}
#'   it is hashed in, yielding a distinct key — this is what prevents a
#'   gaussian(identity) refit from silently reusing a cached Gamma(log) fit.
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
computeModelId <- function(formula, training_data, extra = NULL) {
  fmlStr   <- paste(deparse(formula, width.cutoff = 500), collapse = " ")
  dataHash <- digest::digest(training_data, algo = "sha256")
  payload  <- if (is.null(extra)) list(fmlStr, dataHash) else list(fmlStr, dataHash, extra)
  fullHash <- digest::digest(payload, algo = "sha256")
  c(id = substr(fullHash, 1, 12), id_full = fullHash, data_hash = dataHash)
}
