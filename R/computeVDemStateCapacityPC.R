# nolint start
#' @title computeVDemStateCapacityPC
#' @description Reduces the five-variable V-Dem state-capacity block to two orthogonal
#' principal components via PCA, resolving the collinearity that produces sign reversals
#' when all five indicators are entered simultaneously.
#'
#' Two modes:
#' \describe{
#'   \item{Fit mode (\code{rotation = NULL})}{Fits PCA on the supplied magpie, caches the
#'     rotation and normalisation bounds in \code{.pfm_env$sc_pca_rotation} for later
#'     use by \code{panelDataScenario}, and returns the PC scores as a magpie.}
#'   \item{Apply mode (\code{rotation = list})}{Projects new data onto a pre-fitted rotation
#'     (retrieved from \code{.pfm_env$sc_pca_rotation}) so that historical and scenario
#'     panels share the same PC definition.}
#' }
#'
#' @param scMag A \code{magpie} object containing the five state-capacity indicators,
#'   already inverted and normalised to \code{[0, 1]}:
#'   \emph{Civil Service Professionalism (VDem)}, \emph{Policy Implementation (VDem)},
#'   \emph{Rule Predictability (VDem)}, \emph{Absence of Corruption (VDem)},
#'   \emph{Meritocracy Index (VDem)}.
#' @param nComponents Integer. Number of principal components to return. Default: \code{2}.
#' @param rotation \code{NULL} (fit mode) or a named list with elements
#'   \code{center}, \code{scale}, \code{rotation}, \code{pc_min}, \code{pc_max}, \code{avail}
#'   as returned by a previous fit-mode call (accessible via \code{.pfm_env$sc_pca_rotation}).
#'
#' @return A \code{magpie} object with variables
#'   \code{"State Capacity PC1 (VDem)"} and optionally \code{"State Capacity PC2 (VDem)"},
#'   each normalised to \code{[0, 1]} using the reference distribution.
#'   Returns \code{NULL} if fewer than two state-capacity variables are available or
#'   if there are insufficient complete observations.
#'
#' @importFrom stats prcomp complete.cases quantile
#' @importFrom magclass getRegions getYears getNames new.magpie
#'
#' @export
#' @author Renato Rodrigues
computeVDemStateCapacityPC <- function(scMag, nComponents = 2L, rotation = NULL) {
  SC_VARS <- c(
    "Civil Service Professionalism (VDem)",
    "Policy Implementation (VDem)",
    "Rule Predictability (VDem)",
    "Absence of Corruption (VDem)",
    "Meritocracy Index (VDem)"
  )

  avail <- intersect(SC_VARS, magclass::getNames(scMag))
  if (length(avail) < 2L) {
    warning("computeVDemStateCapacityPC: fewer than 2 state-capacity variables found; PCA skipped.")
    return(NULL)
  }

  regions <- magclass::getRegions(scMag)
  yrs     <- magclass::getYears(scMag)
  nReg    <- length(regions)
  nYr     <- dim(scMag)[2L]

  # Reshape magpie [nReg, nYr, nVar] → matrix [nReg*nYr, nVar]
  # R array storage: first dim varies fastest, so matrix() fills each column with
  # all region-year values for one variable — exactly what we need.
  arr <- as.array(scMag[, , avail])
  mat <- matrix(arr, nrow = nReg * nYr, ncol = length(avail),
                dimnames = list(NULL, avail))

  nComponents <- min(as.integer(nComponents), length(avail))

  if (is.null(rotation)) {
    # ── Fit mode ──────────────────────────────────────────────────────────────
    complete_rows <- stats::complete.cases(mat)
    if (sum(complete_rows) < nComponents + 1L) {
      warning("computeVDemStateCapacityPC: insufficient complete observations for PCA.")
      return(NULL)
    }

    pca_fit   <- stats::prcomp(mat[complete_rows, , drop = FALSE],
                               center = TRUE, scale. = TRUE)
    pc_cc     <- pca_fit$x[, seq_len(nComponents), drop = FALSE]
    pc_min    <- apply(pc_cc, 2L, min)
    pc_max    <- apply(pc_cc, 2L, max)

    # Cache for panelDataScenario
    .pfm_env$sc_pca_rotation <- list(
      center   = pca_fit$center,
      scale    = pca_fit$scale,
      rotation = pca_fit$rotation[, seq_len(nComponents), drop = FALSE],
      pc_min   = pc_min,
      pc_max   = pc_max,
      avail    = avail
    )

    scores <- matrix(NA_real_, nrow = nReg * nYr, ncol = nComponents)
    scores[complete_rows, ] <- pc_cc

  } else {
    # ── Apply mode ─────────────────────────────────────────────────────────────
    shared <- intersect(rotation$avail, colnames(mat))
    if (length(shared) < 2L) {
      warning("computeVDemStateCapacityPC: insufficient overlapping variables for apply mode.")
      return(NULL)
    }
    mat_sub <- mat[, shared, drop = FALSE]
    # Replace NAs with variable mean (centre in scaled space = 0) to avoid propagating gaps
    for (j in seq_len(ncol(mat_sub))) {
      na_j <- is.na(mat_sub[, j])
      if (any(na_j)) mat_sub[na_j, j] <- rotation$center[shared[j]]
    }
    mat_scaled <- scale(mat_sub,
                        center = rotation$center[shared],
                        scale  = rotation$scale[shared])
    scores <- mat_scaled %*% rotation$rotation[shared, , drop = FALSE]
    pc_min <- rotation$pc_min
    pc_max <- rotation$pc_max
  }

  # Normalise scores to [0, 1] using reference range
  for (k in seq_len(nComponents)) {
    rng <- pc_max[k] - pc_min[k]
    scores[, k] <- if (rng > 0) (scores[, k] - pc_min[k]) / rng else 0.5
  }

  # Pack back into magpie [nReg, nYr, nComponents]
  pc_names <- paste0("State Capacity PC", seq_len(nComponents), " (VDem)")
  pc_mag   <- magclass::new.magpie(regions, yrs, pc_names, fill = NA)
  for (k in seq_len(nComponents)) {
    pc_mag[, , pc_names[k]] <- scores[, k]
  }

  return(pc_mag)
}
# nolint end
