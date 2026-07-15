# nolint start
#' Tournament-v2 selection variants: tier-gate winners and the sharing-cost exhibit
#'
#' @description
#' The documentation companion of the Tournament-v2 ranking (ADR 0039). From one
#' per-(model, sector) metrics frame it computes, without any refit:
#' \itemize{
#'   \item the \strong{tier-gate winners} — the worse-sector-\eqn{\Delta R^2} ranking
#'     under each requested tier gate (deployment = the \code{"Green"} winner; the
#'     \code{"Blue"} winner is the documented tier-relaxed row);
#'   \item the \strong{Specification-Sharing Cost exhibit} — each sector's OWN best
#'     spec (max \eqn{\Delta R^2}(theory) among hard-gate passers) and the
#'     \eqn{\Delta R^2} that sector gives up under the shared deployed winner. The
#'     Shared Specification rule stands: per-sector winners are documented, never
#'     deployed (CONTEXT.md "Specification-Sharing Cost").
#' }
#'
#' @param df Per-(model, sector) metrics frame, as consumed by
#'   \code{\link{computeMaximinScore}} (must include \code{deltaR2Theory}).
#' @param sectors Character vector of the two sectors.
#' @param tierGates Character vector of tier gates to document. Default
#'   \code{c("Green", "Blue")}.
#' @param deployedGate Which gate's winner is the deployment (default \code{"Green"}).
#' @param deployedModel Character or \code{NULL}. The ACTUAL deployed spec when it
#'   differs from the gate winner — i.e. the post-sanity selection (the sanity /
#'   responsiveness walk can reject the maximin leader; 2026-07-15: the walk
#'   rejected nine scenario-blind composites before deploying X-0290). The
#'   sharing-cost exhibit is computed against this model so \code{sharingCost}
#'   describes the real deployment, not a rejected leader. \code{NULL} falls back
#'   to the \code{deployedGate} winner.
#' @param ... Further arguments forwarded to \code{\link{computeMaximinScore}}
#'   (near-tie knobs, hard-gate thresholds, ...). \code{rankBy} is fixed to
#'   \code{"worseDeltaR2"}.
#'
#' @return A list: \code{winners} (named by tier gate; \code{NA} when a gate passes
#'   nothing), \code{perSector} (the sharing-cost exhibit: sector, bestModel,
#'   bestDeltaR2, deployedDeltaR2, sharingCost, sharingCostShare),
#'   \code{rankings} (the full ranking per tier gate), \code{deployedGate}.
#'
#' @seealso \code{\link{computeMaximinScore}}, ADR 0039
#' @export
#' @author Renato Rodrigues
computeSelectionVariants <- function(df, sectors = c("Bulk", "Diffuse"),
                                     tierGates = c("Green", "Blue"),
                                     deployedGate = "Green",
                                     deployedModel = NULL, ...) {
  deployedGate <- match.arg(deployedGate, tierGates)
  rankings <- lapply(tierGates, function(tg) {
    computeMaximinScore(df, rankBy = "worseDeltaR2", tierGate = tg, ...)
  })
  names(rankings) <- tierGates
  winners <- vapply(rankings, function(mm) {
    p <- mm[mm$gatePass, , drop = FALSE]
    if (nrow(p) > 0) p$model[1] else NA_character_
  }, character(1))

  # Per-sector best among specs passing the NON-tier hard gates: use the widest
  # documented gate (last of tierGates, typically "Blue") as the admissible pool.
  pool <- rankings[[tierGates[length(tierGates)]]]
  okModels <- pool$model[pool$gatePass]
  dep <- if (!is.null(deployedModel)) deployedModel else winners[[deployedGate]]
  perSector <- do.call(rbind, lapply(sectors, function(sec) {
    s <- df[df$model %in% okModels & df$sector == sec &
              is.finite(df$deltaR2Theory), , drop = FALSE]
    s <- s[order(-s$deltaR2Theory), , drop = FALSE]
    depVal <- if (!is.na(dep)) {
      v <- df$deltaR2Theory[df$model == dep & df$sector == sec]
      if (length(v)) v[1] else NA_real_
    } else NA_real_
    best <- if (nrow(s) > 0) s[1, , drop = FALSE] else NULL
    data.frame(
      sector = sec,
      bestModel = if (!is.null(best)) best$model else NA_character_,
      bestDeltaR2 = if (!is.null(best)) best$deltaR2Theory else NA_real_,
      deployedDeltaR2 = depVal,
      sharingCost = if (!is.null(best)) best$deltaR2Theory - depVal else NA_real_,
      sharingCostShare = if (!is.null(best) && is.finite(depVal) && best$deltaR2Theory > 0) {
        1 - depVal / best$deltaR2Theory
      } else NA_real_,
      stringsAsFactors = FALSE
    )
  }))
  rownames(perSector) <- NULL

  list(winners = as.list(winners), perSector = perSector,
       rankings = rankings, deployedGate = deployedGate,
       deployedModel = dep)
}
# nolint end
