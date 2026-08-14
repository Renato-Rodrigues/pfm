#' Estimator arguments implied by a deployed-spec entry
#'
#' @description
#' Turns a \code{selected-models-psm.yml} entry into the argument list
#' \code{\link{estimatePolicyStringencyModel}} should be called with, by taking
#' every spec field the estimator actually accepts.
#'
#' It exists because the post-selection steps used to hand-list the fields they
#' forwarded. \code{apTransform} was absent from all of them, so the estimator
#' silently fell back to its \code{"linear"} default and the estimator-agreement,
#' influence and IV exhibits described a specification that was **not** the deployed
#' one — while being labelled with the deployed spec's name, which ends in
#' \code{satAP}. Found 2026-08-15 in Run-Group \code{v1}: the agreement fit's BIC
#' matched the non-\code{satAP} sweep row exactly (3367.546 Bulk / 1666.977 Diffuse)
#' and its \code{driverScaling} carried no frozen \code{sat} element.
#'
#' Deriving the list from \code{formals()} rather than restating it means a spec
#' field added later is forwarded automatically, instead of being dropped until
#' somebody notices the numbers are subtly wrong.
#'
#' \code{estimator} and \code{indexMax} are excluded: each step sets those itself
#' (the IV rung fits \code{"satP-iv"}, the agreement suite fits several families).
#'
#' @section Absent is not the same as default:
#' For the driver blocks and the FE mapping, a spec that does not name the field
#' means **none** — not "whatever the estimator defaults to". Those defaults are
#' substantive (\code{regionMappingFixedEffects} is \code{"regionmappingH12.csv"};
#' the driver arguments carry full default blocks), so letting them apply would
#' silently give a spec fixed effects, or drivers, it never asked for. They are
#' therefore always passed, as \code{NULL} when the spec omits them — which is the
#' behaviour the hand-written argument lists had. Every other accepted field is
#' passed only when present, so the estimator's (uniformly \code{FALSE}/linear)
#' defaults still apply.
#'
#' @param cfg One entry of the parsed \code{selected-models-psm.yml}, after the
#'   caller's list-flattening.
#' @param exclude Additional field names the caller overrides itself.
#'
#' @return Named list of estimator arguments.
#' @author Renato Rodrigues
#' @keywords internal
.psmSpecArgs <- function(cfg, exclude = character(0)) {
  if (!is.list(cfg)) cfg <- list()
  accepted <- names(formals(estimatePolicyStringencyModel))
  drop <- c("data", "sector", "estimator", "indexMax", "modelDir", "verbose", exclude)
  # Fields whose absence means NULL rather than the estimator's default.
  nullable <- setdiff(c("actorPowerDrivers", "actorPowerIndex", "instQualityDrivers",
                        "controlDrivers", "regionMappingFixedEffects"), drop)
  present <- setdiff(intersect(names(cfg), accepted), c(drop, nullable))
  args <- c(stats::setNames(lapply(nullable, function(k) cfg[[k]]), nullable),
            cfg[present])
  args[!vapply(args, is.null, logical(1)) | names(args) %in% nullable]
}
