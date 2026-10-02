# nolint start
#' Deprecated psm-prefixed names
#'
#' @description
#' The package's functions, steps and spec file were renamed from \code{psm*} to \code{pfm*} on
#' 2026-10-02 (design note 0005, D21). These aliases keep code written against the old names
#' working for one minor version: each warns and forwards its arguments to the new function.
#' Old step names (\code{psm-*}) and the old spec file (\code{selected-models-psm.yml}) are
#' still read, so Run-Group \code{v5} and earlier resolve unchanged.
#'
#' @param ... Arguments passed to the renamed function.
#' @return Whatever the renamed function returns.
#' @name pfm-deprecated
#' @keywords internal
NULL

#' @rdname pfm-deprecated
#' @export
computePSMCrossDataset <- function(...) {
  .Deprecated("computePFMCrossDataset", package = "pfm")
  computePFMCrossDataset(...)
}

#' @rdname pfm-deprecated
#' @export
projectPSMSpecScenario <- function(...) {
  .Deprecated("projectPFMSpecScenario", package = "pfm")
  projectPFMSpecScenario(...)
}

#' @rdname pfm-deprecated
#' @export
psmAssertSizeWeights <- function(...) {
  .Deprecated("pfmAssertSizeWeights", package = "pfm")
  pfmAssertSizeWeights(...)
}

#' @rdname pfm-deprecated
#' @export
psmCleanSteps <- function(...) {
  .Deprecated("pfmCleanSteps", package = "pfm")
  pfmCleanSteps(...)
}

#' @rdname pfm-deprecated
#' @export
psmCouplingWeights <- function(...) {
  .Deprecated("pfmCouplingWeights", package = "pfm")
  pfmCouplingWeights(...)
}

#' @rdname pfm-deprecated
#' @export
psmSpecs <- function(...) {
  .Deprecated("pfmSpecs", package = "pfm")
  pfmSpecs(...)
}

#' @rdname pfm-deprecated
#' @export
psmStepArtifacts <- function(...) {
  .Deprecated("pfmStepArtifacts", package = "pfm")
  pfmStepArtifacts(...)
}

#' @rdname pfm-deprecated
#' @export
runPSMCouplingBound <- function(...) {
  .Deprecated("runPFMCouplingBound", package = "pfm")
  runPFMCouplingBound(...)
}

#' @rdname pfm-deprecated
#' @export
runPSMDonorAssumptions <- function(...) {
  .Deprecated("runPFMDonorAssumptions", package = "pfm")
  runPFMDonorAssumptions(...)
}

#' @rdname pfm-deprecated
#' @export
runPSMEstimatorAgreement <- function(...) {
  .Deprecated("runPFMEstimatorAgreement", package = "pfm")
  runPFMEstimatorAgreement(...)
}

#' @rdname pfm-deprecated
#' @export
runPSMExportREMINDInputs <- function(...) {
  .Deprecated("runPFMExportREMINDInputs", package = "pfm")
  runPFMExportREMINDInputs(...)
}

#' @rdname pfm-deprecated
#' @export
runPSMFrontier <- function(...) {
  .Deprecated("runPFMFrontier", package = "pfm")
  runPFMFrontier(...)
}

#' @rdname pfm-deprecated
#' @export
runPSMHistoricalReplay <- function(...) {
  .Deprecated("runPFMHistoricalReplay", package = "pfm")
  runPFMHistoricalReplay(...)
}

#' @rdname pfm-deprecated
#' @export
runPSMIV <- function(...) {
  .Deprecated("runPFMIV", package = "pfm")
  runPFMIV(...)
}

#' @rdname pfm-deprecated
#' @export
runPSMInference <- function(...) {
  .Deprecated("runPFMInference", package = "pfm")
  runPFMInference(...)
}

#' @rdname pfm-deprecated
#' @export
runPSMInfluence <- function(...) {
  .Deprecated("runPFMInfluence", package = "pfm")
  runPFMInfluence(...)
}

#' @rdname pfm-deprecated
#' @export
runPSMProjection <- function(...) {
  .Deprecated("runPFMProjection", package = "pfm")
  runPFMProjection(...)
}

#' @rdname pfm-deprecated
#' @export
runPSMSectorSpeeds <- function(...) {
  .Deprecated("runPFMSectorSpeeds", package = "pfm")
  runPFMSectorSpeeds(...)
}

#' @rdname pfm-deprecated
#' @export
runPSMSelectionBootstrap <- function(...) {
  .Deprecated("runPFMSelectionBootstrap", package = "pfm")
  runPFMSelectionBootstrap(...)
}

#' @rdname pfm-deprecated
#' @export
runPSMSweep <- function(...) {
  .Deprecated("runPFMSweep", package = "pfm")
  runPFMSweep(...)
}

#' @rdname pfm-deprecated
#' @export
runPSMTemporalValidation <- function(...) {
  .Deprecated("runPFMTemporalValidation", package = "pfm")
  runPFMTemporalValidation(...)
}
# nolint end
