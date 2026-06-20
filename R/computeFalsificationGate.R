# nolint start
#' @title computeFalsificationGate
#' @description The discriminating test of Difference-First selection (ADR 0014).
#' Re-estimates a candidate specification under the \code{pureFD} panel transform
#' (every driver differenced) and checks the theoretical asymmetry between the two
#' channels:
#' \itemize{
#'   \item \strong{Actor Power (Politics) must persist} — at least one AP \emph{main
#'     effect} is significant (\code{p < pThreshold}) with the correct sign
#'     (Innovator / Actor Power Index positive, Incumbent negative) and \strong{no}
#'     AP main effect is significant with the wrong sign. Actor-power \emph{changes}
#'     should drive policy \emph{changes}, so the dynamic signal should survive
#'     differencing.
#'   \item \strong{Institutional Quality (Polity) must vanish} — \strong{no} IQ
#'     main-effect channel remains significant. A slow-moving structural \emph{level}
#'     should lose its signal once differenced.
#' }
#' The AP\eqn{\times}IQ interaction is \strong{reported, not gated}. The gate is
#' evaluated per sector and (by default) must hold in \emph{both} sectors; a spec that
#' is non-estimable under \code{pureFD} in a sector counts as a \strong{fail} (a claim
#' that cannot be falsified cannot be confirmed).
#'
#' @param cfg A spec config list (the fields consumed by the estimate functions:
#'   \code{actorPowerDrivers}, \code{actorPowerIndex}, \code{instQualityDrivers},
#'   \code{controlDrivers}, \code{regionMappingFixedEffects}, ...).
#' @param data A \code{magpie} object: the historical panel.
#' @param stage Character. \code{"Adoption"} or \code{"Stringency"} (case-insensitive).
#' @param sectors Character vector. Default \code{c("Bulk", "Diffuse")}.
#' @param family Character. Stringency GLM family (default \code{"gaussian"}).
#' @param modelDir Character or NULL. Passed to the estimate functions (default NULL).
#' @param pThreshold Numeric. Significance threshold (default 0.05).
#' @param requireBothSectors Logical. If \code{TRUE} (default) the gate passes only
#'   when every sector passes; if \code{FALSE}, any sector passing is enough.
#' @param iqVanishTest Character. How "IQ vanishes" is tested under pureFD:
#'   \code{"jointBlock"} (default) — a joint Wald test that the IQ main-effect block is
#'   zero (channel-count-neutral; "vanishes" = fail to reject at \code{pThreshold});
#'   \code{"perChannel"} — the stricter, count-biased rule that \emph{no} individual IQ
#'   channel is significant. Falls back to per-channel when the block covariance is
#'   unavailable or singular.
#' @param verbose Logical. Progress messages (default FALSE).
#'
#' @return A list: \code{pass} (logical), \code{detail} (per-sector data.frame:
#'   estimable, nAPsigCorrect, nAPsigWrong, nIQsig, nIntSig, apPass, iqPass,
#'   sectorPass), and \code{reason} (short character summary).
#'
#' @importFrom stats coef
#' @export
#' @author Renato Rodrigues
computeFalsificationGate <- function(cfg, data, stage,
                                     sectors = c("Bulk", "Diffuse"),
                                     family = "gaussian", modelDir = NULL,
                                     pThreshold = 0.05, requireBothSectors = TRUE,
                                     iqVanishTest = c("jointBlock", "perChannel"),
                                     verbose = FALSE) {
  stage <- match.arg(tolower(stage), c("adoption", "stringency"))
  iqVanishTest <- match.arg(iqVanishTest)
  say <- function(...) if (isTRUE(verbose)) message("[falsify] ", ...)

  apPats <- make.names(unique(c(cfg$actorPowerDrivers, cfg$actorPowerIndex)))
  iqPats <- make.names(unique(cfg$instQualityDrivers))

  fitPureFD <- function(sec) {
    tryCatch({
      if (stage == "adoption")
        estimateAdoptionModel(data = data, sector = sec,
          actorPowerDrivers = cfg$actorPowerDrivers, actorPowerIndex = cfg$actorPowerIndex,
          instQualityDrivers = cfg$instQualityDrivers, controlDrivers = cfg$controlDrivers,
          regionMappingFixedEffects = cfg$regionMappingFixedEffects,
          panelTransform = "pureFD", modelDir = modelDir, verbose = FALSE,
          compute = c(ame = FALSE, predictedProbs = FALSE))
      else
        estimatePriceStringencyModel(data = data, sector = sec, family = family,
          actorPowerDrivers = cfg$actorPowerDrivers, actorPowerIndex = cfg$actorPowerIndex,
          instQualityDrivers = cfg$instQualityDrivers, controlDrivers = cfg$controlDrivers,
          regionMappingFixedEffects = cfg$regionMappingFixedEffects,
          panelTransform = "pureFD", modelDir = modelDir, verbose = FALSE)
    }, error = function(e) { say("pureFD fit failed (", sec, "): ", conditionMessage(e)); NULL })
  }

  detail <- do.call(rbind, lapply(sectors, function(sec) {
    base <- data.frame(sector = sec, estimable = FALSE,
                       nAPsigCorrect = NA_integer_, nAPsigWrong = NA_integer_,
                       nIQsig = NA_integer_, nIntSig = NA_integer_, iqJointP = NA_real_,
                       apPass = FALSE, iqPass = FALSE, sectorPass = FALSE,
                       stringsAsFactors = FALSE)
    fit <- fitPureFD(sec)
    ct <- if (!is.null(fit)) fit$coeftest else NULL
    if (is.null(ct) || !nrow(ct)) return(base)
    base$estimable <- TRUE
    pr <- .falsifyPredicate(rownames(ct), ct[, 1], ct[, 4], apPats, iqPats, pThreshold,
                            iqVanishTest = iqVanishTest, vcov = fit$vcov)
    base$nAPsigCorrect <- pr$nAPsigCorrect; base$nAPsigWrong <- pr$nAPsigWrong
    base$nIQsig <- pr$nIQsig; base$nIntSig <- pr$nIntSig; base$iqJointP <- pr$iqJointP
    base$apPass <- pr$apPass; base$iqPass <- pr$iqPass; base$sectorPass <- pr$sectorPass
    base
  }))

  pass <- if (isTRUE(requireBothSectors)) all(detail$sectorPass) else any(detail$sectorPass)
  reason <- if (pass) {
    "AP persists & IQ vanishes under pureFD in required sector(s)."
  } else if (any(!detail$estimable)) {
    paste0("non-estimable under pureFD in: ",
           paste(detail$sector[!detail$estimable], collapse = ", "))
  } else {
    bad <- detail$sector[!detail$sectorPass]
    paste0("falsification failed in: ", paste(bad, collapse = ", "),
           " (AP did not persist or IQ did not vanish)")
  }
  list(pass = isTRUE(pass), detail = detail, reason = reason)
}

# Internal: the falsification predicate on a single fit's coefficient table.
# AP persists (>=1 main effect correct-signed & significant, none wrong-signed &
# significant). IQ vanishes either per-channel (no main-effect channel individually
# significant) or, by default, via a joint Wald test that the IQ main-effect block
# is jointly zero (count-neutral; fails to reject -> "vanishes"). Interaction
# (":"/"_x_") is counted but not gated. Expected AP sign: Incumbent negative,
# Innovator / Actor Power Index positive.
#' @keywords internal
.falsifyPredicate <- function(nm, est, p, apPats, iqPats, pThreshold = 0.05,
                              iqVanishTest = "jointBlock", vcov = NULL) {
  hasPat <- function(x, pats) any(vapply(pats, function(q) grepl(q, x, fixed = TRUE), logical(1)))
  expSign <- function(x) if (grepl("Incumbent", x, ignore.case = TRUE)) -1 else 1
  isInt <- grepl(":|_x_", nm)
  sig <- !is.na(p) & p < pThreshold
  apMain <- !isInt & vapply(nm, hasPat, logical(1), pats = apPats)
  iqMain <- !isInt & vapply(nm, hasPat, logical(1), pats = iqPats)
  apCorrect <- apMain & sig & (sign(est) == vapply(nm, expSign, numeric(1)))
  apWrong   <- apMain & sig & (sign(est) != vapply(nm, expSign, numeric(1)))
  nAPsigCorrect <- sum(apCorrect); nAPsigWrong <- sum(apWrong)
  nIQsig <- sum(iqMain & sig); nIntSig <- sum(isInt & sig)
  apPass <- nAPsigCorrect >= 1 && nAPsigWrong == 0

  # IQ vanishes: joint Wald test of the IQ main-effect block = 0 (count-neutral),
  # falling back to per-channel when the block covariance is unavailable/singular.
  iqJointP <- NA_real_
  if (identical(iqVanishTest, "jointBlock") && any(iqMain) && !is.null(vcov)) {
    idx <- which(iqMain); cn <- nm[idx]
    V <- tryCatch(vcov[cn, cn, drop = FALSE], error = function(e) NULL)
    b <- est[idx]
    if (!is.null(V) && all(is.finite(V)) && all(is.finite(b))) {
      W <- tryCatch(as.numeric(t(b) %*% solve(V) %*% b), error = function(e) NA_real_)
      if (is.finite(W)) iqJointP <- stats::pchisq(W, df = length(b), lower.tail = FALSE)
    }
  }
  iqPass <- if (identical(iqVanishTest, "jointBlock") && is.finite(iqJointP)) {
    iqJointP > pThreshold                       # fail to reject "IQ block = 0" => vanishes
  } else {
    nIQsig == 0                                 # per-channel (or jointBlock fallback)
  }
  list(nAPsigCorrect = nAPsigCorrect, nAPsigWrong = nAPsigWrong, nIQsig = nIQsig,
       nIntSig = nIntSig, iqJointP = iqJointP, apPass = apPass, iqPass = iqPass,
       sectorPass = apPass && iqPass)
}
# nolint end
