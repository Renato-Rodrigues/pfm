#' @title computeTheoryTier
#' @description Canonical Theory Tier classification (hoisted from the
#' model-selection report so pfm owns the business logic). A model is
#' \strong{Green} when it has at least one significant term in \emph{all three}
#' theory groups (Actor Power, Institutional Quality, Interaction);
#' \strong{Blue} when at least one group has a significant term; \strong{Yellow}
#' when no theory group has any significant term.
#'
#' @param sigActorPower Integer vector. Count of significant Actor Power terms.
#' @param sigInstQual Integer vector. Count of significant Institutional Quality terms.
#' @param sigInteractions Integer vector. Count of significant Interaction terms.
#'
#' @return Character vector of tiers: \code{"Green"}, \code{"Blue"}, or \code{"Yellow"}.
#'
#' @export
#' @author Renato Rodrigues
computeTheoryTier <- function(sigActorPower, sigInstQual, sigInteractions) {
  sap <- ifelse(is.na(sigActorPower), 0L, sigActorPower)
  siq <- ifelse(is.na(sigInstQual), 0L, sigInstQual)
  si  <- ifelse(is.na(sigInteractions), 0L, sigInteractions)
  ifelse(sap > 0 & siq > 0 & si > 0, "Green",
         ifelse(sap > 0 | siq > 0 | si > 0, "Blue", "Yellow"))
}

#' @title computeMaximinScore
#' @description Scores Shared Specification candidates across sectors using the
#' Maximin Selection Rule (decided 2026-06-12): a specification is ranked by its
#' \emph{worse} sector's Theory Tier (Green > Blue > Yellow), tie-broken by the
#' mean \eqn{\Delta R^2}(theory) across sectors, with the worse sector's
#' \eqn{\Delta R^2}(theory) also reported. Hard gates apply to \emph{both}
#' sectors: \code{maxVIF < vifGate}, convergence, and no lagged dependent-variable
#' terms. Gate-failing specs are ranked last regardless of tier.
#'
#' @param df Data.frame with one row per (model, sector) and columns:
#'   \describe{
#'     \item{model}{Character. Specification identifier (shared across sectors).}
#'     \item{sector}{Character. e.g. \code{"Bulk"} / \code{"Diffuse"}.}
#'     \item{sigActorPower, sigInstQual, sigInteractions}{Integer significant-term
#'       counts per theory group (as in the model-selection report tables).}
#'     \item{deltaR2Theory}{Numeric. \eqn{\Delta R^2}(theory) for that sector fit.}
#'     \item{maxVIF}{Numeric. Maximum VIF of that sector fit.}
#'     \item{converged}{Logical.}
#'     \item{usesLagged}{Logical, optional (default \code{FALSE}). \code{TRUE} when
#'       the spec includes lagged dependent-variable terms.}
#'   }
#' @param vifGate Numeric. Hard VIF gate applied per sector. Default: \code{10}.
#'
#' @return Data.frame with one row per model, ordered best-first:
#'   \code{model, minTier, meanDeltaR2, minDeltaR2, tierBySector, deltaR2BySector,
#'   gatePass, gateFailReason, rank}. Only models present in \emph{all} sectors
#'   appearing in \code{df} are scored; partial models are returned with
#'   \code{gatePass = FALSE} and reason \code{"missing sector(s)"}.
#'
#' @seealso \code{\link{computeTheoryTier}}, ADR 0004 (guided selection algorithm)
#'
#' @export
#' @author Renato Rodrigues
computeMaximinScore <- function(df, vifGate = 10) {
  required <- c("model", "sector", "sigActorPower", "sigInstQual",
                "sigInteractions", "deltaR2Theory", "maxVIF", "converged")
  missingCols <- setdiff(required, colnames(df))
  if (length(missingCols) > 0) {
    stop("computeMaximinScore: missing columns: ", paste(missingCols, collapse = ", "))
  }
  if (!"usesLagged" %in% colnames(df)) df$usesLagged <- FALSE

  df$tier <- computeTheoryTier(df$sigActorPower, df$sigInstQual, df$sigInteractions)
  tierRank <- c(Green = 3L, Blue = 2L, Yellow = 1L)

  allSectors <- sort(unique(df$sector))
  rows <- lapply(split(df, df$model), function(m) {
    model <- m$model[1]
    haveSectors <- sort(unique(m$sector))
    missingSectors <- setdiff(allSectors, haveSectors)

    # One row per sector (if duplicated, keep the first occurrence)
    m <- m[!duplicated(m$sector), , drop = FALSE]
    m <- m[order(m$sector), , drop = FALSE]

    tierVec <- stats::setNames(m$tier, m$sector)
    dr2Vec  <- stats::setNames(m$deltaR2Theory, m$sector)

    failReasons <- character(0)
    if (length(missingSectors) > 0) {
      failReasons <- c(failReasons,
                       paste0("missing sector(s): ", paste(missingSectors, collapse = ", ")))
    }
    badVIF <- m$sector[!is.na(m$maxVIF) & m$maxVIF >= vifGate]
    if (length(badVIF) > 0) {
      failReasons <- c(failReasons, paste0("VIF >= ", vifGate, ": ", paste(badVIF, collapse = ", ")))
    }
    notConv <- m$sector[!m$converged %in% TRUE]
    if (length(notConv) > 0) {
      failReasons <- c(failReasons, paste0("not converged: ", paste(notConv, collapse = ", ")))
    }
    lagged <- m$sector[m$usesLagged %in% TRUE]
    if (length(lagged) > 0) {
      failReasons <- c(failReasons, paste0("lagged terms: ", paste(lagged, collapse = ", ")))
    }

    minTierRank <- min(tierRank[m$tier])
    data.frame(
      model = model,
      minTier = names(tierRank)[match(minTierRank, tierRank)],
      meanDeltaR2 = mean(m$deltaR2Theory, na.rm = TRUE),
      minDeltaR2 = suppressWarnings(min(m$deltaR2Theory, na.rm = TRUE)),
      tierBySector = paste(paste0(names(tierVec), ": ", tierVec), collapse = "; "),
      deltaR2BySector = paste(paste0(names(dr2Vec), ": ", round(dr2Vec, 3)), collapse = "; "),
      gatePass = length(failReasons) == 0,
      gateFailReason = if (length(failReasons) > 0) paste(failReasons, collapse = "; ") else "",
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL

  # Order: gate pass first, then worse-sector tier, then mean deltaR2(theory)
  out <- out[order(-out$gatePass, -tierRank[out$minTier],
                   -out$meanDeltaR2, out$model), , drop = FALSE]
  out$rank <- seq_len(nrow(out))
  rownames(out) <- NULL
  out
}
