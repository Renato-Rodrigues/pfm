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
#' terms, and a \strong{fit-reliability gate} (added 2026-06-16) that rejects
#' specifications whose \eqn{\Delta R^2}(theory) is \emph{inflated} — exceeding
#' \code{deltaR2Max} (default 1), which is impossible for a genuine incremental
#' McFadden pseudo-R² and signals a degenerate, near-separated baseline (typically
#' heavy unit-level fixed effects such as 54-region dummies); optionally a
#' \code{pseudoR2Range} check rejects specs whose overall pseudo-R² is implausible.
#' This stops a numerically-degenerate spec from winning the within-tier
#' \eqn{\Delta R^2} tie-break over a trustworthy one. Gate-failing specs are ranked
#' last regardless of tier.
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
#' @param deltaR2Max Numeric. Fit-reliability gate: a sector whose
#'   \code{deltaR2Theory} exceeds this is rejected as inflated/degenerate. A genuine
#'   incremental McFadden pseudo-R² lies in [0, 1], so the default \code{1} flags
#'   only mathematically-impossible values. Set \code{Inf} to disable.
#' @param pseudoR2Range Numeric length-2 or \code{NULL}. Optional second reliability
#'   measure: if the \code{pseudoR2} column is present and this is non-NULL, a sector
#'   whose overall pseudo-R² falls outside this band is rejected. Default \code{NULL}
#'   (off) — enable (e.g. \code{c(0, 1)}) for stages where a negative pseudo-R²
#'   genuinely signals degeneracy.
#' @param nearTieEps Numeric. BIC parsimony tie-break tolerance (ADR 0012). Among
#'   gate-passing specs of the same worse-sector tier, those whose mean
#'   \eqn{\Delta R^2}(theory) is within \code{nearTieEps} of the running leader are
#'   treated as theory-equivalent and ranked by lowest summed BIC (most parsimonious)
#'   first; requires a \code{bic} column in \code{df}. Default \code{0.05}; set
#'   \code{0} to disable and fall back to pure tier → \eqn{\Delta R^2} ordering.
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
computeMaximinScore <- function(df, vifGate = 10, deltaR2Max = 1, pseudoR2Range = NULL,
                                nearTieEps = 0.05) {
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
    tol <- 1e-6
    inflated <- m$sector[!is.na(m$deltaR2Theory) & m$deltaR2Theory > deltaR2Max + tol]
    if (length(inflated) > 0) {
      failReasons <- c(failReasons,
                       paste0("deltaR2(theory) > ", deltaR2Max,
                              " (inflated/degenerate fit): ", paste(inflated, collapse = ", ")))
    }
    if (!is.null(pseudoR2Range) && "pseudoR2" %in% colnames(m)) {
      badPR2 <- m$sector[!is.na(m$pseudoR2) &
                           (m$pseudoR2 < pseudoR2Range[1] - tol | m$pseudoR2 > pseudoR2Range[2] + tol)]
      if (length(badPR2) > 0) {
        failReasons <- c(failReasons,
                         paste0("pseudoR2 outside [", pseudoR2Range[1], ", ", pseudoR2Range[2],
                                "]: ", paste(badPR2, collapse = ", ")))
      }
    }

    minTierRank <- min(tierRank[m$tier])
    data.frame(
      model = model,
      minTier = names(tierRank)[match(minTierRank, tierRank)],
      meanDeltaR2 = mean(m$deltaR2Theory, na.rm = TRUE),
      minDeltaR2 = suppressWarnings(min(m$deltaR2Theory, na.rm = TRUE)),
      sumBIC = if ("bic" %in% colnames(m)) sum(m$bic) else NA_real_,
      tierBySector = paste(paste0(names(tierVec), ": ", tierVec), collapse = "; "),
      deltaR2BySector = paste(paste0(names(dr2Vec), ": ", round(dr2Vec, 3)), collapse = "; "),
      gatePass = length(failReasons) == 0,
      gateFailReason = if (length(failReasons) > 0) paste(failReasons, collapse = "; ") else "",
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL

  # Base order: gate pass, worse-sector tier, mean deltaR2(theory), then name.
  out <- out[order(-out$gatePass, -tierRank[out$minTier],
                   -out$meanDeltaR2, out$model), , drop = FALSE]
  rownames(out) <- NULL

  # BIC parsimony tie-break (ADR 0012): within gate-passing specs of the same tier,
  # treat those whose meanDeltaR2(theory) is within nearTieEps of the running leader as
  # theory-equivalent and rank the most parsimonious (lowest summed BIC) first. Falls
  # back to the pure tier->deltaR2 order when BIC is unavailable or nearTieEps == 0.
  haveBIC <- "sumBIC" %in% colnames(out) && any(is.finite(out$sumBIC))
  if (haveBIC && nearTieEps > 0) {
    bicKey <- ifelse(is.finite(out$sumBIC), out$sumBIC, Inf)
    placed <- integer(0)
    remaining <- which(out$gatePass)            # already tier->deltaR2 ordered
    while (length(remaining) > 0) {
      lead <- remaining[1]
      dl <- out$meanDeltaR2[lead]
      band <- if (is.na(dl)) lead else
        remaining[out$minTier[remaining] == out$minTier[lead] &
                    !is.na(out$meanDeltaR2[remaining]) &
                    out$meanDeltaR2[remaining] >= dl - nearTieEps]
      band <- band[order(bicKey[band], out$model[band])]   # parsimony, then name
      placed <- c(placed, band)
      remaining <- setdiff(remaining, band)
    }
    out <- out[c(placed, which(!out$gatePass)), , drop = FALSE]
    rownames(out) <- NULL
  }
  out$rank <- seq_len(nrow(out))
  rownames(out) <- NULL
  out
}
