# nolint start
#' Politically feasible carbon-price bound for the IAM (ADR 0041, TODO 3.1)
#'
#' @description
#' The object the IAM actually consumes: a per-region, per-period bound on the
#' regional mitigation price, derived from the ambition-gap tiers and the validated
#' adjustment speed. This is the last step of the PSM side of the coupling.
#'
#' \strong{No exchange rate is involved.} Earlier designs converted index points
#' into \$/tCO2 via \eqn{\kappa = \partial P/\partial S}; that is retired (ADR
#' 0041) because a bounded 0-10 index times a constant is a bounded price cap,
#' which at any plausible conversion sits far below what an ambitious pathway
#' needs — the constraint would then bind for an arithmetic reason rather than a
#' political one. Instead the coupling is \emph{relative}: politics sets what
#' \emph{share} of the IAM's own \emph{incremental} cost-optimal effort a region
#' can realize.
#'
#' \deqn{P^{*}_{r,t} = P^{ref}_{r,t} + \varphi_r [P^{\circ}_{r,t} - P^{ref}_{r,t}]}
#' \deqn{P_{r,t} = P_{r,t-1} + \lambda^{eff}[P^{*}_{r,t} - P_{r,t-1}], \quad
#'       \lambda^{eff} = 1-(1-\lambda)^{\Delta t}}
#'
#' Two properties make this safe to hand to an IAM. \eqn{P \le P^{\circ}} always,
#' so politics can only slow a pathway, never accelerate it. And a region already
#' at its current-policy price is unaffected, because both the share \emph{and}
#' the speed limit act on the \emph{increment}: politics governs how much
#' \emph{more} a region can be pushed, not what it already does.
#'
#' The two mechanisms are separable, which gives a three-way experimental design
#' rather than a binary:
#' \describe{
#'   \item{uncoupled}{run the IAM without this bound at all.}
#'   \item{\code{theta = 0}}{\eqn{\varphi = 1}: no tier discount, \strong{speed
#'     limit only} — the pathway may reach the full cost-optimal price, but only
#'     as fast as politics has been observed to build policy.}
#'   \item{\code{theta > 0}}{speed limit \emph{and} a tiered discount on how much
#'     of the incremental effort each region can ultimately realize.}
#' }
#' \code{theta = 0} is therefore \strong{not} the uncoupled run — it is the
#' speed-only case, and reporting the three side by side separates "politics is
#' slow" from "politics is unequal".
#'
#' \strong{Sector reconciliation.} The feasibility layer is per sector but the IAM
#' carries one price per region, so the two sectors must be reconciled.
#' \code{sectorRule = "min"} (default) takes the \emph{worse} sector's share and
#' the \emph{slowest} speed — the maximin discipline used throughout this project,
#' and the reading under which the demand side is the binding constraint.
#'
#' @param feasibility Region-level feasibility from
#'   \code{\link{aggregateFeasibilityToRegions}} — needs \code{region, year, phi}
#'   and, when \code{sectorRule != "none"}, a \code{sector} column. Regions whose
#'   ceiling was invalid carry \code{phi = 1} and are therefore uncoupled.
#' @param priceOptimal,priceReference The IAM's unconstrained cost-optimal and
#'   current-policy price paths: \code{magpie} objects (region x year) or
#'   data.frames with \code{region, year, value}. Same units; the bound is
#'   returned in those units.
#' @param lambda Numeric scalar, or named by sector (e.g.
#'   \code{c(Bulk = 0.103, Diffuse = 0.078)}) — the validated adjustment speed(s).
#'   Carry the honesty labels of \code{\link{runPSMSectorSpeeds}} with any value
#'   used here: only the electricity rate beats persistence out of sample.
#' @param sectorRule How to reconcile sectors onto one price:
#'   \code{"min"} (default, worse sector / slowest speed), \code{"mean"},
#'   \code{"Bulk"}, \code{"Diffuse"}, or \code{"none"} if \code{feasibility} is
#'   already single-sector.
#' @param seedYear Integer or \code{NULL}. Year the price recursion starts from
#'   (\code{NULL} = the earliest year present). The seed price is the reference
#'   path, so the bound starts from observed policy, not from zero.
#' @param file Optional path. \code{.csv} writes the table; any other extension is
#'   rejected rather than guessed.
#'
#' @return Data.frame \code{region, year, phi, tier, lambda, priceReference,
#'   priceOptimal, priceTarget, priceBound, binds, bindGap}, where
#'   \code{priceBound} is the speed-limited politically feasible price,
#'   \code{binds} flags region-years where it sits materially below the
#'   cost-optimal price, and \code{bindGap} is the shortfall. Attributes:
#'   \code{"theta"} (carried from \code{feasibility}), \code{"sectorRule"},
#'   \code{"lambda"}, \code{"bindShare"}, \code{"seedYear"}.
#'
#' @seealso \code{\link{aggregateFeasibilityToRegions}},
#'   \code{\link{projectFeasiblePath}}, ADR 0041.
#' @export
#' @author Renato Rodrigues
exportFeasibilityBound <- function(feasibility, priceOptimal, priceReference,
                                   lambda, sectorRule = c("min", "mean", "Bulk",
                                                          "Diffuse", "none"),
                                   seedYear = NULL, file = NULL) {
  sectorRule <- match.arg(sectorRule)
  need <- c("region", "year", "phi")
  miss <- setdiff(need, colnames(feasibility))
  if (length(miss)) {
    stop("exportFeasibilityBound: 'feasibility' is missing column(s): ",
         paste(miss, collapse = ", "), " - pass aggregateFeasibilityToRegions() output.")
  }
  pO <- .psmAsPricePath(priceOptimal, "priceOptimal")
  pR <- .psmAsPricePath(priceReference, "priceReference")

  # --- sector reconciliation --------------------------------------------------
  f <- feasibility
  lamBySector <- length(lambda) > 1 && !is.null(names(lambda))
  if (!identical(sectorRule, "none")) {
    if (!"sector" %in% colnames(f)) {
      stop("exportFeasibilityBound: sectorRule = '", sectorRule, "' needs a ",
           "'sector' column in 'feasibility' (or use sectorRule = 'none').")
    }
    if (sectorRule %in% c("Bulk", "Diffuse")) {
      f <- f[f$sector == sectorRule, , drop = FALSE]
      if (!nrow(f)) stop("exportFeasibilityBound: no rows for sector '", sectorRule, "'.")
      lam <- if (lamBySector) unname(lambda[[sectorRule]]) else lambda[[1]]
      f$lambda <- lam
    } else {
      agg <- if (identical(sectorRule, "min")) min else mean
      # The reported tier must correspond to the SELECTED phi, so under the
      # worse-sector rule it is the WORST (highest) tier, not the lowest - phi is
      # decreasing in tier. Reporting min(tier) alongside min(phi) would print a
      # tier-1 label next to a tier-3 discount.
      tierAgg <- if (identical(sectorRule, "min")) max else mean
      key <- paste(f$region, f$year, sep = "\r")
      f <- do.call(rbind, lapply(split(f, key), function(d) {
        lam <- if (lamBySector) {
          v <- unname(lambda[as.character(d$sector)])
          v <- v[is.finite(v)]
          if (!length(v)) NA_real_ else agg(v)
        } else lambda[[1]]
        tv <- if ("tier" %in% names(d)) d$tier[is.finite(d$tier)] else numeric(0)
        data.frame(region = d$region[1], year = d$year[1],
                   phi = agg(d$phi, na.rm = TRUE),
                   # All-NA tiers (an uncoupled region) must stay NA, not +/-Inf.
                   tier = if (length(tv)) tierAgg(tv) else NA_real_,
                   lambda = lam, stringsAsFactors = FALSE)
      }))
      rownames(f) <- NULL
    }
  } else {
    f$lambda <- if (lamBySector) unname(lambda[[1]]) else lambda[[1]]
  }
  if (!"tier" %in% colnames(f)) f$tier <- NA_real_
  f <- f[, c("region", "year", "phi", "tier", "lambda")]

  # --- join the price paths ----------------------------------------------------
  out <- merge(f, pO, by = c("region", "year"), all.x = TRUE)
  names(out)[names(out) == "value"] <- "priceOptimal"
  out <- merge(out, pR, by = c("region", "year"), all.x = TRUE)
  names(out)[names(out) == "value"] <- "priceReference"
  out <- out[is.finite(out$priceOptimal) & is.finite(out$priceReference), , drop = FALSE]
  if (!nrow(out)) {
    stop("exportFeasibilityBound: no region-year is present in BOTH the ",
         "feasibility table and the two price paths - check the region naming ",
         "and the year grid.")
  }

  # The political target: the share applies to the INCREMENT over current policy.
  out$priceTarget <- out$priceReference +
    out$phi * pmax(out$priceOptimal - out$priceReference, 0)

  # --- speed-limited build-up of the INCREMENT --------------------------------
  # The recursion runs on the incremental effort above current policy, not on the
  # whole price. Applying the speed limit to the level would throttle a region's
  # OWN current-policy pathway - i.e. declare the observed status quo politically
  # infeasible - which is incoherent. On the increment, a region whose optimal
  # price already equals its reference price is untouched (target increment 0),
  # and what the speed limit governs is how fast ADDITIONAL effort is built up.
  sy <- seedYear %||% suppressWarnings(min(out$year, na.rm = TRUE))
  out <- out[order(out$region, out$year), , drop = FALSE]
  out$priceBound <- NA_real_
  for (r in unique(out$region)) {
    idx <- which(out$region == r)
    idx <- idx[order(out$year[idx])]
    delta <- NA_real_
    prevYear <- NA_real_
    for (i in idx) {
      targetDelta <- out$priceTarget[i] - out$priceReference[i]
      if (!is.finite(delta)) {           # seed: no incremental effort banked yet
        delta <- 0
      } else {
        dt <- out$year[i] - prevYear
        lam <- out$lambda[i]
        lamEff <- if (is.finite(lam) && lam > 0 && lam < 1) 1 - (1 - lam)^dt else 1
        delta <- delta + lamEff * (targetDelta - delta)
      }
      prevYear <- out$year[i]
      # Politics can only slow a pathway, never accelerate it.
      out$priceBound[i] <- min(out$priceReference[i] + delta, out$priceOptimal[i])
    }
  }
  tol <- 1e-8 + 1e-6 * pmax(abs(out$priceOptimal), 1)
  out$binds <- out$priceBound < out$priceOptimal - tol
  out$bindGap <- pmax(out$priceOptimal - out$priceBound, 0)
  rownames(out) <- NULL

  attr(out, "theta") <- attr(feasibility, "theta")
  attr(out, "sectorRule") <- sectorRule
  attr(out, "lambda") <- lambda
  attr(out, "seedYear") <- sy
  attr(out, "bindShare") <- mean(out$binds, na.rm = TRUE)

  if (!is.null(file)) {
    if (!grepl("\\.csv$", file, ignore.case = TRUE)) {
      stop("exportFeasibilityBound: 'file' must end in .csv (got '", file, "').")
    }
    dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
    hdr <- c(
      "# Politically feasible carbon-price bound (pfm::exportFeasibilityBound, ADR 0041)",
      paste0("# generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
      paste0("# theta (coupling severity): ", attr(feasibility, "theta") %||% NA),
      paste0("# sectorRule: ", sectorRule, " | lambda: ",
             paste(sprintf("%s=%.4f", names(lambda) %||% "all", lambda), collapse = " ")),
      paste0("# seedYear: ", sy, " | binding share: ",
             round(100 * attr(out, "bindShare"), 1), "%"),
      "# priceBound is the deliverable; priceOptimal/priceReference are the inputs it was built from.",
      "# theta = 0 reproduces the unconstrained run exactly."
    )
    con <- file(file, open = "wt")
    on.exit(close(con), add = TRUE)
    writeLines(hdr, con)
    utils::write.csv(out, con, row.names = FALSE)
  }
  out
}

# Coerce a magpie object or a long data.frame to region/year/value.
#' @keywords internal
.psmAsPricePath <- function(x, what) {
  if (magclass::is.magpie(x)) {
    yrs <- magclass::getYears(x, as.integer = TRUE)
    regs <- magclass::getItems(x, dim = 1)
    return(data.frame(region = rep(regs, times = length(yrs)),
                      year = rep(yrs, each = length(regs)),
                      value = as.numeric(x), stringsAsFactors = FALSE))
  }
  if (is.data.frame(x)) {
    cn <- colnames(x)
    rc <- grep("^(region|iso3c?|RegionCode)$", cn, ignore.case = TRUE)
    yc <- grep("^(year|period)$", cn, ignore.case = TRUE)
    vc <- grep("^(value|price)$", cn, ignore.case = TRUE)
    if (!length(rc) || !length(yc) || !length(vc)) {
      stop(".psmAsPricePath: '", what, "' needs region, year and value columns ",
           "(have: ", paste(cn, collapse = ", "), ").")
    }
    return(data.frame(region = as.character(x[[rc[1]]]),
                      year = as.integer(x[[yc[1]]]),
                      value = as.numeric(x[[vc[1]]]), stringsAsFactors = FALSE))
  }
  stop(".psmAsPricePath: '", what, "' must be a magpie object or a data.frame.")
}
# nolint end
