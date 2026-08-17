# nolint start
#' Coupling-bound Run-Group step: the politically feasible price bound REMIND consumes
#'
#' @description
#' Package port of the former \code{analysis/psm-coupling-bound.R} (ADR 0040/0041).
#' Runs the three-stage chain that turns the estimated frontier into the per-region,
#' per-period carbon-price bound REMIND reads:
#' \enumerate{
#'   \item \code{\link{projectFeasiblePath}} — country-level feasible paths, ceilings, gaps
#'   \item \code{\link{aggregateFeasibilityToRegions}} — REMIND regions, emission-weighted,
#'         out-of-coverage resolved through the band rule, tiers and \eqn{\phi}
#'   \item \code{\link{exportFeasibilityBound}} — the price bound, swept over \eqn{\theta}
#' }
#'
#' \eqn{\theta} is a declared scenario parameter, not an estimate, so it is SWEPT:
#' \eqn{\theta = 0} is the speed-only case (no tier discount), and the uncoupled
#' reference is REMIND without this bound at all.
#'
#' The two REMIND price paths come from the scenario gdxs themselves — \code{refGdx}
#' is current policy (NPi), \code{optGdx} the unconstrained cost-optimal pathway.
#' Both must share a resolution, and price levels are only comparable across runs
#' with the same carbon budget.
#'
#' Writes \code{<group>/coupling/feasibility-bound-theta*.csv} and
#' \code{<group>/coupling/coupling-summary.rds}.
#'
#' @param group Run-Group name.
#' @param resultsDir,modelDir,cachefolder Standard Run-Group locations.
#' @param refGdx,optGdx Reference (current-policy) and optimal (ambitious) gdx paths.
#'   When \code{NULL} they are taken from the Run-Group's scenario registry via
#'   \code{scenarios}.
#' @param scenarios Optional parsed scenario registry (as \code{\link{startRun}} passes
#'   it). Used to resolve \code{refGdx}/\code{optGdx} when those are not given.
#' @param gdxRegionMapping Region mapping matching the gdxs' OWN resolution. Getting
#'   this wrong mis-assigns every region, silently.
#' @param thetas Numeric vector of \eqn{\theta} values to sweep. The default keeps every
#'   value that has ever been called the anchor, so no earlier batch becomes
#'   incomparable: 0.79 (ADR 0041, pre-\code{satAP} frontier, Bulk, country resolution),
#'   0.74 (regenerated frontier, Diffuse, still country resolution), and \strong{0.50} --
#'   the value the anchor actually takes at the resolution the coupling assigns
#'   \eqn{\varphi} on. Only the last is "the efficiency anchor"; 0.74 and 0.79 are swept
#'   points retained for continuity.
#' @param anchorTheta The \eqn{\theta} whose per-region detail is reported and stored
#'   as \code{boundAnchor}. Snapped to the nearest swept value.
#' @param recordAnchorDerivation Derive the efficiency anchor from this run's own
#'   region-level aggregation with \code{\link{computeEfficiencyAnchor}} and store it as
#'   \code{anchorDerivation}. On by default: a swept \eqn{\theta} whose provenance is a
#'   constant in a function signature cannot be re-checked, and this one has already moved
#'   twice (spec regeneration, then sector).
#' @param panelData Optional pre-built historical panel.
#' @param verbose Logical.
#'
#' @return Invisibly, the coupling summary list.
#' @author Renato Rodrigues
#' @export
runPSMCouplingBound <- function(group,
                                resultsDir = getOption("pfm.resultsDir", "output"),
                                modelDir = getOption("pfm.modelDir", "output"),
                                cachefolder = NULL,
                                refGdx = NULL, optGdx = NULL, scenarios = NULL,
                                gdxRegionMapping = "regionmapping_21_EU11.csv",
                                thetas = c(0, 0.25, 0.50, 0.74, 0.79),
                                # 0.50, not 0.74: the anchor derived at REGION resolution
                                # and the seed year, which is where phi is assigned. The
                                # country-level 0.74 was the wrong resolution - aggregation
                                # collapses the gap range, so median u rises and theta
                                # falls. See computeEfficiencyAnchor() and MODEL.md 5.3.
                                anchorTheta = 0.50,
                                recordAnchorDerivation = TRUE,
                                panelData = NULL,
                                verbose = TRUE) {
  groupDir <- .resolveGroupDir(group, resultsDir, modelDir, cachefolder)
  say <- function(...) if (isTRUE(verbose)) message("[PSM-BOUND:", group, "] ", ...)
  t0 <- Sys.time()
  sectors <- c("Bulk", "Diffuse")
  TCO2 <- 1000 / (44 / 12)   # T$/GtC -> US$/tCO2

  need <- c("selected-models-psm.yml", "frontier.rds", "temporal-validation.rds",
            "donor-assignment-band-Bulk.rds", "donor-assignment-band-Diffuse.rds")
  miss <- need[!file.exists(file.path(groupDir, need))]
  if (length(miss)) {
    .recordStep(groupDir, group, "psm-coupling-bound", t0, status = "skipped",
                metrics = list(reason = paste("missing", paste(miss, collapse = ", "))))
    say("skipped — missing: ", paste(miss, collapse = ", "))
    return(invisible(NULL))
  }

  # ── gdx resolution: explicit args win, else the scenario registry ────────────
  if (is.null(refGdx) || is.null(optGdx)) {
    sc <- Filter(function(s) nzchar(s$gdx %||% "") && file.exists(s$gdx),
                 scenarios %||% list())
    usable <- function(s) s$gdx
    # The AMBITIOUS pathway is the GATING scenario — that is what `gating` means, and
    # it is already declared in the registry. Guessing it from the id (matching
    # "pkbudg", say) silently picks whichever budget happens to be listed first, so a
    # registry carrying both PkBudg750 and PkBudg1000 would define the bound against
    # an arbitrary one of them.
    gate <- Filter(function(s) isTRUE(s$gating), sc)
    optGdx <- optGdx %||% (if (length(gate)) usable(gate[[1]]) else NULL)
    # The REFERENCE is current policy: the non-gating scenario, preferring an
    # explicitly NPi/base-looking id when several remain.
    if (is.null(refGdx)) {
      rest <- Filter(function(s) !isTRUE(s$gating), sc)
      named <- Filter(function(s) grepl("npi|base|ref|current",
                                        s$id %||% "", ignore.case = TRUE), rest)
      pool <- if (length(named)) named else rest
      refGdx <- if (length(pool)) usable(pool[[1]]) else NULL
    }
    if (length(sc) > 2 && (is.null(refGdx) || is.null(optGdx))) {
      say("registry has ", length(sc), " usable scenarios — pass refGdx=/optGdx= ",
          "explicitly if the automatic choice is not what you want.")
    }
  }
  for (nm in c("refGdx", "optGdx")) {
    v <- get(nm)
    if (is.null(v) || !file.exists(v)) {
      .recordStep(groupDir, group, "psm-coupling-bound", t0, status = "skipped",
                  metrics = list(reason = paste0(nm, " unavailable")))
      say("skipped — ", nm, " could not be resolved. Pass refGdx=/optGdx= explicitly ",
          "or configure the scenario registry.")
      return(invisible(NULL))
    }
  }
  say("reference gdx: ", refGdx)
  say("optimal   gdx: ", optGdx)

  norm <- function(s) {
    for (f in c("actorPowerDrivers", "actorPowerIndex", "instQualityDrivers", "controlDrivers"))
      if (!is.null(s[[f]])) s[[f]] <- unlist(s[[f]])
    s$panelTransform <- s$panelTransform %||% "levels"
    s
  }
  sel <- yaml::read_yaml(file.path(groupDir, "selected-models-psm.yml"))
  cfg <- lapply(stats::setNames(nm = sectors), function(sec)
    norm(Filter(function(x) identical(x$model_type, paste0("PolicyStringency: ", sec)), sel)[[1]]))
  say("deployed spec: ", cfg$Bulk$name)

  panel <- panelData %||% .psmHistPanel(groupDir, verbose = verbose)
  if (is.null(panel)) {
    .recordStep(groupDir, group, "psm-coupling-bound", t0, status = "failed",
                metrics = list(reason = "historical panel unavailable"))
    return(invisible(NULL))
  }
  fr <- readRDS(file.path(groupDir, "frontier.rds"))
  tv <- readRDS(file.path(groupDir, "temporal-validation.rds"))
  lambda <- vapply(sectors, function(s) tv$bySector[[s]]$ecm$metrics$adjustmentSpeed, numeric(1))
  say(sprintf("lambda: Bulk %.4f | Diffuse %.4f  (validated OOS only in electricity: +0.08)",
              lambda[["Bulk"]], lambda[["Diffuse"]]))

  # Scenario panel, cached per group. This used to be a hardcoded readRDS of a cache
  # file left behind by an earlier run — absent on a fresh checkout, and silently
  # belonging to a DIFFERENT Run-Group even when present.
  scenCache <- file.path(resultsDir, "panel-cache", paste0(group, "-scen-ca.rds"))
  if (file.exists(scenCache)) {
    say("scenario panel: cache hit ", scenCache)
    scen <- readRDS(scenCache)
  } else {
    say("scenario panel: building from ", optGdx)
    scen <- panelDataScenario(gdxFile = optGdx, aggregate = TRUE,
                              gdxRegionMappingFile = gdxRegionMapping,
                              outputRegionMappingFile = "country")
    dir.create(dirname(scenCache), showWarnings = FALSE, recursive = TRUE)
    saveRDS(scen, scenCache)
  }

  # ── 1. country-level feasible paths ─────────────────────────────────────────
  paths <- lapply(stats::setNames(nm = sectors), function(sec) {
    ct <- fr$bySector[[sec]]$coefTable
    fb <- stats::setNames(ct$estimate, ct$term)
    fb <- fb[!names(fb) %in% c("sigmaSq", "gamma")]
    p <- projectFeasiblePath(cfg[[sec]], sec, histData = panel, scenarioData = scen,
                             rule = "speed-limited", frontierBeta = fb,
                             modelDir = modelDir, verbose = FALSE)
    p$sector <- sec
    p
  })
  nCov <- length(unique(paths$Bulk$region[!paths$Bulk$outOfCoverage %in% TRUE]))
  say("country-level paths: ", nrow(paths$Bulk), " rows, ", nCov, " in-coverage countries")

  # ── 2. aggregate to REMIND regions ──────────────────────────────────────────
  map <- pfmGetMapping(gdxRegionMapping, type = "regional")
  # Emission weights are the intended production input (EDGAR). Absent an emissions
  # series in the training panel, GDP x energy intensity is a documented proxy: the
  # closest available correlate of combustion emissions, flagged as such on export.
  wsrc <- panel[, 2022, c("GDP", "Energy Intensity")]
  wts <- stats::setNames(as.numeric(wsrc[, , "GDP"]) * as.numeric(wsrc[, , "Energy Intensity"]),
                         magclass::getItems(wsrc, dim = 1))
  wts[!is.finite(wts) | wts < 0] <- 0
  say("weights: GDP x energy intensity (proxy for emissions), ", sum(wts > 0), " countries")

  asg <- stats::setNames(lapply(sectors, function(sec)
    readRDS(file.path(groupDir, paste0("donor-assignment-band-", sec, ".rds")))), sectors)

  feasByTheta <- lapply(stats::setNames(nm = as.character(thetas)), function(thc) {
    th <- as.numeric(thc)
    do.call(rbind, lapply(sectors, function(sec) {
      a <- aggregateFeasibilityToRegions(paths[[sec]], map, weights = wts,
                                         assignment = asg[[sec]], theta = th, nTiers = 4)
      a$sector <- sec
      a
    }))
  })
  base <- feasByTheta[[as.character(thetas[which.min(abs(thetas - 0.5))])]]
  tierTab <- unique(base[base$sector == "Bulk",
                         c("region", "tier", "inCoverageShare", "shareObserved",
                           "shareDonor", "shareLowBand", "shareMedian", "ceilingValid")])

  # ── 3. price paths from the gdxs ────────────────────────────────────────────
  readPrice <- function(f) {
    x <- gdx::readGDX(f, "pm_taxCO2eq") * TCO2
    yrs <- magclass::getYears(x, as.integer = TRUE)
    x[, yrs[yrs >= 2025 & yrs <= 2100], ]
  }
  pRef <- readPrice(refGdx)
  pOpt <- readPrice(optGdx)

  # ── 4. the bound, swept over theta ──────────────────────────────────────────
  outDir <- file.path(groupDir, "coupling")
  dir.create(outDir, showWarnings = FALSE, recursive = TRUE)
  summ <- do.call(rbind, lapply(as.character(thetas), function(thc) {
    th <- as.numeric(thc)
    b <- exportFeasibilityBound(feasByTheta[[thc]], pOpt, pRef, lambda = lambda,
                                sectorRule = "min",
                                file = file.path(outDir,
                                  sprintf("feasibility-bound-theta%03d.csv", round(100 * th))))
    y50 <- b[b$year == 2050, ]; y30 <- b[b$year == 2030, ]
    data.frame(theta = th, bindShare = mean(b$binds),
               medPrice2030 = stats::median(y30$priceBound),
               medPrice2050 = stats::median(y50$priceBound),
               optPrice2050 = stats::median(y50$priceOptimal),
               shortfall2050pct = 100 * (1 - stats::median(y50$priceBound) /
                                           stats::median(y50$priceOptimal)),
               maxShortfall2050 = max(y50$bindGap))
  }))
  if (isTRUE(verbose)) {
    message("=== theta sweep: what the political constraint costs the ambitious pathway ===")
    print(summ, row.names = FALSE, digits = 4)
  }

  ancKey <- as.character(thetas[which.min(abs(thetas - anchorTheta))])
  bAnchor <- exportFeasibilityBound(feasByTheta[[ancKey]], pOpt, pRef, lambda = lambda,
                                    sectorRule = "min")

  # Where the anchor comes from, recorded next to the number it justifies. theta does not
  # enter the gaps, so any swept frame gives the same answer; `base` is used. Derived at
  # REGION resolution - the level the coupling normalises on - which need not equal the
  # country-level figure quoted in MODEL.md 5.3.
  anchorDeriv <- NULL
  if (isTRUE(recordAnchorDerivation)) {
    anchorDeriv <- tryCatch(
      computeEfficiencyAnchor(base, resolution = "region"),
      error = function(e) { say("anchor derivation failed: ", conditionMessage(e)); NULL })
    if (!is.null(anchorDeriv) && isTRUE(verbose)) {
      message("=== efficiency anchor, derived at REGION resolution ===")
      print(anchorDeriv, row.names = FALSE, digits = 4)
      bad <- anchorDeriv$sector[!anchorDeriv$inRange %in% TRUE]
      if (length(bad)) {
        message("NOTE: no admissible theta reproduces median efficiency for: ",
                paste(bad, collapse = ", "),
                " - the gap distribution is too compressed. Report it, do not chase it.")
      }
    }
  }

  out <- list(thetaSweep = summ, tiers = tierTab, lambda = lambda,
              boundAnchor = bAnchor, thetas = thetas, anchorTheta = as.numeric(ancKey),
              anchorDerivation = anchorDeriv,
              refGdx = refGdx, optGdx = optGdx, inCoverageCountries = nCov)
  saveRDS(out, file.path(outDir, "coupling-summary.rds"))
  say("wrote ", outDir, "/{feasibility-bound-theta*.csv, coupling-summary.rds}")

  .recordStep(groupDir, group, "psm-coupling-bound", t0, metrics = list(
    thetas = paste(thetas, collapse = "/"), anchorTheta = as.numeric(ancKey),
    inCoverageCountries = nCov,
    bindShareAnchor = round(summ$bindShare[summ$theta == as.numeric(ancKey)][1], 3)))
  invisible(out)
}
# nolint end
