# nolint start
#' REMIND <-> PFM coupling step, called from inside the Nash iteration loop
#'
#' @description
#' The interface REMIND invokes between Nash iterations when
#' \code{cm_taxCO2_regiDiff = 11}:
#'
#' \preformatted{
#'   Execute "Rscript -e 'library(pfm); pfm::iterativePFM()'";
#'   Execute_Loadpoint 'p45_regiDiff_phi' p45_regiDiff_phi_aux = p45_regiDiff_phi;
#' }
#'
#' It mirrors \code{edgeTransport::iterativeEdgeTransport()}: run from the REMIND
#' run directory, read the current solution from the gdx REMIND has just written,
#' compute this module's contribution, and hand it back as a gdx that the next
#' presolve loads. \strong{A gdx is used rather than an \code{.inc} file because
#' \code{$include} is a compile-time directive} — an include is baked in once when
#' the model compiles and can never change between iterations.
#'
#' What happens each coupling iteration:
#' \enumerate{
#'   \item read the energy system from the REMIND gdx (VRE share, electrification,
#'     fossil shares) — the ingredients of the actor-power drivers;
#'   \item recompute each region's political feasibility share \eqn{\varphi} from
#'     its ambition gap below the stochastic feasibility frontier;
#'   \item write \code{p45_regiDiff_phi.gdx} for REMIND to load.
#' }
#'
#' \strong{Failure discipline.} If anything goes wrong the function writes no gdx
#' and returns \code{FALSE} with a warning. REMIND's presolve keeps the previous
#' iteration's \eqn{\varphi} in that case, so a broken coupling degrades to a
#' \emph{frozen} coupling rather than silently reverting to an uncoupled run.
#'
#' @param gdx Path to the REMIND gdx holding the current solution. Default
#'   \code{"fulldata.gdx"} in the working directory, which is what REMIND has
#'   written by the time presolve runs.
#' @param outputFile Name of the gdx to write. Must match the
#'   \code{Execute_Loadpoint} name in
#'   \code{45_carbonprice/functionalForm/presolve.gms}.
#' @param group,resultsDir,modelDir The PFM Run-Group supplying the deployed
#'   specification, frontier and speeds. Defaults read the
#'   \code{pfm.couplingGroup} / \code{pfm.resultsDir} / \code{pfm.modelDir}
#'   options so the REMIND run can set them once in its \code{.Rprofile}.
#' @param gdxRegionMapping Mapping matching the REMIND gdx's OWN resolution
#'   (\code{"regionmapping_21_EU11.csv"} for EU21 runs, \code{"regionmappingH12.csv"}
#'   for H12). Reading an EU21 gdx through the H12 mapping mis-assigns every region.
#' @param refGdx Path to the current-policies REMIND gdx supplying the reference
#'   price path \eqn{P^{ref}}. Required for \code{bindMode = 2}; option
#'   \code{pfm.couplingRefGdx}.
#' @param bindMode \code{1} = phi bounds the price ratio, \code{2} = phi caps the
#'   absolute level. Must match GAMS \code{cm_pfmBindMode}. Mode 2 \strong{errors}
#'   rather than continue without a bound - see
#'   \code{docs/psm-coupling-scenario-design.md}.
#' @param weights Aggregation weights. \code{"finalEnergy"} (default) resolves
#'   country-level \code{fe_total} via \code{\link{psmCouplingWeights}} - the closest
#'   available correlate of the emissions a carbon price acts on. A named numeric
#'   vector is used as given. \code{NULL} means \strong{equal} country weights, which
#'   over-represent small emitters and should only be used deliberately.
#' @param weightYear,weightScenario Year and SSP used to project the final-energy
#'   weights (see \code{\link{psmCouplingWeights}}). Default 2050 / \code{"SSP2"}:
#'   mid-century is where the bound bites and where SSP growth paths have visibly
#'   diverged. \strong{\code{weightScenario} must match the SSP the coupled run uses},
#'   or the weights describe a different world than the model does.
#' @param couplingConfig Path to the run-local YAML written by REMIND's
#'   \code{scripts/start/preparePFM.R}, holding the \strong{static} settings with
#'   \strong{relative} paths so the run folder is self-contained. Absent outside a
#'   prepared REMIND run, which is normal.
#' @param runtimeConfig Path to the small YAML \strong{written by GAMS}
#'   (\code{presolve.gms}) carrying \code{bindMode} and \code{theta}. When present it
#'   \strong{overrides} the corresponding arguments, so the scenario config is the
#'   single source of truth and no hand-maintained copy can drift out of sync. Absent
#'   is normal outside GAMS.
#' @param gapMeasure,phiRule Forwarded to
#'   \code{\link{aggregateFeasibilityToRegions}}; the defaults match the offline
#'   pipeline, so the coupled run cannot silently disagree with the published tables.
#' @param theta,nTiers,lambda Coupling knobs, passed through to
#'   \code{\link{aggregateFeasibilityToRegions}}. \code{theta} is a declared
#'   scenario parameter, not an estimate — sweep it.
#' @param mapping Country-to-REMIND-region mapping (file name or data.frame).
#' @param weights Aggregation weights by country; emissions for a price-side
#'   constraint. \code{NULL} warns and uses equal weights.
#' @param verbose Logical.
#'
#' @return Invisibly \code{TRUE} on success, \code{FALSE} if the step was skipped
#'   or failed (in which case no gdx is written).
#'
#' @seealso \code{\link{aggregateFeasibilityToRegions}},
#'   \code{\link{projectFeasiblePath}}, \code{\link{exportFeasibilityRegiDiff}}
#'   (the non-iterative include-file variant), ADR 0041.
#' @export
#' @author Renato Rodrigues
iterativePFM <- function(gdx = "fulldata.gdx",
                         outputFile = "p45_regiDiff_phi.gdx",
                         group = getOption("pfm.couplingGroup", "psm-country-v3"),
                         resultsDir = getOption("pfm.resultsDir", "pfm"),
                         modelDir = getOption("pfm.modelDir", "pfm"),
                         couplingConfig = getOption("pfm.couplingConfig",
                                                    "pfm-coupling.yml"),
                         theta = getOption("pfm.couplingTheta", 0.5),
                         nTiers = 4L,
                         gdxRegionMapping = getOption("pfm.gdxRegionMapping",
                                                      "regionmapping_21_EU11.csv"),
                         gapMeasure = "relative", phiRule = "continuous",
                         refGdx = getOption("pfm.couplingRefGdx", NULL),
                         bindMode = as.integer(getOption("pfm.couplingBindMode", 1L)),
                         lambda = NULL,
                         mapping = getOption("pfm.couplingMapping", "regionmapping_21_EU11.csv"),
                         weights = getOption("pfm.couplingWeights", "finalEnergy"),
                         weightYear = getOption("pfm.couplingWeightYear", 2050),
                         weightScenario = getOption("pfm.couplingWeightScenario",
                                                    "SSP2"),
                         runtimeConfig = getOption("pfm.couplingRuntimeConfig",
                                                   "pfm-coupling-runtime.yml"),
                         verbose = TRUE) {
  say <- function(...) if (isTRUE(verbose)) message("[iterativePFM] ", ...)

  # --- static settings, written into the run folder by preparePFM.R -------------
  # Paths here are RELATIVE to the run folder (the working directory when GAMS calls
  # Rscript), so the folder is self-contained: movable, archivable and re-runnable
  # without editing anything. Absent file = not a prepared REMIND run, which is normal
  # offline.
  if (file.exists(couplingConfig)) {
    sc <- tryCatch(yaml::read_yaml(couplingConfig), error = function(e) NULL)
    if (!is.null(sc)) {
      if (!is.null(sc$group)) group <- sc$group
      if (!is.null(sc$resultsDir)) resultsDir <- sc$resultsDir
      if (!is.null(sc$modelDir)) modelDir <- sc$modelDir
      if (!is.null(sc$couplingMapping)) mapping <- sc$couplingMapping
      if (!is.null(sc$gdxRegionMapping)) gdxRegionMapping <- sc$gdxRegionMapping
      if (!is.null(sc$refGdx)) refGdx <- sc$refGdx
      if (!is.null(sc$weightScenario)) weightScenario <- sc$weightScenario
      if (!is.null(sc$weightYear)) weightYear <- as.numeric(sc$weightYear)
      say("run config from '", couplingConfig, "': group ", group,
          ", resultsDir ", resultsDir)
    }
  }

  # --- runtime settings written BY GAMS ----------------------------------------
  # GAMS owns bindMode and theta - they come from the scenario config - so it writes
  # them here and they OVERRIDE anything set locally. This removes the one-copy-per-
  # place hazard: an .Rprofile that disagreed with the scenario row used to produce a
  # complete and wrong run, silently. Absent file = running outside GAMS, which is
  # fine (tests, offline analysis) and leaves the arguments as given.
  if (file.exists(runtimeConfig)) {
    rt <- tryCatch(yaml::read_yaml(runtimeConfig), error = function(e) NULL)
    if (!is.null(rt)) {
      if (!is.null(rt$bindMode)) bindMode <- as.integer(rt$bindMode)
      if (!is.null(rt$theta)) theta <- as.numeric(rt$theta)
      say("runtime config from GAMS: bindMode ", bindMode, ", theta ",
          signif(theta, 4), if (!is.null(rt$iteration)) paste0(", iteration ", rt$iteration) else "")
    } else {
      warning("iterativePFM: '", runtimeConfig, "' exists but could not be parsed; ",
              "falling back to the local settings. Check bindMode/theta by hand.",
              call. = FALSE)
    }
  } else {
    say("no runtime config at '", runtimeConfig, "' - using local settings ",
        "(bindMode ", bindMode, ", theta ", signif(theta, 4), ")")
  }
  # The two mappings answer different questions - gdxRegionMapping is the resolution
  # of the gdx we READ, mapping is the resolution we DELIVER at - but silently mixing
  # H12 and EU21 mis-assigns every region, so a mismatch is worth saying out loud.
  if (!identical(mapping, gdxRegionMapping)) {
    say("NOTE: reading a '", gdxRegionMapping, "' gdx but delivering at '", mapping,
        "'. Intended only if the resolutions genuinely differ.")
  }
  t0 <- Sys.time()

  ok <- tryCatch({
    if (!requireNamespace("gdx", quietly = TRUE)) {
      stop("the 'gdx' package is required to read the REMIND solution")
    }
    if (!file.exists(gdx)) {
      stop("REMIND gdx not found at '", gdx, "' (working directory: ", getwd(), ")")
    }
    say("reading REMIND state from ", gdx)

    # 1. REMIND -> PFM: the energy system becomes the actor-power drivers. The
    #    scenario panel is built by the same code path the offline projection uses,
    #    so the coupled and uncoupled runs are scored on identical designs.
    # The gdx's OWN resolution - H12 and EU21 runs are both possible, and reading an
    # EU21 gdx through the H12 mapping silently mis-assigns every region.
    scen <- panelDataScenario(gdxFile = gdx, aggregate = TRUE,
                              gdxRegionMappingFile = gdxRegionMapping,
                              outputRegionMappingFile = "country")

    # The Run-Group may be a SUBDIRECTORY of resultsDir, or the artifacts may sit
    # directly in it - preparePFM.R copies them flat into <run>/pfm/ while still
    # recording a group NAME for the log. Resolve by finding the marker file rather
    # than trusting the name to be a path segment.
    marker <- "selected-models-psm.yml"
    gd <- file.path(resultsDir, group)
    if (!file.exists(file.path(gd, marker)) &&
          file.exists(file.path(resultsDir, marker))) {
      gd <- resultsDir
    }
    if (!file.exists(file.path(gd, marker))) {
      stop("no '", marker, "' under '", file.path(resultsDir, group), "' or '",
           resultsDir, "' (working directory: ", getwd(), ")")
    }
    sel <- yaml::read_yaml(file.path(gd, marker))
    mf <- jsonlite::read_json(file.path(gd, "manifest.json"))
    pfile <- paste0("panel_", mf$panel_hash, ".rds")
    pcand <- c(file.path(modelDir, "panels", pfile), file.path(gd, "panels", pfile),
               file.path(gd, pfile), file.path(modelDir, pfile))
    phit <- pcand[file.exists(pcand)][1]
    if (is.na(phit)) stop("panel '", pfile, "' not found in: ",
                          paste(pcand, collapse = ", "))
    panel <- readRDS(phit)
    fr <- readRDS(file.path(gd, "frontier.rds"))
    if (is.null(lambda)) {
      tv <- readRDS(file.path(gd, "temporal-validation.rds"))
      lambda <- vapply(c("Bulk", "Diffuse"),
                       function(s) tv$bySector[[s]]$ecm$metrics$adjustmentSpeed,
                       numeric(1))
    }
    norm <- function(s) {
      for (f in c("actorPowerDrivers", "actorPowerIndex", "instQualityDrivers",
                  "controlDrivers")) {
        if (!is.null(s[[f]])) s[[f]] <- unlist(s[[f]])
      }
      s$panelTransform <- s$panelTransform %||% "levels"
      s
    }

    # The band-rule assignments (static, computed offline by computeDonorAssignment).
    # WITHOUT these the aggregation falls back to excluding uncovered countries and
    # handing their regions phi = 1 - i.e. it rewards absence of data with maximal
    # assumed political capability, the exact defect the band rule removes. A coupled
    # run must not silently disagree with the offline artifacts, so this is an ERROR,
    # not a warning.
    asg <- stats::setNames(lapply(c("Bulk", "Diffuse"), function(sec) {
      f <- file.path(gd, paste0("donor-assignment-band-", sec, ".rds"))
      if (!file.exists(f)) {
        stop("iterativePFM: missing band assignment '", f, "'. Run ",
             "analysis/psm-donor-assumptions.R first - without it uncovered regions ",
             "would revert to phi = 1.")
      }
      readRDS(f)
    }), c("Bulk", "Diffuse"))

    # Aggregation weights. "finalEnergy" resolves country-level fe_total, the closest
    # available correlate of the emissions a carbon price acts on. NULL would fall back
    # to EQUAL country weights, which over-represents small emitters - so the default
    # is the energy weight, not the silent one.
    wts <- if (identical(weights, "finalEnergy")) {
      tryCatch(psmCouplingWeights(year = weightYear, scenario = weightScenario,
                                  verbose = verbose), error = function(e) {
        warning("iterativePFM: final-energy weights unavailable (",
                conditionMessage(e), "); falling back to EQUAL country weights, ",
                "which over-represent small emitters.", call. = FALSE)
        NULL
      })
    } else weights

    # 2. Recompute the feasible paths and the ambition gaps along THIS iteration's
    #    energy system - the Policy -> Politics feedback.
    feas <- do.call(rbind, lapply(c("Bulk", "Diffuse"), function(sec) {
      cfg <- norm(Filter(function(x)
        identical(x$model_type, paste0("PolicyStringency: ", sec)), sel)[[1]])
      ct <- fr$bySector[[sec]]$coefTable
      fb <- stats::setNames(ct$estimate, ct$term)
      fb <- fb[!names(fb) %in% c("sigmaSq", "gamma")]
      # The ECM fit depends only on the historical panel and the spec - neither
      # changes across the coupling loop - so it is fitted ONCE and cached in the run
      # folder. Every later iteration reads it back. This was the dominant cost:
      # two refits per call, ~20 calls per run, all producing the identical object.
      fitCache <- file.path(resultsDir, paste0("ecm-fit-", sec, ".rds"))
      ecm <- if (file.exists(fitCache)) {
        tryCatch(readRDS(fitCache), error = function(e) NULL)
      } else NULL
      if (is.null(ecm)) {
        t1 <- Sys.time()
        ecm <- estimatePolicyStringencyModel(
          data = panel, sector = sec, estimator = "satP", form = "ecm",
          actorPowerDrivers = unlist(cfg$actorPowerDrivers),
          actorPowerIndex = unlist(cfg$actorPowerIndex),
          instQualityDrivers = unlist(cfg$instQualityDrivers),
          controlDrivers = unlist(cfg$controlDrivers),
          regionMappingFixedEffects = cfg$regionMappingFixedEffects,
          logisticTimeTrend = isTRUE(cfg$logisticTimeTrend),
          interactRegionFE = isTRUE(cfg$interactRegionFE),
          useMundlak = isTRUE(cfg$useMundlak),
          gdpGovInteraction = isTRUE(cfg$gdpGovInteraction),
          apTransform = cfg$apTransform %||% "linear",
          modelDir = modelDir, updateIndex = FALSE, verbose = FALSE)
        saveRDS(ecm, fitCache)
        say(sprintf("%s ECM fitted and cached in %.0fs - later iterations reuse it",
                    sec, as.numeric(difftime(Sys.time(), t1, units = "secs"))))
      } else {
        say(sec, " ECM read from cache")
      }
      p <- projectFeasiblePath(cfg, sec, histData = panel, scenarioData = scen,
                               rule = "speed-limited", frontierBeta = fb, fit = ecm,
                               modelDir = modelDir, verbose = FALSE)
      a <- aggregateFeasibilityToRegions(p, mapping, weights = wts,
                                         assignment = asg[[sec]],
                                         theta = theta, nTiers = nTiers,
                                         gapMeasure = gapMeasure, phiRule = phiRule)
      a$sector <- sec
      a
    }))

    # 3. One share per region: the worse sector, the maximin discipline used
    #    throughout. phi is time-invariant by construction (tiers fixed at 2022).
    byReg <- split(feas$phi, as.character(feas$region))
    phi <- vapply(byReg, function(v) {
      v <- v[is.finite(v)]
      if (!length(v)) 1 else min(v)
    }, numeric(1))
    phi <- pmin(pmax(phi, 0), 1)
    if (!length(phi)) stop("no region produced a feasibility share")

    say(sprintf("phi over %d regions: median %.3f, min %.3f, uncoupled (phi=1) %d",
                length(phi), stats::median(phi), min(phi), sum(phi >= 1 - 1e-12)))

    # 4. Convergence. The loop is a fixed point in phi: REMIND's energy system moves
    #    the ambition gaps, which move phi, which moves the price, which moves the
    #    energy system. It has converged when a further PFM call stops changing phi.
    #    The delta is measured against the PREVIOUS call's phi, kept in a history
    #    file next to the gdx so the criterion survives a GAMS restart. A first call
    #    has nothing to compare against and is deliberately reported as NOT
    #    converged (Inf), so the loop can never stop on iteration one.
    histFile <- file.path(dirname(outputFile), "pfm-phi-history.rds")
    prev <- if (file.exists(histFile)) readRDS(histFile) else list()
    delta <- if (length(prev)) {
      last <- prev[[length(prev)]]$phi
      common <- intersect(names(phi), names(last))
      if (length(common)) max(abs(phi[common] - last[common])) else Inf
    } else Inf
    prev[[length(prev) + 1L]] <- list(iteration = length(prev) + 1L,
                                      time = Sys.time(), phi = phi, delta = delta)
    saveRDS(prev, histFile)
    say(sprintf("phi delta vs previous call: %s (tolerance is enforced in GAMS)",
                if (is.finite(delta)) sprintf("%.5f", delta) else "first call"))

    # 5. PFM -> REMIND. magclass writes the gdx REMIND's Execute_Loadpoint reads;
    #    the symbol names must match the presolve statements.
    out <- .psmGdxParam(magclass::new.magpie(names(phi), NULL, "p45_regiDiff_phi",
                                             fill = as.numeric(phi)),
                        "p45_regiDiff_phi")
    # Written over the SAME regions as phi, constant, because the GAMS side loads it
    # into a parameter indexed on regi - a GLO-only symbol would sum to nothing there
    # and read as a delta of 0, i.e. false convergence on the first call.
    # A large finite number rather than Inf: GAMS has no Inf on load, and any value
    # above a sane tolerance keeps the loop running, which is the safe direction.
    dOut <- .psmGdxParam(magclass::new.magpie(names(phi), NULL, "p45_pfmDelta",
                                              fill = if (is.finite(delta)) delta else 1e6),
                         "p45_pfmDelta")
    # Bind mode 2 needs the ABSOLUTE politically feasible price per region-period.
    # Exported unconditionally: it costs one extra symbol, and a mode-2 run that
    # silently found no bound would cap prices at zero.
    syms <- list(p45_regiDiff_phi = out, p45_pfmDelta = dOut)
    # priceOptimal is THIS iteration's cost-optimal path, read from the same gdx we
    # were handed. priceReference is the current-policies path and cannot come from
    # here - it is a different scenario - so it is an argument. Without it there is
    # no bound, and under bind mode 2 that would cap every price at zero, so mode 2
    # refuses to continue rather than producing a silently wrong run.
    bnd <- NULL
    if (!is.null(refGdx)) {
      bnd <- tryCatch({
        TCO2 <- 1000 / (44 / 12)
        pO <- gdx::readGDX(gdx, "pm_taxCO2eq") * TCO2
        pR <- gdx::readGDX(refGdx, "pm_taxCO2eq") * TCO2
        exportFeasibilityBound(feas, priceOptimal = pO, priceReference = pR,
                               lambda = lambda, sectorRule = "min", file = NULL)
      }, error = function(e) { say("price bound failed: ", conditionMessage(e)); NULL })
    }
    # --- bind mode 3: the mild-progression price path (Elmar variant) ------------
    # Generated bottom-up from the political gap rather than constraining the
    # cost-optimal path, so it needs a SEED: the observed current-policy price. That
    # comes from the reference gdx, exactly as P_ref does for mode 2.
    if (identical(bindMode, 3L)) {
      if (is.null(refGdx) || !nzchar(refGdx) || !file.exists(refGdx)) {
        stop("iterativePFM: bind mode 3 (mild progression) seeds the price path from ",
             "the current-policy run, but no reference gdx was found (refGdx = ",
             refGdx %||% "NULL", "). Refusing to continue - an unseeded path would ",
             "start every region at zero.")
      }
      TCO2 <- 1000 / (44 / 12)
      pR <- gdx::readGDX(refGdx, "pm_taxCO2eq") * TCO2
      seedYr <- suppressWarnings(min(magclass::getYears(pR, as.integer = TRUE)[
        magclass::getYears(pR, as.integer = TRUE) >= 2025]))
      seed <- stats::setNames(as.numeric(pR[, seedYr, ]),
                              magclass::getItems(pR, dim = 1))
      seed <- seed[is.finite(seed)]
      # One stringency path per region: the WORSE sector, the same maximin discipline
      # phi uses, so mode 3 is constrained by the same sector that sets the share.
      key <- paste(feas$region, feas$year)
      ord <- order(key, feas$feasibleIndex)
      one <- feas[ord, ][!duplicated(key[ord]), , drop = FALSE]
      mp <- projectMildProgressionPrice(
        one[, c("region", "year", "feasibleIndex", "ceilingIndex")],
        priceSeed = seed, lambda = mean(lambda, na.rm = TRUE), seedYear = seedYr)
      cs <- attr(mp, "cappedShare")
      if (is.finite(cs) && cs > 0.25) {
        warning("iterativePFM: the mild-progression growth cap bound ",
                round(100 * cs), "% of steps - the path is being driven by maxGrowth, ",
                "not by the political dynamics. Do not quote it as a result.",
                call. = FALSE)
      }
      say(sprintf("mild progression: seed %d, %d regions, capped share %.2f",
                  seedYr, length(unique(mp$region)), cs))
      yrsM <- sort(unique(mp$year))
      mpg <- magclass::new.magpie(sort(unique(mp$region)), yrsM, "p45_pfmMPPrice",
                                  fill = 0)
      mpg[cbind(as.character(mp$region), as.character(mp$year))] <- mp$price
      syms$p45_pfmMPPrice <- .psmGdxParam(mpg, "p45_pfmMPPrice")
    }

    if (identical(bindMode, 2L) && is.null(bnd)) {
      stop("iterativePFM: bind mode 2 caps the ABSOLUTE price, but no feasibility ",
           "bound could be built (refGdx = ", refGdx %||% "NULL", "). Refusing to ",
           "continue - a missing bound would cap every price at zero.")
    }
    if (!is.null(bnd) && all(c("region", "year", "priceBound") %in% names(bnd))) {
      yrs <- sort(unique(bnd$year))
      pb <- magclass::new.magpie(sort(unique(bnd$region)), yrs, "p45_pfmPriceBound",
                                 fill = 0)
      pb[cbind(as.character(bnd$region), as.character(bnd$year))] <- bnd$priceBound
      pb <- .psmGdxParam(pb, "p45_pfmPriceBound")
      syms$p45_pfmPriceBound <- pb
      say(sprintf("price bound exported for %d regions x %d periods",
                  length(unique(bnd$region)), length(yrs)))
    } else {
      say("NOTE: no price bound exported - bind mode 2 would have nothing to cap on")
    }
    gdx::writeGDX(syms, outputFile)
    say("wrote ", outputFile, " in ",
        round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1), "s")
    TRUE
  }, error = function(e) {
    # message(), NOT warning(): R DEFERS warnings in batch mode and prints only
    # "There were N warnings", so the one diagnostic that matters is the one the log
    # hides. This text is why the coupling produced no gdx - it must always be visible.
    msg <- conditionMessage(e)
    message("[iterativePFM] **** COUPLING STEP FAILED ****")
    message("[iterativePFM] reason: ", msg)
    message("[iterativePFM] no gdx written - REMIND keeps the previous phi. ",
            "Execute_Loadpoint will report a missing file; that is the symptom, ",
            "not the cause.")
    warning("iterativePFM: coupling step FAILED (", msg, ")", call. = FALSE)
    FALSE
  })
  invisible(ok)
}
# nolint end

#' Tag a magpie so gdx::writeGDX can write it
#'
#' \code{gdx::writeGDX} reads its symbol metadata from \code{attr(x, "gdxdata")},
#' which is populated by \code{readGDX} but is \strong{absent on a freshly
#' constructed magpie}. Without it the writer dereferences \code{out$type} on a NULL
#' and dies with "argument is of length zero" - an error that names nothing about the
#' real cause. Every symbol built here must be tagged before writing.
#'
#' @param x A magpie object.
#' @param name GAMS symbol name; must match the name the GAMS side loads.
#' @return \code{x} with the \code{gdxdata} attribute set.
#' @keywords internal
#' @author Renato Rodrigues
.psmGdxParam <- function(x, name) {
  magclass::getSets(x)[1] <- "all_regi"
  attr(x, "gdxdata") <- list(name = name, type = "parameter")
  x
}
