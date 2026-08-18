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
                         tierYear = getOption("pfm.couplingTierYear", NULL),
                         verbose = TRUE) {
  say <- function(...) if (isTRUE(verbose)) message("[iterativePFM] ", ...)
  rtIteration <- NULL

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
      if (!is.null(rt$iteration)) rtIteration <- rt$iteration
      # The tier year decides WHERE the ambition gap is read, and that single choice
      # decides whether this is a feedback loop at all. REMIND fixes its solution before
      # cm_startyear, so a tier year inside that window makes phi a constant: it came
      # back bit-identical on every call of the 2026-08-13 batch, delta exactly 0, and
      # the energy system could not move it by construction. Place it at the first
      # projection period STRICTLY after the frozen window unless told otherwise.
      if (is.null(tierYear) && !is.null(rt$startYear)) {
        sy <- suppressWarnings(as.numeric(rt$startYear))
        if (is.finite(sy)) {
          tierYear <- sy + 5
          say("tier year derived from cm_startyear ", sy, " -> ", tierYear,
              " (the first period REMIND is free to change)")
        }
      }
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
    # available correlate of the emissions a carbon price acts on.
    #
    # Both branches are ASSERTED (psmAssertSizeWeights), because both silent failures in this
    # family look like a completed run: the old fallback to equal weights on a warning, and the
    # normalised-index proxy that broke the offline bound on 2026-08-18. Inside a multi-hour
    # coupled batch a warning scrolls past unread and 26 finished gdxs carry uniform country
    # weights. Failing at the first PFM call is far cheaper. `weights = NULL` remains the
    # explicit, deliberate opt-out into equal weights and is left alone.
    wts <- if (identical(weights, "finalEnergy")) {
      w <- tryCatch(psmCouplingWeights(year = weightYear, scenario = weightScenario,
                                       verbose = verbose), error = function(e) {
        stop("iterativePFM: final-energy weights are unavailable (", conditionMessage(e),
             "). This does NOT fall back to equal weights - that silently over-represents ",
             "small emitters across every multi-country region. Fix the madrat cache ",
             "(calcFE, calcPE, calcEmber, calcGDP) or pass weights = NULL to accept equal ",
             "weights deliberately.", call. = FALSE)
      })
      psmAssertSizeWeights(w, "iterativePFM")
    } else if (is.null(weights)) {
      say("weights = NULL — countries aggregate EQUALLY. Deliberate opt-out; small emitters ",
          "are over-represented in every multi-country region.")
      NULL
    } else psmAssertSizeWeights(weights, "iterativePFM (supplied weights)")

    # 2. Recompute the feasible paths and the ambition gaps along THIS iteration's
    #    energy system - the Policy -> Politics feedback.
    feas <- do.call(rbind, lapply(c("Bulk", "Diffuse"), function(sec) {
      cfg <- norm(Filter(function(x)
        identical(x$model_type, paste0("PolicyStringency: ", sec)), sel)[[1]])
      ct <- fr$bySector[[sec]]$coefTable
      fb <- stats::setNames(ct$estimate, ct$term)
      fb <- fb[!names(fb) %in% c("sigmaSq", "gamma")]
      # The ECM fit depends only on the historical panel and the spec - neither changes
      # across the coupling loop - so it is fitted ONCE and cached. Every later iteration
      # reads it back. This was the dominant cost: two refits per call, ~20 calls per run,
      # all producing the identical object.
      #
      # CONTENT-ADDRESSED, not named by sector alone (fixed 2026-08-17). In a coupled run
      # resultsDir is "pfm", RELATIVE to the REMIND run folder (preparePFM.R), so the cache
      # is per-run and a FRESH run folder always starts without one - preparePFM.R stages
      # six named artifacts and the panel, never this. The exposure was a REUSED or
      # restarted run folder: the staged artifacts are overwritten, a stale ecm-fit-<sec>.rds
      # is not, and keyed on the sector alone nothing could notice that the spec or the panel
      # had moved under it - PITFALLS.md 14, on the coupling's hot path. The key covers the
      # spec fields that reach the estimator plus a hash of the panel, so a mismatch refits
      # instead of being silently wrong. Offline callers passing a shared resultsDir get the
      # same protection, which is where a shared cache would actually bite.
      fitKey <- substr(digest::digest(
        list("ecm", sec, .psmSpecArgs(cfg), digest::digest(panel, algo = "sha256")),
        algo = "sha256"), 1, 16)
      fitCache <- file.path(resultsDir, paste0("ecm-fit-", sec, "-", fitKey, ".rds"))
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
        say(sprintf("%s ECM fitted and cached in %.0fs [key %s] - later iterations reuse it",
                    sec, as.numeric(difftime(Sys.time(), t1, units = "secs")), fitKey))
      } else {
        say(sec, " ECM read from cache [key ", fitKey, "]")
      }
      p <- projectFeasiblePath(cfg, sec, histData = panel, scenarioData = scen,
                               rule = "speed-limited", frontierBeta = fb, fit = ecm,
                               modelDir = modelDir, verbose = FALSE)
      a <- aggregateFeasibilityToRegions(p, mapping, weights = wts,
                                         assignment = asg[[sec]],
                                         theta = theta, nTiers = nTiers,
                                         tierYear = tierYear,
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

    # 3b. Each sector's share on its own, for the per-market markups (ADR 0042).
    #     REMIND prices ETS and ES separately through pm_taxemiMkt; the map is in
    #     .psmSectorMarkets(). Delivering min() alone throws away whichever sector is
    #     NOT binding, and min() of two noisy estimates is biased low - in the direction
    #     that INFLATES the paper's headline.
    #
    #     SYMMETRIC since 2026-08-17. The first version carried Bulk only, which capped
    #     the demand side at the Bulk price wherever BULK was the worse sector (14 of 48
    #     countries on the deployed frontier) - reintroducing the same information loss
    #     on the other sector. Every sector now carries its own share, and floor +
    #     markup reproduces that sector's own price exactly.
    #
    #     Exported unconditionally: GAMS decides whether to use them
    #     (cm_pfmSectorMarkup), and symbols that are present but ignored cost nothing.
    phiSector <- stats::setNames(lapply(names(.psmSectorMarkets()), function(sec) {
      v <- phi
      if ("sector" %in% names(feas)) {
        s <- feas[as.character(feas$sector) == sec, , drop = FALSE]
        if (nrow(s)) {
          w <- vapply(split(s$phi, as.character(s$region)), function(x) {
            x <- x[is.finite(x)]
            if (!length(x)) NA_real_ else min(x)
          }, numeric(1))
          w <- pmin(pmax(w, 0), 1)
          common <- intersect(names(v), names(w))
          # Never below the floor: the markup is max(sector - floor, 0) on the GAMS side,
          # so a share under the floor would just be silently clipped to zero there.
          v[common] <- pmax(w[common], phi[common])
        }
      }
      v
    }), names(.psmSectorMarkets()))
    for (sec in names(phiSector)) {
      say(sprintf("phi(%s): median %.3f, above the floor in %d of %d regions",
                  sec, stats::median(phiSector[[sec]]),
                  sum(phiSector[[sec]] > phi + 1e-12), length(phi)))
    }

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

    # 5. PFM -> REMIND. The symbol names AND the index structure must match the
    #    declarations in 45_carbonprice/functionalForm/declarations.gms exactly:
    #      p45_regiDiff_phi(all_regi)        - ONE dimension
    #      p45_pfmDelta_aux(all_regi)        - ONE dimension
    #      p45_pfmPriceBound(ttot,all_regi)  - TWO, and ttot comes FIRST
    #    See .psmCouplingSym1d() for why this cannot go through magclass.
    out <- .psmCouplingSym1d("p45_regiDiff_phi", phi)
    # Written over the SAME regions as phi, constant, because the GAMS side loads it
    # into a parameter indexed on regi - a GLO-only symbol would sum to nothing there
    # and read as a delta of 0, i.e. false convergence on the first call.
    # A large finite number rather than Inf: GAMS has no Inf on load, and any value
    # above a sane tolerance keeps the loop running, which is the safe direction.
    dOut <- .psmCouplingSym1d("p45_pfmDelta",
                              stats::setNames(rep(if (is.finite(delta)) delta else 1e6,
                                                  length(phi)), names(phi)))
    # Freshness stamp. GAMS cannot otherwise tell a gdx written by THIS call from one
    # left over by a previous one: a failed call writes nothing, Execute_Loadpoint keeps
    # the old values, and a stale small delta would read as convergence. Echoing the
    # iteration GAMS asked for lets the model verify the answer is its own.
    itSeen <- suppressWarnings(as.numeric(if (!is.null(rtIteration)) rtIteration else NA))
    if (!is.finite(itSeen)) itSeen <- -1
    iOut <- .psmCouplingSym1d("p45_pfmIterSeen",
                              stats::setNames(rep(itSeen, length(phi)), names(phi)))
    # Bind mode 2 needs the ABSOLUTE politically feasible price per region-period.
    # Exported unconditionally: it costs one extra symbol, and a mode-2 run that
    # silently found no bound would cap prices at zero.
    syms <- list(out, dOut, iOut)
    # priceOptimal is the COST-OPTIMAL path the political layer discounts. It must be
    # a path the cap cannot touch.
    #
    # It used to be read straight from the run's own pm_taxCO2eq, and that made bind
    # mode 2 a DOWNWARD RATCHET: under mode 2 pm_taxCO2eq has already been capped by
    # the previous iteration's bound, so each call computed
    #     bound(i+1) = P_ref + phi * (bound(i) - P_ref)
    # - a contraction that multiplies the distance above P_ref by phi every call. At
    # phi = 0.21 the gap fell to 21%, then 4%, then 0.9%: the bound collapsed onto the
    # current-policy price within three or four calls whatever the politics said. In
    # the 2026-08-16 batch every non-EU 2050 bound had landed within a whisker of its
    # NPi reference (USA 0 -> $0.06, SSA 1.34 -> 1.64, REF 4.05 -> 4.11), and the $0.06
    # US price was the ratchet's fixed point, not a political finding.
    #
    # p45_taxCO2eq_anchor is the uncapped global anchor - the cost-optimal price path
    # the module builds every regional price from, and exactly the object intended. It
    # is a ttot-only symbol, so it is broadcast across the regions of P_ref.
    #
    # priceReference is the current-policies path and cannot come from here - it is a
    # different scenario - so it is an argument. Without it there is no bound, and
    # under bind mode 2 that would cap every price at zero, so mode 2 refuses to
    # continue rather than producing a silently wrong run.
    bnd <- NULL
    bndSector <- NULL
    if (!is.null(refGdx)) {
      prices <- tryCatch({
        TCO2 <- 1000 / (44 / 12)
        pR <- gdx::readGDX(refGdx, "pm_taxCO2eq") * TCO2
        list(pR = pR, pO = .psmCouplingOptimalPath(gdx, pR, TCO2, say))
      }, error = function(e) { say("price paths failed: ", conditionMessage(e)); NULL })

      if (!is.null(prices)) {
        # The economy-wide floor: the worse sector, as before.
        bnd <- tryCatch(
          exportFeasibilityBound(feas, priceOptimal = prices$pO,
                                 priceReference = prices$pR, lambda = lambda,
                                 sectorRule = "min", file = NULL),
          error = function(e) { say("price bound failed: ", conditionMessage(e)); NULL })
        # One bound per sector, for the per-market markups (ADR 0042). Same inputs and
        # the same speed-limit machinery - only the sector reconciliation differs - so
        # each is directly comparable to the floor and their difference IS that market's
        # markup. Failure is not fatal for a sector: without it GAMS falls back to the
        # floor on that market, which is the pre-ADR-0042 behaviour.
        bndSector <- Filter(Negate(is.null), stats::setNames(
          lapply(names(.psmSectorMarkets()), function(sec) {
            tryCatch(
              exportFeasibilityBound(feas, priceOptimal = prices$pO,
                                     priceReference = prices$pR, lambda = lambda,
                                     sectorRule = sec, file = NULL),
              error = function(e) {
                say(sec, " bound failed: ", conditionMessage(e)); NULL })
          }), names(.psmSectorMarkets())))
        if (!length(bndSector)) bndSector <- NULL
      }
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
      syms[[length(syms) + 1L]] <- .psmCouplingSym2d("p45_pfmMPPrice", mp, "price")

      # One path per sector, for the per-market markups (ADR 0042). Same seed and the
      # same recursion; only the sector selected differs. lambda is that sector's own
      # speed limit where one was supplied per sector - Bulk moves faster (0.1023 vs
      # 0.0770/yr) - and using the pooled mean would understate exactly the headroom the
      # markup is meant to express.
      if ("sector" %in% names(feas)) {
        mpSector <- Filter(Negate(is.null), stats::setNames(
          lapply(names(.psmSectorMarkets()), function(sec) {
            oneS <- feas[as.character(feas$sector) == sec, , drop = FALSE]
            if (!nrow(oneS)) return(NULL)
            lamS <- if (!is.null(names(lambda)) && sec %in% names(lambda)) {
              unname(lambda[[sec]])
            } else mean(lambda, na.rm = TRUE)
            out <- tryCatch(projectMildProgressionPrice(
              oneS[, c("region", "year", "feasibleIndex", "ceilingIndex")],
              priceSeed = seed, lambda = lamS, seedYear = seedYr),
              error = function(e) {
                say(sec, " mild path failed: ", conditionMessage(e)); NULL })
            if (!is.null(out)) {
              say(sprintf("%s mild path built (lambda %.4f); median 2050 markup: %s",
                          sec, lamS, .psmFmtMarkup(mp, out, valueCol = "price")))
            }
            out
          }), names(.psmSectorMarkets())))
        if (length(mpSector)) {
          syms[[length(syms) + 1L]] <-
            .psmCouplingSymMkt2d("p45_pfmMPPriceMkt", mpSector, "price")
        }
      }
    }

    if (identical(bindMode, 2L) && is.null(bnd)) {
      stop("iterativePFM: bind mode 2 caps the ABSOLUTE price, but no feasibility ",
           "bound could be built (refGdx = ", refGdx %||% "NULL", "). Refusing to ",
           "continue - a missing bound would cap every price at zero.")
    }
    if (!is.null(bnd) && all(c("region", "year", "priceBound") %in% names(bnd))) {
      syms[[length(syms) + 1L]] <-
        .psmCouplingSym2d("p45_pfmPriceBound", bnd, "priceBound")
      say(sprintf("price bound exported for %d regions x %d periods",
                  length(unique(bnd$region)), length(unique(bnd$year))))
    } else {
      say("NOTE: no price bound exported - bind mode 2 would have nothing to cap on")
    }

    # --- the per-market companions (ADR 0042) ------------------------------------
    # Indexed over all_emiMkt rather than one parameter per market. The ADR originally
    # rejected the market dimension because defect 4 was a rank/order failure and a new
    # rank is that trap one dimension up "for no gain" - but the symmetric markup needs
    # BOTH sectors delivered, so the alternative is eight flat parameters against four
    # indexed ones and the gain is now real. The trap is answered by the rank/domain
    # assertions in .psmVerifyCouplingGdx() and by test-gdxRoundTrip.R.
    #
    # GAMS decides whether to use these (cm_pfmSectorMarkup); with the switch off they
    # are inert, so exporting them unconditionally cannot change an existing run.
    syms[[length(syms) + 1L]] <- .psmCouplingSymMkt1d("p45_pfmPhiMkt", phiSector)
    # Each sector's own closure rate. Modes 2 and 3 carry it inside the price paths they
    # receive from here; mode 1 rebuilds its path in GAMS from phi and a rate, and
    # without this symbol it falls back to p45_regiDiff_lambda - which, having come
    # through sectorRule = "min", is the SLOWER sector's speed. Bulk 0.1023/yr vs
    # Diffuse 0.0770/yr, so the faster market would close on the anchor about a third
    # too slowly. Broadcast over the same regions as phi: lambda is estimated per
    # SECTOR, not per region, exactly as exportFeasibilityRegiDiff() broadcasts the
    # economy-wide rate. A zero is GAMS's "not supplied" default there, so a non-finite
    # or absent sector speed is written as 0 and GAMS keeps the floor rate.
    lamSector <- stats::setNames(lapply(names(phiSector), function(sec) {
      l <- if (!is.null(names(lambda)) && sec %in% names(lambda)) {
        unname(lambda[[sec]])
      } else suppressWarnings(mean(lambda, na.rm = TRUE))
      if (!is.finite(l) || l < 0) l <- 0
      stats::setNames(rep(l, length(phiSector[[sec]])), names(phiSector[[sec]]))
    }), names(phiSector))
    syms[[length(syms) + 1L]] <- .psmCouplingSymMkt1d("p45_pfmLambdaMkt", lamSector)
    say(sprintf("lambda per sector exported: %s",
                paste(sprintf("%s %.4f", names(lamSector),
                              vapply(lamSector, function(x) x[[1]], numeric(1))),
                      collapse = " | ")))
    if (!is.null(bndSector) && length(bndSector)) {
      ok <- vapply(bndSector, function(d)
        all(c("region", "year", "priceBound") %in% names(d)), logical(1))
      if (any(ok)) {
        syms[[length(syms) + 1L]] <-
          .psmCouplingSymMkt2d("p45_pfmPriceBoundMkt", bndSector[ok], "priceBound")
        for (sec in names(bndSector)[ok]) {
          say(sprintf("%s price bound exported; median 2050 markup over the floor: %s",
                      sec, .psmFmtMarkup(bnd, bndSector[[sec]])))
        }
      }
    }
    .psmWriteCouplingGdx(outputFile, syms)
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

#' Build the coupling gdx exactly as REMIND declares it
#'
#' The symbols REMIND loads are declared in
#' \code{45_carbonprice/functionalForm/declarations.gms} as
#' \code{p45_regiDiff_phi(all_regi)}, \code{p45_pfmDelta_aux(all_regi)},
#' \code{p45_pfmPriceBound(ttot,all_regi)} and \code{p45_pfmMPPrice(ttot,all_regi)}.
#' Both the RANK and the index ORDER are part of that contract, and both have been got
#' wrong here before:
#' \itemize{
#'   \item Wrong rank - \code{Execute_Loadpoint} reports
#'     \code{**** GDX ERROR - Dimensions do not match}, loads NOTHING, leaves phi at
#'     its previous value (1, i.e. uncoupled) and defers the execution error to the
#'     next solve, hours later, as an unrelated infeasibility.
#'   \item Wrong order - a region-first symbol has the correct rank, raises
#'     \strong{no error at all}, and loads as \code{( ALL 0.000 )}. Worse than the
#'     rank mismatch, because nothing anywhere reports it.
#' }
#'
#' \strong{Written with GAMS Transfer}, which is the reason the contract is now
#' enforceable rather than merely documented: the domain SETS go into the gdx as real
#' GAMS sets, so records that do not belong to the declared domain are refused at write
#' time with "Domain violation". The predecessor (\code{gdxrrw::wgdx.lst}) has no domain
#' concept and writes whatever index matrix it is handed, which is exactly how the
#' order defect reached a cluster run. \code{magclass} cannot express these symbols at
#' all: a magpie is inherently three-dimensional (region, year, data), so it produced
#' rank 2 for phi and rank 3 for the bound.
#'
#' Year labels are bare (\code{"2030"}), matching the \code{ttot} set elements -
#' \strong{not} magclass's \code{"y2030"}, which would drop every record on load.
#'
#' @param name GAMS symbol name; must match what presolve.gms loads.
#' @param v Named numeric vector over regions (1-dimensional symbols).
#' @param df Long data.frame with \code{region}, \code{year} and a value column.
#' @param valueCol Name of the value column in \code{df}.
#' @param regionCol,yearCol Names of the index columns in \code{df}.
#' @param file Destination gdx.
#' @param syms List of symbol descriptions built by these helpers.
#' @param verbose Logical; report the symbols written.
#' @return The symbol description; \code{.psmWriteCouplingGdx} returns \code{file}.
#' Median 2050 markup of the ETS path over the economy-wide floor, for the log.
#'
#' Purely diagnostic: it is the one number that says at a glance whether the sector
#' split is doing anything this iteration. A markup of zero everywhere means Bulk is
#' never the easier sector, which would make ADR 0042 inert and is worth seeing.
#'
#' @param floorDF,etsDF Long data.frames with \code{region, year} and the value column.
#' @param valueCol Name of the value column.
#' @param year Year to report.
#' @return A formatted string.
#' @keywords internal
#' @author Renato Rodrigues
.psmFmtMarkup <- function(floorDF, etsDF, valueCol = "priceBound", year = 2050) {
  if (is.null(floorDF) || is.null(etsDF)) return("n/a")
  k <- function(d) paste(d$region, d$year)
  a <- stats::setNames(floorDF[[valueCol]], k(floorDF))
  b <- stats::setNames(etsDF[[valueCol]], k(etsDF))
  sel <- grep(paste0(" ", year, "$"), names(a), value = TRUE)
  sel <- intersect(sel, names(b))
  if (!length(sel)) return("n/a")
  d <- pmax(b[sel] - a[sel], 0)
  sprintf("$%.1f (max $%.1f over %d regions)", stats::median(d), max(d), length(d))
}

#' The cost-optimal price path for the mode-2 bound, taken from a symbol the cap
#' cannot touch.
#'
#' \code{p45_taxCO2eq_anchor(ttot)} is the uncapped global anchor every regional price
#' is built from, so it is the cost-optimal counterfactual the political share is meant
#' to discount. It carries no region dimension, so it is broadcast across the regions of
#' the reference path. Falling back to the run's own \code{pm_taxCO2eq} restores the
#' ratchet documented at the call site, so the fallback WARNS rather than passing
#' quietly - a run that takes it is not quotable.
#'
#' @param gdx The run's own gdx.
#' @param pR The reference price path (magpie), used for the region list.
#' @param TCO2 Unit conversion from T$/GtC to US$/tCO2.
#' @param say Progress reporter.
#' @return A long data.frame of \code{region, year, value} in US$/tCO2.
#' @keywords internal
#' @author Renato Rodrigues
.psmCouplingOptimalPath <- function(gdx, pR, TCO2, say = function(...) NULL) {
  regs <- magclass::getItems(pR, dim = 1)
  anc <- tryCatch(gdx::readGDX(gdx, "p45_taxCO2eq_anchor", react = "silent"),
                  error = function(e) NULL)
  av <- NULL
  if (!is.null(anc) && magclass::is.magpie(anc) && length(anc)) {
    yrs <- magclass::getYears(anc, as.integer = TRUE)
    # ttot-only symbols come back with a single (GLO) region; guard anyway so a
    # region-indexed anchor cannot silently recycle values onto the wrong years.
    v <- as.numeric(anc)
    if (length(v) == length(yrs)) av <- stats::setNames(v, yrs)
  }
  if (!is.null(av)) av <- av[is.finite(av)]
  if (!is.null(av) && length(av) && any(av > 0)) {
    say("bind mode 2: P_opt from p45_taxCO2eq_anchor (uncapped), ",
        length(av), " periods x ", length(regs), " regions")
    return(data.frame(
      region = rep(regs, times = length(av)),
      year   = rep(as.integer(names(av)), each = length(regs)),
      value  = rep(as.numeric(av) * TCO2, each = length(regs)),
      stringsAsFactors = FALSE))
  }
  warning("iterativePFM: p45_taxCO2eq_anchor is missing or all-zero, so P_opt falls ",
          "back to the run's own pm_taxCO2eq - which under bind mode 2 is ALREADY ",
          "CAPPED and makes the bound ratchet down onto the reference price. Do not ",
          "quote a price level from this run.", call. = FALSE)
  say("bind mode 2: WARNING - P_opt fell back to the capped pm_taxCO2eq")
  gdx::readGDX(gdx, "pm_taxCO2eq") * TCO2
}

#' @keywords internal
#' @author Renato Rodrigues
#' @rdname psmCouplingGdx
.psmCouplingSym1d <- function(name, v) {
  u <- names(v)
  if (is.null(u) || anyNA(u) || !all(nzchar(u))) {
    stop(".psmCouplingSym1d: '", name, "' needs a fully named vector of regions")
  }
  list(name = name, domain = "all_regi",
       records = data.frame(all_regi = u, value = unname(as.numeric(v)),
                            stringsAsFactors = FALSE))
}

#' @keywords internal
#' @rdname psmCouplingGdx
.psmCouplingSym2d <- function(name, df, valueCol,
                              regionCol = "region", yearCol = "year") {
  yrs <- suppressWarnings(as.integer(df[[yearCol]]))
  if (anyNA(yrs)) stop(".psmCouplingSym2d: non-numeric years in '", name, "'")
  # ttot FIRST, all_regi second - the declared order, and the column order the
  # gamstransfer domain is built from.
  list(name = name, domain = c("ttot", "all_regi"),
       records = data.frame(ttot = as.character(yrs),
                            all_regi = as.character(df[[regionCol]]),
                            value = as.numeric(df[[valueCol]]),
                            stringsAsFactors = FALSE))
}

#' The PFM sector to REMIND emission-market map (ADR 0042)
#'
#' The single place this mapping is written down. Bulk is the ETS sector (electricity +
#' industry); Diffuse covers effort sharing and everything not otherwise assigned.
#' \code{"other"} follows \code{"ES"} deliberately - it is REMIND's own convention
#' (\code{47_regipol/regiCarbonPrice/postsolve.gms:409} does \code{other = ES}) and it
#' matches the mapping ADR 0042 states, "ETS ~ Bulk, ES + other ~ Diffuse".
#'
#' @keywords internal
#' @rdname psmCouplingGdx
.psmSectorMarkets <- function() {
  list(Bulk = "ETS", Diffuse = c("ES", "other"))
}

#' Region x market (rank 2) and year x region x market (rank 3) symbols
#'
#' The market dimension is a real set rather than one parameter per market: the
#' symmetric markup needs BOTH sectors delivered, which would otherwise be eight flat
#' parameters. ADR 0042 originally rejected the extra rank because defect 4 was a
#' rank/order failure; the answer is the rank/domain assertion in
#' \code{.psmVerifyCouplingGdx()} plus \code{test-gdxRoundTrip.R}, not avoiding the rank.
#'
#' \code{bySector} is a named list of per-sector values, fanned out to that sector's
#' markets via \code{.psmSectorMarkets()}.
#'
#' @keywords internal
#' @rdname psmCouplingGdx
.psmCouplingSymMkt1d <- function(name, bySector) {
  map <- .psmSectorMarkets()
  rows <- do.call(rbind, lapply(names(bySector), function(sec) {
    v <- bySector[[sec]]
    u <- names(v)
    if (is.null(u) || anyNA(u) || !all(nzchar(u))) {
      stop(".psmCouplingSymMkt1d: '", name, "' sector '", sec,
           "' needs a fully named vector of regions")
    }
    mkts <- map[[sec]]
    if (is.null(mkts)) stop(".psmCouplingSymMkt1d: no market maps to sector '", sec, "'")
    do.call(rbind, lapply(mkts, function(mk) {
      data.frame(all_regi = u, all_emiMkt = mk, value = unname(as.numeric(v)),
                 stringsAsFactors = FALSE)
    }))
  }))
  # all_regi FIRST, all_emiMkt second - the declared order in declarations.gms.
  list(name = name, domain = c("all_regi", "all_emiMkt"), records = rows)
}

#' @keywords internal
#' @rdname psmCouplingGdx
.psmCouplingSymMkt2d <- function(name, bySector, valueCol,
                                 regionCol = "region", yearCol = "year") {
  map <- .psmSectorMarkets()
  rows <- do.call(rbind, lapply(names(bySector), function(sec) {
    df <- bySector[[sec]]
    yrs <- suppressWarnings(as.integer(df[[yearCol]]))
    if (anyNA(yrs)) {
      stop(".psmCouplingSymMkt2d: non-numeric years in '", name, "' sector '", sec, "'")
    }
    mkts <- map[[sec]]
    if (is.null(mkts)) stop(".psmCouplingSymMkt2d: no market maps to sector '", sec, "'")
    do.call(rbind, lapply(mkts, function(mk) {
      data.frame(ttot = as.character(yrs),
                 all_regi = as.character(df[[regionCol]]),
                 all_emiMkt = mk,
                 value = as.numeric(df[[valueCol]]),
                 stringsAsFactors = FALSE)
    }))
  }))
  # ttot, all_regi, all_emiMkt - the declared order, and the order pm_taxemiMkt uses.
  list(name = name, domain = c("ttot", "all_regi", "all_emiMkt"), records = rows)
}

#' @keywords internal
#' @rdname psmCouplingGdx
.psmCouplingUels <- function(name, v) {
  v <- unique(as.character(v))
  # ttot elements sort NUMERICALLY; "2100" < "255" as text.
  if (identical(name, "ttot")) as.character(sort(as.integer(v))) else sort(v)
}

#' @keywords internal
#' @rdname psmCouplingGdx
.psmWriteCouplingGdx <- function(file, syms, verbose = FALSE) {
  if (!requireNamespace("gamstransfer", quietly = TRUE)) {
    stop(".psmWriteCouplingGdx: the 'gamstransfer' package is required to write the ",
         "coupling gdx. It ships with GAMS (apifiles/R/gamstransfer) and is on CRAN.")
  }
  m <- gamstransfer::Container$new()
  # Domain sets first, as real GAMS sets - that is what makes the domain check possible.
  # Elements are the union over every symbol that uses them.
  uels <- list()
  for (s in syms) {
    for (d in s$domain) {
      uels[[d]] <- union(uels[[d]] %||% character(0), as.character(s$records[[d]]))
    }
  }
  sets <- stats::setNames(
    lapply(names(uels), function(d) m$addSet(d, records = .psmCouplingUels(d, uels[[d]]))),
    names(uels))
  for (s in syms) {
    m$addParameter(s$name, domain = unname(sets[s$domain]), records = s$records)
  }
  # Refuses rather than producing a gdx GAMS would silently read as zero.
  m$write(file)
  .psmVerifyCouplingGdx(file, syms)
  if (isTRUE(verbose)) {
    message("[iterativePFM] wrote ", length(syms), " symbols and verified their domains")
  }
  invisible(file)
}

#' Read the coupling gdx back and assert what GAMS will see
#'
#' Cheap (milliseconds) and worth it, because every defect this guards against is
#' SILENT in production: the run either dies hours later citing something unrelated, or
#' does not complain at all and reports an uncoupled world as a coupled one. Checking
#' the FILE rather than the objects that produced it is the point - it is the file GAMS
#' reads.
#'
#' @param file The gdx just written.
#' @param syms The symbol descriptions it was built from.
#' @return \code{TRUE} invisibly, or an error naming the symbol and what is wrong.
#' @keywords internal
#' @author Renato Rodrigues
#' @rdname psmCouplingGdx
.psmVerifyCouplingGdx <- function(file, syms) {
  back <- gamstransfer::Container$new(file)
  for (s in syms) {
    if (!length(back$getSymbols(s$name))) {
      stop(".psmVerifyCouplingGdx: '", s$name, "' is missing from ", file)
    }
    got <- back$getSymbols(s$name)[[1]]
    if (!identical(as.integer(got$dimension), length(s$domain))) {
      stop(".psmVerifyCouplingGdx: '", s$name, "' was written with rank ",
           got$dimension, " but REMIND declares rank ", length(s$domain),
           " - Execute_Loadpoint would load nothing.")
    }
    if (!identical(as.character(got$domainNames), s$domain)) {
      stop(".psmVerifyCouplingGdx: '", s$name, "' is indexed (",
           paste(got$domainNames, collapse = ", "), ") but REMIND declares (",
           paste(s$domain, collapse = ", "), "). GAMS would load it as all zeros ",
           "without reporting anything.")
    }
  }
  invisible(TRUE)
}
