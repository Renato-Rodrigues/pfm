# nolint start
#' Run a Political Feasibility Model sweep into its own Run-Group
#'
#' @description
#' The PFM counterpart of \code{\link{runSweep}} (ADR 0036). Reuses the shared
#' machinery end to end — the channel spec grid (\code{createChannelConfigs},
#' adapted by \code{\link{pfmSpecs}}), \code{\link{runFitGrid}} with the single
#' stage \code{"PolicyStringency"} (satP engine fits, Fit-Cache reuse), the
#' unchanged \code{\link{computeMaximinScore}} worse-sector ranking, and — when a
#' scenario panel is available — the bounded-index Projection Sanity gate
#' (\code{\link{computePolicyStringencySanity}} via the expanding-batch walker).
#' Writes the standard Run-Group artifacts into \code{<resultsDir>/<group>/}:
#' \code{sweep.rds}, \code{selected-models-pfm.yml}, and \code{manifest.json}.
#'
#' The PFM has \strong{one} stage (no adoption hurdle), its outcome axis headline
#' is the sectoral power & industry / buildings & transport mapping, and estimator adjudication never touches
#' selection — the sweep runs on the satP engine only ("PFM Estimator Suite" in
#' CONTEXT.md); the estimator-agreement exhibit is computed on the selected spec
#' afterwards (\code{\link{computeEstimatorAgreement}}).
#'
#' @param group Character. Run-Group name (e.g. \code{"psm-exhaustive"}). Required.
#' @param mode Character. \code{"exhaustive"} or \code{"guided"} spec grid.
#' @param resultsDir,modelDir,cachefolder,gdxFile As in \code{\link{runSweep}}.
#' @param panelData Optional pre-built historical panel. When NULL it is built
#'   with \code{panelDataHistorical(includePolicyStringency = TRUE)}.
#' @param scenarioData Optional pre-built scenario panel (enables the sanity gate).
#' @param specs Optional list of normalised specs (bypasses
#'   \code{createChannelConfigs}; used by tests and custom experiments). They are
#'   still passed through \code{\link{pfmSpecs}}.
#' @param y,outputRegionMappingFile,movingAverage,sectors,nCores,forceRefit,verbose
#'   As in \code{\link{runSweep}}.
#' @param indexMax Numeric. Index ceiling (CAPMF: 10).
#' @param selectFE Character vector or NULL. Region-FE constraint on the
#'   deliverable (model-name \code{fe:} tags), as in the price-model selection.
#' @param nearTieEps,feParsimonyWeight,dropIdleControls,softVifGate Maximin knobs,
#'   forwarded to \code{\link{computeMaximinScore}}. \code{softVifGate} and
#'   \code{inferenceTGate} are the within-band soft keys of ADR 0048: \code{pfmRun} takes them
#'   from \code{config.yml} \code{sweep:} (\code{Inf} / \code{0} when declared \code{"off"}),
#'   and the manifest records them (\code{sweepOptions}).
#' @param trendDominanceGate Numeric or \code{NULL}. Hard trend-dominance gate
#'   forwarded to \code{\link{computeMaximinScore}} (ADR 0033). \strong{Defaults to
#'   \code{0.9} for the PFM}, relaxed from the price model's \code{0.5}: the CAPMF
#'   policy-stringency index accumulates almost monotonically over time, so the linear
#'   \code{timeTrend} is a legitimate common-shock control rather than atheoretical
#'   extrapolation. The first real-data sweep (2026-07-06) had a median \code{trendShare}
#'   of 0.64, so a \code{0.5} gate rejected the trended-but-driver-informed majority; the
#'   \code{0.9} ceiling still culls near-pure-trend degenerates (trendShare > 0.9). The
#'   \emph{soft} within-tie low-trend preference is retained and \code{trendShare} is
#'   surfaced in the report, so a trend-heavy winner stays visible. \code{NULL} disables
#'   the hard gate entirely.
#' @param deltaR2Max Numeric. Fit-reliability gate forwarded to
#'   \code{\link{computeMaximinScore}} (default \code{1} — a mathematical validity
#'   bound on the incremental McFadden pseudo-R2; a spec exceeding it is degenerate).
#' @param inferenceTGate Numeric or \code{NULL}. Inference-fragility within-band
#'   preference (ADR 0037), forwarded to \code{\link{computeMaximinScore}}. Defaults
#'   to \code{2.33} (roughly p < .02) for the PFM: among theory-equivalent specs a
#'   p=.049 squeaker on a theory term loses the near-tie to a comfortable margin.
#'   Never a hard gate — significance is not an admission criterion. \code{NULL} or
#'   \code{0} disables; through \code{pfmRun} use \code{0}, since \code{modifyList} drops a
#'   \code{NULL} (ADR 0048).
#' @param rankBy,tierGate Tournament v2 (ADR 0039), forwarded to
#'   \code{\link{computeMaximinScore}}. PFM defaults: \code{rankBy = "worseDeltaR2"}
#'   (rank gate-passers by the worse sector's \eqn{\Delta R^2}(theory)) and
#'   \code{tierGate = "Green"} (the deployment must be Green in both sectors). Set
#'   \code{rankBy = "tierMean", tierGate = NULL} for the pre-v2 semantics.
#' @param documentTierGates Character vector. Tier gates for the documented
#'   selection variants (\code{\link{computeSelectionVariants}}: tier-gate winners +
#'   the Specification-Sharing Cost exhibit), written to
#'   \code{selection-variants.rds}. \code{NULL} skips the exhibit.
#' @param referenceScenarioData Optional pre-built REFERENCE scenario panel (the
#'   non-gating Policy Scenario, e.g. National Policies). When supplied together
#'   with \code{scenarioData}, the sanity walk applies the
#'   \strong{scenario-responsiveness gate} (ADR 0039): specs whose gating and
#'   reference projections are indistinguishable (median in-coverage |Δindex| below
#'   \code{minScenarioDelta} within \code{deltaWindow}) are severe-flagged
#'   \code{scenarioBlind} and deselected.
#' @param referenceGdxFile Optional path to the reference scenario's
#'   \code{fulldata.gdx}; used to build \code{referenceScenarioData} when that is
#'   \code{NULL}. \code{\link{pfmRun}} passes the registry's non-gating scenario
#'   (\code{\link{scenarioReferenceEntry}}); without it \code{scenarioBlind} is skipped,
#'   with a warning.
#' @param referenceGdxRegionMappingFile The reference gdx's own native mapping (PITFALLS
#'   §5). \code{NULL} (default) uses \code{gdxRegionMappingFile}.
#' @param apPcForms Character vector. Per-capita actor-power forms appended to the spec
#'   grid (design-notes/0001); forwarded to \code{\link{createChannelConfigs}}. Defaults
#'   to all three — \code{"splitAPpc"}, \code{"mixedAP"} and \code{"bothIncAP"} — so
#'   selection compares share against level head-to-head. \code{character(0)} restores
#'   the pre-2026-08-24 grid. Only consulted when \code{specs} is \code{NULL}.
#' @param gdxRegionMappingFile Character. The **gdx's own native resolution**, not a
#'   target (PITFALLS §5) — \code{"regionmappingH12.csv"} (default) for an H12 gdx,
#'   \code{"regionmapping_21_EU11.csv"} for an EU21 one. Forwarded to
#'   \code{\link{panelDataScenario}} for both the gating and reference panels. Getting
#'   it wrong makes the scenario panel fail and \strong{silently disables every gate
#'   that needs it} — the sanity walk, \code{scenarioBlind}, \code{extrapolationDominated}
#'   and \code{ceilingFallGate} — leaving maximin-only selection. Run-Group v1 was
#'   selected this way without anyone noticing.
#' @param gammaGate Numeric or \code{NA}. Severe-gate threshold on the frontier's
#'   \eqn{\gamma}. \strong{On by default at 0.999 since 2026-09-15 (ADR 0046).}
#'   \eqn{\gamma \to 1} means \eqn{\sigma_v^2 \to 0}: the variance decomposition
#'   attributes ALL composed error to slack and none to noise, so the *stochastic*
#'   frontier has degenerated into a deterministic one, the likelihood sits on the
#'   boundary of its parameter space, and \code{frontier::sfa} itself warns that
#'   statistical tests are no longer valid. 0.999 is placed in an observed gap: across
#'   the 19-specification survey of 2026-09-15 the healthy fits top out at 0.99840 and
#'   the degenerate ones start at 0.99991, with nothing in between
#'   (\code{../_archive/_wip/2026-10-01/docs/reference/spec-selection-2026-09-15/}). \code{NA} disables.
#' @param vcovGate Character. Covariance-check statuses (from \code{.pfmFrontierVcov})
#'   that raise a \strong{severe} \code{frontierVcov} flag in the sanity walk (design note
#'   0005 E13). Default \code{c("likelihood-mismatch", "flat")}: the statuses that leave
#'   untrustworthy standard errors in the deliverable, either because FRONTIER's matrix
#'   could not be checked or because the information matrix is not negative definite.
#'   \code{"corrupt"} is not gated, because the recomputed matrix replaces FRONTIER's.
#'   \code{"boundary"} is the \code{gammaGate}'s. The status is recorded for every
#'   evaluated spec either way. \code{character(0)} disables. Like \code{gammaGate}, it
#'   reads the frontier fit of the ceiling check, so it runs only while
#'   \code{ceilingFallGate} is set.
#' @param ceilingFallGate Numeric or \code{NA}. Severe-gate threshold on the
#'   median FRONTIER ceiling's end/start ratio across the scenario horizon: a
#'   spec whose ceiling falls below this fraction of its first projected value by
#'   the horizon end is rejected (ADR 0043). \strong{Default \code{0.90}}; set
#'   \code{NA} to restore pre-2026-08-23 selection. Costs one frontier fit per
#'   candidate per sector and only runs inside the sanity walk.
#'   \strong{Do not disable the \code{supportShareGate} or
#'   \code{minScenarioDelta} gates while this one is on} — the cheapest way to
#'   hold a ceiling flat is to stop responding to the scenario, so without them
#'   this gate would select inert specifications (measured: corr(ceiling ratio,
#'   cross-country ceiling SD) = −0.42).
#' @param supportShareGate Numeric. Severe-gate threshold on the mean
#'   \code{driverOutOfSupport} share within \code{deltaWindow} over in-coverage
#'   rows (ADR 0040; default 0.275 since ADR 0045). A spec whose projection in the evaluation
#'   window is mostly winsorized is scoring the extrapolation guard, not the
#'   model - the diagnosed cause of the ceiling-feedback sign artifact. Set
#'   \code{NA} to disable.
#' @param apTransforms,dropCompositeAP The actor-power axes of the grid, see
#'   \code{\link{pfmSpecs}}. Defaults reproduce every Run-Group up to v5; v6 sweeps all four
#'   forms and drops the composite Actor Power Index (design note 0005 D7; \code{config.yml}
#'   \code{sweep:}).
#' @param apExtrapolationGate,apExtrapolationSd,apExtrapolationWindow The actor-power
#'   extrapolation gate of the sanity walk (0005 D7): severe when more than
#'   \code{apExtrapolationGate} of the in-coverage country-years in \code{apExtrapolationWindow}
#'   have an actor-power driver more than \code{apExtrapolationSd} training SDs beyond its
#'   guard range, on the gating or the reference projection. Saturating columns are guarded at
#'   their physical domain and pass by construction. \code{Inf} (default) disables it, as up
#'   to v5.
#' @param interactionWeights Character. Panel variables (0-1) that each add an interaction-weighted
#'   twin of every spec with interactions, see \code{\link{pfmSpecs}} (design note 0007, O10). Empty
#'   (default): none, as up to v6.
#' @param fossilFloors The fossil lower-bound axis, see \code{\link{pfmSpecs}} (design note 0009 section
#'   7.1): training quantiles in (0, 1), and \code{"none"} to keep the unbounded specs. Empty (default):
#'   no bound, as up to v7.
#' @param slackDrivers Panel variables that set the size of the frontier's slack, by sector (a list named by
#'   sector, e.g. \code{list(Diffuse = "Fossil Rents")}; design note 0009 section 8.2): carried into every spec
#'   (\code{\link{pfmSpecs}}), so every frontier fit of the sanity walk and of the frontier step uses them. They
#'   do not enter the ceiling, so the satP selection fits are unchanged. Empty (default): none, as up to v7.
#'   Needs the variables in the panel (\code{"Fossil Rents"}: the panel key \code{fossilRents}).
#' @param extraAccountability Character. Extra accountability channels added to the spec grid as a
#'   further option of the accountability slot (\code{"OvAcc"}: V-Dem's overall accountability, the
#'   PoliClim index; needs the panel key \code{overallAccountability}), see \code{\link{channelSpecs}}.
#'   Empty (default): none, as up to v6. Only consulted when \code{specs} is \code{NULL}.
#' @param edgeGate \code{NULL} (default, as up to v6) or a list of thresholds for the edge-of-data
#'   gate of the sanity walk (\code{share}, \code{effect}, \code{capShift}, \code{cap},
#'   \code{mapping}; design note 0007 section 5). Severe when a driver is pushed against the
#'   frontier's reading in a material share of a region, or when capping the interactions at the
#'   sample's range moves the regional ceilings.
#' @param theoryGate \code{NULL} (default, as up to v7's first selection) or a list (\code{p}, \code{structural},
#'   \code{contingency}, \code{coherence}) for the theory gate (design note 0007 section 15): H1 fossil dependence
#'   lowers the ceiling (main effect < 0, p < \code{p}) and H2 depends on institutions (a fossil x institution term
#'   at p < \code{p} / their number) in every sector, screened on the fits before the walk; H3 the fossil effect,
#'   interactions included, is negative in at least \code{coherence} of the estimation rows, severe in the walk.
#'   The verdicts are kept as \code{sweep.rds$theory}.
#' @param minScenarioDelta,deltaWindow Responsiveness-gate knobs (default
#'   \code{0.05} index points over \code{c(2040, 2060)}).
#' @param sanityBatchSize,sanityMaxModels,sanityThresholds Sanity-walk knobs;
#'   thresholds are the \code{\link{computePolicyStringencySanity}} overrides.
#' @param overwriteConfig Logical. Regenerate the auto-generated spec YAML.
#'
#' @return Invisibly, a list: \code{results} (per-fit metrics), \code{coefficients},
#'   \code{maximin}, \code{selected}, \code{sanity}, \code{specs},
#'   \code{selectedConfigPath}, \code{fitSummary}.
#'
#' @seealso \code{\link{runSweep}}, \code{\link{estimatePolicyStringencyModel}},
#'   \code{\link{computePolicyStringencySanity}}, ADR 0036
#' @export
#' @author Renato Rodrigues
runPFMSweep <- function(group,
                        mode = c("exhaustive", "guided"),
                        resultsDir = getOption("pfm.resultsDir", "output"),
                        modelDir = getOption("pfm.modelDir", "output"),
                        cachefolder = NULL,
                        gdxFile = NULL,
                        panelData = NULL,
                        scenarioData = NULL,
                        specs = NULL,
                        y = .pfmPanelYears(),
                        outputRegionMappingFile = "regionmapping_54.csv",
                        movingAverage = .pfmPanelMA(),
                        sectors = c("Bulk", "Diffuse"),
                        indexMax = 10,
                        nCores = 1L,
                        forceRefit = FALSE,
                        selectFE = c("H12", "OECDp", "Mundlak"),
                        nearTieEps = 0.025,
                        feParsimonyWeight = 0,
                        dropIdleControls = TRUE,
                        softVifGate = 6,
                        trendDominanceGate = 0.9,
                        deltaR2Max = 1,
                        inferenceTGate = 2.33,
                        rankBy = "worseDeltaR2",
                        tierGate = "Green",
                        documentTierGates = c("Green", "Blue"),
                        referenceScenarioData = NULL,
                        referenceGdxFile = NULL,
                        referenceGdxRegionMappingFile = NULL,
                        minScenarioDelta = 0.05,
                        deltaWindow = c(2040, 2060),
                        supportShareGate = 0.275,
                        ceilingFallGate = 0.90,
                        gammaGate = 0.999,
                        vcovGate = c("likelihood-mismatch", "flat"),
                        apTransforms = c("linear", "saturating"),
                        dropCompositeAP = FALSE,
                        apExtrapolationGate = Inf,
                        apExtrapolationSd = 1,
                        apExtrapolationWindow = c(2025, 2100),
                        interactionWeights = character(0),
                        fossilFloors = character(0),
                        slackDrivers = NULL,
                        edgeGate = NULL,
                        theoryGate = NULL,
                        extraAccountability = character(0),
                        gdxRegionMappingFile = "regionmappingH12.csv",
                        apPcForms = c("splitAPpc", "mixedAP", "bothIncAP"),
                        sanityBatchSize = 5,
                        # Raised from 20 on 2026-08-24. The walk stops at the first
                        # candidate that clears every severe gate, and with
                        # ceilingFallGate active only ~10% of v1's specs did - 20 was
                        # a real risk of exhausting the pool and forcing a pick. Each
                        # extra candidate costs 2 frontier fits, so 60 adds ~20 min to
                        # a cluster job and buys ~6 expected passes instead of ~2.
                        # ADR 0043: raise this before weakening the gate.
                        sanityMaxModels = 60,
                        sanityThresholds = list(),
                        overwriteConfig = TRUE,
                        verbose = TRUE) {
  mode <- match.arg(mode)
  if (missing(group) || is.null(group) || !nzchar(group)) {
    stop("runPFMSweep: 'group' is required.", call. = FALSE)
  }
  if (is.null(resultsDir)) {
    stop("runPFMSweep: supply 'resultsDir' or set options(pfm.resultsDir = '...').", call. = FALSE)
  }
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("runPFMSweep: the 'yaml' package is required.")
  }
  say <- function(...) if (isTRUE(verbose)) message("[runPFMSweep:", group, "] ", ...)

  .useMadratCache(cachefolder)
  t0 <- Sys.time()

  groupDir <- file.path(resultsDir, group)
  dir.create(groupDir, showWarnings = FALSE, recursive = TRUE)
  if (!is.null(modelDir)) {
    dir.create(modelDir, showWarnings = FALSE, recursive = TRUE)
    options(pfm.modelDir = modelDir)
  }

  # ── Historical panel (with the PFM outcomes) ─────────────────────────────────
  if (is.null(panelData)) {
    say("Building historical panel incl. Policy Stringency (", min(y), "-", max(y), ") ...")
    panelData <- panelDataHistorical(
      aggregate = TRUE, y = y,
      outputRegionMappingFile = outputRegionMappingFile, movingAverage = movingAverage,
      includePolicyStringency = TRUE
    )
  }
  psVars <- paste0("Policy Stringency|", sectors)
  if (magclass::is.magpie(panelData) && !all(psVars %in% magclass::getNames(panelData))) {
    stop("runPFMSweep: panelData lacks ", paste(setdiff(psVars, magclass::getNames(panelData)),
         collapse = ", "), " - build it with includePolicyStringency = TRUE.", call. = FALSE)
  }
  if (magclass::is.magpie(panelData) &&
        "GDP per Capita" %in% magclass::getNames(panelData) &&
        !"GDP per Capita Sq" %in% magclass::getNames(panelData)) {
    panelData <- magclass::mbind(
      panelData,
      magclass::setNames(panelData[, , "GDP per Capita"]^2, "GDP per Capita Sq")
    )
  }
  if (!is.null(modelDir)) saveTrainingPanel(panelData, dir = modelDir)

  # ── Scenario panel (optional; enables the bounded-index sanity gate) ─────────
  # gdxRegionMappingFile is the gdx's OWN native resolution, not a target (PITFALLS
  # §5). Until 2026-08-24 this call did not pass it, so it silently defaulted to H12
  # while Run-Group v1's gdx was EU21 - the panel died with `subscript out of bounds
  # ("EUR")`, the tryCatch swallowed it, and the whole sweep selected on maximin
  # alone. `v1/sweep.rds` carries 0 sanity entries because of exactly this.
  if (is.null(scenarioData) && !is.null(gdxFile) && file.exists(gdxFile)) {
    say("Building scenario panel from gdx (gdx mapping: ", gdxRegionMappingFile, ") ...")
    scenarioData <- tryCatch(
      panelDataScenario(gdxFile = gdxFile, aggregate = TRUE,
                        gdxRegionMappingFile = gdxRegionMappingFile,
                        outputRegionMappingFile = outputRegionMappingFile,
                        histYears = y, movingAverage = movingAverage),
      error = function(e) {
        say("scenario panel failed (", conditionMessage(e), "); sanity gate skipped.")
        NULL
      }
    )
  }
  # Reference scenario panel (ADR 0039: the responsiveness gate needs a second,
  # non-gating pathway to compare against).
  if (is.null(referenceScenarioData) && !is.null(referenceGdxFile) &&
        file.exists(referenceGdxFile)) {
    refMapping <- referenceGdxRegionMappingFile %||% gdxRegionMappingFile
    say("Building REFERENCE scenario panel from gdx (gdx mapping: ", refMapping, ") ...")
    referenceScenarioData <- tryCatch(
      panelDataScenario(gdxFile = referenceGdxFile, aggregate = TRUE,
                        gdxRegionMappingFile = refMapping,
                        outputRegionMappingFile = outputRegionMappingFile,
                        histYears = y, movingAverage = movingAverage),
      error = function(e) {
        say("reference scenario panel failed (", conditionMessage(e),
            "); responsiveness gate skipped.")
        NULL
      }
    )
  }
  if (!is.null(referenceScenarioData) && is.null(scenarioData)) {
    say("WARNING: reference scenario supplied without a gating scenario - ",
        "responsiveness gate needs both; it will be skipped.")
  }
  # The silent case: a gating panel but no reference. Every other scenario gate runs, so
  # the walk looks complete - v6 was selected like this and selected a spec whose gating
  # and reference projections differ by 0.004 index points (PITFALLS 28).
  if (!is.null(scenarioData) && is.null(referenceScenarioData)) {
    say("WARNING: no reference scenario panel - the responsiveness gate (scenarioBlind) ",
        "is NOT applied. Pass referenceGdxFile (pfmRun does, from the scenario registry).")
  }

  # ── Specs: shared channel grid adapted for the PFM ───────────────────────────
  if (is.null(specs)) {
    configPath <- createChannelConfigs(groupDir, mode, apPcForms = apPcForms,
                                      extraAccountability = extraAccountability,
                                      overwrite = overwriteConfig)
    specs <- yaml::read_yaml(configPath)
  }
  specs <- pfmSpecs(specs, verbose = verbose, apTransforms = apTransforms,
                    dropCompositeAP = dropCompositeAP, interactionWeights = interactionWeights,
                    fossilFloors = fossilFloors, slackDrivers = slackDrivers)
  say(length(specs), " PFM specs after adaptation.")

  # ── Fit grid: one stage, satP engine ─────────────────────────────────────────
  grid <- runFitGrid(
    specs = specs, sectors = sectors, stages = "PolicyStringency", panelData = panelData,
    family = "gaussian", modelDir = modelDir, nCores = nCores, forceRefit = forceRefit,
    verbose = verbose, say = say
  )
  results <- grid$results

  # ── Selection: maximin, then the bounded-index sanity gate ───────────────────
  specByName <- stats::setNames(specs, vapply(specs, `[[`, character(1), "name"))
  maximin <- list()
  selected <- list()
  sanity <- list()
  sub <- results[results$stage == "PolicyStringency", , drop = FALSE]
  if (!is.null(selectFE)) {
    fePat <- paste0("fe:(", paste(selectFE, collapse = "|"), ")")
    keep <- grepl(fePat, sub$model)
    if (any(keep)) {
      sub <- sub[keep, , drop = FALSE]
    } else {
      say("WARNING: selectFE matched no PFM specs; ignoring the FE constraint.")
    }
  }
  mmCols <- intersect(c("model", "sector", "sigActorPower", "sigInstQual",
                        "sigInteractions", "deltaR2Theory", "pseudoR2", "bic", "maxVIF",
                        "converged", "usesLagged", "nFE", "nObs", "sigControl", "nControl",
                        "trendShare", "minSigTheoryT"), colnames(sub))
  mm <- computeMaximinScore(sub[, mmCols, drop = FALSE], nearTieEps = nearTieEps,
                            feParsimonyWeight = feParsimonyWeight,
                            dropIdleControls = dropIdleControls,
                            softVifGate = softVifGate,
                            trendDominanceGate = trendDominanceGate,
                            deltaR2Max = deltaR2Max,
                            inferenceTGate = inferenceTGate,
                            rankBy = rankBy, tierGate = tierGate)
  maximin[["PolicyStringency"]] <- mm
  # Documented selection variants (ADR 0039): tier-gate winners + the
  # Specification-Sharing Cost exhibit. Documentation only - never the deployment.
  selectionVariants <- NULL
  if (!is.null(documentTierGates) && identical(rankBy, "worseDeltaR2")) {
    selectionVariants <- tryCatch(
      computeSelectionVariants(sub[, mmCols, drop = FALSE], sectors = sectors,
                               tierGates = documentTierGates,
                               deployedGate = if (!is.null(tierGate)) tierGate else documentTierGates[1],
                               nearTieEps = nearTieEps,
                               feParsimonyWeight = feParsimonyWeight,
                               dropIdleControls = dropIdleControls,
                               softVifGate = softVifGate,
                               trendDominanceGate = trendDominanceGate,
                               deltaR2Max = deltaR2Max,
                               inferenceTGate = inferenceTGate),
      error = function(e) {
        say("selection-variants exhibit failed: ", conditionMessage(e))
        NULL
      }
    )
    if (!is.null(selectionVariants)) {
      saveRDS(selectionVariants, file.path(groupDir, "selection-variants.rds"))
      say("Selection variants: ",
          paste(names(selectionVariants$winners), unlist(selectionVariants$winners),
                sep = " -> ", collapse = " | "))
      ps <- selectionVariants$perSector
      say("Sharing cost: ",
          paste(sprintf("%s best %s (dR2 %.3f, cost %.3f)", ps$sector, ps$bestModel,
                        ps$bestDeltaR2, ps$sharingCost), collapse = " | "))
    }
  }
  # Theory gate, H1-H2 (design note 0007 section 15): every spec of the pool whose fits do not show fossil
  # dependence lowering the ceiling, and depending on institutions, in every sector leaves the pool before the walk.
  theoryScreen <- NULL; theoryPassAll <- NULL
  if (!is.null(theoryGate) && !is.null(grid$coefficients)) {
    theoryScreen <- .pfmTheoryScreen(grid$coefficients, unique(results$model), sectors, theoryGate)
    theoryPassAll <- theoryScreen$model[theoryScreen$pass]
    bad <- mm$gatePass & !mm$model %in% theoryPassAll
    why <- theoryScreen$reason[match(mm$model[bad], theoryScreen$model)]
    mm$gateFailReason[bad] <- ifelse(nzchar(mm$gateFailReason[bad]), paste0(mm$gateFailReason[bad], "; "), "")
    mm$gateFailReason[bad] <- paste0(mm$gateFailReason[bad], "theory: ", why)
    mm$gatePass[bad] <- FALSE
    maximin[["PolicyStringency"]] <- mm
    say("Theory gate (H1 structural power, H2 contingency): ", sum(mm$gatePass), " of ", sum(mm$gatePass | bad),
        " pool spec(s) pass; best-ranked passing: ", mm$model[mm$gatePass][1] %||% "none")
  }
  pass <- mm[mm$gatePass, , drop = FALSE]
  if (nrow(pass) == 0) {
    # Self-diagnosing: tally WHY every spec failed the hard gate (reason labels,
    # sector suffix stripped) so the SLURM log states the cause without sweep.rds.
    reasons <- mm$gateFailReason[nzchar(mm$gateFailReason)]
    labels <- trimws(sub(":.*$", "", unlist(strsplit(reasons, "; "))))
    tally <- sort(table(labels), decreasing = TRUE)
    say("WARNING: no gate-passing PFM spec. Gate-failure tally: ",
        if (length(tally)) paste(sprintf("%s (x%d)", names(tally), as.integer(tally)),
                                 collapse = "; ") else "(no reasons recorded)")
  } else if (is.null(scenarioData)) {
    # Falling through to maximin-only is a REAL degradation, not a variant: every
    # scenario-dependent gate is skipped at once - the sanity walk, scenarioBlind,
    # extrapolationDominated and ceilingFallGate (ADR 0043, on by default). It is a
    # warning, not a log line, because Run-Group v1 was selected this way for a whole
    # cycle without anyone noticing: the gdx was EU21, gdxRegionMappingFile defaulted
    # to H12, panelDataScenario died on "EUR", and the tryCatch above swallowed it.
    active <- c(if (is.finite(ceilingFallGate)) "ceilingFallGate",
                if (is.finite(gammaGate)) "gammaGate",
                if (length(vcovGate)) "vcovGate",
                if (is.finite(supportShareGate)) "supportShareGate",
                if (is.finite(apExtrapolationGate)) "apExtrapolationGate",
                "sanity walk", "scenarioBlind")
    warning("runPFMSweep: no scenario panel - selecting on maximin ALONE. ",
            "Skipped: ", paste(active, collapse = ", "), ". ",
            "Check gdxFile and that gdxRegionMappingFile ('", gdxRegionMappingFile,
            "') matches the gdx's native resolution (PITFALLS 5).", call. = FALSE)
    selected[["PolicyStringency"]] <- pass$model[1]
    say("WARNING: Selected PFM spec (maximin only - no scenario, ALL scenario gates skipped): ",
        pass$model[1], " (", pass$minTier[1], ", mean dR2 = ",
        round(pass$meanDeltaR2[1], 3), ")")
  } else {
    sel <- .pfmSanitySelect(
      passModels = pass$model, specByName = specByName, sectors = sectors,
      panelData = panelData, scenarioData = scenarioData, modelDir = modelDir,
      batchSize = sanityBatchSize, maxModels = sanityMaxModels,
      thresholds = sanityThresholds, regionBlocks = .h12RegionBlocks(),
      histIndexBySector = .histIndexBySector(panelData, sectors),
      indexMax = indexMax,
      referenceScenarioData = referenceScenarioData,
      minScenarioDelta = minScenarioDelta, deltaWindow = deltaWindow,
      supportShareGate = supportShareGate,
      ceilingFallGate = ceilingFallGate,
      gammaGate = gammaGate,
      vcovGate = vcovGate,
      apExtrapolationGate = apExtrapolationGate, apExtrapolationSd = apExtrapolationSd,
      apExtrapolationWindow = apExtrapolationWindow,
      edgeGate = edgeGate, theoryGate = theoryGate,
      say = say
    )
    # Tier-relaxed fallback (ADR 0039): if every Green-gate candidate fails the
    # sanity walk (e.g. all scenario-blind), walk the Blue-gate pool before
    # accepting a forced least-flagged Green spec.
    if (isTRUE(sel$forced) && identical(tierGate, "Green") &&
          !is.null(selectionVariants) && "Blue" %in% names(selectionVariants$rankings)) {
      mmBlue <- selectionVariants$rankings$Blue
      blueOnly <- setdiff(mmBlue$model[mmBlue$gatePass], pass$model)
      if (!is.null(theoryScreen)) blueOnly <- intersect(blueOnly, theoryPassAll)
      if (length(blueOnly) > 0) {
        say("Green pool exhausted in sanity - walking the tier-relaxed (Blue) pool ...")
        selBlue <- .pfmSanitySelect(
          passModels = blueOnly, specByName = specByName, sectors = sectors,
          panelData = panelData, scenarioData = scenarioData, modelDir = modelDir,
          batchSize = sanityBatchSize, maxModels = sanityMaxModels,
          thresholds = sanityThresholds, regionBlocks = .h12RegionBlocks(),
          histIndexBySector = .histIndexBySector(panelData, sectors),
          indexMax = indexMax,
          referenceScenarioData = referenceScenarioData,
          minScenarioDelta = minScenarioDelta, deltaWindow = deltaWindow,
          supportShareGate = supportShareGate,
          ceilingFallGate = ceilingFallGate,
          gammaGate = gammaGate,
          vcovGate = vcovGate,
          apExtrapolationGate = apExtrapolationGate, apExtrapolationSd = apExtrapolationSd,
          apExtrapolationWindow = apExtrapolationWindow,
          edgeGate = edgeGate, theoryGate = theoryGate,
          say = say
        )
        if (!isTRUE(selBlue$forced)) {
          selBlue$tierRelaxed <- TRUE
          sel <- selBlue
          say("Tier-relaxed deployment: a Blue-gate spec passed sanity where no Green did.")
        }
      }
    }
    selected[["PolicyStringency"]] <- sel$chosen
    sanity[["PolicyStringency"]] <- sel
    say("Selected PFM spec: ", sel$chosen,
        if (isTRUE(sel$forced)) " (LEAST-FLAGGED fallback - no candidate passed sanity)"
        else if (isTRUE(sel$tierRelaxed)) " (tier-relaxed: Blue gate; passed sanity + responsiveness)"
        else " (passed bounded-index Projection Sanity)")
  }
  # Post-sanity recompute of the variants exhibit (ADR 0039 caveat fix): the
  # sanity/responsiveness walk can reject the maximin leader, so the sharing-cost
  # columns must reference the FINAL deployment, not the pre-sanity gate winner.
  if (!is.null(selectionVariants) && length(selected) > 0 &&
        !identical(selected[["PolicyStringency"]], selectionVariants$deployedModel)) {
    selectionVariants <- tryCatch(
      computeSelectionVariants(sub[, mmCols, drop = FALSE], sectors = sectors,
                               tierGates = documentTierGates,
                               deployedGate = if (!is.null(tierGate)) tierGate else documentTierGates[1],
                               deployedModel = selected[["PolicyStringency"]],
                               nearTieEps = nearTieEps,
                               feParsimonyWeight = feParsimonyWeight,
                               dropIdleControls = dropIdleControls,
                               softVifGate = softVifGate,
                               trendDominanceGate = trendDominanceGate,
                               deltaR2Max = deltaR2Max,
                               inferenceTGate = inferenceTGate),
      error = function(e) selectionVariants
    )
    saveRDS(selectionVariants, file.path(groupDir, "selection-variants.rds"))
    ps <- selectionVariants$perSector
    say("Sharing cost vs the DEPLOYED spec (post-sanity): ",
        paste(sprintf("%s best %s (dR2 %.3f, cost %.3f)", ps$sector, ps$bestModel,
                      ps$bestDeltaR2, ps$sharingCost), collapse = " | "))
  }
  results$tier <- computeTheoryTier(results$sigActorPower, results$sigInstQual,
                                    results$sigInteractions)

  # ── Run-Group artifacts ───────────────────────────────────────────────────────
  selectedConfigPath <- NULL
  if (length(selected) > 0) {
    selectedConfigPath <- .writeSelectedPFMConfig(
      specs = specs, selected = selected, maximin = maximin,
      mode = mode, configDir = groupDir, sectors = sectors, indexMax = indexMax
    )
  }
  res <- list(
    results = results, coefficients = grid$coefficients,
    maximin = maximin, selected = selected, sanity = sanity,
    selectionVariants = selectionVariants, theory = theoryScreen,
    specs = specs, selectedConfigPath = selectedConfigPath,
    fitSummary = list(nJobs = grid$nJobs, nNew = grid$nNew, nFailed = grid$nFailed)
  )
  saveRDS(res, file.path(groupDir, "sweep.rds"))

  .writeRunGroupManifest(
    groupDir, group = group, mode = paste0("pfm-", mode),
    panelData = panelData, scenarioData = scenarioData, gdxFile = gdxFile,
    selectionMethod = "pfm-maximin", nCores = nCores,
    # What the group was selected with and fitted on, read back by every later step
    # (.pfmSweepOptionsForGroup, pfmPanelDef).
    sweepOptions = list(apTransforms = as.list(apTransforms), dropCompositeAP = dropCompositeAP,
                        apExtrapolationGate = apExtrapolationGate, apExtrapolationSd = apExtrapolationSd,
                        apExtrapolationWindow = as.list(apExtrapolationWindow),
                        # The within-band rule the group was selected under (ADR 0048); the
                        # selection bootstrap re-ranks its resamples under the same one.
                        softVifGate = softVifGate %||% Inf, inferenceTGate = inferenceTGate %||% 0,
                        # the interaction-weight axis and the edge-of-data gate (design note 0007)
                        interactionWeights = as.list(interactionWeights),
                        # the fossil lower-bound axis (design note 0009 section 7.1)
                        fossilFloors = as.list(as.character(unlist(fossilFloors))),
                        # the slack drivers of the frontier, by sector (design note 0009 section 8.2)
                        slackDrivers = if (length(slackDrivers)) lapply(slackDrivers, function(v) as.list(as.character(unlist(v)))) else list(),
                        edgeGate = edgeGate %||% "off",
                        theoryGate = theoryGate %||% "off",
                        extraAccountability = as.list(extraAccountability)),
    # Every key of the definition: a key left out is read back as its default by every later step
    # (diagnostics, downstream, the coupling), which would then build panels without the columns the
    # selected spec needs (the democracy weights, the overall-accountability channel).
    panelDef = list(firstYear = min(y), lastYear = max(y), movingAverage = movingAverage %||% 1L,
                    ieaVersion = pfmPanelDef()$ieaVersion, geothermal = pfmPanelDef()$geothermal,
                    democracy = pfmPanelDef()$democracy,
                    overallAccountability = pfmPanelDef()$overallAccountability,
                    fossilRents = pfmPanelDef()$fossilRents)
  )
  # Which scenario gates the selection actually saw, in the audit: a skipped gate has no
  # error, so its absence must be readable after the fact (PITFALLS 18, 28).
  .recordStep(groupDir, group, "sweep-pfm", t0, mode = paste0("pfm-", mode),
              metrics = c(res$fitSummary,
                          list(nCores = nCores,
                               sanityGate = !is.null(scenarioData),
                               responsivenessGate = !is.null(scenarioData) &&
                                 !is.null(referenceScenarioData))))

  say("PFM Run-Group written: ", groupDir)
  invisible(res)
}

#' Adapt the shared channel spec grid for the Political Feasibility Model
#'
#' @description
#' The PFM mirrors the carbon-price model's theory RHS by design (ADR 0036), so
#' it sweeps the \emph{same} channel spec grid — minus the axes that have no PFM
#' meaning:
#' \itemize{
#'   \item \strong{FD/hazard transforms dropped} — the PFM currently estimates in
#'     levels only (no adoption stage exists, and the FD-on-logit-scale variant is
#'     future work), so \code{panelTransform != "levels"} specs are removed.
#'   \item \strong{Saturating-price variants dropped} — the \code{priceLink} axis
#'     (ADR 0028) is the price model's bounded-form competition; the PFM is
#'     \emph{always} the bounded satP form, so the \code{"| satP"} variants would be
#'     duplicates.
#'   \item \strong{Ridge specs dropped} — not supported by the PFM estimator.
#'   \item \code{includeLaggedECP} (the price-lag dynamics rung) maps to
#'     \code{includeLaggedPS} (the policy-ratcheting rung).
#' }
#'
#' @param specs List of spec lists (raw YAML entries or already normalised).
#' @param verbose Logical.
#' @param apTransforms The actor-power forms each SPLIT actor-power spec is fitted in
#'   (\code{\link{preparePanelData}}'s \code{apTransform}): \code{"linear"} keeps the spec as
#'   it is, the others append a variant with the suffix \code{" satAP"} (both groups saturating),
#'   \code{" satInn"} (innovator only) or \code{" satInc"} (incumbent only). Default
#'   \code{c("linear", "saturating")}, the grid of every Run-Group up to v5.
#' @param dropCompositeAP Logical. Drop the specs whose actor power is the composite Actor
#'   Power Index (a difference that cannot be saturated; design note 0005 D7). Default
#'   \code{FALSE}, as up to v5.
#' @param interactionWeights Character. Panel variables (0-1) each of which adds a twin of every
#'   spec with actor-power x institution interactions, its interactions multiplied by that variable
#'   (\code{interactionWeight}; design note 0007, O10). \code{"none"} or empty (default) adds none.
#'   The twins carry the suffix \code{" iw:poly"} (V-Dem electoral democracy), \code{" iw:libdem"}
#'   (liberal democracy) or \code{" iw:fexp"} (freedom of expression and alternative information);
#'   another variable gets \code{" iw:"} and its syntactic name. A variable followed by
#'   \code{"@accRoL"} weights only the accountability and rule-of-law interactions (design note 0007
#'   section 8, A7); its twins carry the scope in the suffix (\code{" iw:libdem@accRoL"}).
#' @param fossilFloors The fossil lower-bound axis (design note 0009 section 7.1): training quantiles in
#'   (0, 1) at which every spec with a fossil term gets its fossil terms bounded below
#'   (\code{fossilFloor}; suffix \code{" fos5"} for 0.05). With \code{"none"} among them the bounded specs
#'   are added as twins of the unbounded ones; without it they replace them, so the grid keeps its size.
#'   Empty (default): no bound. Applied last, so the bound covers every twin of the other axes.
#' @param slackDrivers \code{NULL} (default) or a list of panel variables named by sector: set on every spec as
#'   \code{slackDrivers}, read by the frontier estimator only (design note 0009 section 8.2). Not a grid axis.
#' @return The adapted, normalised spec list.
#' @export
pfmSpecs <- function(specs, verbose = TRUE, apTransforms = c("linear", "saturating"),
                     dropCompositeAP = FALSE, interactionWeights = character(0),
                     fossilFloors = character(0), slackDrivers = NULL) {
  apTransforms <- match.arg(apTransforms, c("linear", "saturating", "saturating-innovator",
                                            "saturating-incumbent"), several.ok = TRUE)
  normalize <- function(cfg) {
    for (f in c("actorPowerDrivers", "actorPowerIndex", "instQualityDrivers", "controlDrivers")) {
      if (!is.null(cfg[[f]])) cfg[[f]] <- unlist(cfg[[f]])
    }
    for (f in c("includeLagged", "includeLaggedECP", "includeLaggedPS", "nickellCorrection",
                "interactRegionFE", "logisticTimeTrend", "useMundlak", "gdpGovInteraction",
                "ridgeInteractions", "stringencyOnly")) {
      cfg[[f]] <- isTRUE(cfg[[f]])
    }
    cfg$panelTransform <- if (is.null(cfg$panelTransform)) "levels" else cfg$panelTransform
    cfg$apTransform <- if (is.null(cfg$apTransform)) "linear" else cfg$apTransform
    cfg
  }
  specs <- lapply(specs, normalize)
  n0 <- length(specs)
  keep <- vapply(specs, function(cfg) {
    identical(cfg$panelTransform, "levels") &&
      !isTRUE(cfg$stringencyOnly) &&
      !isTRUE(cfg$ridgeInteractions)
  }, logical(1))
  specs <- specs[keep]
  specs <- lapply(specs, function(cfg) {
    cfg$includeLaggedPS <- isTRUE(cfg$includeLaggedPS) || isTRUE(cfg$includeLaggedECP)
    cfg$includeLaggedECP <- FALSE
    # Adoption-only knobs have no PFM meaning; neutralised so the maximin
    # unintended-lag gate keys off nothing.
    cfg$includeLagged <- FALSE
    cfg$nickellCorrection <- FALSE
    cfg
  })
  if (isTRUE(verbose) && length(specs) < n0) {
    message("[pfmSpecs] ", n0 - length(specs), " of ", n0,
            " specs dropped (FD transforms, satP variants, ridge).")
  }

  # --- Saturating actor-power axis (ADR 0040) ---------------------------------
  # Appended as TWINS so the linear originals keep their X-numbers and remain
  # comparable with earlier sweeps; the variant carries the " satAP" suffix. Only
  # SPLIT actor-power specs get a variant: the saturating map x/(x + xBar) is defined
  # for shares, and the composite Actor Power Index is a difference (innovator
  # minus incumbent) that takes negative values (preparePanelData would skip it
  # and the variant would be a duplicate of its linear parent). This is also why the
  # axis roughly doubles only the split half of the grid rather than all of it.
  isSplitAP <- function(cfg) {
    api <- unlist(cfg$actorPowerIndex)
    length(api) > 0 && !any(grepl("^Actor Power Index", api))
  }
  isComposite <- function(cfg) any(grepl("^Actor Power Index", unlist(cfg$actorPowerIndex)))
  if (isTRUE(dropCompositeAP)) {
    nComp <- sum(vapply(specs, isComposite, logical(1)))
    specs <- Filter(Negate(isComposite), specs)
    if (isTRUE(verbose)) message("[pfmSpecs] ", nComp, " composite Actor Power Index specs dropped.")
  }
  # One variant per non-linear form (design note 0005 D7). The suffixes keep the names of the
  # Run-Groups up to v5, where the only variant was " satAP" (both groups saturating).
  twinForm <- list(
    saturating = c(sfx = " satAP", txt = "[saturating actor power: x/(x+median), ADR 0040]"),
    "saturating-innovator" = c(sfx = " satInn", txt = "[saturating innovator power, linear incumbents: 0005 D7]"),
    "saturating-incumbent" = c(sfx = " satInc", txt = "[saturating incumbent power, linear innovators: 0005 D7]")
  )
  split <- Filter(isSplitAP, specs)
  twins <- unlist(lapply(setdiff(apTransforms, "linear"), function(tf) {
    lapply(split, function(cfg) {
      cfg$apTransform <- tf
      cfg$name <- paste0(cfg$name, twinForm[[tf]][["sfx"]])
      cfg$description <- paste0(cfg$description %||% "",
                                if (nzchar(cfg$description %||% "")) " " else "",
                                twinForm[[tf]][["txt"]])
      cfg
    })
  }), recursive = FALSE)
  # Without "linear" the split originals leave the grid (composite specs are never twinned).
  if (!"linear" %in% apTransforms) specs <- Filter(Negate(isSplitAP), specs)
  specs <- c(specs, twins)
  if (isTRUE(verbose)) {
    message("[pfmSpecs] ", length(twins), " saturating actor-power variants appended",
            " (total ", length(specs), " specs).")
  }

  # --- Interaction-weight axis (design note 0007, O10) --------------------------
  # A twin of every spec that HAS actor-power x institution interactions, with every interaction
  # multiplied by a 0-1 panel variable (preparePanelData `interactionWeight`). The V-Dem electoral
  # democracy index makes interactions estimated mostly on democracies act in proportion to how
  # democratic a country is, instead of being extended to closed autocracies.
  interactionWeights <- setdiff(as.character(unlist(interactionWeights)), c("none", ""))
  hasInteractions <- function(cfg) length(unlist(cfg$actorPowerIndex)) > 0 && length(unlist(cfg$instQualityDrivers)) > 0
  iwShort <- c("Electoral Democracy (VDem)" = "poly", "Liberal Democracy (VDem)" = "libdem",
               "Freedom of Expression (VDem)" = "fexp")
  # a scoped weight ("<variable>@accRoL", .pfmIwScopes) keeps its scope in the suffix: " iw:libdem@accRoL"
  iwSuffix <- function(wv) {
    p <- .pfmIwParse(wv)
    paste0(" iw:", if (p$variable %in% names(iwShort)) iwShort[[p$variable]] else make.names(p$variable),
           if (!is.null(p$scope)) paste0("@", p$scope) else "")
  }
  base <- Filter(hasInteractions, specs)
  # A scoped weight differs from the unscoped one only where the spec interacts with institutions both in
  # and out of the scope: with none in scope its twin is the unweighted spec, with all in scope it is the
  # unscoped twin. Those duplicates are not added.
  scopeDiffers <- function(cfg, wv) {
    sc <- .pfmIwParse(wv)$scope
    if (is.null(sc)) return(TRUE)
    inScope <- grepl(.pfmIwScopes[[sc]], make.names(unlist(cfg$instQualityDrivers)))
    any(inScope) && !all(inScope)
  }
  wTwins <- unlist(lapply(interactionWeights, function(wv) lapply(Filter(function(cfg) scopeDiffers(cfg, wv), base), function(cfg) {
    cfg$interactionWeight <- wv
    cfg$name <- paste0(cfg$name, iwSuffix(wv))
    cfg$description <- paste0(cfg$description %||% "", if (nzchar(cfg$description %||% "")) " " else "",
                              "[interactions weighted by ", wv, ": design note 0007]")
    cfg
  })), recursive = FALSE)
  if (length(wTwins)) {
    specs <- c(specs, wTwins)
    if (isTRUE(verbose)) message("[pfmSpecs] ", length(wTwins), " interaction-weighted variants appended",
                                 " (total ", length(specs), " specs).")
  }

  # --- Fossil lower-bound axis (design note 0009 section 7.1) -------------------
  # The linear fossil term is mis-specified at the low edge: below about the sample's 5th percentile,
  # less fossil dependence no longer predicts a higher ceiling (Iceland, v7). preparePanelData bounds the
  # fossil terms there and freezes the bound, so the projections hold them at it.
  ff <- as.character(unlist(fossilFloors))
  keepUnbounded <- "none" %in% ff
  q <- suppressWarnings(as.numeric(setdiff(ff, c("none", ""))))
  if (anyNA(q) || any(q <= 0 | q >= 1)) stop("pfmSpecs: 'fossilFloors' must be \"none\" or quantiles in (0, 1).", call. = FALSE)
  if (length(q)) {
    hasFossil <- function(cfg) any(grepl("^Incumbent Power", c(unlist(cfg$actorPowerIndex), unlist(cfg$actorPowerDrivers))))
    bounded <- unlist(lapply(q, function(fq) lapply(Filter(hasFossil, specs), function(cfg) {
      cfg$fossilFloor <- fq
      cfg$name <- paste0(cfg$name, " fos", format(100 * fq))
      cfg$description <- paste0(cfg$description %||% "", if (nzchar(cfg$description %||% "")) " " else "",
                                "[fossil terms bounded below at the training ", format(100 * fq), "th percentile: design note 0009]")
      cfg
    })), recursive = FALSE)
    specs <- c(if (keepUnbounded) specs else Filter(Negate(hasFossil), specs), bounded)
    if (isTRUE(verbose)) message("[pfmSpecs] ", length(bounded), " fossil-bounded variants ",
                                 if (keepUnbounded) "appended" else "in place of the unbounded ones",
                                 " (total ", length(specs), " specs).")
  }

  # --- Slack drivers of the frontier (design note 0009 section 8.2): a property of every spec, not an axis ---
  if (length(slackDrivers)) {
    if (!is.list(slackDrivers) || is.null(names(slackDrivers)) || !all(names(slackDrivers) %in% c("Bulk", "Diffuse"))) {
      stop("pfmSpecs: 'slackDrivers' must be a list named by sector (Bulk, Diffuse).", call. = FALSE)
    }
    sd <- lapply(slackDrivers, function(v) as.character(unlist(v)))
    specs <- lapply(specs, function(cfg) { cfg$slackDrivers <- sd; cfg })
    if (isTRUE(verbose)) message("[pfmSpecs] frontier slack drivers: ",
                                 paste(names(sd), vapply(sd, paste, "", collapse = "+"), sep = " = ", collapse = "; "))
  }
  specs
}

# Internal: writes selected-models-pfm.yml (one entry per sector, single stage).
#' @keywords internal
.writeSelectedPFMConfig <- function(specs, selected, maximin, mode, configDir,
                                    sectors, indexMax = 10) {
  specByName <- stats::setNames(specs, vapply(specs, `[[`, character(1), "name"))
  # Pin the trend shape into every entry. A spec that omits it silently means
  # "whatever preparePanelData's default was at fit time", so replaying an old
  # spec after a default change reproduces neither the fit nor the projection
  # (that default moved on 2026-08-22; TODO item 0b). .pfmSpecArgs() forwards any
  # spec key matching an estimator formal, so writing them here is enough for the
  # whole downstream chain to honour them.
  trendDefaults <- list(trendMidpoint = formals(preparePanelData)$trendMidpoint,
                        trendSteepness = formals(preparePanelData)$trendSteepness)
  entries <- list()
  for (stg in names(selected)) {
    cfg <- specByName[[selected[[stg]]]]
    if (is.null(cfg)) next
    mm <- maximin[[stg]]
    mmRow <- mm[mm$model == selected[[stg]], , drop = FALSE]
    for (sec in sectors) {
      e <- cfg
      e$model_type <- paste0(stg, ": ", sec)
      e$estimator <- "satP"
      e$indexMax <- indexMax
      for (k in names(trendDefaults)) {
        e[[k]] <- as.numeric(e[[k]] %||% trendDefaults[[k]])
      }
      e$description <- paste0(
        "Maximin-selected PFM shared spec (pfm-", mode, ", ", format(Sys.Date()), "). ",
        "Worse-sector tier: ", mmRow$minTier[1],
        " | mean dR2(theory): ", round(mmRow$meanDeltaR2[1], 3),
        " | ", mmRow$tierBySector[1], " | ", mmRow$deltaR2BySector[1]
      )
      entries[[length(entries) + 1L]] <- e
    }
  }
  out <- file.path(configDir, "selected-models-pfm.yml")
  header <- paste0(
    "# PFM Selected Models - Political Feasibility Model (pfm-", mode, ") - ",
    format(Sys.Date()), "\n",
    "# GENERATED by pfm::runPFMSweep() via the Maximin Selection Rule (ADR 0036).\n",
    "# One shared spec, applied to both sectors; satP engine, bounded index.\n\n"
  )
  writeLines(paste0(header, yaml::as.yaml(entries, indent.mapping.sequence = TRUE)), out)
  out
}
# nolint end
