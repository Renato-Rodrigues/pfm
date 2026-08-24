# nolint start
#' @title channelSpecs
#' @description Builds the Institutional Quality Channels specification lists
#' (ADR 0004) programmatically — the single source of truth for the
#' \code{channels-guided.yml} and \code{channels-exhaustive.yml} sweep configs.
#'
#' \strong{guided} (14 specs): Stage-1 slot screens (each channel candidate alone
#' in the D4 backbone), Stage-2 joint all-three-channel specs across the two
#' consistent Actor Power Forms, two Panel Transform variants (ADR 0005), and the
#' two incumbent benchmarks (D2 / D4). State capacity = WGI Government
#' Effectiveness (V-Dem State-Capacity PCA dropped 2026-06-16).
#'
#' \strong{exhaustive} (1445 specs, ADR 0011): full cross of the state-capacity
#' slot \{WGI GovEff, out\} x Rule of Law \{in, out\} x Accountability
#' \{Vertical, Horizontal, Diagonal, out\} (empty institutional set excluded; 15
#' combos) x 2 Actor Power Forms x 8 control sets, where \code{levels} specs also
#' cross 5 region-FE resolutions \{none, H12, EU_OECDp, 54-unit, Mundlak\} and
#' \code{hybridFD} specs carry no FE (differenced out): 15x2x8x(5+1) = 1440, plus
#' one pureFD demonstration spec and four lagged-stringency variants.
#' Control sets use projection-safe transforms (GDP-Q (+sq), log GDP (+sq), log
#' Population, Hydro/Nuclear share); see ADR 0011.
#'
#' Backbone for all specs unless overridden: GDP per Capita (Q-centred) control,
#' logistic time trend, no Hydro Nuclear Share, H12 region FE, no Mundlak,
#' no ridge, no lagged terms.
#'
#' @param mode Character. \code{"guided"} or \code{"exhaustive"}.
#'
#' @return A list of specification lists, each with the fields consumed by the
#'   model-selection report (\code{name}, \code{description},
#'   \code{actorPowerDrivers}, \code{actorPowerIndex}, \code{instQualityDrivers},
#'   \code{controlDrivers}, backbone flags, \code{panelTransform}).
#'
#' @export
#' @author Renato Rodrigues
channelSpecs <- function(mode = c("guided", "exhaustive"),
                         apPcForms = c("splitAPpc", "mixedAP", "bothIncAP")) {
  mode <- match.arg(mode)

  backbone <- list(
    controlDrivers = c("GDP per Capita (Q-centred)"),
    includeLagged = FALSE,       # adoption-stage lag (lagged_adoption)
    includeLaggedECP = FALSE,    # stringency-stage price lag (lagged_ecp)
    nickellCorrection = FALSE,   # split-panel jackknife when lag + region FE
    interactRegionFE = FALSE,
    regionMappingFixedEffects = "regionmappingH12.csv",
    useMundlak = FALSE,
    logisticTimeTrend = TRUE,
    gdpGovInteraction = FALSE,
    ridgeInteractions = FALSE
  )

  # State capacity = WGI Government Effectiveness (2026-06-16). The V-Dem
  # State-Capacity PCA (PC1/PC2) was removed from all configs per the user's
  # directive (WGI GovEff is the literature-preferred, directly interpretable
  # measure); it is no longer offered in any spec.
  WGE  <- "Government Effectiveness (WGI)"
  ROL  <- "Rule of Law (VDem)"
  VER  <- "Vertical Accountability (VDem)"
  HOR  <- "Horizontal Accountability (VDem)"
  DIA  <- "Diagonal Accountability (VDem)"
  CSP  <- "Civil Service Professionalism (VDem)"
  SPLIT <- c("Innovator Power", "Incumbent Power")
  COMP  <- "Actor Power Index"
  # Per-capita counterparts (design-notes/0001). Shares operationalise STRUCTURAL
  # power (dependence, lock-in); per-capita levels operationalise INSTRUMENTAL power
  # (rents, employment, lobbying resources). MIXED is the design note's recommendation:
  # innovator influence as policy feedback (share-like), incumbent influence as
  # instrumental weight (level-like). Legal because the deployed form is `splitAP` -
  # the two enter separately, so the composite difference is never taken across scales.
  SPLITPC <- c("Innovator Power pc", "Incumbent Power pc")
  MIXED   <- c("Innovator Power", "Incumbent Power pc")
  # Both incumbent terms at once. This NESTS `splitAP` and `mixedAP`, so it is the
  # test that resolves the design note's own main risk: dropping the share loses the
  # dependence / lock-in channel, which is well established in this literature. With
  # both present the sweep answers directly whether the share still carries
  # information once the level is in. Costs three extra parameters (one main effect
  # plus its two institution interactions).
  BOTHINC <- c("Innovator Power", "Incumbent Power", "Incumbent Power pc")

  # Control variables (ADR 0011). Projection-safe transforms only: GDP-Q quantile
  # (+ its square), log GDP per capita (+ square), log Population. Raw GDP+GDP^2 is
  # deliberately avoided (extrapolates). Derived columns are computed on the fly
  # in preparePanelData from the base panel variables.
  GDPQ    <- "GDP per Capita (Q-centred)"
  GDPQ2   <- "GDP per Capita (Q-centred) Sq"
  LOGGDP  <- "GDP per Capita (log)"
  LOGGDP2 <- "GDP per Capita (log) Sq"
  LOGPOP  <- "Population (log)"
  HYDRO   <- "Hydro Nuclear Share"

  spec <- function(name, description, apDrivers, apIndex, iq,
                   transform = "levels", overrides = list()) {
    s <- c(
      list(name = name, description = description,
           actorPowerDrivers = apDrivers, actorPowerIndex = apIndex,
           instQualityDrivers = iq),
      backbone,
      list(panelTransform = transform)
    )
    s[names(overrides)] <- overrides
    s
  }

  if (mode == "guided") {
    return(list(
      # ── Stage 1 — slot screens (composite AP held fixed) ──────────────────────
      # Option 2b (2026-06-14): the hybrid form (split mains + composite-index
      # interactions) fed the same information twice and produced ill-conditioned
      # +/-15 coefficients that exploded under projection. Only consistent forms
      # are used now: composite/composite (default) or split/split.
      spec("S1-GE WGI Government Effectiveness",
           paste0("Stage-1 slot screen - state-capacity channel: WGI Government ",
                  "Effectiveness (the literature-preferred measure; V-Dem State-Capacity ",
                  "PCA dropped 2026-06-16)."),
           COMP, COMP, WGE),
      spec("S1-RL1 Rule of Law (VDem)",
           "Stage-1 slot screen - Rule of Law channel alone, composite AP.",
           COMP, COMP, ROL),
      spec("S1-AC1 Vertical Accountability (VDem)",
           "Stage-1 slot screen - Accountability candidate 1 (elections/suffrage).",
           COMP, COMP, VER),
      spec("S1-AC2 Horizontal Accountability (VDem)",
           "Stage-1 slot screen - Accountability candidate 2 (judicial/legislative constraints).",
           COMP, COMP, HOR),
      spec("S1-AC3 Diagonal Accountability (VDem)",
           "Stage-1 slot screen - Accountability candidate 3 (media/civil society).",
           COMP, COMP, DIA),
      # ── Stage 2 — joint all-three specs (state capacity = WGI GovEff) ─────────
      spec("J-01 WGIge + RoL + VerAcc (composite AP)",
           "Stage-2 joint spec - all three IQ channels, composite AP (3 interactions).",
           COMP, COMP, c(WGE, ROL, VER)),
      spec("J-02 WGIge + RoL + HorAcc (composite AP)",
           "Stage-2 joint spec - Horizontal Accountability variant (expect RoL-HorAcc VIF stress).",
           COMP, COMP, c(WGE, ROL, HOR)),
      spec("J-03 WGIge + RoL + DiagAcc (composite AP)",
           "Stage-2 joint spec - Diagonal Accountability variant.",
           COMP, COMP, c(WGE, ROL, DIA)),
      spec("J-04 WGIge + RoL + VerAcc (split/split AP)",
           "Stage-2 joint spec - Actor Power Form: split mains AND split interactions (6 interactions).",
           SPLIT, SPLIT, c(WGE, ROL, VER)),
      # ── Panel Transform variants (ADR 0005) ───────────────────────────────────
      spec("FD-01 J-01 hybridFD",
           "Headline FD variant of J-01: deltaAP + IQ levels; hazard adoption, Gaussian delta-ECP stringency.",
           COMP, COMP, c(WGE, ROL, VER), transform = "hybridFD"),
      spec("FD-02 J-01 pureFD",
           "Pure first differences - expected to extinguish the institutional channels (ADR 0005).",
           COMP, COMP, c(WGE, ROL, VER), transform = "pureFD"),
      spec("FD-03 D4 hybridFD",
           "Headline FD variant of the D4 incumbent (WGIge only), levels-vs-FD comparison.",
           SPLIT, SPLIT, WGE, transform = "hybridFD"),
      # ── Incumbent benchmarks (to beat, never assumptions) ─────────────────────
      spec("B-D2 Split AP Logistic GDP-Q (incumbent)",
           "Incumbent publication spec: split/split AP, WGIge, GDP-Q + Hydro, logistic trend, H12.",
           SPLIT, SPLIT, WGE,
           overrides = list(controlDrivers = c("GDP per Capita (Q-centred)", "Hydro Nuclear Share"))),
      spec("B-D4 Split AP Logistic GDP-Q No-Hydro (incumbent)",
           "Incumbent best-per-sector spec: D2 without Hydro Nuclear Share.",
           SPLIT, SPLIT, WGE)
      # B-IQ01 (linear-trend incumbent) dropped 2026-06-15: the linear trend is
      # unbounded (year-1999 -> ~101 by 2100) and would carry an unbounded
      # extrapolation into projection. All retained specs use the bounded
      # saturating logistic trend.
    ))
  }

  # ── exhaustive ────────────────────────────────────────────────────────────────
  # State capacity is measured by WGI Government Effectiveness (2026-06-16): the
  # V-Dem State-Capacity PCA (PC1/PC2) was dropped per the user's directive —
  # WGI GovEff is the literature-preferred, directly interpretable state-capacity
  # measure, and PC1/PC2 are not used in any future run.
  govEffOptions <- list("WGIge" = WGE, "noGE" = character(0))
  rolOptions <- list("RoL" = ROL, "noRoL" = character(0))
  accOptions <- list("VerAcc" = VER, "HorAcc" = HOR, "DiagAcc" = DIA,
                     "noAcc" = character(0))
  # Option 2b (2026-06-14): hybrid form removed (split mains + composite-index
  # interactions duplicated information -> ill-conditioned coefficients ->
  # projection explosion). Only consistent forms remain.
  apForms <- list("compAP" = list(drivers = COMP, index = COMP),
                  "splitAP" = list(drivers = SPLIT, index = SPLIT))
  # The per-capita forms are NOT added here: doing so interleaves them into the
  # nested crosses and renumbers every X-NNNN downstream, breaking the numbering
  # stability this file has maintained through three previous additions. They are
  # appended as their own block at the end instead - see `apPcForms` below.
  pcForms <- list("splitAPpc" = list(drivers = SPLITPC, index = SPLITPC),
                  "mixedAP"   = list(drivers = MIXED,   index = MIXED),
                  "bothIncAP" = list(drivers = BOTHINC, index = BOTHINC))
  unknown <- setdiff(apPcForms, names(pcForms))
  if (length(unknown)) {
    stop("channelSpecs: unknown apPcForms: ", paste(unknown, collapse = ", "),
         ". Available: ", paste(names(pcForms), collapse = ", "))
  }
  pcForms <- pcForms[apPcForms]

  # ADR 0011 — control-set axis (8 curated sets incl. no-controls, no-GDP, and
  # the GDP-Q vs log-GDP comparison) and region-FE axis (5 levels incl. Mundlak).
  controlSets <- list(
    "ctlNone"          = character(0),
    "GDPq"             = GDPQ,
    "GDPq.sq"          = c(GDPQ, GDPQ2),
    "Pop.Hyd"          = c(LOGPOP, HYDRO),
    "GDPq.Pop.Hyd"     = c(GDPQ, LOGPOP, HYDRO),
    "GDPq.sq.Pop.Hyd"  = c(GDPQ, GDPQ2, LOGPOP, HYDRO),
    "lnGDP.sq"         = c(LOGGDP, LOGGDP2),
    "lnGDP.sq.Pop.Hyd" = c(LOGGDP, LOGGDP2, LOGPOP, HYDRO)
  )
  # FE axis applies to LEVELS specs only (FD transforms difference region effects
  # out). Mundlak = within-region group means instead of dummies (fe = NULL,
  # useMundlak = TRUE).
  # Candidate set restricted to the defensible block-FE strategies (ADR 0011, revised
  # 2026-06-19 per FULL_RERUN_DECISIONS #10 / Discussion 2): `noFE` (pooled -> omitted-
  # variable bias) and `FE54` (54 region dummies -> quasi-separation / degenerate inflated
  # deltaR2) are NOT swept as maximin candidates; they are fit separately only as labelled
  # bias-variance baselines. This shrinks the candidate space and the multiple-comparisons
  # exposure and removes the circular "include a degenerate option then gate it out".
  feLevels <- list(
    "H12"     = list(fe = "regionmappingH12.csv",       mundlak = FALSE),
    "OECDp"   = list(fe = "regionmapping_EU_OECDp.csv", mundlak = FALSE),
    "Mundlak" = list(fe = NULL,                         mundlak = TRUE)
  )
  # Reported baselines only (not crossed with the IQ x AP x control grid; not in the
  # maximin pool). Generation of the baseline fits is a follow-up (see ADR 0011).
  feBaselines <- list(
    "noFE" = list(fe = NULL,                   mundlak = FALSE),
    "FE54" = list(fe = "regionmapping_54.csv", mundlak = FALSE)
  )

  specs <- list()
  idx <- 0L
  for (ge in names(govEffOptions)) {
    for (rl in names(rolOptions)) {
      for (ac in names(accOptions)) {
        iq <- c(govEffOptions[[ge]], rolOptions[[rl]], accOptions[[ac]])
        if (length(iq) == 0) next
        for (ap in names(apForms)) {
          for (cs in names(controlSets)) {
            # levels: full cross with the FE axis
            for (fl in names(feLevels)) {
              idx <- idx + 1L
              specs[[idx]] <- spec(
                sprintf("X-%04d %s|%s|%s %s lev ctl:%s fe:%s", idx, ge, rl, ac, ap, cs, fl),
                paste0("Exhaustive cross (ADR 0011): GovEff=", ge, ", RoL=", rl,
                       ", Acc=", ac, ", AP=", ap, ", controls=", cs, ", FE=", fl,
                       ", levels."),
                apForms[[ap]]$drivers, apForms[[ap]]$index, iq, transform = "levels",
                overrides = list(controlDrivers = controlSets[[cs]],
                                 regionMappingFixedEffects = feLevels[[fl]]$fe,
                                 useMundlak = feLevels[[fl]]$mundlak)
              )
            }
            # hybridFD: no FE axis (region effects differenced out)
            idx <- idx + 1L
            specs[[idx]] <- spec(
              sprintf("X-%04d %s|%s|%s %s hybFD ctl:%s", idx, ge, rl, ac, ap, cs),
              paste0("Exhaustive cross (ADR 0011): GovEff=", ge, ", RoL=", rl,
                     ", Acc=", ac, ", AP=", ap, ", controls=", cs,
                     ", hybridFD (FE differenced out)."),
              apForms[[ap]]$drivers, apForms[[ap]]$index, iq, transform = "hybridFD",
              overrides = list(controlDrivers = controlSets[[cs]],
                               regionMappingFixedEffects = NULL, useMundlak = FALSE)
            )
          }
        }
      }
    }
  }
  idx <- idx + 1L
  specs[[idx]] <- spec(
    sprintf("X-%04d WGIge|RoL|VerAcc compAP pureFD", idx),
    "pureFD demonstration (ADR 0005): everything differenced. D4 backbone.",
    COMP, COMP, c(WGE, ROL, VER), transform = "pureFD"
  )

  # ── Capture-channel specs (R9, 2026-07-06) — appended AFTER the main cross so
  # existing X-numbers (and any comparison with earlier sweeps) stay stable.
  # Tests the mechanism behind the positive Incumbent x GovEff interaction of the
  # first PSM run: incumbent influence should operate through CORRUPTIBILITY, so
  # Control of Corruption is offered as an IQ channel (alone and paired). Panel
  # already carries the variable — no new data.
  COR <- "Control of Corruption (WGI)"
  captureCombos <- list(
    list(nm = "Cor",       iq = COR),
    list(nm = "WGIge.Cor", iq = c(WGE, COR)),
    list(nm = "RoL.Cor",   iq = c(ROL, COR))
  )
  for (ap in names(apForms)) {
    for (cc in captureCombos) {
      for (cs in c("ctlNone", "GDPq")) {
        for (fl in names(feLevels)) {
          idx <- idx + 1L
          specs[[idx]] <- spec(
            sprintf("X-%04d cap:%s %s lev ctl:%s fe:%s", idx, cc$nm, ap, cs, fl),
            paste0("Capture channel (R9): IQ=", cc$nm, ", AP=", ap, ", controls=", cs,
                   ", FE=", fl, ", levels."),
            apForms[[ap]]$drivers, apForms[[ap]]$index, cc$iq, transform = "levels",
            overrides = list(controlDrivers = controlSets[[cs]],
                             regionMappingFixedEffects = feLevels[[fl]]$fe,
                             useMundlak = feLevels[[fl]]$mundlak)
          )
        }
      }
    }
  }

  # ── Diffusion-mechanism specs (R7, 2026-07-06) — EU-acquis accession dummy as
  # an additional control next to the trend: whether it takes over trend share is
  # directly readable from the trendShare metric. Derived in preparePanelData
  # (region + year), projection-safe; aggregates uncoded pending composition
  # verification (see preparePanelData).
  EUM <- "EU Membership"
  for (ap in names(apForms)) {
    for (cs in list(c(EUM), c(GDPQ, EUM))) {
      for (fl in names(feLevels)) {
        idx <- idx + 1L
        csNm <- if (length(cs) == 1) "EU" else "GDPq.EU"
        specs[[idx]] <- spec(
          sprintf("X-%04d dif:EU %s lev ctl:%s fe:%s", idx, ap, csNm, fl),
          paste0("Diffusion mechanism (R7): EU-acquis dummy, AP=", ap,
                 ", controls=", csNm, ", FE=", fl, ", levels."),
          apForms[[ap]]$drivers, apForms[[ap]]$index, c(WGE, VER), transform = "levels",
          overrides = list(controlDrivers = cs,
                           regionMappingFixedEffects = feLevels[[fl]]$fe,
                           useMundlak = feLevels[[fl]]$mundlak)
        )
      }
    }
  }

  # ── Context-dummies control block (ADR 0038, 2026-07-13) — appended AFTER the
  # existing crosses so X-numbers stay stable. One ±block switch adding the von
  # Dulong context controls (EU Membership + Transition Economy, derived in
  # preparePanelData) to every curated control set, across the full channel/AP/FE
  # grid — the control axis effectively doubles (8 -> 16 combinations). Levels
  # only: the transition dummy is time-invariant and EU near-invariant, so their
  # FD transforms are (near-)zero columns with no identifying variation.
  TRN <- "Transition Economy"
  CTX <- c(EUM, TRN)
  for (ge in names(govEffOptions)) {
    for (rl in names(rolOptions)) {
      for (ac in names(accOptions)) {
        iq <- c(govEffOptions[[ge]], rolOptions[[rl]], accOptions[[ac]])
        if (length(iq) == 0) next
        for (ap in names(apForms)) {
          for (cs in names(controlSets)) {
            for (fl in names(feLevels)) {
              idx <- idx + 1L
              specs[[idx]] <- spec(
                sprintf("X-%04d %s|%s|%s %s lev ctl:%s.Ctx fe:%s", idx, ge, rl, ac, ap, cs, fl),
                paste0("Context-dummies block (ADR 0038): GovEff=", ge, ", RoL=", rl,
                       ", Acc=", ac, ", AP=", ap, ", controls=", cs,
                       " + EU/transition dummies, FE=", fl, ", levels."),
                apForms[[ap]]$drivers, apForms[[ap]]$index, iq, transform = "levels",
                overrides = list(controlDrivers = c(controlSets[[cs]], CTX),
                                 regionMappingFixedEffects = feLevels[[fl]]$fe,
                                 useMundlak = feLevels[[fl]]$mundlak)
              )
            }
          }
        }
      }
    }
  }

  # ── Per-capita actor-power block (design-notes/0001, 2026-08-24) ───────────
  # APPENDED after every existing cross so X-numbers stay stable, exactly as the
  # capture-channel, EU-diffusion and context-dummies blocks were before it.
  #
  # The base grid's indices are SHARES of the energy system, which operationalise
  # STRUCTURAL power (dependence, lock-in) and are driven to a floor by a mitigation
  # scenario BY CONSTRUCTION - measured on v1, ~5 SD below the estimation range by
  # 2100 against ~1 SD for a per-capita level. These forms let selection compare that
  # against INSTRUMENTAL power (rents, employment, lobbying resources):
  #   splitAPpc - both indices per capita
  #   mixedAP   - innovator share (policy feedback), incumbent per capita
  #               (instrumental weight): the design note's recommendation
  # Levels only, and only across the FE axis with the full IQ x control cross, which
  # mirrors the base `lev` block so the comparison is like-for-like.
  for (ge in names(govEffOptions)) {
    for (rl in names(rolOptions)) {
      for (ac in names(accOptions)) {
        iq <- c(govEffOptions[[ge]], rolOptions[[rl]], accOptions[[ac]])
        if (length(iq) == 0) next
        for (ap in names(pcForms)) {
          for (cs in names(controlSets)) {
            for (fl in names(feLevels)) {
              idx <- idx + 1L
              specs[[idx]] <- spec(
                sprintf("X-%04d %s|%s|%s %s lev ctl:%s fe:%s", idx, ge, rl, ac, ap, cs, fl),
                paste0("Per-capita actor power (design-notes/0001): GovEff=", ge,
                       ", RoL=", rl, ", Acc=", ac, ", AP=", ap, ", controls=", cs,
                       ", FE=", fl, ", levels."),
                pcForms[[ap]]$drivers, pcForms[[ap]]$index, iq, transform = "levels",
                overrides = list(controlDrivers = controlSets[[cs]],
                                 regionMappingFixedEffects = feLevels[[fl]]$fe,
                                 useMundlak = feLevels[[fl]]$mundlak)
              )
            }
          }
        }
      }
    }
  }

  # ── Lagged stringency-price variants (2026-06-14) ──────────────────────────
  # Carbon prices are sticky; a lagged price both fits that and damps projection
  # swings. A lagged dependent + region FE incurs dynamic-panel (Nickell) bias, so
  # each lag spec is offered in TWO alternatives: (a) FE + split-panel-jackknife
  # bias correction, and (b) no region FE (bias-free by construction). Lag is on
  # the stringency stage only (includeLaggedECP); adoption is unchanged.
  lagSpec <- function(tag, iq, ap, fe, nickell) {
    s <- spec(
      sprintf("LAG-%s", tag),
      sprintf("Lagged stringency price: IQ=%s, AP=%s, %s.",
              paste(iq, collapse = "+"),
              if (identical(ap, COMP)) "composite" else "split",
              if (is.null(fe)) "no region FE (bias-free)" else "H12 FE + Nickell SPJ correction"),
      ap, ap, iq
    )
    s$includeLaggedECP <- TRUE
    s$nickellCorrection <- nickell
    s$regionMappingFixedEffects <- fe
    s
  }
  H12 <- "regionmappingH12.csv"
  specs <- c(specs, list(
    lagSpec("01 WGIge+RoL+VerAcc FE+SPJ", c(WGE, ROL, VER), COMP, H12, TRUE),
    lagSpec("02 WGIge+RoL+VerAcc noFE",   c(WGE, ROL, VER), COMP, NULL, FALSE),
    lagSpec("03 WGIge FE+SPJ",            WGE, COMP, H12, TRUE),
    lagSpec("04 WGIge noFE",              WGE, COMP, NULL, FALSE)
  ))

  # ── Saturating-price twins (ADR 0028) ────────────────────────────────────────
  # Each LEVELS spec gets a Pmax-bounded stringency twin (priceLink = "saturating",
  # E[price] = Pmax*logit^-1(Xbeta), Pmax = 1000). The twin is STRINGENCY-ONLY: runFitGrid
  # skips its adoption fits (priceLink does not change adoption, and twinning it would
  # duplicate every adoption row). Saturating is a level-price form, so only
  # transform == "levels" specs are twinned (FD/hybridFD/pureFD model delta-log-price,
  # where a Pmax-logit on the level is undefined). See CONTEXT.md "Saturating Stringency
  # Response" and ADR 0028.
  satTwins <- lapply(
    Filter(function(s) identical(s$panelTransform %||% "levels", "levels"), specs),
    function(s) {
      s$name <- paste0(s$name, " | satP")
      s$description <- paste0(s$description, " [Saturating price twin, Pmax=1000, stringency-only.]")
      s$priceLink <- "saturating"
      s$priceCeilingMax <- 1000
      s$stringencyOnly <- TRUE
      s
    })
  specs <- c(specs, satTwins)
  specs
}

#' @title createChannelConfigs
#' @description Writes the \code{channels-<mode>.yml} sweep configuration built by
#' \code{\link{channelSpecs}} to a directory (typically
#' \code{pfm-reports/reports/model-selection/model-configs/}).
#'
#' @param dir Character. Target directory (created if missing).
#' @param mode Character. \code{"guided"} or \code{"exhaustive"}.
#' @param overwrite Logical. Overwrite an existing file. Default: \code{FALSE}
#'   (the existing file is kept and its path returned).
#'
#' @return Invisible character: path to the YAML file.
#'
#' @export
#' @author Renato Rodrigues
createChannelConfigs <- function(dir, mode = c("guided", "exhaustive"),
                                 apPcForms = c("splitAPpc", "mixedAP", "bothIncAP"),
                                 overwrite = FALSE) {
  mode <- match.arg(mode)
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("createChannelConfigs: the 'yaml' package is required.")
  }
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  out <- file.path(dir, paste0("channels-", mode, ".yml"))
  if (file.exists(out) && !isTRUE(overwrite)) {
    message("createChannelConfigs: keeping existing ", out)
    return(invisible(out))
  }
  specs <- channelSpecs(mode, apPcForms = apPcForms)
  nSatTwin <- sum(vapply(specs, function(s) isTRUE(s$stringencyOnly), logical(1)))
  nFits <- (length(specs) - nSatTwin) * 4L + nSatTwin * 2L  # twins are stringency-only (ADR 0028)
  header <- paste0(
    "# PFM Model Selection - Institutional Quality Channels, ", toupper(mode), " configuration\n",
    "# GENERATED by pfm::createChannelConfigs() - do not edit by hand.\n",
    "# Generated: ", format(Sys.Date()), " - ", length(specs), " specs (incl. ", nSatTwin,
    " saturating stringency-only twins) = ", nFits, " fits.\n",
    "# Decisions: ADR 0004 (guided selection), ADR 0005 (Panel Transform), CONTEXT.md.\n\n"
  )
  writeLines(paste0(header, yaml::as.yaml(specs, indent.mapping.sequence = TRUE)), out)
  message("createChannelConfigs: wrote ", length(specs), " specs to ", out)
  invisible(out)
}
# nolint end
