# Tutorial 5 — Understanding the model

A conceptual orientation to *what* PFM estimates, so the run options and reports make sense.
Authoritative definitions live in `CONTEXT.md`; the design rationale in `docs/adr/`.

## The question

Given a region's energy, economic, and governance characteristics, **how politically feasible
is carbon pricing** — both *whether* a price is adopted and *how stringent* it is? PFM answers
this with a statistical model fit on historical data (2000–2022, 54 regions), then projects it
onto REMIND scenario trajectories.

## The two-stage hurdle model

Carbon pricing is modelled in two stages, because "no price" and "the level of a price that
exists" are different processes:

1. **Adoption stage** — a logistic regression for *whether* an effective carbon price exists.
   Uses **Firth's penalized logit** (`logistf`) to handle perfect separation in a small sample.
2. **Stringency stage** — a GLM for the *level* given adoption: a **Gaussian identity-link**
   model on `log(1 + ECP)` (so `E[log(1+ECP)] = Xβ`, `ECP = expm1(Xβ)` — not a Gamma/log
   double-log).

Each stage is estimated separately for two **sectors**:
- **Bulk** — industrial / power (large point sources).
- **Diffuse** — households / transport / buildings (dispersed sources).

## The drivers: three theoretical channels

Predictors group into theory channels (plus controls):

- **Actor Power** — the political weight of incumbents vs. clean-energy innovators: fossil
  primary-energy shares, electrification, VRE share, fossil-share-in-industry, etc. (built in
  `mrpfm` from IEA PE/FE + Ember). An **Actor Power Index** composites these.
- **Institutional Quality** — governance capacity to implement a price: V-Dem state-capacity
  components, WGI Government Effectiveness, Rule of Law, accountability indices.
- **Interaction** — Actor Power × Institutional Quality (does governance moderate the power of
  incumbents?).
- **Controls** — GDP per capita (and its square), population, hydro/nuclear share, etc.

## Theory tiers (Green / Blue / Yellow)

Each fit is scored by *how much of the theory survives* as significant:

- **Green** — a significant term in **all three** theory groups (Actor Power, Inst. Quality, Interaction).
- **Blue** — at least one group significant.
- **Yellow** — none.

Tier is the headline qualitative verdict on a specification.

## How the deliverable is selected

The sweep fits hundreds of specifications; selection picks one **shared specification** per
stage (the same formula for both sectors) by the **Maximin Selection Rule** (ADR 0012):

1. **Worse-sector tier first** — rank by the *weaker* of the two sectors' tiers (a model must
   work in both).
2. **Mean ΔR²(theory)** — tie-break by theory's unique explanatory contribution (pseudo-R² lost
   when all theory terms are stripped), averaged over sectors.
3. **BIC parsimony** — final tie-break toward the simpler model (within `nearTieEps`).

Hard gates exclude a spec: non-convergence, `maxVIF ≥ 10` (collinearity), or lagged-DV terms.

## The gates

- **Projection-Sanity gate** (ADR 0006/0012) — after maximin, candidates are walked in
  expanding batches and their REMIND-scenario projections checked against *severe* rules (price
  > 2000 USD/tCO₂, invalid prices, a never-adopting region block). The first spec that passes
  in both sectors is selected; if none passes, the least-flagged is taken and flagged. (Needs
  the scenario panel — i.e. a `gdxFile`; otherwise selection is pure maximin.)
- **Falsification Gate** (difference-first only, ADR 0014) — re-fits the candidate under
  `pureFD`: Actor Power must keep its sign and significance, Institutional Quality must vanish.
  A dynamic-identification check on whether the Actor-Power effect is real or a level artefact.

## Panel transforms (ADR 0005)

The axis that defines *what* is regressed:

- **levels** (default) — status logit / level GLM. Projectable onto scenarios.
- **hybridFD** — Actor Power differenced, Institutional Quality in levels; discrete-time
  hazard/onset sample.
- **pureFD** — all drivers differenced. Used by the Falsification Gate.

Only levels specs are projected (FD specs carry no projection method yet), which is why the
difference-first method re-estimates its winner in levels for deployment.

## Robustness checks (the post-processing steps)

- **Robustness Ladder** (ADR 0012) — one-knob perturbations of the deliverable (composite AP,
  single-IQ, first-difference, lagged DV, ridge) to see if the tier/ΔR² survives.
- **Parsimony frontier** — how the maximin winner shifts as the BIC tie-break tolerance grows.
- **Control specification-curve** — the deliverable refit across a grid of control sets.
- **LORO** (ADR 0013) — leave-one-region-out: 54-fold spatial refit for coefficient stability + out-of-sample prediction.
- **Temporal split** (ADR 0015) — train on early years, predict later years (out-of-time).
- **Subnational** (ADR 0016) — does folding in within-country instruments (California, RGGI, …) change the picture?

## The data pipeline (mrpfm → pfm)

`mrpfm` turns external sources into magpie objects via madrat `read*/calc*/convert*`:
Carbon Pricing Dashboard + EDGAR (effective carbon price), IEA PE/FE + Ember (Actor Power),
V-Dem + WGI (Institutional Quality), SSP2 (GDP/Population). `pfm::panelDataHistorical()`
assembles these into the 54-region × 2000–2022 training panel; `panelDataScenario()` builds the
future trajectory from a REMIND `fulldata.gdx`. The compute layer runs **offline from the
madrat cache** — see Tutorial 6.

## Where to go deeper

- Concepts & exact definitions: **`CONTEXT.md`**.
- Why each design choice: **`docs/adr/`** (0004 channels, 0005 transforms, 0010–0017 model decisions).
- The narrative writeup: **`pfm-reports/PFM_PAPER_DRAFT.md`**.
