# Tutorial 1: PFM Model Overview

> What the model is, why it exists, and how it works

## Table of Contents

1. [Why a Political Feasibility Model?](#why-a-political-feasibility-model)
2. [The Two-Stage Hurdle Model](#the-two-stage-hurdle-model)
3. [Sectors: Bulk and Diffuse](#sectors-bulk-and-diffuse)
4. [Variable Groups](#variable-groups)
5. [Data Structure](#data-structure)
6. [What the model outputs](#what-the-model-outputs)

---

## Why a Political Feasibility Model?

Integrated Assessment Models (IAMs) such as REMIND compute optimal carbon pricing pathways to meet climate targets. But carbon pricing adoption in the real world is not determined by optimality — it is determined by politics. Fossil fuel incumbents, institutional capacity, economic conditions, and public governance all shape whether a country adopts a carbon price and at what level.

The Political Feasibility Module (PFM) closes this gap by providing **empirically-estimated probabilities and price levels** as a function of observable political and economic variables. REMIND can use these estimates to generate politically-informed scenarios.

---

## The Two-Stage Hurdle Model

The PFM is a **two-stage hurdle model** — a standard econometric approach for outcomes that combine a binary decision with a continuous quantity.

```
Stage 1 — Adoption:    Is there a carbon price at all?   (binary: yes/no)
Stage 2 — Stringency:  How high is the price?            (continuous: $/tCO₂)
```

Stage 2 is only estimated on observations where adoption = 1 (ECP > 0). This correctly handles the mass of zeros (no policy) without forcing the continuous model to explain them.

### Stage 1: Adoption Model

A **logistic regression** predicting the probability that a region has an Effective Carbon Price (ECP) above zero in a given year.

By default, **Firth's penalised likelihood** (`logistf`) is used instead of standard maximum likelihood. This is because carbon pricing adoption is a rare event in many regions, leading to quasi-complete separation — a condition where logistic regression coefficients diverge to ±∞. Firth's method applies a log-determinant penalty to the likelihood, producing finite, well-behaved estimates even with rare events. See Tutorial 5 for when to turn this off.

### Stage 2: Stringency Model

A **GLM with a log link** predicting the carbon price level (ECP, in $/tCO₂) conditional on adoption.

By default, a **Gamma family** is used. Carbon prices are positive and right-skewed — the Gamma distribution is a natural fit because its variance scales with the mean. A Gaussian family with log link is available as an alternative (equivalent to OLS on log-prices).

The dependent variable is optionally log-transformed (`log(1 + ECP)`) before fitting. With Gamma + log link this double-logs the data; with Gaussian + log link it runs OLS on log-prices.

---

## Sectors: Bulk and Diffuse

The model is estimated **separately for two sectors**, reflecting that different political forces govern industry and households.

| Sector | Covered activities | Key political economy |
|---|---|---|
| **Bulk** | Electricity generation, heavy industry, large emitters | Fossil fuel incumbents (coal, oil/gas), energy-intensive firms. These actors have concentrated interests and strong lobbying power against carbon pricing. |
| **Diffuse** | Transport fuels, heating, households | More diffuse opposition — households and small businesses. Electrification of transport shifts the balance toward renewables lobbies. |

The **Actor Power Index** is computed with different weights for Bulk and Diffuse, reflecting which industries dominate each sector's political landscape.

---

## Variable Groups

Variables entering the model are organised into four groups.

### Actor Power Index (API)

A composite index aggregating the relative political weight of carbon-pricing innovators (renewables, electrified transport) versus incumbents (fossil fuels, fossil-fuel-intensive industry). Higher values mean stronger innovator power relative to incumbents.

The index is sector-specific (`Actor Power Index|Bulk`, `Actor Power Index|Diffuse`) and is also used in **interaction terms** with institutional quality — the effect of governance quality on adoption may depend on how powerful pro-climate actors are.

Individual actor power drivers (VRE share, electrification, coal share, oil/gas share, fossil industry share) can be included instead of the index if `actorPowerIndex = NULL`.

### Institutional Quality (IQ)

Governance indicators measuring the state capacity and quality needed to design and enforce a carbon pricing system.

| Variable | Source | What it captures |
|---|---|---|
| Government Effectiveness (WGI) | World Bank | Quality of public services, civil service, policy implementation |
| Control of Corruption (WGI) | World Bank | Extent of corruption in public and private spheres |
| Voice and Accountability (WGI) | World Bank | Political freedoms, civil society, media |
| Political Stability (WGI) | World Bank | Likelihood of political instability or violence |
| Regulatory Quality (WGI) | World Bank | Government ability to formulate sound regulations |
| Rule of Law (WGI) | World Bank | Property rights, contract enforcement, courts |
| Rule of Law (VDem) | V-Dem | Rule of law from V-Dem's liberal democracy framework |
| Vertical Accountability (VDem) | V-Dem | Electoral accountability to citizens |

### Controls

Variables accounting for economic and structural differences across regions.

| Variable | Rationale |
|---|---|
| GDP per Capita | Richer regions have more fiscal capacity and tend to adopt carbon pricing earlier |
| Population | Scale control |
| Land Area | Geographic scale control |
| Urban Population Share | Urban populations tend to be more supportive of climate policy |
| Gini Income Inequality Coefficient | High inequality may reduce political feasibility |
| Gender Inequality Index | Governance and development proxy |
| Energy Intensity | Countries dependent on energy-intensive industries face stronger opposition |

### Region Fixed Effects

Region dummy variables capturing all time-invariant region-level heterogeneity — culture, geography, historical institutions. Two resolution options are commonly used:

- `regionmapping_EU_OECDp.csv` — groups EU and OECD+ countries; gives a single FE dummy for this bloc
- `regionmappingH12.csv` — 12 broader regional blocs

---

## Data Structure

Panel data is a `magclass` object with dimensions:

```
[regions × years × variables]
   54   ×  23  ×   29
```

- **54 regions** — ISO country-level aggregates mapped to REMIND regions
- **23 years** — 2000 to 2022 (historical)
- **29 variables** — ECP for each sector, Actor Power Index, WGI/VDem indicators, control variables

Time lags (default: 1 year) are applied to all predictor variables, so the model predicts ECP in year *t* from drivers in year *t − 1*. This reduces simultaneity bias (high prices may themselves affect governance indicators).

---

## What the model outputs

After estimation, each sector-stage combination produces:

- **Coefficients** — log-odds (adoption) or log-price effect (stringency) per variable
- **Robust standard errors** — clustered by region (HC1 sandwich estimator)
- **Fit statistics** — AIC, BIC, AICc, HQIC, log-likelihood, pseudo-R²
- **Diagnostics** — VIF, separation detection, convergence flags

Projected onto scenario data (from REMIND's `fulldata.gdx`), these coefficients yield:

- **Adoption probability** per region-year under each scenario
- **Price stringency** per region-year conditional on adoption

See [Tutorial 2](02-running-estimation.md) for how to run the model and [Tutorial 4](04-statistics-and-diagnostics.md) for how to interpret all these outputs.
