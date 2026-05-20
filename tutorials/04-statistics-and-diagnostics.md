# Tutorial 4: Statistics and Diagnostics

> Interpreting AIC, BIC, pseudo-R², VIF, separation, and coefficients

## Table of Contents

1. [Reading the coefficient table](#reading-the-coefficient-table)
2. [Information criteria: AIC, BIC, AICc, HQIC](#information-criteria-aic-bic-aicc-hqic)
3. [Pseudo-R²: McFadden's measure](#pseudo-r-mcfaddens-measure)
4. [VIF: Variance Inflation Factor](#vif-variance-inflation-factor)
5. [Separation detection](#separation-detection)
6. [Convergence and warnings](#convergence-and-warnings)
7. [Overfitting](#overfitting)
8. [Correlation matrices](#correlation-matrices)
9. [Quick diagnostic checklist](#quick-diagnostic-checklist)

---

## Reading the coefficient table

```r
m <- loadPFMModel("a3f2c1b8e9d0")
print(m$coeftest)
```

The table has four columns:

| Column | Meaning |
|---|---|
| `Estimate` | The regression coefficient (interpretation depends on stage — see below) |
| `Std. Error` | Robust standard error, clustered by region (HC1 sandwich estimator) |
| `z value` | Estimate ÷ Std. Error — the test statistic |
| `Pr(>|z|)` | Two-sided p-value for H₀: coefficient = 0 |

Significance convention: `***` p < 0.001, `**` p < 0.01, `*` p < 0.05, `.` p < 0.1.

### Interpreting adoption coefficients (Stage 1)

The adoption model is a logistic regression. Coefficients are **log-odds ratios**.

- A **positive** coefficient means higher values of that variable are associated with a higher probability of carbon pricing adoption.
- A coefficient of β means a one-unit increase in the variable multiplies the **odds** of adoption by exp(β).

**Example:** If the coefficient on `Government Effectiveness (WGI)` is 1.2, then a one-unit improvement in governance multiplies the odds of adoption by exp(1.2) ≈ 3.3.

To convert log-odds to a change in probability, the baseline probability matters:

- At p = 0.5 (50% baseline), a log-odds increase of 1 raises probability by roughly 25 percentage points.
- Near the tails (p = 0.1 or p = 0.9), the same log-odds change has a much smaller effect on probability.

### Interpreting stringency coefficients (Stage 2)

The stringency model is a GLM with a log link. Coefficients are **log-scale effects on the carbon price**, conditional on adoption having occurred.

- A **positive** coefficient means higher values of that variable are associated with a higher carbon price.
- A coefficient of β means a one-unit increase in the variable multiplies the expected price by exp(β).

**Example:** A coefficient of 0.5 on `GDP per Capita` means a one-unit increase is associated with a 65% higher carbon price (exp(0.5) ≈ 1.65).

**Note on the log-transform:** If `logTransform = TRUE` (default), the dependent variable is `log(1 + ECP)`. Coefficients still have the same qualitative interpretation, but their magnitude is on the log-log scale. See [Tutorial 5](05-model-options.md) for when to disable this.

---

## Information criteria: AIC, BIC, AICc, HQIC

```r
m$diagnostics$aic
m$diagnostics$bic
m$diagnostics$aicc
m$diagnostics$hqic
```

These metrics compare model fit while penalising complexity. All take the form:

```
Criterion = −2 × log-likelihood + penalty × k
```

where k is the number of parameters and the penalty differs:

| Criterion | Penalty | Interpretation |
|---|---|---|
| **AIC** | 2 | General use; asymptotically efficient |
| **BIC** | log(n) | Consistent; favours parsimony; penalty grows with sample size |
| **AICc** | 2k(k+1)/(n−k−1) + 2 | AIC corrected for small samples; converges to AIC as n → ∞ |
| **HQIC** | 2·log(log(n)) | Intermediate between AIC and BIC |

**Lower is always better.** A difference between two models of:

- < 2: negligible — models are essentially equivalent
- 2–6: modest — the lower model has some support
- > 10: substantial — the lower model is clearly preferred

### When AIC and BIC disagree

AIC favours predictive accuracy; BIC favours parsimony. If they point to different models:

1. Is the sample size large? BIC is more trustworthy with large n.
2. Are the extra variables in the AIC winner theoretically justified?
3. Does the AIC winner have better pseudo-R² and significant coefficients?

The selection path log explicitly reports conflicts — look for `[Conflict]` lines in `result$evalLogs`.

---

## Pseudo-R²: McFadden's measure

```r
m$diagnostics$pseudoR2
```

Defined as:

```
pseudo-R² = 1 − (log-likelihood of fitted model / log-likelihood of null model)
```

The null model contains only an intercept. A pseudo-R² of 0 means the model explains nothing beyond the null; 1 means perfect fit.

| Pseudo-R² | Interpretation |
|---|---|
| < 0.1 | Weak |
| 0.1 – 0.2 | Acceptable |
| 0.2 – 0.4 | **Good to excellent** (common target for logistic models) |
| > 0.4 | Very strong — check for overfitting |

Unlike OLS R², pseudo-R² is **not** the proportion of variance explained. A value of 0.25 means the log-likelihood improved by 25% relative to the null, not that the model explains 25% of variation.

---

## VIF: Variance Inflation Factor

```r
m$diagnostics$vif$values   # VIF for each predictor
m$diagnostics$vif$maxVIF   # maximum VIF across all predictors
m$diagnostics$vif$highVIF  # TRUE if any VIF > 5
m$diagnostics$vif$flagged  # names of variables with VIF > 5
```

VIF measures multicollinearity — how much of a predictor's variance is explained by the other predictors. High VIF means the model cannot reliably separate individual effects of correlated variables.

| VIF | Severity |
|---|---|
| 1 | No multicollinearity |
| 1–5 | Acceptable |
| **5–10** | **Moderate — flagged in pfm** |
| > 10 | Severe — coefficients are unreliable |

During model selection, any model with VIF > 5 is **automatically rejected**, regardless of AIC.

**Common causes in PFM:** WGI indicators are highly correlated with each other and with GDP per Capita. Including several WGI indicators simultaneously raises VIF sharply. The group analysis table (Tutorial 3) helps identify which group causes the collinearity.

**What to do:** Remove correlated variables, or use a composite (such as the Actor Power Index, which aggregates correlated individual drivers into one variable).

---

## Separation detection

```r
m$diagnostics$separation  # TRUE if structural separation detected
m$diagnostics$highZ       # TRUE if extreme z-values detected
m$diagnostics$maxAbsZ     # maximum |z| across all coefficients (excluding intercept)
```

**Quasi-complete separation** occurs when a linear combination of predictors perfectly (or near-perfectly) predicts the binary outcome. For carbon pricing adoption — a rare event in many regions — separation is common.

Two checks are applied:

1. **Structural separation:** Any absolute coefficient > 10, or extreme fitted probabilities (near 0 or 1).
2. **Z-based separation:** Any |z| > 15 (the `maxZThreshold`), even when abs(coef) ≤ 10. This catches cases where a variable essentially perfectly separates the outcome but Firth's penalty has compressed the coefficient.

**What happens without Firth:** Standard logistic regression (MLE) produces coefficients that diverge to ±∞ and standard errors that blow up. The model appears to converge, but the estimates are meaningless.

**What Firth's method does:** Applies a log-determinant penalty (Jeffrey's prior) to the likelihood. This pulls extreme estimates toward finite values. In pfm, `useFirth = TRUE` (default for adoption) uses `logistf::logistf()`.

**What to do if separation persists even with Firth:** The flagged variable essentially perfectly predicts adoption in your dataset and cannot be included as a predictor. Remove it from the candidate pool for that sector-stage combination.

---

## Convergence and warnings

```r
m$diagnostics$converged       # did the optimiser converge?
m$diagnostics$maxitWarning    # did it hit the iteration limit?
m$diagnostics$rejectionReason # human-readable reason if the model was rejected
```

A model that failed to converge (`converged = FALSE`) has unreliable coefficient estimates. During model selection, these are automatically rejected.

**Common causes:**

- Separation — the most frequent cause for adoption models
- Too many parameters relative to observations (kOverN > 0.1)
- Near-singular data matrix from correlated predictors

---

## Overfitting

```r
m$diagnostics$kOverN      # ratio: n parameters / n observations
m$diagnostics$overfitting  # TRUE if kOverN > 0.1 (fewer than 10 obs per parameter)
```

The kOverN ratio is a simple overfitting heuristic. If k/n > 0.1 — fewer than 10 observations per parameter — the model is at high risk of fitting noise rather than signal.

This is flagged but **not automatically rejected**, because kOverN should be assessed alongside pseudo-R² and theoretical motivation. A model with kOverN = 0.12 and a strong theoretical basis may still be useful; a model with kOverN = 0.25 and a marginal fit gain is not.

---

## Correlation matrices

Each saved model stores Pearson and Spearman correlation matrices computed on the variables that appear in its formula (not the full panel data):

```r
m$correlations$pearson   # linear pairwise correlations
m$correlations$spearman  # rank pairwise correlations
```

These are diagnostic tools for understanding multicollinearity **before** running a VIF check. High pairwise correlations (|r| > 0.7) between predictors are a warning sign. The Spearman version is more robust to outliers and non-linearity in the predictors.

To see correlations across all variables in the full panel data (not just the model's variables), use `computeCorrelationMatrix(panelData, variableGroup)`.

---

## Quick diagnostic checklist

After fitting or loading a model, run through this checklist:

| Check | Command | Green flag |
|---|---|---|
| Convergence | `m$diagnostics$converged` | `TRUE` |
| No separation | `m$diagnostics$separation` | `FALSE` |
| No extreme z-values | `m$diagnostics$highZ` | `FALSE` |
| Acceptable VIF | `m$diagnostics$vif$highVIF` | `FALSE` |
| Reasonable fit | `m$diagnostics$pseudoR2` | 0.15 – 0.40 |
| Not overfitted | `m$diagnostics$kOverN` | < 0.10 |
| At least one significant predictor | `m$diagnostics$nSignificant` | ≥ 1 |
