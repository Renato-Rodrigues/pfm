#' @title diagnoseModelResults
#' @description Analyses results from one or more \code{modelSelectionWorkflow}
#'   outputs and returns a structured list with dynamic diagnosis text, quality
#'   ratings, issue flags, and IAM readiness assessments for every
#'   sector × stage combination.
#'
#' @param workflows Named list of workflow results, e.g.
#'   \code{list(md = md, md2 = md2)}.  Each element must be the return value of
#'   \code{\link{modelSelectionWorkflow}}.
#' @param maxZThreshold Numeric. Z-value above which a coefficient is considered
#'   to indicate quasi-complete separation. Default: \code{15}.
#' @param r2GoodThreshold  Numeric. Pseudo-R² threshold for "Good" fit.
#'   Default: \code{0.4}.
#' @param r2ModThreshold   Numeric. Pseudo-R² threshold for "Moderate" fit.
#'   Default: \code{0.2}.
#' @param vifThreshold     Numeric. VIF above which multicollinearity is a
#'   concern. Default: \code{10}.
#'
#' @return A list with components:
#'   \describe{
#'     \item{\code{modelDiagnoses}}{Named list (workflow × combo) of per-model
#'       diagnosis lists, each containing \code{fitQuality}, \code{verdict},
#'       \code{verdictIcon}, \code{separationDetail}, \code{extremeZTerms},
#'       \code{topPredictors}, \code{iamReady}, \code{iamRationale}, and
#'       \code{issues}.}
#'     \item{\code{globalFindings}}{Character. Multi-bullet executive summary
#'       built from actual data.}
#'     \item{\code{issuesTable}}{data.frame. Prioritised issue table across all
#'       models, suitable for \code{kable()}.}
#'     \item{\code{readinessTable}}{data.frame. Per-model IAM readiness
#'       assessment table.}
#'     \item{\code{comparisonNarrative}}{Named list with adoption and stringency
#'       comparison text.}
#'     \item{\code{fitSummaryNarrative}}{Character. Narrative paragraph for the
#'       model fit summary section.}
#'   }
#'
#' @author Renato Rodrigues
#' @export
diagnoseModelResults <- function(
    workflows,
    maxZThreshold = 15,
    r2GoodThreshold = 0.4,
    r2ModThreshold = 0.2,
    vifThreshold = 10) {
  # ---- helpers ---------------------------------------------------------------
  .fitQualityLabel <- function(r2) {
    if (length(r2) == 0 || is.na(r2) || r2 < 0) {
      return(list(label = "Failed", colour = "bad"))
    }
    if (r2 >= r2GoodThreshold) {
      return(list(label = "Good", colour = "good"))
    }
    if (r2 >= r2ModThreshold) {
      return(list(label = "Moderate", colour = "warn"))
    }
    list(label = "Poor", colour = "bad")
  }

  .extremeZ <- function(coefs, threshold) {
    if (is.null(coefs) || nrow(coefs) == 0) {
      return(character(0))
    }
    non_int <- coefs[coefs$term != "(Intercept)", ]
    non_int$term[abs(non_int$statistic) > threshold]
  }

  .topPredictors <- function(coefs, n = 3) {
    if (is.null(coefs) || nrow(coefs) == 0) {
      return(character(0))
    }
    non_int <- coefs[coefs$term != "(Intercept)", ]
    sig <- non_int[!is.na(non_int$pValue) & non_int$pValue < 0.05, ]
    if (nrow(sig) == 0) {
      return(character(0))
    }
    sig <- sig[order(abs(sig$statistic), decreasing = TRUE), ]
    head(sig$term, n)
  }

  .verdictForModel <- function(diag, r2, stage) {
    extreme_z <- length(diag$extremeZTerms) > 0
    sep <- isTRUE(diag$separation)
    fq <- diag$fitQuality$label
    converged <- isTRUE(diag$converged)

    if (!converged) {
      return(list(icon = "\U0001f6a8", text = "Not converged — model failed to fit. Do not use.", ready = "no"))
    }
    if (extreme_z) {
      return(list(icon = "\U0001f6a8", text = paste0(
        "Critical separation detected (extreme z-value on: ",
        paste(diag$extremeZTerms, collapse = ", "),
        "). Coefficient estimates are numerically unreliable. Do not use for IAM projection without structural fix."
      ), ready = "no"))
    }
    if (stage == "adoption" && sep && fq == "Good") {
      return(list(
        icon = "\u26a0\ufe0f",
        text = "Good fit but separation flag detected. logistf handles this via penalised ML, but prediction intervals should be widened beyond the historical predictor range.",
        ready = "monitor"
      ))
    }
    if (stage == "stringency" && fq == "Poor") {
      return(list(
        icon = "\u26a0\ufe0f",
        text = paste0(
          "Low pseudo-R\u00b2 (", round(r2, 3), "). The model captures little variation in price levels — ",
          "acceptable as a conservative baseline but should not be over-interpreted."
        ),
        ready = "caution"
      ))
    }
    if (fq == "Good") {
      return(list(
        icon = "\u2705",
        text = "Well-specified and well-converged. Suitable for IAM projection.",
        ready = "yes"
      ))
    }
    if (fq == "Moderate") {
      return(list(
        icon = "\u26a0\ufe0f",
        text = paste0("Moderate fit (pseudo-R\u00b2 = ", round(r2, 3), "). Use with caution; supplement with expert judgement."),
        ready = "caution"
      ))
    }
    list(
      icon = "\u26a0\ufe0f",
      text = paste0("Poor fit (pseudo-R\u00b2 = ", round(r2, 3), "). Limited explanatory power."),
      ready = "caution"
    )
  }

  .iamReadinessLabel <- function(ready) {
    switch(ready,
      yes     = "\u2705 Ready",
      monitor = "\u26a0\ufe0f Monitor separation",
      caution = "\u26a0\ufe0f Use with caution",
      no      = "\u274c Not ready"
    )
  }

  # ---- main loop over workflows × combos ------------------------------------
  sectors <- c("Bulk", "Diffuse")
  stages <- c("adoption", "stringency")
  modelDiagnoses <- list()
  allIssues <- list()
  allReadiness <- list()

  for (wfName in names(workflows)) {
    wf <- workflows[[wfName]]
    modelDiagnoses[[wfName]] <- list()

    for (sec in sectors) {
      for (stg in stages) {
        comboName <- paste(sec, stg, sep = "_")
        bm_row <- wf$bestModels[wf$bestModels$sector == sec &
          wf$bestModels$stage == stg, ]
        bestDiag <- wf$selections[[comboName]]$bestModel

        r2 <- if (length(bm_row$pseudoR2) > 0) bm_row$pseudoR2 else NA_real_
        aic_val <- if (length(bm_row$aic) > 0) bm_row$aic else NA_real_
        n_sig <- if (length(bm_row$nSignificant) > 0) bm_row$nSignificant else NA_integer_
        n_pred <- if (length(bm_row$nPredictors) > 0) bm_row$nPredictors else NA_integer_
        max_vif <- if (length(bm_row$maxVIF) > 0) bm_row$maxVIF else NA_real_
        converged <- isTRUE(bestDiag$converged)
        separation <- isTRUE(bestDiag$separation)

        coefs <- tryCatch(
          coeftestToDataFrame(bestDiag$coeftest),
          error = function(e) NULL
        )

        extremeZ <- .extremeZ(coefs, maxZThreshold)
        topPred <- .topPredictors(coefs, n = 3)
        fq <- .fitQualityLabel(r2)

        diag_entry <- list(
          sector = sec,
          stage = stg,
          model = wfName,
          r2 = r2,
          aic = aic_val,
          nSig = n_sig,
          nPred = n_pred,
          maxVIF = max_vif,
          converged = converged,
          separation = separation,
          fitQuality = fq,
          extremeZTerms = extremeZ,
          topPredictors = topPred
        )
        diag_entry$verdict <- .verdictForModel(diag_entry, r2, stg)
        diag_entry$iamReady <- diag_entry$verdict$ready
        diag_entry$iamRationale <- diag_entry$verdict$text
        diag_entry$verdictIcon <- diag_entry$verdict$icon

        # Build per-model issues
        issues <- list()
        if (length(extremeZ) > 0) {
          for (z_term in extremeZ) {
            z_val <- coefs$statistic[coefs$term == z_term]
            issues <- c(issues, list(data.frame(
              Priority = "\U0001f534 Critical",
              Model = paste0(wfName, " — ", sec, " ", stg),
              Issue = paste0(
                z_term, " has |z| = ", round(abs(z_val), 1),
                " — quasi-complete separation"
              ),
              Consequence = paste0(
                "Coefficient for '", z_term, "' is numerically unstable. ",
                "ECP projections for ", sec, " ", stg, " in model ", wfName, " will be biased."
              ),
              Fix = "Remove or regularise the separating variable; check subsample sparsity.",
              stringsAsFactors = FALSE
            )))
          }
        }
        if (stg == "adoption" && separation && length(extremeZ) == 0) {
          issues <- c(issues, list(data.frame(
            Priority = "\U0001f7e1 Important",
            Model = paste0(wfName, " — ", sec, " ", stg),
            Issue = "Separation flag = TRUE (structural, not z-based)",
            Consequence = "Confidence intervals may be too narrow near prediction boundaries.",
            Fix = "logistf is appropriate; widen prediction intervals in IAM output.",
            stringsAsFactors = FALSE
          )))
        }
        if (!is.na(max_vif) && max_vif > vifThreshold) {
          issues <- c(issues, list(data.frame(
            Priority = "\U0001f7e1 Important",
            Model = paste0(wfName, " — ", sec, " ", stg),
            Issue = paste0("Max VIF = ", round(max_vif, 1), " — multicollinearity concern"),
            Consequence = "Inflated standard errors; coefficient estimates are unstable.",
            Fix = "Remove or combine highly correlated predictors.",
            stringsAsFactors = FALSE
          )))
        }
        if (!is.na(r2) && r2 < r2ModThreshold && stg == "stringency") {
          issues <- c(issues, list(data.frame(
            Priority = "\U0001f7e1 Important",
            Model = paste0(wfName, " — ", sec, " ", stg),
            Issue = paste0("Pseudo-R\u00b2 = ", round(r2, 3), " — very poor model fit"),
            Consequence = paste0("Model captures only ", round(r2 * 100, 1), "% of variation in ECP levels."),
            Fix = "Consider Bayesian shrinkage, sector-specific intercepts, or additional drivers.",
            stringsAsFactors = FALSE
          )))
        }
        diag_entry$issues <- issues
        allIssues <- c(allIssues, issues)

        modelDiagnoses[[wfName]][[comboName]] <- diag_entry

        # Readiness row
        allReadiness <- c(allReadiness, list(data.frame(
          Model = paste0(sec, " ", tools::toTitleCase(stg), " (", wfName, ")"),
          Ready = .iamReadinessLabel(diag_entry$iamReady),
          Rationale = diag_entry$iamRationale,
          stringsAsFactors = FALSE
        )))
      }
    }
  }

  # ---- issues table ----------------------------------------------------------
  issuesTable <- if (length(allIssues) > 0) {
    df <- do.call(rbind, allIssues)
    df[order(grepl("stringency", df$Model, ignore.case = TRUE), df$Priority), ]
  } else {
    data.frame(
      Priority = character(), Model = character(), Issue = character(),
      Consequence = character(), Fix = character()
    )
  }

  # ---- readiness table -------------------------------------------------------
  readinessTable <- do.call(rbind, allReadiness)
  readinessTable <- readinessTable[order(grepl("stringency", readinessTable$Model, ignore.case = TRUE)), ]
  rownames(readinessTable) <- NULL

  # ---- global findings -------------------------------------------------------
  all_r2 <- sapply(names(workflows), function(wfn) {
    sapply(
      c("Bulk_adoption", "Diffuse_adoption", "Bulk_stringency", "Diffuse_stringency"),
      function(cn) modelDiagnoses[[wfn]][[cn]]$r2
    )
  })
  adopt_r2 <- all_r2[c("Bulk_adoption", "Diffuse_adoption"), , drop = FALSE]
  str_r2 <- all_r2[c("Bulk_stringency", "Diffuse_stringency"), , drop = FALSE]
  n_crit <- sum(issuesTable$Priority == "\U0001f534 Critical")

  globalFindings <- paste0(
    "* Adoption models achieve pseudo-R\u00b2 ranging from **",
    round(min(adopt_r2, na.rm = TRUE), 3), "** to **",
    round(max(adopt_r2, na.rm = TRUE), 3),
    "** — placing them in the **",
    if (min(adopt_r2, na.rm = TRUE) >= r2GoodThreshold) "Good" else "Moderate",
    "** fit range. The political-feasibility driver framework successfully identifies the extensive margin of carbon pricing.\n",
    "* Stringency models are substantially weaker (pseudo-R\u00b2 ",
    round(min(str_r2, na.rm = TRUE), 3), "\u2013",
    round(max(str_r2, na.rm = TRUE), 3),
    "): once jurisdictions adopt carbon pricing, price levels reflect domestic policy design details not fully captured by macro-level governance indicators.\n",
    if (n_crit > 0) {
      paste0(
        "* **", n_crit, " critical separation issue(s)** were detected across models. ",
        "Affected models must not be used for IAM projection without structural fixes.\n"
      )
    } else {
      "* No critical separation issues were detected.\n"
    },
    "* The more parsimonious workflow variants (API composite rather than individual AP drivers) ",
    "are generally preferred for out-of-sample projection because the Actor Power Index is ",
    "directly available from REMIND scenario outputs."
  )

  # ---- comparison narratives -------------------------------------------------
  wf_names <- names(workflows)

  adoptNarrative <- if (length(wf_names) >= 2) {
    r2_w1 <- sapply(
      c("Bulk_adoption", "Diffuse_adoption"),
      function(cn) modelDiagnoses[[wf_names[1]]][[cn]]$r2
    )
    r2_w2 <- sapply(
      c("Bulk_adoption", "Diffuse_adoption"),
      function(cn) modelDiagnoses[[wf_names[2]]][[cn]]$r2
    )
    delta <- abs(mean(r2_w1) - mean(r2_w2))
    paste0(
      "The adoption models are nearly identical across variants ",
      "(mean \u0394Pseudo-R\u00b2 \u2248 ", round(delta, 3), "). ",
      "For IAM projection, the **", wf_names[length(wf_names)], "** variant is preferred: ",
      "it is more parsimonious and its Actor Power Index composite is directly projectable ",
      "from REMIND scenario outputs without requiring five separate energy-system drivers."
    )
  } else {
    ""
  }

  strNarrative <- if (length(wf_names) >= 2) {
    r2_bs <- sapply(wf_names, function(wfn) modelDiagnoses[[wfn]][["Bulk_stringency"]]$r2)
    r2_ds <- sapply(wf_names, function(wfn) modelDiagnoses[[wfn]][["Diffuse_stringency"]]$r2)
    ds_crit <- sapply(wf_names, function(wfn) {
      length(modelDiagnoses[[wfn]][["Diffuse_stringency"]]$extremeZTerms) > 0
    })
    paste0(
      "* **Bulk Stringency:** All variants select models with pseudo-R\u00b2 \u2248 ",
      paste(round(r2_bs, 3), collapse = " / "),
      ". Individual AP drivers contribute no additional explanatory power beyond the composite API.\n",
      "* **Diffuse Stringency:** ",
      paste(sapply(seq_along(wf_names), function(i) {
        paste0(
          wf_names[i], " achieves R\u00b2 = ", round(r2_ds[i], 3),
          if (ds_crit[i]) " but has critical separation" else ""
        )
      }), collapse = "; "),
      ". Neither variant is trustworthy for projection without structural redesign.\n",
      "> **Overall:** Stringency modelling remains the weakest component. ",
      "Bulk Stringency can serve as a conservative baseline; ",
      "Diffuse Stringency requires fundamental reformulation before deployment."
    )
  } else {
    ""
  }

  # ---- fit summary narrative -------------------------------------------------
  n_adopt_good <- sum(adopt_r2 >= r2GoodThreshold, na.rm = TRUE)
  n_str_poor <- sum(str_r2 < r2ModThreshold, na.rm = TRUE)
  sep_models <- sum(sapply(names(workflows), function(wfn) {
    any(sapply(
      c("Bulk_adoption", "Diffuse_adoption"),
      function(cn) isTRUE(modelDiagnoses[[wfn]][[cn]]$separation)
    ))
  }))
  max_vif_all <- max(sapply(names(workflows), function(wfn) {
    sapply(
      c("Bulk_adoption", "Diffuse_adoption", "Bulk_stringency", "Diffuse_stringency"),
      function(cn) {
        v <- modelDiagnoses[[wfn]][[cn]]$maxVIF
        if (is.na(v)) 0 else v
      }
    )
  }), na.rm = TRUE)

  fitSummaryNarrative <- paste0(
    "**Findings & Interpretation:**\n\n",
    "* **", n_adopt_good, " of ", length(wf_names) * 2,
    " adoption model(s)** achieve pseudo-R\u00b2 \u2265 ", r2GoodThreshold,
    ", confirming the political-feasibility framework successfully explains the ",
    "extensive margin of carbon-pricing policy.\n",
    "* **All stringency models** show substantially lower explanatory power (pseudo-R\u00b2 ",
    round(min(str_r2, na.rm = TRUE), 3), "\u2013",
    round(max(str_r2, na.rm = TRUE), 3),
    "). This is expected: price levels are driven by domestic policy design details ",
    "not captured by macro governance indicators.\n",
    if (sep_models > 0) {
      paste0(
        "* **Separation flags** appear across ", sep_models, " adoption model variant(s). ",
        "While `logistf` handles this via penalised ML, coefficients should be treated as ",
        "regularised estimates with wider true uncertainty.\n"
      )
    } else {
      "* No adoption separation flags detected.\n"
    },
    "* **Max VIF = ", round(max_vif_all, 1),
    "** across all models — multicollinearity is ",
    if (max_vif_all < vifThreshold) "well-controlled." else "a concern in some models.",
    "\n\n> **Key takeaway:** The hurdle architecture works well for adoption but stringency ",
    "remains a weak link. Adoption probabilities can be used with confidence; stringency ",
    "projections should be bounded and supplemented with expert judgement."
  )

  # ---- projection caveats ----------------------------------------------------
  # 1. Temporal extrapolation: extract time-trend beta from adoption models
  time_betas <- lapply(names(workflows), function(wfn) {
    lapply(c("Bulk", "Diffuse"), function(sec) {
      coefs <- tryCatch(
        coeftestToDataFrame(workflows[[wfn]]$selections[[paste0(sec, "_adoption")]]$bestModel$coeftest),
        error = function(e) NULL
      )
      if (is.null(coefs)) return(NULL)
      tr <- coefs[grepl("^year$|^time$|^trend$|Year|Time", coefs$term, ignore.case = TRUE), ]
      if (nrow(tr) == 0) return(NULL)
      list(wf = wfn, sector = sec, term = tr$term[1], beta = round(tr$estimate[1], 3))
    })
  })
  time_betas <- Filter(Negate(is.null), do.call(c, time_betas))
  if (length(time_betas) > 0) {
    beta_vals <- sapply(time_betas, `[[`, "beta")
    mean_beta <- round(mean(beta_vals, na.rm = TRUE), 3)
    time_caveat <- paste0(
      "**Temporal extrapolation:** The time trend coefficient averages **\u03b2 \u2248 ", mean_beta,
      " per year** across adoption models (",
      paste(sapply(time_betas, function(x) paste0(x$sector, " ", x$wf, ": \u03b2=", x$beta)), collapse = "; "),
      "). This will eventually push Pr(adopt) \u2192 1 for all regions. ",
      "Apply a **saturation cap (e.g., 95%)** in long-run projections beyond 2050."
    )
  } else {
    time_caveat <- paste0(
      "**Temporal extrapolation:** No explicit time trend term was found in the adoption models. ",
      "Verify whether secular adoption dynamics are captured by other drivers and ",
      "apply a saturation cap in long-run projections."
    )
  }

  # 2. Covariate extrapolation: identify which drivers are used across models
  all_terms <- unique(unlist(lapply(names(workflows), function(wfn) {
    unlist(lapply(c("Bulk_adoption", "Diffuse_adoption", "Bulk_stringency", "Diffuse_stringency"),
      function(cn) {
        coefs <- tryCatch(
          coeftestToDataFrame(workflows[[wfn]]$selections[[cn]]$bestModel$coeftest),
          error = function(e) NULL
        )
        if (is.null(coefs)) return(character(0))
        coefs$term[coefs$term != "(Intercept)"]
      }))
  })))
  gdp_used  <- any(grepl("gdp|GDP|income", all_terms, ignore.case = TRUE))
  pop_used  <- any(grepl("pop|Pop|population", all_terms, ignore.case = TRUE))
  coal_used <- any(grepl("coal|Coal", all_terms, ignore.case = TRUE))

  covariate_caveat <- paste0(
    "**Covariate extrapolation:** ",
    if (pop_used)
      "Population is used as a predictor — its extreme future-year SSP values may push projections out of the training range. "
    else
      "Population was excluded due to extreme future-year values. ",
    if (gdp_used)
      "GDP per Capita is included and may reach extreme values under SSP5 (very high growth) — monitor whether future values remain within the historical range. "
    else "",
    "Always scale future predictor values using historical (training-data) parameters — ",
    "never re-scale to 2100 maxima, as this compresses the training range and makes coefficients unidentifiable."
  )

  # 3. Separation-linked extrapolation caveats
  sep_terms_all <- unique(unlist(lapply(names(workflows), function(wfn) {
    unlist(lapply(c("Bulk_adoption", "Diffuse_adoption", "Bulk_stringency", "Diffuse_stringency"),
      function(cn) modelDiagnoses[[wfn]][[cn]]$extremeZTerms))
  })))
  not_ready_models <- readinessTable$Model[grepl("\u274c", readinessTable$Ready)]

  counterfactual_caveat <- if (length(sep_terms_all) > 0) {
    paste0(
      "**Counterfactual scenarios:** The following variables caused quasi-complete separation: **",
      paste(sep_terms_all, collapse = "**, **"),
      "**. When REMIND projects conditions that move these variables far from their historical range ",
      "(e.g., rapid coal phase-out, OECD expansion), the affected models will extrapolate through a region ",
      "where they were effectively fitting a step-function. ",
      if (length(not_ready_models) > 0)
        paste0("Models currently flagged Not Ready (",
               paste(not_ready_models, collapse = ", "),
               ") are most at risk and must not be used in such scenarios.")
      else
        "Apply bounding and expert review for all out-of-distribution scenario projections."
    )
  } else {
    paste0(
      "**Counterfactual scenarios:** No extreme separation variables were detected. ",
      "Standard out-of-sample projection caveats apply: monitor whether REMIND scenario ",
      "pathways remain within the historical predictor range."
    )
  }

  projectionCaveats <- list(
    temporal     = time_caveat,
    covariate    = covariate_caveat,
    scaling      = paste0(
      "**Historical scaling:** Always apply the same standardisation (mean/SD) used during model ",
      "training when generating out-of-sample predictions. Never re-scale to future-year maxima, ",
      "as this compresses the training-data range and renders coefficients unidentifiable."
    ),
    regionHeterogeneity = paste0(
      "**Region heterogeneity:** The catch-all region category may aggregate highly heterogeneous ",
      "countries (e.g., ECP ranging from 0 to > 60 USD/tCO\u2082). Consider disaggregating into ",
      "additional region categories or using country-level random effects in a future model extension."
    ),
    counterfactual = counterfactual_caveat,
    asNumberedList = paste(
      paste0("1. ", time_caveat),
      paste0("2. ", covariate_caveat),
      paste0("3. ", paste0(
        "**Historical scaling:** Always apply the same standardisation used during model training ",
        "when generating out-of-sample predictions. Never re-scale to future-year maxima."
      )),
      paste0("4. ", paste0(
        "**Region heterogeneity:** The catch-all region category may group highly heterogeneous countries. ",
        "Consider disaggregating or using country-level random effects in a future extension."
      )),
      paste0("5. ", counterfactual_caveat),
      sep = "\n\n"
    )
  )

  list(
    modelDiagnoses      = modelDiagnoses,
    globalFindings      = globalFindings,
    issuesTable         = issuesTable,
    readinessTable      = readinessTable,
    comparisonNarrative = list(adoption = adoptNarrative, stringency = strNarrative),
    fitSummaryNarrative = fitSummaryNarrative,
    projectionCaveats   = projectionCaveats
  )
}
