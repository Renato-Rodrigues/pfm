# nolint start
# The theory gate of the specification search (v7; design note 0007 section 15).
#
# Three declared hypotheses from the political economy of the transition (ADR 0056), tested on every candidate:
#   H1 structural power   fossil dependence lowers the ceiling at the sample's typical institutions, in every
#                         sector: the main effect of the fossil term is negative with p < `p` (cluster-robust).
#                         Structural power works by anticipation (Lindblom 1977; Culpepper & Reinke 2014): the
#                         frontier's ceiling is where it should show.
#   H2 contingency        the fossil effect depends on institutions, in every sector: at least one fossil x
#                         institution term has p < `p` / (number of such terms) (Bonferroni). Both fossil terms count,
#                         the share and the per-person form: only the pair is interpretable (ADR 0056), and in v6 the
#                         contingency shows mostly on the per-person term. The sign is free:
#                         accountability can empower the diffuse public against concentrated interests, or carry
#                         energy-cost backlash and the weight of fossil regions into policy (Hacker & Pierson 2002;
#                         Mildenberger 2020; Stokes 2020). The theory fixes that it matters, not its sign.
#   H3 coherence          the fossil effect stays a constraint over the observed sample: it is negative in at least
#                         `coherence` of the estimation country-years (the effect including its interactions, each
#                         in its weight's scope). Large interaction terms of opposite sign on correlated institutions
#                         can turn the conditional effect around within the sample; this is what H3 refuses.
# The fossil term is the fossil dependence share (`Incumbent.Power`), or the per-person form where the spec has no
# share form (as .pfmEdgeGate reads it). The clean-energy terms carry no requirement: their early influence is mostly
# instrumental and their structural weight grows with the sector (ADR 0056, limits).
# H1 and H2 screen the ranked pool before the sanity walk, on the coefficient tables of the sweep's fits; H3 runs in
# the walk, on the frontier fit (severe flag `theoryCoherence`).

#' @keywords internal
.pfmTheoryGateDefaults <- function() {
  list(p = 0.05, structural = TRUE, contingency = TRUE, coherence = 0.75)
}

# The fossil term of a coefficient vector's names: the share form, else the per-person form.
#' @keywords internal
.pfmFossilTerm <- function(terms) {
  if ("Incumbent.Power" %in% terms) "Incumbent.Power" else if ("Incumbent.Power.pc" %in% terms) "Incumbent.Power.pc" else NA_character_
}

#' H1 and H2 of the theory gate on the sweep's coefficient tables
#'
#' @param coefs The sweep's coefficient rows (\code{runFitGrid()$coefficients}: \code{model}, \code{sector},
#'   \code{term}, \code{estimate}, \code{pValue}).
#' @param models Spec names to screen.
#' @param sectors Sectors every hypothesis must hold in.
#' @param gate A list (\code{.pfmTheoryGateDefaults()} for missing elements).
#' @return data.frame: \code{model}, \code{pass}, \code{reason} (empty when passing).
#' @keywords internal
.pfmTheoryScreen <- function(coefs, models, sectors, gate = list()) {
  g <- utils::modifyList(.pfmTheoryGateDefaults(), gate %||% list())
  cf <- coefs[coefs$model %in% models & coefs$sector %in% sectors, c("model", "sector", "term", "estimate", "pValue")]
  by <- split(cf, paste(cf$model, cf$sector, sep = "\r"))
  verdict <- function(x) {
    fos <- .pfmFossilTerm(x$term)
    if (is.na(fos)) return("no fossil term")
    why <- character(0)
    if (isTRUE(g$structural)) {
      i <- match(fos, x$term)
      if (!(is.finite(x$pValue[i]) && x$estimate[i] < 0 && x$pValue[i] < g$p)) {
        why <- c(why, sprintf("H1 fossil %+.2f (p %.2f)", x$estimate[i], x$pValue[i]))
      }
    }
    if (isTRUE(g$contingency)) {
      k <- which(startsWith(x$term, "Incumbent.Power_x_") | startsWith(x$term, "Incumbent.Power.pc_x_"))
      pk <- x$pValue[k]
      if (!length(k) || !any(is.finite(pk) & pk < g$p / length(k))) {
        why <- c(why, sprintf("H2 no fossil x institution term at p < %.3f (min %.3f)", g$p / max(1, length(k)),
                              if (length(k)) min(pk, na.rm = TRUE) else NA_real_))
      }
    }
    paste(why, collapse = ", ")
  }
  out <- lapply(models, function(m) {
    rs <- vapply(sectors, function(s) {
      x <- by[[paste(m, s, sep = "\r")]]
      if (is.null(x)) "not fitted" else verdict(x)
    }, character(1))
    bad <- nzchar(rs)
    data.frame(model = m, pass = !any(bad),
               reason = paste(sprintf("%s: %s", sectors[bad], rs[bad]), collapse = "; "), stringsAsFactors = FALSE)
  })
  do.call(rbind, out)
}

#' H3 of the theory gate: the share of estimation rows where the fossil effect is a constraint
#'
#' @param beta Named frontier coefficients.
#' @param df The prepared estimation frame (standardised drivers, the weight columns).
#' @return A list: \code{share} (of rows with a negative conditional fossil effect), \code{driver}.
#' @keywords internal
.pfmTheoryCoherence <- function(beta, df) {
  fos <- .pfmFossilTerm(names(beta))
  if (is.na(fos) || !fos %in% colnames(df)) return(list(share = NA_real_, driver = fos))
  eff <- rep(beta[[fos]], nrow(df))
  for (k in names(beta)[startsWith(names(beta), paste0(fos, "_x_"))]) {
    inst <- sub(paste0("^", fos, "_x_"), "", k)
    if (!inst %in% colnames(df)) next
    eff <- eff + beta[[k]] * df[[inst]] * .pfmIwFactor(df, inst)
  }
  list(share = mean(eff < 0, na.rm = TRUE), driver = fos)
}
# nolint end
