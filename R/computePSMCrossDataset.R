#' Cross-dataset concordance of CAPMF stringency with independent policy indices (T4)
#'
#' Correlates the CAPMF-based policy-stringency composite with independent measures —
#' the OECD Environmental Policy Stringency (EPS) index and the CLIMAPP/ACCUPOL
#' portfolio dataset — at the overlapping country-year level (Steinebach et al. 2024),
#' to show the outcome is not an OECD-measurement idiosyncrasy. Compute lives in
#' \pkg{pfm} so the robustness analysis is executable without the paper repo; the
#' external datasets are passed in (read them via \pkg{mrpfm} readers or from file).
#'
#' @param eps,climapp optional data frames with columns `iso3`, `year`, and the index
#'   value (`eps` / `climapp`). Pass `NULL` to skip a comparison.
#' @param capmf optional data frame `iso3,year,capmf`; if `NULL`, sourced from
#'   `mrpfm::calcOutput("PolicyStringency", aggregate = FALSE)` (composite, else bulk).
#' @return A matrix (one row per available comparison) with columns
#'   `within_country_r`, `cross_section_r`, `rank_concordance`.
#' @author Renato Rodrigues
#' @export
computePSMCrossDataset <- function(eps = NULL, climapp = NULL, capmf = NULL) {
  if (is.null(capmf)) {
    ps <- madrat::calcOutput("PolicyStringency", aggregate = FALSE)
    v  <- if ("composite" %in% magclass::getNames(ps)) "composite" else "bulk"
    df <- as.data.frame(ps[, , v])
    capmf <- data.frame(iso3 = as.character(df$Region),
                        year = as.integer(gsub("y", "", as.character(df$Year))),
                        capmf = as.numeric(df$Value))
  }
  withinR <- function(d, a, b) {
    z <- do.call(rbind, by(d, d$iso3, function(g) {
      g[[a]] <- g[[a]] - mean(g[[a]], na.rm = TRUE)
      g[[b]] <- g[[b]] - mean(g[[b]], na.rm = TRUE); g
    }))
    stats::cor(z[[a]], z[[b]], use = "complete.obs")
  }
  xsecR   <- function(d, a, b, yr = max(d$year)) {
    s <- d[d$year == yr, ]; stats::cor(s[[a]], s[[b]], use = "complete.obs")
  }
  rankCon <- function(d, a, b, yr = max(d$year)) {
    s <- d[d$year == yr, ]; stats::cor(s[[a]], s[[b]], method = "spearman", use = "complete.obs")
  }
  rows <- list()
  if (!is.null(eps)) {
    d <- merge(capmf, eps, by = c("iso3", "year"))
    rows[["CAPMF vs OECD-EPS"]] <- c(withinR(d, "capmf", "eps"),
                                     xsecR(d, "capmf", "eps"), rankCon(d, "capmf", "eps"))
  }
  if (!is.null(climapp)) {
    d <- merge(capmf, climapp, by = c("iso3", "year"))
    rows[["CAPMF vs CLIMAPP"]] <- c(withinR(d, "capmf", "climapp"),
                                    xsecR(d, "capmf", "climapp"), rankCon(d, "capmf", "climapp"))
  }
  if (!length(rows)) {
    warning("computePSMCrossDataset: no comparison dataset supplied (eps and climapp both NULL).")
    return(invisible(NULL))
  }
  out <- do.call(rbind, rows)
  colnames(out) <- c("within_country_r", "cross_section_r", "rank_concordance")
  out
}
