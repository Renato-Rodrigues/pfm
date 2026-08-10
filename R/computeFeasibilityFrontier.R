# nolint start
#' Feasibility frontier, political slack and the frontier Implementability ratio
#'
#' @description
#' Post-processing for the stochastic-frontier rung of the PSM estimator suite
#' (Tier-1 direction 1, `docs/psm-theoretical-directions.md`): decomposes each
#' estimation row into the \strong{feasibility frontier} (the maximum attainable
#' transformed stringency given political-economy fundamentals,
#' \eqn{\eta_F = X'\beta}) and \strong{political slack} (the one-sided
#' underperformance term \eqn{E[u|\varepsilon]}, Jondrow et al. 1982), and maps
#' both through the bounded satP response onto the natural 0-\code{indexMax}
#' scale. The ratio observed/frontier is the \emph{principled} Implementability
#' Factor: unlike \code{index/indexMax} it compares each polity to what its own
#' fundamentals support, and lies in (0, 1] by construction of the frontier.
#'
#' Two summary statistics matter before reading any score: \code{frontierGamma}
#' (share of composed-error variance attributed to slack — near 0 means the data
#' show \emph{no} frontier structure and the exercise collapses to the mean
#' regression) and \code{frontierLR} (mixed chi-square LR test vs. the OLS
#' no-frontier null). Both are attached to the fit result by
#' \code{\link{estimatePolicyStringencyModel}}.
#'
#' @param fit A fit result from
#'   \code{\link{estimatePolicyStringencyModel}}\code{(estimator = "frontier")}
#'   (needs \code{model}, \code{formula}, \code{data}, \code{indexMax},
#'   \code{outcomeNatural}).
#'
#' @return Data.frame \code{region, year, observedIndex, frontierIndex,
#'   expectedIndex, expectedIndexP10/P50/P90, slackIndex, efficiencyRatio}:
#'   \code{frontierIndex} is the
#'   feasibility ceiling, \code{expectedIndex} the slack-adjusted expectation,
#'   the \code{P10/P50/P90} columns the per-row percentiles of the conditional
#'   slack distribution mapped onto the index scale (the stochastic band, ADR
#'   0040 — quote these rather than a point ceiling, given boundary gamma),
#'   \code{slackIndex} their gap (the "political ambition gap" in index points)
#'   and \code{efficiencyRatio} = observed/frontier (the frontier
#'   Implementability Factor).
#'
#' @author Renato Rodrigues
#'
#' @importFrom stats coef model.matrix as.formula dnorm pnorm plogis
#'
#' @export
computeFeasibilityFrontier <- function(fit) {
  df <- fit$data
  indexMax <- fit$indexMax %||% 10
  beta <- stats::coef(fit$model)
  sigmaSq <- as.numeric(beta[["sigmaSq"]])
  gamma <- as.numeric(beta[["gamma"]])
  mm <- stats::model.matrix(stats::as.formula(fit$formula), data = df)
  bX <- beta[colnames(mm)]
  etaF <- as.numeric(mm %*% bX)
  y <- df[rownames(mm), "ecp"]
  eps <- y - etaF

  # Jondrow et al. (1982) conditional slack for the production-frontier
  # composition eps = v - u, u ~ half-normal:
  #   sigma_u^2 = gamma * sigmaSq, sigma_v^2 = (1 - gamma) * sigmaSq
  #   mu*_i = -eps_i * gamma, sigma*^2 = gamma (1 - gamma) sigmaSq
  #   E[u_i | eps_i] = mu*_i + sigma* dnorm(mu*_i/sigma*) / pnorm(mu*_i/sigma*)
  sigmaStar <- sqrt(pmax(gamma * (1 - gamma) * sigmaSq, 1e-12))
  muStar <- -eps * gamma
  zed <- muStar / sigmaStar
  slackEta <- muStar + sigmaStar * stats::dnorm(zed) / pmax(stats::pnorm(zed), 1e-12)
  slackEta <- pmax(slackEta, 0)

  obs <- fit$outcomeNatural
  obsRows <- if (!is.null(names(obs))) as.numeric(obs[rownames(mm)]) else NA_real_

  frontierIndex <- indexMax * stats::plogis(etaF)
  expectedIndex <- indexMax * stats::plogis(etaF - slackEta)

  # --- Stochastic band (ADR 0040) ---------------------------------------------
  # The frontier is STOCHASTIC: u | eps is a normal(muStar, sigmaStar^2) truncated
  # at 0, so each row carries a full conditional slack DISTRIBUTION, not just its
  # Jondrow point estimate. Reporting only the point estimate is the project's
  # most attackable number given gamma sits at the boundary (0.98-0.99), where the
  # variance decomposition is degenerate and the SEs are meaningless. The
  # percentiles below are quantiles of that conditional distribution PER ROW - not
  # cross-country quantiles of the point estimates, which would be a different and
  # wrong object - and give the coupled exercise an honest band instead of a false
  # point. Note the inversion: a HIGH slack quantile is a LOW index, so the p10
  # index uses the 90th slack percentile.
  qTruncSlack <- function(q) {
    a <- stats::pnorm(-muStar / sigmaStar)             # mass below the 0 truncation
    z <- stats::qnorm(pmin(pmax(a + q * (1 - a), 1e-12), 1 - 1e-12))
    pmax(muStar + sigmaStar * z, 0)
  }
  expectedIndexP10 <- indexMax * stats::plogis(etaF - qTruncSlack(0.90))
  expectedIndexP50 <- indexMax * stats::plogis(etaF - qTruncSlack(0.50))
  expectedIndexP90 <- indexMax * stats::plogis(etaF - qTruncSlack(0.10))

  data.frame(
    region = df[rownames(mm), "region"],
    year = df[rownames(mm), "year"],
    observedIndex = obsRows,
    frontierIndex = frontierIndex,
    expectedIndex = expectedIndex,
    expectedIndexP10 = expectedIndexP10,
    expectedIndexP50 = expectedIndexP50,
    expectedIndexP90 = expectedIndexP90,
    slackIndex = frontierIndex - expectedIndex,
    efficiencyRatio = pmin(pmax(obsRows / pmax(frontierIndex, 1e-9), 0), 1),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

#' SFA robustness rungs for the feasibility frontier
#'
#' @description
#' The gamma-at-the-boundary problem of the pooled half-normal frontier (first
#' real-data run: gamma = 1.0 — slack absorbs all noise and the within-FE-group
#' country heterogeneity) is interrogated with the standard SFA sensitivity
#' battery, run on the SAME estimation data and formula as the headline fit:
#' \describe{
#'   \item{truncnorm}{Truncated-normal inefficiency distribution (the
#'     distributional-assumption rung offered by the \pkg{frontier} package).}
#'   \item{panel}{Panel frontier with time-invariant region inefficiency
#'     (Battese–Coelli 1992 via \code{plm::pdata.frame}; region-FE dummies
#'     dropped — the panel u_i takes their place). This is the opposite
#'     attribution pole from the pooled fit: ALL persistent region deviation is
#'     slack. Together the two bracket the heterogeneity-vs-slack ambiguity.}
#'   \item{decay}{Battese–Coelli time-varying efficiency
#'     (\code{timeEffect = TRUE}): u_it = u_i exp(-eta (t - T)). The decay
#'     parameter \code{eta} is a direct estimate of the slack-closure rate —
#'     the ratcheting-speed quantity of the feasibility-as-speed reading.}
#' }
#' The statistic that decides whether the ambition-gap exhibit survives is
#' \code{slackRankCor}: the Spearman correlation between each rung's region-mean
#' slack ranking and the headline ranking.
#'
#' @param fit A fit result from
#'   \code{\link{estimatePolicyStringencyModel}}\code{(estimator = "frontier")}.
#' @param rungs Character vector; subset of \code{c("truncnorm","panel","decay")}.
#'
#' @return Named list per rung: \code{gamma}, \code{logLik}, \code{converged},
#'   \code{slackRankCor} (vs the headline pooled slack, region means),
#'   \code{eta} (decay rung only: the slack-closure rate, positive = gaps
#'   closing over time), or \code{$error} when a rung failed.
#'
#' @author Renato Rodrigues
#' @export
computeFrontierRobustness <- function(fit, rungs = c("truncnorm", "panel", "decay")) {
  rungs <- match.arg(rungs, c("truncnorm", "panel", "decay"), several.ok = TRUE)
  df <- fit$data
  base <- computeFeasibilityFrontier(fit)
  baseSlack <- tapply(base$slackIndex, base$region, mean, na.rm = TRUE)

  fml <- stats::as.formula(fit$formula)
  fmlNoFE <- if ("regionFE" %in% all.vars(fml)) stats::update(fml, . ~ . - regionFE) else fml
  panelDf <- function() {
    d <- df
    d$region <- as.character(d$region)
    plm::pdata.frame(d, index = c("region", "year"))
  }

  slackOf <- function(sfaFit, d) {
    # frontier::efficiencies gives E[exp(-u)]-type measures; for ranking use
    # -log(efficiency) as the slack proxy on the response scale (monotone in u).
    eff <- tryCatch(frontier::efficiencies(sfaFit, asInData = TRUE), error = function(e) NULL)
    if (is.null(eff)) return(NULL)
    u <- -log(pmax(as.numeric(eff), 1e-9))
    tapply(u, as.character(d$region), mean, na.rm = TRUE)
  }
  rankCor <- function(slack) {
    if (is.null(slack)) return(NA_real_)
    common <- intersect(names(slack), names(baseSlack))
    if (length(common) < 5) return(NA_real_)
    suppressWarnings(stats::cor(slack[common], baseSlack[common], method = "spearman"))
  }
  gammaOf <- function(f) tryCatch(as.numeric(stats::coef(f)[["gamma"]]), error = function(e) NA_real_)

  out <- list()
  if ("truncnorm" %in% rungs) {
    out$truncnorm <- tryCatch({
      f <- frontier::sfa(fml, data = df, truncNorm = TRUE)
      list(gamma = gammaOf(f), logLik = as.numeric(stats::logLik(f)),
           converged = all(is.finite(stats::coef(f))), slackRankCor = rankCor(slackOf(f, df)))
    }, error = function(e) list(error = conditionMessage(e)))
  }
  if ("panel" %in% rungs) {
    out$panel <- tryCatch({
      pd <- panelDf()
      f <- frontier::sfa(fmlNoFE, data = pd)
      list(gamma = gammaOf(f), logLik = as.numeric(stats::logLik(f)),
           converged = all(is.finite(stats::coef(f))), slackRankCor = rankCor(slackOf(f, pd)))
    }, error = function(e) list(error = conditionMessage(e)))
  }
  if ("decay" %in% rungs) {
    out$decay <- tryCatch({
      pd <- panelDf()
      f <- frontier::sfa(fmlNoFE, data = pd, timeEffect = TRUE)
      eta <- tryCatch(as.numeric(stats::coef(f)[["time"]]), error = function(e) NA_real_)
      list(gamma = gammaOf(f), logLik = as.numeric(stats::logLik(f)),
           converged = all(is.finite(stats::coef(f))), eta = eta,
           slackRankCor = rankCor(slackOf(f, pd)))
    }, error = function(e) list(error = conditionMessage(e)))
  }
  out
}
# nolint end
