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
#'   expectedIndex, slackIndex, efficiencyRatio}: \code{frontierIndex} is the
#'   feasibility ceiling, \code{expectedIndex} the slack-adjusted expectation,
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
  data.frame(
    region = df[rownames(mm), "region"],
    year = df[rownames(mm), "year"],
    observedIndex = obsRows,
    frontierIndex = frontierIndex,
    expectedIndex = expectedIndex,
    slackIndex = frontierIndex - expectedIndex,
    efficiencyRatio = pmin(pmax(obsRows / pmax(frontierIndex, 1e-9), 0), 1),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}
# nolint end
