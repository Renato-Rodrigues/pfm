#' Trustworthy covariance matrix for a stochastic-frontier fit
#'
#' @description
#' \code{frontier::sfa()} wraps Coelli's FRONTIER 4.1 Fortran and reports standard
#' errors from that routine's own covariance matrix. On 2026-08-26 that matrix was
#' found to be \strong{corrupt} for Run-Group \code{v4} Bulk: it gave a median
#' standard error of 0.924 and 0 of 15 significant coefficients, where the true
#' median standard error at the very same maximum is 0.062 and 12 of 15 are
#' significant — a factor of \strong{14.8}. The point estimates were never affected.
#'
#' Nothing in the fit object flags it. \code{converged} is \code{TRUE}, the
#' log-likelihood is correct, and the Hessian at the reported optimum is negative
#' definite and well conditioned (condition number 2236 — \emph{better} than fits
#' whose reported standard errors are fine). The only symptom is that the reported
#' standard errors are wrong, and the only way to see it is to compute them again.
#'
#' So this recomputes them, from the likelihood \code{frontier::sfa()} actually
#' maximised, and hands back a covariance matrix that has been checked rather than
#' assumed. See \code{docs/TODO.md} item 14f.
#'
#' @section How:
#' The Aigner–Lovell–Schmidt normal/half-normal production frontier, which is what
#' \code{frontier::sfa()} fits under its defaults (\code{ineffDecrease = TRUE},
#' \code{truncNorm = FALSE}, cross-section):
#'
#' \deqn{\varepsilon_i = y_i - x_i'\beta, \quad
#'       \sigma^2 = \sigma_u^2 + \sigma_v^2, \quad
#'       \gamma = \sigma_u^2/\sigma^2, \quad \lambda = \sqrt{\gamma/(1-\gamma)}}
#' \deqn{\ln L = \sum_i \left[\log 2 - \log\sigma + \log\phi(\varepsilon_i/\sigma)
#'       + \log\Phi(-\varepsilon_i\lambda/\sigma)\right]}
#'
#' \eqn{\sigma^2} and \eqn{\gamma} are carried on the log and logit scales so the
#' information matrix is unconstrained and comparable across fits.
#'
#' The Hessian is obtained by differencing the \strong{analytic score}, not the
#' log-likelihood. That is one order more accurate and it matters here: differencing
#' the log-likelihood with \code{numDeriv::hessian()} at its default
#' \code{r = 4} gave median standard errors 0.9\% off on the Diffuse fit and its
#' \code{r = 2} produced \code{NaN}, while differencing the score is stable to six
#' figures across step sizes spanning two orders of magnitude. The score is verified
#' against a numeric gradient in \code{test-frontierVcov.R} (agreement 5e-8).
#'
#' @section What it does NOT do:
#' It does not re-optimise. The parameter vector is \code{frontier::sfa()}'s own;
#' only the second derivatives at that point are recomputed. If the fit is at a bad
#' maximum, this will faithfully report the curvature of a bad maximum.
#'
#' It also cannot help when \eqn{\gamma} sits \emph{at} the boundary. At
#' \eqn{\gamma = 1} the composed error is one-sided, the likelihood is degenerate
#' and \strong{neither} covariance matrix means anything — this is
#' \code{MODEL.md} §3.4's "standard errors on \eqn{\gamma} are meaningless at the
#' boundary", and it is the state Run-Group \code{v3} is in. That case is detected
#' and reported, not papered over.
#'
#' @param fit A fitted \code{frontier} object from \code{frontier::sfa()}.
#' @param fml The model formula the fit was built with.
#' @param df The data frame the fit was built on.
#' @param gammaBoundary Numeric. \eqn{\gamma} at or above this counts as the
#'   boundary case. Default \code{1 - 1e-8}.
#' @param ratioWarn Numeric. Report \code{status = "corrupt"} when the reported
#'   median standard error exceeds the recomputed one by more than this factor.
#'   Default 2 — well clear of finite-difference noise (~1\%) and far below the
#'   14.8 that motivated this.
#'
#' @return A list:
#'   \item{vcov}{the covariance matrix to use}
#'   \item{status}{\code{"ok"}, \code{"corrupt"}, \code{"boundary"},
#'     \code{"flat"} (Hessian not negative definite — the large reported standard
#'     errors are honest) or \code{"likelihood-mismatch"} (the check is on the wrong
#'     likelihood; nothing is recomputed)}
#'   \item{source}{\code{"recomputed"} or \code{"frontier"} — which matrix
#'     \code{vcov} is}
#'   \item{ratio}{reported median SE / recomputed median SE, or \code{NA}}
#'   \item{logLikReported,logLikCheck}{the two log-likelihoods, which must agree}
#'
#' @author Renato Rodrigues
#' @keywords internal
.pfmFrontierVcov <- function(fit, fml, df, gammaBoundary = 1 - 1e-8, ratioWarn = 2) {
  out <- list(vcov = as.matrix(stats::vcov(fit)), status = "ok", source = "frontier",
              ratio = NA_real_, logLikReported = NA_real_, logLikCheck = NA_real_)

  cf <- stats::coef(fit)
  if (!all(c("sigmaSq", "gamma") %in% names(cf))) return(out)

  # At the boundary the likelihood is degenerate; neither matrix is interpretable.
  if (.pfmFrontierAtBoundary(cf[["gamma"]], cf[["sigmaSq"]], gammaBoundary)) {
    out$status <- "boundary"
    return(out)
  }

  inp <- .pfmFrontierDesign(cf, fml, df)
  if (is.null(inp)) return(out)
  k <- ncol(inp$design)

  llReported <- tryCatch(as.numeric(stats::logLik(fit)), error = function(e) NA_real_)
  llCheck <- .pfmFrontierLogLik(inp$theta, inp$design, inp$y)
  out$logLikReported <- llReported
  out$logLikCheck <- llCheck
  # If our likelihood is not the one that was maximised, the recomputation is
  # meaningless. Say so and keep frontier's matrix rather than quietly substituting.
  if (!.pfmLogLikAgree(llCheck, llReported)) {
    out$status <- "likelihood-mismatch"
    return(out)
  }

  hess <- .pfmFrontierHessian(inp$theta, inp$design, inp$y)
  if (is.null(hess) || any(!is.finite(hess))) return(out)
  vOwn <- tryCatch(solve(-hess), error = function(e) NULL)
  if (is.null(vOwn) || any(!is.finite(vOwn)) || any(diag(vOwn)[seq_len(k)] <= 0)) {
    # A genuinely flat likelihood: the large reported standard errors are honest.
    out$status <- "flat"
    return(out)
  }

  out$ratio <- .pfmSeRatio(seReported = sqrt(pmax(diag(out$vcov)[names(inp$beta)], 0)),
                           seOwn = sqrt(diag(vOwn))[seq_len(k)])

  parNames <- c(colnames(inp$design), "logSigmaSq", "logitGamma")
  dimnames(vOwn) <- list(parNames, parNames)
  out$vcov <- vOwn
  out$source <- "recomputed"
  if (is.finite(out$ratio) && out$ratio > ratioWarn) out$status <- "corrupt"
  out
}

#' gamma at (or beyond) the boundary, or a non-positive variance
#'
#' @param gamma,sigmaSq The fit's \code{gamma} and \code{sigmaSq}.
#' @param gammaBoundary See \code{.pfmFrontierVcov}.
#' @return Logical scalar.
#' @keywords internal
#' @noRd
.pfmFrontierAtBoundary <- function(gamma, sigmaSq, gammaBoundary) {
  !is.finite(gamma) || !is.finite(sigmaSq) || gamma >= gammaBoundary || gamma <= 0 ||
    sigmaSq <= 0
}

#' Do the recomputed and the reported log-likelihood agree?
#'
#' @param llCheck,llReported The two log-likelihoods.
#' @return Logical scalar: both finite and within a relative 1e-4.
#' @keywords internal
#' @noRd
.pfmLogLikAgree <- function(llCheck, llReported) {
  is.finite(llCheck) && is.finite(llReported) &&
    abs(llCheck - llReported) <= 1e-4 * max(1, abs(llReported))
}

#' Reported over recomputed median standard error
#'
#' @param seReported,seOwn The two standard-error vectors for the coefficients.
#' @return The ratio of medians, or \code{NA} when it is not defined.
#' @keywords internal
#' @noRd
.pfmSeRatio <- function(seReported, seOwn) {
  if (all(is.finite(seReported)) && stats::median(seOwn) > 0) {
    stats::median(seReported) / stats::median(seOwn)
  } else {
    NA_real_
  }
}

#' Design matrix, response and parameter vector for the likelihood check
#'
#' @param cf The fit's coefficients (\code{beta}, \code{sigmaSq}, \code{gamma}).
#' @param fml,df Formula and data the fit was built on.
#' @return \code{list(design, y, beta, theta)} with \code{theta} on the unconstrained
#'   scale \code{c(beta, log(sigmaSq), qlogis(gamma))}, or \code{NULL} when the design
#'   cannot be rebuilt or does not match the fit's coefficients.
#' @keywords internal
#' @noRd
.pfmFrontierDesign <- function(cf, fml, df) {
  design <- tryCatch(stats::model.matrix(fml, data = df), error = function(e) NULL)
  y <- tryCatch(stats::model.response(stats::model.frame(fml, data = df)),
                error = function(e) NULL)
  if (is.null(design) || is.null(y) || !is.numeric(y) || nrow(design) != length(y)) return(NULL)
  beta <- cf[setdiff(names(cf), c("sigmaSq", "gamma"))]
  if (!all(colnames(design) %in% names(beta))) return(NULL)
  beta <- beta[colnames(design)]
  list(design = design, y = y, beta = beta,
       theta = c(beta, log(cf[["sigmaSq"]]), stats::qlogis(cf[["gamma"]])))
}

#' ALS normal/half-normal log-likelihood
#'
#' @param theta \code{c(beta, log(sigmaSq), qlogis(gamma))}.
#' @param design,y Design matrix and response.
#' @return Scalar log-likelihood.
#' @keywords internal
#' @noRd
.pfmFrontierLogLik <- function(theta, design, y) {
  k <- ncol(design)
  b <- theta[seq_len(k)]
  s <- sqrt(exp(theta[k + 1L]))
  g <- stats::plogis(theta[k + 2L])
  if (!is.finite(s) || s <= 0 || !is.finite(g) || g <= 0 || g >= 1) return(NA_real_)
  e <- as.numeric(y - design %*% b)
  sum(log(2) - log(s) + stats::dnorm(e / s, log = TRUE) +
        stats::pnorm(-e * sqrt(g / (1 - g)) / s, log.p = TRUE))
}

#' Analytic score of the ALS log-likelihood
#'
#' Exact, and verified against a numeric gradient in \code{test-frontierVcov.R}.
#' The inverse Mills ratio is formed as \code{exp(log phi - log Phi)} so it stays
#' finite in the far tail, which is where \code{frontier}'s own routine gets into
#' trouble.
#'
#' @inheritParams .pfmFrontierLogLik
#' @return Numeric vector, same length as \code{theta}.
#' @keywords internal
#' @noRd
.pfmFrontierScore <- function(theta, design, y) {
  k <- ncol(design)
  b <- theta[seq_len(k)]
  s2 <- exp(theta[k + 1L])
  g <- stats::plogis(theta[k + 2L])
  s <- sqrt(s2)
  lam <- sqrt(g / (1 - g))
  e <- as.numeric(y - design %*% b)
  a <- e / s
  cc <- -a * lam
  m <- exp(stats::dnorm(cc, log = TRUE) - stats::pnorm(cc, log.p = TRUE))
  gBeta <- as.numeric(crossprod(design, a + lam * m)) / s
  gLogS2 <- sum(-0.5 + 0.5 * a^2 + 0.5 * m * a * lam)
  # lambda = sqrt(g/(1-g))  =>  dlambda/dgamma = 1 / (2 sqrt(g(1-g)) (1-g))
  dLamDG <- 1 / (2 * sqrt(g * (1 - g)) * (1 - g))
  gLogitG <- sum(-m * a) * dLamDG * g * (1 - g)
  c(gBeta, gLogS2, gLogitG)
}

#' Hessian by central differences of the analytic score
#'
#' Steps are scaled per parameter, because the coefficients, \code{log(sigmaSq)} and
#' \code{qlogis(gamma)} differ by orders of magnitude and a single absolute step is
#' accurate for at most one of them.
#'
#' @inheritParams .pfmFrontierLogLik
#' @param rel Relative step size.
#' @return Symmetric matrix, or \code{NULL} if the score cannot be evaluated.
#' @keywords internal
#' @noRd
.pfmFrontierHessian <- function(theta, design, y, rel = 1e-5) {
  n <- length(theta)
  h <- pmax(abs(theta), 1e-2) * rel
  jac <- matrix(NA_real_, n, n)
  for (j in seq_len(n)) {
    ej <- numeric(n)
    ej[j] <- h[j]
    up <- tryCatch(.pfmFrontierScore(theta + ej, design, y), error = function(e) NULL)
    dn <- tryCatch(.pfmFrontierScore(theta - ej, design, y), error = function(e) NULL)
    if (is.null(up) || is.null(dn)) return(NULL)
    jac[, j] <- (up - dn) / (2 * h[j])
  }
  (jac + t(jac)) / 2
}
