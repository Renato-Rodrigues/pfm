# nolint start
# The frontier with a heteroscedastic slack (design note 0009 section 8.2; Caudill, Ford & Gropper 1995).
#
#   y*_it = x_it' beta + v_it - u_it,   v ~ N(0, sigma_v^2),   u_it ~ |N(0, sigma_u,it^2)|,
#   log sigma_u,it = c + z_it' w
#
# The ceiling x' beta is the one of the homoscedastic frontier; only the size of the distance below it may depend
# on z (fossil rents in buildings & transport on Run-Group v7: w = 0.25, about 11 standard errors, and better out
# of sample in 37 of 48 countries, analysis/v8/hetSlackCV.R). With w = 0 the model is the homoscedastic frontier.
#
# frontier::sfa has no heteroscedastic slack, so the likelihood is maximised here (BFGS), started at the
# homoscedastic sfa fit, with the standard errors from the numerical Hessian at the optimum. The object answers
# coef(), vcov() and logLik() like an sfa fit. coef() carries the ceiling, then `sigmaSq` and `gamma` at z = 0 (the
# sample-average country: z is standardised), then the slack terms `slack_(Intercept)` and `slack_<column>`, so the
# code that strips sigmaSq and gamma and reads the ceiling by name keeps working.

# Per-row log-likelihood; p = (beta, log sigma_v, c, w).
#' @keywords internal
.pfmHetLogLikRows <- function(p, y, X, Z) {
  kb <- ncol(X); b <- p[seq_len(kb)]; sv <- exp(p[kb + 1]); su <- exp(as.numeric(Z %*% p[-seq_len(kb + 1)]))
  e <- y - as.numeric(X %*% b); s <- sqrt(sv^2 + su^2); lam <- su / sv
  log(2) - log(s) + stats::dnorm(e / s, log = TRUE) + stats::pnorm(-e * lam / s, log.p = TRUE)
}

#' Fit the frontier with a heteroscedastic slack
#'
#' @param fml The frontier formula (the ceiling).
#' @param df The prepared estimation frame; carries the response, the design and the slack columns.
#' @param slackCols Column names of \code{df} that set the size of the slack (standardised drivers).
#' @param start An \code{frontier::sfa} fit of \code{fml} on \code{df}: the starting values and the
#'   homoscedastic log-likelihood the gain is measured against.
#' @param maxit Iteration cap of the optimiser.
#' @return An object of class \code{pfmHetFrontier}.
#' @keywords internal
.pfmHetFrontierFit <- function(fml, df, slackCols, start, maxit = 5000L) {
  mf <- stats::model.frame(fml, df)
  X <- stats::model.matrix(fml, mf); y <- stats::model.response(mf)
  miss <- setdiff(slackCols, colnames(df))
  if (length(miss)) stop(".pfmHetFrontierFit: slack column(s) absent from the frame: ", paste(miss, collapse = ", "), call. = FALSE)
  Z <- cbind(`(Intercept)` = 1, as.matrix(df[rownames(X), slackCols, drop = FALSE]))
  if (anyNA(Z)) stop(".pfmHetFrontierFit: missing slack values in ", sum(!stats::complete.cases(Z)), " estimation row(s)", call. = FALSE)
  b0 <- stats::coef(start)
  s2 <- b0[["sigmaSq"]]; ga <- min(max(b0[["gamma"]], 1e-4), 1 - 1e-4)
  p0 <- c(b0[colnames(X)], logSigmaV = log(sqrt((1 - ga) * s2)), stats::setNames(c(log(sqrt(ga * s2)), rep(0, ncol(Z) - 1)), paste0("slack_", colnames(Z))))
  nll <- function(p) -sum(.pfmHetLogLikRows(p, y, X, Z))
  o <- stats::optim(p0, nll, method = "BFGS", control = list(maxit = maxit, reltol = 1e-12))
  H <- tryCatch(stats::optimHess(o$par, nll), error = function(e) NULL)
  V <- if (is.null(H)) NULL else tryCatch(solve(H), error = function(e) NULL)
  if (!is.null(V)) dimnames(V) <- list(names(o$par), names(o$par))
  kb <- ncol(X); sv <- exp(o$par[[kb + 1]]); su0 <- exp(o$par[[kb + 2]])
  structure(list(par = o$par, beta = o$par[seq_len(kb)], logSigmaV = o$par[[kb + 1]],
                 slack = o$par[-seq_len(kb + 1)], slackCols = slackCols,
                 sigmaSq = sv^2 + su0^2, gamma = su0^2 / (sv^2 + su0^2),
                 logLikValue = -o$value, nobs = length(y), vcovPar = V,
                 mleLogl = -o$value, olsLogl = start$olsLogl %||% NA_real_, homLogl = as.numeric(stats::logLik(start)),
                 nIter = o$counts[["function"]], maxit = maxit, convergence = o$convergence,
                 converged = o$convergence == 0 && !is.null(V)),
            class = "pfmHetFrontier")
}

#' @export
coef.pfmHetFrontier <- function(object, ...) {
  c(object$beta, sigmaSq = object$sigmaSq, gamma = object$gamma, object$slack)
}

#' @export
vcov.pfmHetFrontier <- function(object, ...) {
  # The ceiling and the slack terms from the Hessian; sigmaSq and gamma (derived at z = 0) without a variance.
  nm <- names(coef(object)); V <- matrix(NA_real_, length(nm), length(nm), dimnames = list(nm, nm))
  if (!is.null(object$vcovPar)) { k <- intersect(nm, rownames(object$vcovPar)); V[k, k] <- object$vcovPar[k, k] }
  V
}

#' @export
logLik.pfmHetFrontier <- function(object, ...) {
  structure(object$logLikValue, df = length(object$par), nobs = object$nobs, class = "logLik")
}

# The slack scale sigma_u of every row of `df` under a fitted model (homoscedastic: one value).
#' @keywords internal
.pfmSlackSigma <- function(model, df) {
  if (inherits(model, "pfmHetFrontier")) {
    Z <- cbind(1, as.matrix(df[, model$slackCols, drop = FALSE]))
    return(exp(as.numeric(Z %*% model$slack)))
  }
  b <- stats::coef(model); rep(sqrt(b[["gamma"]] * b[["sigmaSq"]]), nrow(df))
}

# The slack drivers of a spec for one sector: a character vector (every sector) or a named list by sector.
#' @keywords internal
.pfmSlackDriversFor <- function(slackDrivers, sector) {
  if (is.null(slackDrivers) || !length(slackDrivers)) return(character(0))
  if (is.list(slackDrivers)) return(as.character(unlist(slackDrivers[[sector]] %||% character(0))))
  # a by-sector list flattened by unlist() (names Diffuse, or Diffuse1, Diffuse2): still by sector
  nm <- sub("[0-9]+$", "", names(slackDrivers) %||% rep("", length(slackDrivers)))
  if (any(nzchar(nm))) return(unname(as.character(slackDrivers[nm == sector])))
  as.character(slackDrivers)
}
# nolint end

# The ceiling's coefficients of a frontier coefficient vector: without sigmaSq, gamma and the slack terms.
#' @keywords internal
.pfmCeilingBeta <- function(b) b[!names(b) %in% c("sigmaSq", "gamma") & !startsWith(names(b), "slack_")]
