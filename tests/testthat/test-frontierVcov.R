# Regression tests for TODO 14f: frontier::sfa() returned a covariance matrix that
# was 14.8x too large for Run-Group v4 Bulk while the estimates, the log-likelihood
# and `converged` were all correct. Nothing in the fit object flagged it.
#
# The bug pattern these pin, in order:
#   (1) our likelihood IS the one frontier::sfa maximises  -- otherwise a
#       "recomputation" silently substitutes a different model's curvature;
#   (2) the analytic score is right                        -- the Hessian is built
#       from it, so an error here is invisible and fatal;
#   (3) a fit whose REPORTED vcov is inflated is detected and overridden;
#   (4) the two cases where recomputing is NOT allowed -- gamma at the boundary,
#       and a likelihood mismatch -- fall back instead of substituting;
#   (5) end to end, estimatePolicyStringencyModel() ships the recomputed standard
#       errors and a vcovCheck the artifact can be audited from.
#
# (3) and (4) use a stub fit rather than a real one, because the corruption cannot
# be provoked on demand -- it is a property of a particular FRONTIER 4.1 optimisation
# path. The stub reproduces the SHAPE of the failure (coef/logLik correct, vcov
# wrong), which is what the code has to react to.

.alsFixture <- function(n = 400, seed = 4) {
  set.seed(seed)
  x1 <- stats::rnorm(n)
  x2 <- stats::runif(n)
  # y = frontier - u + v, u half-normal (slack), v symmetric noise
  y <- 1.5 + 0.8 * x1 - 1.2 * x2 - abs(stats::rnorm(n, sd = 0.7)) + stats::rnorm(n, sd = 0.25)
  data.frame(y = y, x1 = x1, x2 = x2)
}

.alsParts <- function(fit, fml, df) {
  xMat <- stats::model.matrix(fml, data = df)
  y <- stats::model.response(stats::model.frame(fml, data = df))
  cf <- stats::coef(fit)
  b <- cf[setdiff(names(cf), c("sigmaSq", "gamma"))][colnames(xMat)]
  list(X = xMat, y = y,
       par = c(b, log(cf[["sigmaSq"]]), stats::qlogis(cf[["gamma"]])))
}

test_that("our ALS log-likelihood is the one frontier::sfa maximises", {
  skip_if_not_installed("frontier")
  df <- .alsFixture()
  fml <- y ~ x1 + x2
  fit <- frontier::sfa(fml, data = df)
  p <- .alsParts(fit, fml, df)

  expect_equal(pfm:::.pfmFrontierLogLik(p$par, p$X, p$y),
               as.numeric(stats::logLik(fit)), tolerance = 1e-6)
})

test_that("the analytic score matches a numeric gradient and is ~0 at the MLE", {
  skip_if_not_installed("frontier")
  df <- .alsFixture()
  fml <- y ~ x1 + x2
  fit <- frontier::sfa(fml, data = df)
  p <- .alsParts(fit, fml, df)

  analytic <- pfm:::.pfmFrontierScore(p$par, p$X, p$y)
  # central differences of the log-likelihood, base R -- deliberately a different
  # route from the one under test
  h <- pmax(abs(p$par), 1e-2) * 1e-6
  numeric <- vapply(seq_along(p$par), function(j) {
    e <- numeric(length(p$par))
    e[j] <- h[j]
    (pfm:::.pfmFrontierLogLik(p$par + e, p$X, p$y) -
       pfm:::.pfmFrontierLogLik(p$par - e, p$X, p$y)) / (2 * h[j])
  }, numeric(1))

  expect_equal(unname(analytic), unname(numeric), tolerance = 1e-4)
  # at a maximum the score is small relative to the curvature it is measured against
  expect_lt(max(abs(analytic)), 0.5)
})

test_that("the Hessian is negative definite and step-size stable at the MLE", {
  skip_if_not_installed("frontier")
  df <- .alsFixture()
  fml <- y ~ x1 + x2
  fit <- frontier::sfa(fml, data = df)
  p <- .alsParts(fit, fml, df)

  h4 <- pfm:::.pfmFrontierHessian(p$par, p$X, p$y, rel = 1e-4)
  h6 <- pfm:::.pfmFrontierHessian(p$par, p$X, p$y, rel = 1e-6)
  expect_true(all(eigen(h4, symmetric = TRUE, only.values = TRUE)$values < 0))
  # Differencing the analytic score is stable across 100x in step size. This is why
  # the score is differenced rather than the log-likelihood: doing the latter with
  # numDeriv's defaults was 0.9% off on the v4 Diffuse fit and NaN at r = 2.
  expect_equal(h4, h6, tolerance = 1e-4)
})

test_that("a healthy fit is reported ok and agrees with frontier's own vcov", {
  skip_if_not_installed("frontier")
  df <- .alsFixture()
  fml <- y ~ x1 + x2
  fit <- frontier::sfa(fml, data = df)

  res <- pfm:::.pfmFrontierVcov(fit, fml, df)
  expect_identical(res$status, "ok")
  expect_identical(res$source, "recomputed")
  expect_equal(res$logLikCheck, res$logLikReported, tolerance = 1e-6)
  # on a fit FRONTIER handles correctly the two matrices must agree closely;
  # the defect this guards against is a factor of ~15, not a few percent
  expect_lt(abs(res$ratio - 1), 0.2)
})

# --- the bug pattern itself ------------------------------------------------
# A stub whose coef/logLik are correct and whose vcov is inflated 15x, i.e. exactly
# what v4 Bulk looked like from outside.
coef.pfmStubFit <- function(object, ...) object$cf
logLik.pfmStubFit <- function(object, ...) object$ll
vcov.pfmStubFit <- function(object, ...) object$vc

.stubFit <- function(fml, df, inflate = 15) {
  real <- frontier::sfa(fml, data = df)
  vcReal <- as.matrix(stats::vcov(real))
  structure(list(cf = stats::coef(real), ll = stats::logLik(real),
                 vc = vcReal * inflate^2), class = "pfmStubFit")
}

test_that("an inflated reported vcov is detected and overridden", {
  skip_if_not_installed("frontier")
  df <- .alsFixture()
  fml <- y ~ x1 + x2
  registerS3method("coef", "pfmStubFit", coef.pfmStubFit)
  registerS3method("logLik", "pfmStubFit", logLik.pfmStubFit)
  registerS3method("vcov", "pfmStubFit", vcov.pfmStubFit)

  res <- pfm:::.pfmFrontierVcov(.stubFit(fml, df, inflate = 15), fml, df)

  expect_identical(res$status, "corrupt")
  expect_identical(res$source, "recomputed")     # NOT the inflated one
  expect_gt(res$ratio, 10)
  # and the matrix handed back is the recomputed one, not the stub's
  expect_lt(stats::median(sqrt(diag(res$vcov))[1:3]), 1)
})

test_that("gamma at the boundary falls back instead of substituting", {
  skip_if_not_installed("frontier")
  df <- .alsFixture()
  fml <- y ~ x1 + x2
  registerS3method("coef", "pfmStubFit", coef.pfmStubFit)
  registerS3method("logLik", "pfmStubFit", logLik.pfmStubFit)
  registerS3method("vcov", "pfmStubFit", vcov.pfmStubFit)

  stub <- .stubFit(fml, df, inflate = 1)
  stub$cf[["gamma"]] <- 1                        # the Run-Group v3 state
  res <- pfm:::.pfmFrontierVcov(stub, fml, df)

  # At gamma = 1 the likelihood is degenerate and NEITHER matrix is interpretable.
  # Recomputing there would manufacture false precision, so it must not happen.
  expect_identical(res$status, "boundary")
  expect_identical(res$source, "frontier")
  expect_true(is.na(res$ratio))
})

test_that("a likelihood mismatch refuses to substitute", {
  skip_if_not_installed("frontier")
  df <- .alsFixture()
  fml <- y ~ x1 + x2
  registerS3method("coef", "pfmStubFit", coef.pfmStubFit)
  registerS3method("logLik", "pfmStubFit", logLik.pfmStubFit)
  registerS3method("vcov", "pfmStubFit", vcov.pfmStubFit)

  stub <- .stubFit(fml, df, inflate = 1)
  stub$ll <- stub$ll - 500                       # our likelihood is not theirs
  res <- pfm:::.pfmFrontierVcov(stub, fml, df)

  expect_identical(res$status, "likelihood-mismatch")
  expect_identical(res$source, "frontier")
})

test_that("estimatePolicyStringencyModel ships recomputed SEs and a vcovCheck", {
  skip_if_not_installed("frontier")
  m <- makePFMagpie()
  fit <- pfmFit(m, estimator = "frontier")

  expect_false(is.null(fit$vcovCheck))
  expect_true(fit$vcovCheck$status %in%
                c("ok", "corrupt", "boundary", "flat", "likelihood-mismatch"))

  if (identical(fit$vcovCheck$source, "recomputed")) {
    # the coeftest must be built from the matrix that was checked, not from
    # stats::vcov(fit$model) -- that substitution is the whole bug
    k <- nrow(fit$coeftest) - 2L
    expect_equal(unname(fit$coeftest[seq_len(k), "Std. Error"]),
                 unname(sqrt(diag(fit$vcov))[seq_len(k)]), tolerance = 1e-8)
    # sigmaSq and gamma come back on the natural scale via the delta method
    expect_true(all(is.finite(fit$coeftest[c("sigmaSq", "gamma"), "Std. Error"])))
    expect_gt(fit$coeftest["gamma", "Std. Error"], 0)
  }
  expect_true(all(is.finite(fit$coeftest[, "Estimate"])))
})
