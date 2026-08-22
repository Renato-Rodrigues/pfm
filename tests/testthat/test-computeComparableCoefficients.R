test_that("betaXsd puts an unscaled regressor on the same footing as a scaled one", {
  set.seed(1)
  n <- 400
  # x1 is standardized (sd 1); x2 imitates the logistic trend: same influence,
  # but a small range, so its raw coefficient has to be ~10x larger.
  df <- data.frame(x1 = stats::rnorm(n), x2 = stats::runif(n, 0.09, 0.35))
  df$ecp <- 0.5 * df$x1 + 5 * df$x2 + stats::rnorm(n, 0, 0.1)
  fit <- list(model = stats::lm(ecp ~ x1 + x2, data = df),
              formula = ecp ~ x1 + x2, data = df,
              driverScaling = list(x1 = c(mean = 0, sd = 1)))

  res <- computeComparableCoefficients(fit, digits = NULL)

  expect_setequal(res$term, c("x1", "x2"))
  expect_false("(Intercept)" %in% res$term)
  # raw coefficient on x2 is far larger; on the comparable footing they are close
  raw <- stats::setNames(res$estimate, res$term)
  cmp <- stats::setNames(res$betaXsd, res$term)
  expect_gt(raw[["x2"]] / raw[["x1"]], 5)
  expect_lt(abs(cmp[["x2"]] / cmp[["x1"]]), 2)
  # betaXsd is exactly estimate * sd of the regressor
  expect_equal(cmp[["x1"]], raw[["x1"]] * stats::sd(df$x1), tolerance = 1e-8)
  # the unscaled regressor is flagged so it is never ranked as if it were per-SD
  expect_true(res$scaled[res$term == "x1"])
  expect_false(res$scaled[res$term == "x2"])
  # ordered by |betaXsd|
  expect_equal(res$betaXsd, res$betaXsd[order(-abs(res$betaXsd))])
})

test_that("sdOutOfSample measures extrapolation distance and is 0 inside the range", {
  set.seed(2)
  df <- data.frame(x1 = stats::rnorm(200))
  df$ecp <- df$x1 + stats::rnorm(200, 0, 0.1)
  fit <- list(model = stats::lm(ecp ~ x1, data = df), formula = ecp ~ x1, data = df,
              driverScaling = list(x1 = c(mean = 0, sd = 1)))

  inside <- computeComparableCoefficients(
    fit, scenarioValues = c(x1 = stats::median(df$x1)), digits = NULL)
  expect_equal(inside$sdOutOfSample[inside$term == "x1"], 0)

  beyond <- max(df$x1) + 3 * stats::sd(df$x1)
  outside <- computeComparableCoefficients(fit, scenarioValues = c(x1 = beyond), digits = NULL)
  expect_equal(outside$sdOutOfSample[outside$term == "x1"], 3, tolerance = 0.05)

  # a regressor absent from scenarioValues is NA, not an error
  quiet <- computeComparableCoefficients(fit, scenarioValues = c(other = 1), digits = NULL)
  expect_true(is.na(quiet$sdOutOfSample[quiet$term == "x1"]))
})

test_that("it refuses a fit that cannot supply a design", {
  expect_error(computeComparableCoefficients(list(model = 1)), "model, formula and data")
})
