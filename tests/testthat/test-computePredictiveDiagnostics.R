test_that("adoption diagnostics from a glm logit are sane", {
  set.seed(11)
  n <- 500
  x <- rnorm(n)
  y <- rbinom(n, 1, plogis(1.5 * x))
  fit <- glm(y ~ x, family = binomial())
  pd <- computePredictiveDiagnostics(fit, "adoption")
  expect_equal(pd$n, n)
  expect_lt(pd$brier, pd$brierBase) # informative model beats base rate
  expect_gt(pd$auc, 0.7)
  expect_lt(pd$auc, 1)
  # in-sample logistic fit is calibrated by construction: slope ~ 1
  expect_equal(pd$calibrationSlope, 1, tolerance = 0.15)
  expect_equal(pd$baseRate, mean(y))
})

test_that("stringency diagnostics from a gaussian glm are sane", {
  set.seed(12)
  n <- 200
  x <- rnorm(n)
  y <- 2 + 0.5 * x + rnorm(n, sd = 0.3)
  fit <- glm(y ~ x, family = gaussian())
  pd <- computePredictiveDiagnostics(fit, "stringency")
  expect_equal(pd$n, n)
  expect_equal(pd$rmse, sqrt(mean((y - fitted(fit))^2)))
  expect_gt(pd$corObsPred, 0.7)
  expect_lte(pd$mae, pd$rmse)
})

test_that("logistf fitted values are read from $predict", {
  skip_if_not_installed("logistf")
  set.seed(13)
  n <- 300
  x <- rnorm(n)
  d <- data.frame(x = x, y = rbinom(n, 1, plogis(x)))
  fit <- logistf::logistf(y ~ x, data = d)
  pd <- computePredictiveDiagnostics(fit, "adoption")
  expect_false(is.null(pd))
  expect_gt(pd$auc, 0.6)
  expect_true(is.finite(pd$brier))
})

test_that("NULL and degenerate models return NULL", {
  expect_null(computePredictiveDiagnostics(NULL, "adoption"))
  expect_null(computePredictiveDiagnostics(list(y = 1), "adoption"))
})
