makeDynamicPanel <- function(rho = 0.6, beta = 1.0, nReg = 20, nYr = 16, seed = 1) {
  set.seed(seed)
  rows <- list()
  for (g in seq_len(nReg)) {
    alpha <- rnorm(1, 0, 1)          # region fixed effect
    x <- rnorm(nYr)
    y <- numeric(nYr)
    ylag <- 0
    for (t in seq_len(nYr)) {
      y[t] <- alpha + rho * ylag + beta * x[t] + rnorm(1, 0, 0.3)
      ylag <- y[t]
    }
    rows[[g]] <- data.frame(
      region = sprintf("R%02d", g), year = 2000 + seq_len(nYr),
      y = y, x = x, lagged_ecp = c(0, head(y, -1))
    )
  }
  do.call(rbind, rows)
}

test_that("split-panel jackknife returns bias-corrected coefficients", {
  df <- makeDynamicPanel()
  df$region <- factor(df$region)
  fml <- y ~ lagged_ecp + x + region
  spj <- splitPanelJackknife(fml, df, gaussian(link = "identity"))
  expect_false(is.null(spj))
  expect_true("lagged_ecp" %in% spj$corrected)
  # within (FE) estimator of rho is biased DOWN by Nickell; SPJ should pull the
  # lag coefficient UP toward the true 0.6 (i.e. larger than the naive within fit)
  naive <- coef(glm(fml, data = df, family = gaussian()))[["lagged_ecp"]]
  expect_gt(spj$coefficients[["lagged_ecp"]], naive)
})

test_that("SPJ returns NULL (graceful) when a half is not estimable", {
  # Too few observations per region to split and refit with FE => not estimable
  df <- makeDynamicPanel(nReg = 3, nYr = 3)
  df$region <- factor(df$region)
  spj <- splitPanelJackknife(y ~ lagged_ecp + x + region, df, gaussian("identity"))
  expect_null(spj)
})

test_that("channelSpecs exhaustive includes the four lag alternatives", {
  ex <- channelSpecs("exhaustive")
  # exclude the saturating stringency-only twins (ADR 0028) — each base lag spec
  # is twinned, so the raw includeLaggedECP count is 8
  lag <- Filter(function(s) isTRUE(s$includeLaggedECP) && !isTRUE(s$stringencyOnly), ex)
  expect_equal(length(lag), 4)
  # each lag spec is either FE + Nickell, or no-FE (bias-free)
  for (s in lag) {
    if (is.null(s$regionMappingFixedEffects)) {
      expect_false(isTRUE(s$nickellCorrection)) # no FE => no Nickell bias
    } else {
      expect_true(isTRUE(s$nickellCorrection))  # FE => correction on
    }
  }
})
