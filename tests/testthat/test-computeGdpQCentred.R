makeGdpMagpie <- function(regions, years, values) {
  m <- magclass::new.magpie(regions, years, "GDP per Capita", fill = NA)
  for (i in seq_along(regions)) m[regions[i], , ] <- values[[i]]
  m
}

resetGdpQFit <- function() {
  env <- pfm:::.pfm_env
  if (exists("gdppc_q_fit", envir = env)) rm("gdppc_q_fit", envir = env)
  if (exists("gdppc_q_breaks", envir = env)) rm("gdppc_q_breaks", envir = env)
}

test_that("scenario apply-mode reuses historical breaks, means and assignments", {
  resetGdpQFit()
  regions <- c("R01", "R02", "R03", "R04", "R05", "R06", "R07", "R08")
  histYears <- 2000:2010
  # Region means spread over [0.1, 0.8] => two regions per quartile
  histVals <- lapply(seq(0.1, 0.8, by = 0.1), function(v) rep(v, length(histYears)))
  histMag <- makeGdpMagpie(regions, histYears, histVals)

  histQ <- pfm:::computeGdpQCentred(histMag, storeBreaks = TRUE)
  fit <- pfm:::.pfm_env$gdppc_q_fit
  expect_false(is.null(fit))
  expect_length(fit$group, 8)
  expect_equal(sort(unique(unname(fit$group))), 1:4)

  # Scenario: same regions, MUCH higher GDP (everyone grows by +0.15)
  scenYears <- 2025:2035
  scenVals <- lapply(seq(0.1, 0.8, by = 0.1) + 0.15, function(v) rep(v, length(scenYears)))
  scenMag <- makeGdpMagpie(regions, scenYears, scenVals)
  scenQ <- pfm:::computeGdpQCentred(scenMag, storeBreaks = FALSE)

  # Frozen reference: scenario value = raw - HISTORICAL group mean of the
  # region's HISTORICAL group => exactly historical Q-centred + growth (0.15)
  # for every region, even those whose scenario mean crosses a stored break.
  for (r in regions) {
    expect_equal(
      as.numeric(scenQ[r, 1, ]),
      as.numeric(histQ[r, 1, ]) + 0.15,
      tolerance = 1e-12
    )
  }
})

test_that("regions absent from the historical fit are assigned via stored breaks", {
  resetGdpQFit()
  regions <- c("R01", "R02", "R03", "R04", "R05", "R06", "R07", "R08")
  histVals <- lapply(seq(0.1, 0.8, by = 0.1), function(v) rep(v, 5))
  histMag <- makeGdpMagpie(regions, 2000:2004, histVals)
  invisible(pfm:::computeGdpQCentred(histMag, storeBreaks = TRUE))
  fit <- pfm:::.pfm_env$gdppc_q_fit

  # New region with mean 0.75 lands in the top historical quartile
  scenMag <- makeGdpMagpie(c("R01", "NEW"), 2025:2027, list(rep(0.25, 3), rep(0.75, 3)))
  scenQ <- pfm:::computeGdpQCentred(scenMag, storeBreaks = FALSE)
  topQMean <- unname(fit$means["4"])
  expect_equal(as.numeric(scenQ["NEW", 1, ]), 0.75 - topQMean, tolerance = 1e-12)
})

test_that("computeModelId distinguishes fits by the extra (family) key", {
  f <- y ~ x
  d <- data.frame(y = 1:10, x = rnorm(10))
  base <- computeModelId(f, d)
  # NULL extra reproduces the legacy two-argument key exactly (adoption-safe)
  expect_identical(base[["id"]], computeModelId(f, d, extra = NULL)[["id"]])
  # different family strings yield different keys
  g <- computeModelId(f, d, extra = "Gamma")
  h <- computeModelId(f, d, extra = "gaussian")
  expect_false(identical(g[["id"]], h[["id"]]))
  expect_false(identical(base[["id"]], g[["id"]]))
})

test_that("without a stored fit, apply-mode falls back to fitting on the data", {
  resetGdpQFit()
  regions <- paste0("R", 1:8)
  mag <- makeGdpMagpie(regions, 2000:2004,
                       lapply(seq(0.1, 0.8, by = 0.1), function(v) rep(v, 5)))
  out <- pfm:::computeGdpQCentred(mag, storeBreaks = FALSE)
  expect_false(is.null(out))
  # no fit cached when storeBreaks = FALSE
  expect_null(pfm:::.pfm_env$gdppc_q_fit)
  resetGdpQFit()
})
