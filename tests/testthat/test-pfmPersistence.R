# nolint start
# Cache-layer mechanics (ADR 0009): slim Fitted Model, the cache/ subfolder
# layout, the content-addressed Training Panel store, and the disposable
# Projection store. The full predictFeasibility round-trip against a REMIND gdx
# is exercised by the channels workflow; here we test the persistence contract.

makeToyFit <- function(n = 60) {
  set.seed(11)
  df <- data.frame(
    region   = rep(c("AAA", "BBB", "CCC"), length.out = n),
    year     = rep(2000:2019, length.out = n),
    x1       = rnorm(n),
    regionFE = factor(rep(c("R1", "R2"), length.out = n))
  )
  df$ecp <- pmax(0, 2 + 1.5 * df$x1 + rnorm(n))
  fml <- ecp ~ x1 + regionFE
  m <- stats::glm(fml, data = df, family = stats::gaussian())
  attr(df, "driverScaling") <- list(x1 = list(mean = 0, sd = 1))
  list(
    fit = list(model = m, formula = fml,
               coeftest = summary(m)$coefficients,
               vcov = stats::vcov(m)),
    df = df
  )
}

test_that("savePFMModel writes a slim model under models/ and round-trips", {
  toy <- makeToyFit()
  dir <- withr::local_tempdir()
  options(pfm.trainingPanelHash = "deadbeefdeadbeef")
  on.exit(options(pfm.trainingPanelHash = NULL), add = TRUE)

  pm <- buildPFMModel(toy$fit, toy$df, sector = "Bulk", stage = "stringency",
                      family = "gaussian", useFirth = FALSE,
                      prepSpec = list(actorPowerDrivers = "x1"))
  savePFMModel(pm, dir)

  # Lands in the models/ subfolder; index.json at the cache root.
  expect_true(file.exists(file.path(dir, "models", paste0(pm$id, ".rds"))))
  expect_true(file.exists(file.path(dir, "index.json")))

  loaded <- loadPFMModel(pm$id, dir)
  expect_s3_class(loaded, "PFMModel")
  # Training data is NOT embedded; it is referenced by hash.
  expect_null(loaded$training_data)
  expect_identical(loaded$training_panel_hash, "deadbeefdeadbeef")
  # Frozen transforms + apply-state travel with the model.
  expect_true(!is.null(loaded$transforms$driverScaling))
  expect_equal(loaded$transforms$prepSpec$actorPowerDrivers, "x1")
  expect_true(!is.null(loaded$applyState$seed_prices))
  expect_true("R1" %in% loaded$applyState$regionFE_levels)
})

test_that(".stripFit drops data-bearing slots but the fit still predicts", {
  toy <- makeToyFit()
  pm <- buildPFMModel(toy$fit, toy$df, sector = "Bulk", stage = "stringency",
                      family = "gaussian", useFirth = FALSE)
  for (slot in c("model", "residuals", "fitted.values", "data", "y")) {
    expect_null(pm$model[[slot]])
  }
  # qr is deliberately retained: predict.lm dereferences qr$pivot.
  expect_false(is.null(pm$model$qr))
  # predict.glm needs terms/xlevels/coefficients/family/contrasts + qr$pivot.
  nd <- toy$df[1:3, ]
  pred <- stats::predict(pm$model, newdata = nd, type = "response")
  expect_length(pred, 3)
  expect_true(all(is.finite(pred)))
})

test_that("Training Panel store is content-addressed and sets the hash option", {
  dir <- withr::local_tempdir()
  panel <- matrix(rnorm(20), 5, 4)
  h1 <- saveTrainingPanel(panel, dir)
  expect_identical(getOption("pfm.trainingPanelHash"), h1)
  expect_true(file.exists(file.path(dir, "panels", paste0("panel_", h1, ".rds"))))
  # Same content -> same hash, no second file.
  h2 <- saveTrainingPanel(panel, dir)
  expect_identical(h1, h2)
  expect_length(list.files(file.path(dir, "panels")), 1L)
  # Round-trips back.
  expect_equal(loadTrainingPanel(h1, dir), panel)
  expect_null(loadTrainingPanel(NA_character_, dir))
})

test_that("Projection store persists under a caller label and round-trips", {
  dir <- withr::local_tempdir()
  proj <- data.frame(region = "AAA", year = 2030, price = 42)
  saveProjection(proj, label = "NPi/2030 run", dir = dir, meta = list(gdx = "x.gdx"))
  expect_true("NPi_2030_run" %in% listProjections(dir))
  expect_equal(loadProjection("NPi/2030 run", dir), proj)
  expect_null(loadProjection("does-not-exist", dir))
})

test_that(".rehydrateFitForConsumers restores y/fitted on a stripped glm", {
  toy <- makeToyFit()
  pm <- buildPFMModel(toy$fit, toy$df, sector = "Bulk", stage = "stringency",
                      family = "gaussian", useFirth = FALSE)
  expect_null(pm$model$y)           # stripped on save
  expect_null(pm$model$fitted.values)
  reh <- pfm:::.rehydrateFitForConsumers(pm$model, toy$df, toy$fit$formula, "ecp")
  expect_false(is.null(reh$y))
  expect_false(is.null(reh$fitted.values))
  expect_equal(length(stats::fitted(reh)), length(reh$y))
})

test_that("predictFeasibility validates model stages", {
  toy <- makeToyFit()
  aPm <- buildPFMModel(toy$fit, toy$df, sector = "Bulk", stage = "adoption",
                       family = "logistf", useFirth = TRUE)
  sPm <- buildPFMModel(toy$fit, toy$df, sector = "Bulk", stage = "stringency",
                       family = "gaussian", useFirth = FALSE)
  # Swapped stages must error.
  expect_error(predictFeasibility(sPm, aPm, scenarioData = NULL),
               "stage='adoption'")
})
# nolint end
