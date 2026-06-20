# nolint start
# The fit-grid engine (ADR 0019): fitOneSpec / runFitGrid, the unit the parallel sweep is
# built on. Self-contained on a small synthetic panel.

test_that("fitOneSpec returns metrics + coef rows and self-persists without the index", {
  skip_if_not_installed("logistf")
  panel <- makeSyntheticPanel()
  dir <- withr::local_tempdir()
  out <- fitOneSpec(syntheticSpec(), "Bulk", "Adoption", panelData = panel, modelDir = dir)
  expect_true(is.data.frame(out$metrics) && nrow(out$metrics) == 1)
  expect_identical(out$metrics$model, "t1")
  # the fit was written to the store but the index was NOT touched (worker contract)
  expect_gt(length(list.files(file.path(dir, "models"), pattern = "\\.rds$")), 0)
  expect_false(file.exists(file.path(dir, "index.json")))
})

test_that("runFitGrid writes the index once, and resume avoids duplicated work", {
  skip_if_not_installed("logistf")
  panel <- makeSyntheticPanel()
  dir <- withr::local_tempdir()
  sectors <- c("Bulk", "Diffuse"); stages <- c("Adoption", "Stringency")
  g1 <- runFitGrid(list(syntheticSpec()), sectors, stages, panel, modelDir = dir,
                   nCores = 1, verbose = FALSE)
  expect_equal(g1$nJobs, 4)
  expect_equal(nrow(g1$results), 4)
  # master wrote the index once; its rows equal the number of fits actually written
  expect_true(file.exists(file.path(dir, "index.json")))
  expect_equal(nrow(listPFMModels(dir)), g1$nNew)
  # resume: a second identical run re-fits nothing
  g2 <- runFitGrid(list(syntheticSpec()), sectors, stages, panel, modelDir = dir,
                   nCores = 1, verbose = FALSE)
  expect_equal(g2$nNew, 0)
  # forceRefit re-estimates the successful specs, overwriting the existing rds in place
  # (so no NEW files are added: nNew == 0), evidenced by advanced modification times.
  mtBefore <- max(file.mtime(list.files(file.path(dir, "models"), full.names = TRUE)))
  Sys.sleep(1.1)
  g3 <- runFitGrid(list(syntheticSpec()), sectors, stages, panel, modelDir = dir,
                   nCores = 1, forceRefit = TRUE, verbose = FALSE)
  mtAfter <- max(file.mtime(list.files(file.path(dir, "models"), full.names = TRUE)))
  expect_equal(g3$nNew, 0)
  expect_equal(nrow(g3$results), 4)
  expect_gt(as.numeric(mtAfter), as.numeric(mtBefore))
})

test_that("parallel runFitGrid agrees with sequential (parity)", {
  skip_if_not_installed("logistf")
  skip_if_not_installed("future.apply")
  panel <- makeSyntheticPanel()
  sectors <- c("Bulk", "Diffuse"); stages <- c("Adoption", "Stringency")
  d1 <- withr::local_tempdir(); d2 <- withr::local_tempdir()
  g1 <- runFitGrid(list(syntheticSpec()), sectors, stages, panel, modelDir = d1, nCores = 1, verbose = FALSE)
  g2 <- runFitGrid(list(syntheticSpec()), sectors, stages, panel, modelDir = d2, nCores = 2, verbose = FALSE)
  key <- function(df) {
    cc <- intersect(names(df), c("model", "sector", "stage", "aic", "bic", "pseudoR2", "converged"))
    df[order(df$model, df$sector, df$stage), cc]
  }
  expect_equal(key(g1$results), key(g2$results), tolerance = 1e-6)
  expect_equal(g1$nNew, g2$nNew)
})
# nolint end
