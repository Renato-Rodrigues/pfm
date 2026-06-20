# Fit Cache write-concurrency primitives (ADR 0019): savePFMModel(updateIndex=),
# rebuildPFMModelIndex(), and corrupt-rds tolerance.

test_that("savePFMModel(updateIndex = FALSE) writes the rds but not the index", {
  dir <- withr::local_tempdir()
  savePFMModel(syntheticPFMModel("aaa"), dir, updateIndex = FALSE)
  savePFMModel(syntheticPFMModel("bbb"), dir, updateIndex = FALSE)
  expect_true(file.exists(file.path(dir, "models", "aaa.rds")))
  expect_false(file.exists(file.path(dir, "index.json")))
  expect_equal(nrow(listPFMModels(dir)), 0)
})

test_that("rebuildPFMModelIndex reconstructs the index from the rds files", {
  dir <- withr::local_tempdir()
  savePFMModel(syntheticPFMModel("aaa"), dir, updateIndex = FALSE)
  savePFMModel(syntheticPFMModel("bbb"), dir, updateIndex = FALSE)
  idx <- rebuildPFMModelIndex(dir)
  expect_equal(nrow(idx), 2)
  expect_setequal(listPFMModels(dir)$id, c("aaa", "bbb"))
  # default save still updates the index incrementally
  savePFMModel(syntheticPFMModel("ccc"), dir)
  expect_equal(nrow(listPFMModels(dir)), 3)
})

test_that("rebuildPFMModelIndex skips unreadable/corrupt fits with a warning", {
  dir <- withr::local_tempdir()
  savePFMModel(syntheticPFMModel("aaa"), dir, updateIndex = FALSE)
  writeLines("not-an-rds", file.path(dir, "models", "zzz.rds"))
  expect_warning(idx <- rebuildPFMModelIndex(dir), "unreadable|invalid")
  expect_equal(nrow(idx), 1)
  expect_equal(listPFMModels(dir)$id, "aaa")
})
