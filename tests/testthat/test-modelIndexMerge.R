# The sweep's master merges the index rows its workers return into index.json, instead of re-reading
# every fit in the Fit Cache (v7, 2026-10-22: the full rebuild read 600 GB on one core).

test_that("index rows are bound by column, like the rbind they replace", {
  e <- list(data.frame(id = "a", aic = 1, converged = TRUE, stringsAsFactors = FALSE),
            data.frame(id = "b", aic = 2, converged = NA, stringsAsFactors = FALSE))
  expect_equal(.pfmBindIndexRows(e), do.call(rbind, e))
  expect_equal(nrow(.pfmBindIndexRows(list())), 0)
})

test_that("a merge replaces rows by id, keeps the others and drops rows without a file", {
  d <- withr::local_tempdir()
  dir.create(file.path(d, "models"))
  for (id in c("a", "b", "c")) saveRDS(1, file.path(d, "models", paste0(id, ".rds")))
  old <- data.frame(id = c("a", "b", "gone"), aic = c(1, 2, 9), stringsAsFactors = FALSE)
  jsonlite::write_json(old, file.path(d, "index.json"), auto_unbox = TRUE)
  idx <- .pfmMergeModelIndex(d, list(data.frame(id = "b", aic = 20, bic = 5, stringsAsFactors = FALSE),
                                     data.frame(id = "c", aic = 3, bic = 6, stringsAsFactors = FALSE)))
  idx <- idx[order(idx$id), ]
  expect_identical(idx$id, c("a", "b", "c"))
  expect_equal(idx$aic, c(1, 20, 3))
  expect_true(is.na(idx$bic[1]))
  expect_equal(listPFMModels(d)$id[order(listPFMModels(d)$id)], c("a", "b", "c"))
})

test_that("a first merge writes the index from the new rows alone", {
  d <- withr::local_tempdir()
  idx <- .pfmMergeModelIndex(d, list(data.frame(id = "x", aic = 1, stringsAsFactors = FALSE)))
  expect_identical(idx$id, "x")
  expect_true(file.exists(file.path(d, "index.json")))
})

test_that("runFitGrid no longer rebuilds the index from disk", {
  src <- paste(deparse(body(runFitGrid)), collapse = "\n")
  expect_false(grepl("rebuildPFMModelIndex(modelDir)", src, fixed = TRUE))
  expect_match(src, ".pfmMergeModelIndex(modelDir, entries", fixed = TRUE)
})

test_that("only the listed fits without a row are read", {
  d <- withr::local_tempdir()
  dir.create(file.path(d, "models"))
  m <- structure(list(id = "r", sector = "Bulk", formula = y ~ x, diagnostics = list()), class = "PFMModel")
  saveRDS(m, file.path(d, "models", "r.rds"))
  saveRDS("not read", file.path(d, "models", "s.rds"))
  idx <- .pfmMergeModelIndex(d, list(data.frame(id = "s", aic = 1, stringsAsFactors = FALSE)), readIds = c("r", "s"))
  expect_setequal(idx$id, c("r", "s"))
  expect_equal(idx$aic[idx$id == "s"], 1)
})
