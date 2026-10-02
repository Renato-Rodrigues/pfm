# nolint start
# The psm -> pfm rename (2026-10-02, design note 0005 D21). Run-Groups written before it carry
# the legacy names: selected-models-psm.yml and psm-* steps. They must keep resolving, or every
# frozen Run-Group (v5 and the paper bundle built on it) silently stops being reproducible.

test_that("the spec file resolves to the new name, falls back to the legacy one", {
  d <- withr::local_tempdir()
  expect_identical(basename(pfm:::.pfmSelectedModels(d)), "selected-models-pfm.yml")  # neither
  writeLines("x: 1", file.path(d, "selected-models-psm.yml"))
  expect_identical(basename(pfm:::.pfmSelectedModels(d)), "selected-models-psm.yml")  # legacy only
  writeLines("x: 2", file.path(d, "selected-models-pfm.yml"))
  expect_identical(basename(pfm:::.pfmSelectedModels(d)), "selected-models-pfm.yml")  # both: new wins
})

test_that("legacy step names map to pfm-*, and only the psm- prefix is touched", {
  expect_message(s <- pfm:::.pfmLegacySteps(c("psm-frontier", "pfm-donor", "sweep")), "now pfm-")
  expect_identical(s, c("pfm-frontier", "pfm-donor", "sweep"))
  expect_silent(s <- pfm:::.pfmLegacySteps("psm-replay", quiet = TRUE))
  expect_identical(s, "pfm-replay")
  expect_null(pfm:::.pfmLegacySteps(NULL))
  expect_identical(names(suppressMessages(pfmStepArtifacts("psm-frontier"))), "pfm-frontier")
})

test_that("resume keys pfm-sweep off sweep.rds, and clean removes both spec-file names", {
  a <- pfmStepArtifacts("pfm-sweep")[["pfm-sweep"]]
  expect_identical(a[1], "sweep.rds")
  expect_true(all(c("selected-models-pfm.yml", "selected-models-psm.yml") %in% a))
})

test_that("the old exported names still work, with a deprecation warning", {
  expect_warning(old <- psmStepArtifacts("pfm-frontier"), "deprecated")
  expect_identical(old, pfmStepArtifacts("pfm-frontier"))
})

test_that("the frozen v5 Run-Group resolves through the new code", {
  # Workstation only: output/ is not in git. tests/testthat -> models/pfm -> models -> project.
  v5 <- normalizePath(file.path(testthat::test_path(), "..", "..", "..", "..", "output", "pfm", "v5"),
                      mustWork = FALSE)
  skip_if_not(dir.exists(v5), "output/pfm/v5 not present (workstation only)")
  p <- pfm:::.pfmSelectedModels(v5)
  expect_true(file.exists(p))
  sel <- yaml::read_yaml(p)
  expect_true(length(sel) >= 1 && is.character(sel[[1]]$name) && nzchar(sel[[1]]$name))
})
# nolint end
