# Design note 0005 E17/F7: the harmonisation's historical panel is built once per REMIND run and
# read from the run folder afterwards, with the .pfm_env state it leaves for the scenario panel.

test_that("the harmonisation panel is cached with its state, and rebuilt when the arguments change", {
  calls <- 0L
  fake <- function(...) {
    calls <<- calls + 1L
    assign("sc_pca_rotation", list(rot = calls), envir = pfm:::.pfm_env)
    assign("gdppc_q_fit", list(fit = calls), envir = pfm:::.pfm_env)
    paste("panel", calls)
  }
  testthat::local_mocked_bindings(panelDataHistorical = fake)
  cache <- file.path(withr::local_tempdir(), "pfm", "hist-harmonisation-cache.rds")
  args <- list(aggregate = TRUE, y = 2000:2023, outputRegionMappingFile = "x.csv",
               movingAverage = 5L, ieaVersion = "latest", geothermal = TRUE, coeff = list())
  expect_identical(pfm:::.pfmHistHarmonisationPanel(args, cache), "panel 1")
  expect_true(file.exists(cache))
  # state overwritten by something else in the session ...
  assign("sc_pca_rotation", "other", envir = pfm:::.pfm_env)
  # ... a second call reads the file, does not rebuild, and restores the state
  expect_identical(pfm:::.pfmHistHarmonisationPanel(args, cache), "panel 1")
  expect_identical(calls, 1L)
  expect_identical(get("sc_pca_rotation", envir = pfm:::.pfm_env), list(rot = 1L))
  # another definition (annual values) is another key: rebuilt
  args$movingAverage <- NULL
  expect_identical(pfm:::.pfmHistHarmonisationPanel(args, cache), "panel 2")
  # no cache: always built, nothing written
  expect_identical(pfm:::.pfmHistHarmonisationPanel(args, NULL), "panel 3")
})
