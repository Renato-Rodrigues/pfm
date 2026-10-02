# Design note 0005 E23 / PITFALLS section 18: a step that produces no fresh artifact must fail the
# run, and an artifact it did not rewrite must not stay where the next reader would take it.

staleGroup <- function() {
  rd <- withr::local_tempdir(.local_envir = parent.frame())
  gd <- file.path(rd, "g")
  dir.create(file.path(gd, "coupling"), recursive = TRUE)
  f <- file.path(gd, "coupling", "coupling-summary.rds")
  saveRDS(1, f)
  Sys.setFileTime(f, Sys.time() - 3600)
  list(rd = rd, f = f)
}

test_that("an artifact the step did not rewrite is moved aside to *.stale, and the run warns", {
  oldFc <- madrat::getConfig("forcecache", verbose = FALSE)
  withr::defer(suppressMessages(madrat::setConfig(forcecache = oldFc, .verbose = FALSE)))
  g <- staleGroup()
  # pfm-coupling-bound self-skips: its inputs (spec, frontier, ...) are absent
  expect_warning(
    r <- suppressMessages(runModelGroup("g", steps = "pfm-coupling-bound", resultsDir = g$rd,
                                        modelDir = g$rd, verbose = FALSE)),
    "did not produce a fresh artifact"
  )
  expect_identical(attr(r, "notRefreshed"), "pfm-coupling-bound")
  expect_false(file.exists(g$f))
  expect_true(file.exists(paste0(g$f, ".stale")))
})

test_that("startRun fails on a run with gaps, unless failOnGaps = FALSE", {
  oldFc <- madrat::getConfig("forcecache", verbose = FALSE)
  withr::defer(suppressMessages(madrat::setConfig(forcecache = oldFc, .verbose = FALSE)))
  g <- staleGroup()
  expect_error(
    suppressWarnings(suppressMessages(startRun("g", steps = "pfm-coupling-bound", resultsDir = g$rd,
                                               modelDir = g$rd, cluster = "local", verbose = FALSE))),
    "incomplete"
  )
  man <- jsonlite::read_json(file.path(g$rd, "g", "manifest.json"))
  expect_identical(man$run$status, "incomplete")
  g2 <- staleGroup()
  r <- suppressWarnings(suppressMessages(startRun("g", steps = "pfm-coupling-bound", resultsDir = g2$rd,
                                                  modelDir = g2$rd, cluster = "local", verbose = FALSE,
                                                  failOnGaps = FALSE)))
  expect_identical(r$status, "incomplete")
})
