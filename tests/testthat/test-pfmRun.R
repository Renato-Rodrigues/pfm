test_that("stage resolution is complete and correctly ordered", {
  # Regression guard for the defect found 2026-08-14: `stage = "all"` was assembled
  # from the other stage vectors, and the four inference/validation steps belonged to
  # none of them. They were reachable only through the interactive `custom` menu, so
  # a Run-Group produced with stage = "all" looked complete while carrying no
  # wild-cluster p-values, no IV, no influence diagnostics and no replay gate.
  grp <- "unit-test-group"
  dir.create(file.path(tempdir(), "res", grp), recursive = TRUE, showWarnings = FALSE)
  res <- file.path(tempdir(), "res")
  # A spec file makes the run look like a finished sweep, so the "no deployed spec"
  # prompt cannot fire and dryRun stays non-interactive.
  writeLines("- name: dummy", file.path(res, grp, "selected-models-psm.yml"))

  plan <- function(stage) {
    pfmRun(group = grp, stage = stage, cluster = "local", resultsDir = res,
           modelDir = res, ask = FALSE, dryRun = TRUE)$steps
  }

  diagnostics <- c("psm-agreement", "psm-iv", "psm-influence", "psm-inference", "psm-replay")

  expect_setequal(plan("diagnostics"), diagnostics)

  allSteps <- plan("all")
  # The point of the guard: every other stage is a subset of "all".
  for (s in c("sweep", "diagnostics", "downstream", "remind")) {
    expect_true(all(plan(s) %in% allSteps),
                info = paste("stage", s, "is not contained in stage 'all'"))
  }
  expect_true(all(diagnostics %in% allSteps))

  # Dependency order: the sweep produces the spec that diagnostics and downstream
  # read, and the REMIND export consumes the result of both.
  expect_lt(max(match(plan("sweep"), allSteps)),
            min(match(diagnostics, allSteps)))
  expect_lt(max(match(plan("downstream"), allSteps)),
            match("psm-remind-inputs", allSteps))

  # Every step must have an artifact entry, or resume/clean silently disagree
  # about what the step produced (see psmStepArtifacts()).
  known <- names(psmStepArtifacts())
  expect_true(all(allSteps %in% known),
              info = paste("unmapped steps:",
                           paste(setdiff(allSteps, known), collapse = ", ")))
})

test_that("several stages are one chain, in dependency order, with the export in the job", {
  grp <- "unit-test-chain"
  res <- file.path(tempdir(), "res-chain")
  dir.create(file.path(res, grp), recursive = TRUE, showWarnings = FALSE)
  writeLines("- name: dummy", file.path(res, grp, "selected-models-psm.yml"))
  run <- function(stage, dryRun = TRUE) {
    pfmRun(group = grp, stage = stage, cluster = "local", resultsDir = res, modelDir = res,
           remindDir = file.path(tempdir(), "ri"), ask = FALSE, prepareCache = FALSE,
           dryRun = dryRun)
  }
  all <- suppressMessages(run("all"))$steps
  chain <- suppressMessages(run(c("downstream", "sweep", "remind")))$steps
  # the union of the stages, re-sorted into the order of "all": the sweep comes first
  expect_equal(chain, intersect(all, chain))
  expect_equal(chain[1], "psm-sweep")
  expect_equal(utils::tail(chain, 1), "psm-remind-inputs")
  expect_error(suppressMessages(run(c("custom", "sweep"))), "cannot be combined")

  # the export travels with the other steps, to an absolute destination
  got <- NULL
  testthat::local_mocked_bindings(startRun = function(...) { got <<- list(...); invisible(NULL) })
  suppressMessages(run(c("downstream", "remind"), dryRun = FALSE))
  expect_true("psm-remind-inputs" %in% got$steps)
  expect_equal(got$dest, normalizePath(file.path(tempdir(), "ri"), winslash = "/", mustWork = FALSE))
})
