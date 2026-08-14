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

  diagnostics <- c("psm-agreement", "psm-iv", "psm-influence", "psm-replay")

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
