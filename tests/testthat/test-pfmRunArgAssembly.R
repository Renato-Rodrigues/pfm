# nolint start
# pfmRun() builds startRun()'s argument list from its own resolved defaults PLUS
# the caller's `...`. On 2026-08-25 those were spliced into one list() literal, so
# passing `cachefolder` -- which every startRun example in the docs does -- produced
# a duplicated name and do.call died with:
#
#   formal argument "cachefolder" matched by multiple actual arguments
#
# The failure landed AFTER the plan printed and AFTER `dryRun = TRUE` had reported
# success, because dryRun returns before the assembly. Both halves are pinned here:
# the caller must win, and a dry run must not bless a call that cannot execute.

collidable <- c("cachefolder", "scenarios", "outputRegionMappingFile")

test_that("caller arguments override pfmRun's resolved defaults instead of colliding", {
  base <- list(group = "g", steps = "psm-sweep", cluster = "slurm",
               resultsDir = "r", modelDir = "m", resume = TRUE,
               scenarios = NULL, cachefolder = "FROM_CONFIG",
               outputRegionMappingFile = "country")
  for (nm in collidable) {
    dots <- stats::setNames(list("FROM_CALLER"), nm)
    args <- utils::modifyList(base, dots)
    expect_false(any(duplicated(names(args))), info = nm)
    expect_identical(args[[nm]], "FROM_CALLER", info = nm)
    # do.call must accept the result -- the actual failure mode
    expect_silent(do.call(function(cachefolder = NULL, scenarios = NULL,
                                   outputRegionMappingFile = NULL, ...) NULL, args))
  }
})

test_that("pfmRun assembles args with modifyList, not a spliced literal", {
  body <- paste(deparse(pfmRun), collapse = "\n")
  expect_match(body, "modifyList", fixed = TRUE)
  # the regression: `...` spliced directly into the same list() as cachefolder
  expect_false(grepl("cachefolder = rc$cachefolder,\n           outputRegionMappingFile = \"country\", ...)",
                     body, fixed = TRUE))
})

test_that("unknown dots are warned about before the dryRun exit", {
  body <- paste(deparse(pfmRun), collapse = "\n")
  iCheck <- regexpr("not recognised by startRun", body, fixed = TRUE)
  # Anchor on the EXIT message, not on "dryRun = TRUE" -- that string also appears
  # in the earlier psmCleanSteps(dryRun = TRUE) preview call, and matching it made
  # this test compare against the wrong line.
  iDry <- regexpr("nothing was run", body, fixed = TRUE)
  expect_true(iCheck > 0)
  expect_true(iDry > 0)
  # the validation must run BEFORE dryRun returns, or a dry run still blesses a
  # call that will fail live
  expect_lt(iCheck, iDry)
})

test_that("gammaGate and sanityMaxModels survive the dots filter into runPSMSweep", {
  # runPostProcessing filters dots to formals(runPSMSweep); a gate that does not
  # appear there is DROPPED SILENTLY and the sweep runs ungated with no error.
  fs <- names(formals(runPSMSweep))
  expect_true("gammaGate" %in% fs)
  expect_true("sanityMaxModels" %in% fs)
  # `config` is the known casualty of this filter -- keep it documented in a test
  expect_false("config" %in% names(formals(startRun)))
})
# nolint end
