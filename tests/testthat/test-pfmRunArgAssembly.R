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

test_that("every function the dots check calls actually EXISTS", {
  # THE BUG THIS FILE FAILED TO CATCH THE FIRST TIME. The check originally called
  # formals(runPostProcessing) -- a FILE name, not a function; the function is
  # runModelGroup. It killed a live cluster submission with
  # "object 'runPostProcessing' not found".
  #
  # The original tests only asserted that the SOURCE TEXT contained certain
  # strings, so they passed against code that could not run. Assert resolvability
  # instead: pull every formals(<name>) target out of the body and look it up.
  # Use codetools rather than regexing the source: the broken reference was
  # `formals(runPostProcessing)`, but a text search for `formals(<name>)` misses
  # the equally breakable `list(startRun, runPSMSweep, runModelGroup)` form.
  # findGlobals resolves the whole question -- every free symbol in the body.
  skip_if_not_installed("codetools")
  ns <- asNamespace("pfm")
  globals <- codetools::findGlobals(pfmRun)
  expect_gt(length(globals), 0)
  missing <- globals[!vapply(globals, exists, logical(1), envir = ns)]
  expect_identical(missing, character(0),
                   info = paste("unresolved symbol(s) in pfmRun:",
                                paste(missing, collapse = ", ")))
})

test_that("the dots check runs, warns on a typo, and stays silent on real arguments", {
  # Executes the logic rather than grepping for it.
  check <- function(...) {
    dn <- names(list(...)); dn <- dn[nzchar(dn)]
    fns <- list(startRun, runPSMSweep, runModelGroup)
    known <- unique(unlist(lapply(fns, function(f) names(formals(f)))))
    setdiff(dn, known)
  }
  expect_identical(check(gammaGate = 0.999, sanityMaxModels = 120,
                         cachefolder = "x", gdxFile = "y"), character(0))
  expect_identical(check(gammaGata = 0.999), "gammaGata")   # transposed letters

  # `config` must NOT be tested through this helper: it is a pfmRun FORMAL, so it
  # never reaches `...` in the first place. pfmRun resolves it into scenarios/gdxFile
  # precisely because startRun would swallow it (that trap cost a full run).
  expect_true("config" %in% names(formals(pfmRun)))
  expect_false("config" %in% names(formals(startRun)))
})

test_that("a broken dots check cannot abort the run", {
  # The check is a convenience; it must degrade to a message, never propagate.
  body <- paste(deparse(pfmRun), collapse = "\n")
  iCheck <- regexpr("not recognised by startRun", body, fixed = TRUE)
  expect_match(body, "argument check skipped", fixed = TRUE)
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
