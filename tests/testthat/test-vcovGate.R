# nolint start
# The frontier covariance gate (design note 0005 E13; was TODO 40). .pfmFrontierVcov() checks
# FRONTIER 4.1's covariance on every frontier fit, but until 2026-10-02 selection never looked at
# the result, so a spec whose standard errors could not be trusted could be deployed.
#
# Pinned here, in the same way test-gammaGate.R pins the gamma gate:
#   1. the gated statuses, and that "corrupt" is NOT among them (the recomputed matrix
#      replaces FRONTIER's, TODO 14f) - a silent change would retro-invalidate a Run-Group;
#   2. the gate reaches both sanity-walk call sites;
#   3. the status comes from the frontier fit the walk already makes - no extra fit;
#   4. the status is recorded even when the gate is off.

test_that("vcovGate defaults to the two untrustworthy statuses in the sweep and the walk", {
  want <- c("likelihood-mismatch", "flat")
  expect_identical(eval(formals(runPFMSweep)$vcovGate), want)
  expect_identical(eval(formals(pfm:::.pfmSanitySelect)$vcovGate), want)
  expect_false("corrupt" %in% want)
})

test_that("vcovGate reaches every sanity-walk call site", {
  body <- paste(deparse(runPFMSweep), collapse = "\n")
  nCeil <- lengths(regmatches(body, gregexpr("ceilingFallGate = ceilingFallGate", body)))
  nVcov <- lengths(regmatches(body, gregexpr("vcovGate = vcovGate", body)))
  expect_gt(nCeil, 0)
  expect_identical(nVcov, nCeil)
})

test_that("the ceiling helper returns the covariance status of the fit it already made", {
  body <- paste(deparse(pfm:::.pfmCeilingTrajectory), collapse = "\n")
  expect_match(body, "vcovStatus = ff$vcovCheck$status", fixed = TRUE)
  walk <- paste(deparse(pfm:::.pfmSanitySelect), collapse = "\n")
  expect_match(walk, "ct$vcovStatus", fixed = TRUE)
  expect_match(walk, "frontierVcov", fixed = TRUE)
})

test_that("the covariance status is recorded even when the gate is off", {
  walk <- paste(deparse(pfm:::.pfmSanitySelect), collapse = "\n")
  expect_match(walk, "modelVcov[[sec]] <- ct$vcovStatus", fixed = TRUE)
  expect_match(walk, "vcovStatus = vcovByModel", fixed = TRUE)
  # the assignment must sit BEFORE the gate test
  i <- regexpr("modelVcov[[sec]] <- ct$vcovStatus", walk, fixed = TRUE)
  j <- regexpr("ct$vcovStatus %in% vcovGate", walk, fixed = TRUE)
  expect_true(i > 0 && j > 0 && i < j)
})

# Functional: the fixtures switch ceilingFallGate off (helper-pfm.R), and the covariance check
# only runs with the ceiling check, so these two switch it on with a permissive threshold.
test_that("a real sweep records a valid covariance status for the evaluated specs", {
  res <- suppressMessages(suppressWarnings(
    pfmTestSweep("pfm-vcov-record", withr::local_tempdir(), withr::local_tempdir(),
                 scenarioData = makePFMSweepScenarioMagpie(), ceilingFallGate = 0.01)))
  v <- unlist(res$sanity$PolicyStringency$vcovStatus)
  expect_gt(length(v), 0)
  expect_true(all(v %in% c("ok", "corrupt", "boundary", "flat", "likelihood-mismatch")))
})

test_that("a gated status raises a severe frontierVcov flag", {
  allStatuses <- c("ok", "corrupt", "boundary", "flat", "likelihood-mismatch")
  res <- suppressMessages(suppressWarnings(
    pfmTestSweep("pfm-vcov-gate", withr::local_tempdir(), withr::local_tempdir(),
                 scenarioData = makePFMSweepScenarioMagpie(), ceilingFallGate = 0.01,
                 vcovGate = allStatuses)))
  sel <- res$sanity$PolicyStringency
  fl <- do.call(rbind, sel$flags)
  expect_true(any(fl$rule == "frontierVcov" & fl$severity == "severe"))
  expect_true(isTRUE(sel$forced))   # every evaluated spec is flagged, so none passes
})
# nolint end
