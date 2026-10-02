# nolint start
# The gamma screen (ADR 0043 consequences, implemented 2026-08-25). ADR 0043 said
# "screen gamma on any winner" and nothing implemented it, which is how v3's
# X-2367 was deployed at gamma = 0.99999999 in BOTH sectors unremarked.
#
# Two properties are pinned here:
#   1. The gate is ON by default at 0.999 (ADR 0046, 2026-09-15 - the deliberate, dated
#      act that switched it on; until then it was off). A silent change of the default
#      would retro-invalidate a Run-Group, so the value is pinned, not just "set".
#   2. gamma is CARRIED whether or not the gate is on, so enabling it is never the
#      first time the number is seen. This mirrors test-ceilingFallGate.R's
#      reasoning about production defaults not drifting.

test_that("gammaGate defaults to 0.999 in both the sweep and the sanity walk (ADR 0046)", {
  expect_identical(eval(formals(runPFMSweep)$gammaGate), 0.999)
  expect_identical(eval(formals(pfm:::.pfmSanitySelect)$gammaGate), 0.999)
})

test_that("gammaGate reaches every sanity-walk call site", {
  # Two call sites exist: the main Green walk and the ADR 0039 tier-relaxed Blue
  # fallback. Passing it to only one would gate the first pool and not the second,
  # so a rejected spec could return through the back door. Read the deparsed
  # function rather than a source path, which does not exist once installed.
  body <- paste(deparse(runPFMSweep), collapse = "\n")
  nCeil <- lengths(regmatches(body, gregexpr("ceilingFallGate = ceilingFallGate", body)))
  nGam <- lengths(regmatches(body, gregexpr("gammaGate = gammaGate", body)))
  expect_gt(nCeil, 0)
  expect_identical(nGam, nCeil)
})

test_that("the ceiling helper returns gamma, so the gate costs no extra frontier fit", {
  # If this field ever disappears the gate silently stops firing rather than
  # erroring -- the failure mode that let the original ADR 0043 omission survive.
  body <- paste(deparse(pfm:::.pfmCeilingTrajectory), collapse = "\n")
  expect_match(body, "gamma = gm", fixed = TRUE)
  # and the walk must read it back off the same object
  walk <- paste(deparse(pfm:::.pfmSanitySelect), collapse = "\n")
  expect_match(walk, "ct$gamma", fixed = TRUE)
  expect_match(walk, "gammaBoundary", fixed = TRUE)
})

test_that("gamma is recorded even when the gate is off", {
  # The point of reporting-when-off: switching the gate on must never be the first
  # time anyone sees the number. v3's X-2367 shipped at 0.99999999 unremarked
  # precisely because nothing carried it.
  walk <- paste(deparse(pfm:::.pfmSanitySelect), collapse = "\n")
  expect_match(walk, "modelGamma[[sec]] <- ct$gamma", fixed = TRUE)
  expect_match(walk, "gamma = gammaByModel", fixed = TRUE)
  # the assignment must sit OUTSIDE the is.finite(gammaGate) branch
  i <- regexpr("modelGamma[[sec]] <- ct$gamma", walk, fixed = TRUE)
  j <- regexpr("is.finite(gammaGate)", walk, fixed = TRUE)
  expect_true(i > 0 && j > 0 && i < j)
})
# nolint end
