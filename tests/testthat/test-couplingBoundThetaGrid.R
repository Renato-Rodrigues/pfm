# nolint start
# The swept theta grid is a published quantity: MODEL.md 5.3, COUPLING.md's sweep
# table, paper-design Fig 3c and claims.md all enumerate it, and a silent change
# desynchronises them from the artifact. It has already drifted once (0.74 dropped
# from MODEL.md while still being swept). Pin it.

test_that("the default theta grid is even, spans [0, 1) and brackets v3's Bulk anchor", {
  th <- eval(formals(runPSMCouplingBound)$thetas)

  expect_identical(th, c(0, 0.25, 0.50, 0.75, 0.95, 0.99))

  # 0 is the uncoupled null and must always be swept: it is the only point that
  # shows what the constraint costs relative to no constraint at all.
  expect_true(0 %in% th)
  # The declared central value (MODEL.md 5.3); anchorTheta snaps to it.
  expect_true(0.50 %in% th)

  # TODO 14b(b): v3's region-resolution Bulk anchor is 0.951. The grid must BRACKET
  # it, not merely approach it -- the anchor fixes a scale by analogy, so the honest
  # object is a swept neighbourhood rather than a point.
  expect_true(any(th < 0.951) && any(th > 0.951))

  # The legacy country-resolution anchors were dropped here on purpose (2026-08-25).
  # Pinned so they cannot drift back in unnoticed alongside 0.75.
  expect_false(any(c(0.74, 0.79) %in% th))

  expect_false(any(duplicated(th)))
  expect_identical(th, sort(th))
})

test_that("every swept theta is admissible to aggregateFeasibilityToRegions", {
  # aggregateFeasibilityToRegions() validates [0, 1) and stops otherwise, so an
  # inadmissible default would not fail at review -- it would fail mid-run, after
  # the frontier and donor steps had already been paid for.
  th <- eval(formals(runPSMCouplingBound)$thetas)
  expect_true(all(th >= 0 & th < 1))

  # theta = 1 gives phi = 0 for the largest-gap tier: degenerate, not severe.
  # `path` is validated BEFORE `theta`, so it has to be well-formed for the theta
  # check to be the one that fires.
  path <- data.frame(region = "AAA", year = 2030, feasibleIndex = 5,
                     ceilingIndex = 8, outOfCoverage = FALSE)
  expect_error(
    aggregateFeasibilityToRegions(path, mapping = NULL, theta = 1),
    "theta must be a single value in \\[0, 1\\)"
  )
  expect_error(
    aggregateFeasibilityToRegions(path, mapping = NULL, theta = -0.1),
    "theta must be a single value in \\[0, 1\\)"
  )
})

test_that("swept thetas do not collide in the written CSV filenames", {
  # exportFeasibilityBound writes sprintf("...theta%03d.csv", round(100 * theta)),
  # so two thetas within 0.005 of each other would silently overwrite one file.
  th <- eval(formals(runPSMCouplingBound)$thetas)
  expect_false(any(duplicated(sprintf("theta%03d", round(100 * th)))))
})
# nolint end
