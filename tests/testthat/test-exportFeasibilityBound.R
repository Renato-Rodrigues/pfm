# The carbon-price bound handed to the IAM (ADR 0041, TODO 3.1).
#
# The properties tested here are the ones that make the object safe to couple:
# politics must only ever slow a pathway, a region already at its current-policy
# price must be unaffected (both the share AND the speed limit act on the
# increment), and the two mechanisms - the tier discount and the speed limit -
# must be separable, so theta = 0 is the SPEED-ONLY case rather than the
# uncoupled run.

feasFix <- function(theta = 0.5, sectors = TRUE) {
  yrs <- seq(2025, 2050, by = 5)
  regs <- c("EUR", "USA", "CHA")
  phi <- c(EUR = 1, USA = 1 - theta / 3, CHA = 1 - theta)   # tiers 1, 2, 4
  d <- expand.grid(region = regs, year = yrs, stringsAsFactors = FALSE)
  d$phi <- as.numeric(phi[d$region])
  d$tier <- as.integer(c(EUR = 1, USA = 2, CHA = 4)[d$region])
  if (sectors) {
    b <- d
    b$sector <- "Bulk"
    dd <- d
    dd$sector <- "Diffuse"
    dd$phi <- pmin(dd$phi + 0.1, 1)      # Diffuse deliberately less constrained
    d <- rbind(b, dd)
  }
  attr(d, "theta") <- theta
  d
}

pricePath <- function(base, growth = 1) {
  yrs <- seq(2025, 2050, by = 5)
  regs <- c("EUR", "USA", "CHA")
  d <- expand.grid(region = regs, year = yrs, stringsAsFactors = FALSE)
  d$value <- base * growth^((d$year - 2025) / 5)
  d
}

test_that("theta = 0 is the SPEED-ONLY case, not the uncoupled run", {
  f <- feasFix(theta = 0)
  b <- exportFeasibilityBound(f, pricePath(100, 1.4), pricePath(10),
                              lambda = c(Bulk = 0.10, Diffuse = 0.08))
  expect_true(all(f$phi == 1))
  expect_equal(attr(b, "theta"), 0)
  # No tier discount: every region shares the same (full) target ...
  expect_equal(b$priceTarget, b$priceOptimal, tolerance = 1e-8)
  # ... but the speed limit still bites, so this is NOT the unconstrained run.
  expect_true(any(b$binds))
  expect_true(all(b$priceBound <= b$priceOptimal + 1e-8))
  # A very fast lambda collapses it onto the optimum after the seed period (the
  # seed has no banked increment yet, by construction): the two mechanisms separate.
  fast <- exportFeasibilityBound(f, pricePath(100, 1.4), pricePath(10),
                                 lambda = 0.999, sectorRule = "Bulk")
  post <- fast[fast$year > min(fast$year), ]
  expect_equal(post$priceBound, post$priceOptimal, tolerance = 1e-3)
  seed <- fast[fast$year == min(fast$year), ]
  expect_equal(seed$priceBound, seed$priceReference, tolerance = 1e-8)
})

test_that("politics can only slow a pathway, never accelerate it", {
  for (th in c(0, 0.25, 0.5, 0.9)) {
    b <- exportFeasibilityBound(feasFix(th), pricePath(100, 1.4), pricePath(10),
                                lambda = c(Bulk = 0.10, Diffuse = 0.08))
    expect_true(all(b$priceBound <= b$priceOptimal + 1e-8))
    expect_true(all(b$priceBound >= 0))
  }
})

test_that("a harsher theta binds harder, monotonically", {
  bind <- vapply(c(0, 0.25, 0.5, 0.9), function(th) {
    b <- exportFeasibilityBound(feasFix(th), pricePath(100, 1.4), pricePath(10),
                                lambda = 0.10, sectorRule = "Bulk")
    mean(b$bindGap)
  }, numeric(1))
  expect_true(all(diff(bind) >= -1e-8))
  expect_gt(bind[4], bind[1])
})

test_that("the share applies to the INCREMENT, so a region at current policy is unaffected", {
  # Optimal == reference: there is no incremental effort to discount, so the
  # bound must equal the optimum whatever phi says.
  p <- pricePath(50, 1.2)
  b <- exportFeasibilityBound(feasFix(0.9), p, p, lambda = 0.10, sectorRule = "Bulk")
  expect_equal(b$priceBound, b$priceOptimal, tolerance = 1e-8)
  expect_false(any(b$binds))
})

test_that("sectorRule = 'min' takes the worse share and the slower speed", {
  f <- feasFix(0.6)
  bMin <- exportFeasibilityBound(f, pricePath(100, 1.4), pricePath(10),
                                 lambda = c(Bulk = 0.30, Diffuse = 0.05),
                                 sectorRule = "min")
  bBulk <- exportFeasibilityBound(f, pricePath(100, 1.4), pricePath(10),
                                  lambda = c(Bulk = 0.30, Diffuse = 0.05),
                                  sectorRule = "Bulk")
  expect_equal(unique(bMin$lambda), 0.05)
  expect_equal(unique(bBulk$lambda), 0.30)
  # Bulk is the more constrained sector in the fixture, so "min" picks its phi ...
  expect_equal(sort(unique(bMin$phi)), sort(unique(bBulk$phi)))
  # ... and the slower speed makes the bound bite harder overall.
  expect_gt(mean(bMin$bindGap), mean(bBulk$bindGap))
})

test_that("an uncoupled region (phi = 1) is never constrained below the optimum at convergence", {
  f <- feasFix(0.5)
  b <- exportFeasibilityBound(f, pricePath(100, 1.4), pricePath(10),
                              lambda = 0.9, sectorRule = "Bulk")   # fast: converges
  eur <- b[b$region == "EUR", ]
  cha <- b[b$region == "CHA", ]
  expect_lt(mean(eur$bindGap), mean(cha$bindGap))
  expect_equal(eur$phi[1], 1)
})

test_that("magpie price paths are accepted and the region/year grid is intersected", {
  skip_if_not_installed("magclass")
  yrs <- seq(2025, 2050, by = 5)
  m <- magclass::new.magpie(c("EUR", "USA", "CHA"), yrs, "x", fill = 100)
  r <- magclass::new.magpie(c("EUR", "USA", "CHA"), yrs, "x", fill = 10)
  b <- exportFeasibilityBound(feasFix(0.5), m, r, lambda = 0.1, sectorRule = "Bulk")
  expect_equal(nrow(b), 3 * length(yrs))
  expect_true(all(b$priceOptimal == 100))
  # A price path sharing no region with the feasibility table must fail loudly.
  bad <- magclass::new.magpie(c("XXX"), yrs, "x", fill = 100)
  expect_error(exportFeasibilityBound(feasFix(0.5), bad, r, lambda = 0.1,
                                      sectorRule = "Bulk"), "no region-year")
})

test_that("the csv carries a provenance header and refuses other extensions", {
  d <- withr::local_tempdir()
  f <- file.path(d, "bound.csv")
  b <- exportFeasibilityBound(feasFix(0.5), pricePath(100, 1.4), pricePath(10),
                              lambda = 0.1, sectorRule = "Bulk", file = f)
  expect_true(file.exists(f))
  hdr <- readLines(f, n = 7)
  expect_true(any(grepl("ADR 0041", hdr)))
  expect_true(any(grepl("theta", hdr)))
  expect_true(any(grepl("binding share", hdr)))
  back <- utils::read.csv(f, comment.char = "#")
  expect_equal(nrow(back), nrow(b))
  expect_error(exportFeasibilityBound(feasFix(0.5), pricePath(100), pricePath(10),
                                      lambda = 0.1, sectorRule = "Bulk",
                                      file = file.path(d, "x.gdx")), "must end in .csv")
})

test_that("a sector column is required when reconciling, and missing inputs fail loudly", {
  f1 <- feasFix(0.5, sectors = FALSE)
  expect_error(exportFeasibilityBound(f1, pricePath(100), pricePath(10), lambda = 0.1),
               "needs a 'sector' column")
  b <- exportFeasibilityBound(f1, pricePath(100), pricePath(10), lambda = 0.1,
                              sectorRule = "none")
  expect_equal(nrow(b), nrow(f1))
  expect_error(exportFeasibilityBound(f1[, c("region", "year")], pricePath(100),
                                      pricePath(10), lambda = 0.1, sectorRule = "none"),
               "missing column")
})

test_that("the reported tier matches the selected phi, and stays NA when uncoupled", {
  f <- feasFix(0.6)
  # Make the two sectors disagree on tier so min(phi) and min(tier) diverge.
  f$tier[f$sector == "Diffuse"] <- 4L
  f$phi[f$sector == "Diffuse"] <- 1 - 0.6
  b <- exportFeasibilityBound(f, pricePath(100, 1.4), pricePath(10),
                              lambda = 0.1, sectorRule = "min")
  # phi is the worse sector's, so the tier label must be the WORSE (higher) tier.
  expect_true(all(b$tier == 4))
  expect_equal(unique(round(b$phi, 8)), round(1 - 0.6, 8))

  # An uncoupled region (all-NA tiers) keeps NA rather than becoming +/-Inf.
  f2 <- feasFix(0.6)
  f2$tier <- NA_integer_
  f2$phi <- 1
  b2 <- exportFeasibilityBound(f2, pricePath(100, 1.4), pricePath(10),
                               lambda = 0.1, sectorRule = "min")
  expect_true(all(is.na(b2$tier)))
  expect_false(any(is.infinite(b2$tier)))
})

# --- the symmetric markup invariant (ADR 0042, 2026-08-17) --------------------
#
# The per-market markup is max(P_sector - P_floor, 0) on the GAMS side, with the floor
# from sectorRule = "min". Two things have to hold for that to be a faithful delivery of
# both sectors rather than a cap on one of them.

test_that("every per-sector bound sits at or above the min floor", {
  # If a sector bound could fall BELOW the floor, GAMS would clamp its markup to zero
  # and that sector would silently be delivered at the other sector's price - the exact
  # information loss the symmetric markup removes.
  f <- feasFix(theta = 0.5)
  args <- list(f, pricePath(100, 1.4), pricePath(10),
               lambda = c(Bulk = 0.10, Diffuse = 0.08))
  floorB <- do.call(exportFeasibilityBound, c(args, list(sectorRule = "min")))
  key <- function(d) paste(d$region, d$year)
  for (sec in c("Bulk", "Diffuse")) {
    s <- do.call(exportFeasibilityBound, c(args, list(sectorRule = sec)))
    m <- merge(floorB[, c("region", "year", "priceBound")],
               s[, c("region", "year", "priceBound")],
               by = c("region", "year"), suffixes = c(".floor", ".sec"))
    expect_true(all(m$priceBound.sec >= m$priceBound.floor - 1e-8),
                info = paste(sec, "bound dipped below the min floor"))
  }
})

test_that("floor + markup reproduces each sector's own price exactly", {
  # THE invariant the whole design rests on. Whatever the floor is, the markup is
  # defined as the difference, so each market ends up at its own sector's price and
  # neither sector is capped by the other. This is what makes min() arithmetic rather
  # than a modelling step that discards a sector.
  f <- feasFix(theta = 0.5)
  args <- list(f, pricePath(100, 1.4), pricePath(10),
               lambda = c(Bulk = 0.10, Diffuse = 0.08))
  fl <- do.call(exportFeasibilityBound, c(args, list(sectorRule = "min")))
  bk <- do.call(exportFeasibilityBound, c(args, list(sectorRule = "Bulk")))
  df <- do.call(exportFeasibilityBound, c(args, list(sectorRule = "Diffuse")))
  m <- Reduce(function(a, b) merge(a, b, by = c("region", "year")),
              list(setNames(fl[, c("region", "year", "priceBound")],
                            c("region", "year", "floor")),
                   setNames(bk[, c("region", "year", "priceBound")],
                            c("region", "year", "bulk")),
                   setNames(df[, c("region", "year", "priceBound")],
                            c("region", "year", "diffuse"))))
  mkUpB <- pmax(m$bulk - m$floor, 0)
  mkUpD <- pmax(m$diffuse - m$floor, 0)
  expect_true(all(mkUpB >= 0) && all(mkUpD >= 0))       # never a subsidy
  expect_equal(m$floor + mkUpB, m$bulk, tolerance = 1e-8)
  expect_equal(m$floor + mkUpD, m$diffuse, tolerance = 1e-8)
})

test_that("the min floor can sit BELOW both sector prices - it mixes phi and lambda", {
  # Documented, not accidental, and easy to get wrong when reasoning about the markup.
  # sectorRule = "min" takes the worse SHARE *and* the slower SPEED (pinned by the
  # "takes the worse share and the slower speed" test above), and those can come from
  # DIFFERENT sectors. The floor is then the most-constrained combination, belonging to
  # neither sector, and strictly below both - so BOTH markups can be positive at once.
  #
  # Consequences worth remembering:
  #  - "exactly one markup is positive" is FALSE; only non-negativity is guaranteed.
  #  - pm_taxCO2eq is then a price no market actually faces, and everything reading
  #    pm_taxCO2eqSum (MAC curves, land-use tax, trade tariffs, net-negative penalty)
  #    sees it. That is the conservative direction, but it is a real distortion.
  f <- feasFix(theta = 0.5)
  args <- list(f, pricePath(100, 1.4), pricePath(10),
               lambda = c(Bulk = 0.10, Diffuse = 0.08))
  fl <- do.call(exportFeasibilityBound, c(args, list(sectorRule = "min")))
  bk <- do.call(exportFeasibilityBound, c(args, list(sectorRule = "Bulk")))
  df <- do.call(exportFeasibilityBound, c(args, list(sectorRule = "Diffuse")))
  # the floor takes the SLOWER speed, which here is Diffuse's
  expect_equal(unique(fl$lambda), 0.08)
  expect_equal(unique(bk$lambda), 0.10)
  m <- Reduce(function(a, b) merge(a, b, by = c("region", "year")),
              list(setNames(fl[, c("region", "year", "priceBound")],
                            c("region", "year", "floor")),
                   setNames(bk[, c("region", "year", "priceBound")],
                            c("region", "year", "bulk")),
                   setNames(df[, c("region", "year", "priceBound")],
                            c("region", "year", "diffuse"))))
  # never above either sector, so no markup is ever negative ...
  expect_true(all(m$floor <= pmin(m$bulk, m$diffuse) + 1e-8))
  # ... but strictly below both somewhere, so both markups are positive there
  expect_gt(sum(m$floor < pmin(m$bulk, m$diffuse) - 1e-8), 0)
})

test_that("the Diffuse markup is non-zero somewhere when Bulk is the worse sector", {
  # The fixture makes Diffuse LESS constrained, so Bulk is the floor and the Diffuse
  # markup must carry the difference. Under the pre-2026-08-17 asymmetric code this was
  # structurally zero - the bug this test exists to prevent recurring.
  f <- feasFix(theta = 0.5)
  args <- list(f, pricePath(100, 1.4), pricePath(10),
               lambda = c(Bulk = 0.10, Diffuse = 0.08))
  fl <- do.call(exportFeasibilityBound, c(args, list(sectorRule = "min")))
  df <- do.call(exportFeasibilityBound, c(args, list(sectorRule = "Diffuse")))
  m <- merge(fl[, c("region", "year", "priceBound")],
             df[, c("region", "year", "priceBound")],
             by = c("region", "year"), suffixes = c(".floor", ".diffuse"))
  expect_gt(max(m$priceBound.diffuse - m$priceBound.floor), 0)
})
