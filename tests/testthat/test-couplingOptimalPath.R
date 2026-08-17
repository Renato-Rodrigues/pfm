# P_opt for the bind-mode-2 bound must come from a path the cap CANNOT touch.
#
# The defect this pins (found 2026-08-17): priceOptimal was read from the run's own
# pm_taxCO2eq, which under bind mode 2 has already been capped by the previous
# iteration's bound. Each coupling call then computed
#
#     bound(i+1) = P_ref + phi * (bound(i) - P_ref)
#
# a contraction that multiplies the distance above P_ref by phi every call. At
# phi = 0.21 the gap fell to 21%, 4%, 0.9%: the bound collapsed onto the current-policy
# price within three or four calls whatever the politics said. In the 2026-08-16 batch
# every non-EU 2050 bound had landed within a whisker of its NPi reference - the USA at
# $0.06 against a reference of zero - and that number was read as a political finding.
#
# p45_taxCO2eq_anchor is the uncapped global anchor, so it breaks the recursion.

skip_if_no_gt <- function() skip_if_not_installed("gamstransfer")

# A minimal gdx carrying the two symbols the helper chooses between. Real domain sets,
# because gdx::readGDX resolves years and regions from them.
mkRunGdx <- function(file, anchor = c("2030" = 0.1, "2050" = 0.2),
                     capped = NULL, regs = c("EUR", "USA")) {
  m <- gamstransfer::Container$new()
  yrs <- names(anchor)
  ttot <- m$addSet("ttot", records = yrs)
  regi <- m$addSet("all_regi", records = regs)
  m$addParameter("p45_taxCO2eq_anchor", domain = list(ttot),
                 records = data.frame(ttot = yrs, value = as.numeric(anchor)))
  if (!is.null(capped)) {
    m$addParameter("pm_taxCO2eq", domain = list(ttot, regi),
                   records = data.frame(ttot = rep(yrs, times = length(regs)),
                                        all_regi = rep(regs, each = length(yrs)),
                                        value = as.numeric(capped)))
  }
  m$write(file)
  file
}

test_that("P_opt comes from the uncapped anchor, not the run's capped price", {
  skip_if_no_gt()
  skip_if_not_installed("gdx")
  f <- withr::local_tempfile(fileext = ".gdx")
  # The capped price is deliberately near zero - the ratchet's fixed point.
  mkRunGdx(f, anchor = c("2030" = 0.1, "2050" = 0.2),
           capped = c(0.0001, 0.0002, 0.0001, 0.0002))

  pR <- magclass::new.magpie(c("EUR", "USA"), c(2030, 2050), fill = 0)
  got <- pfm:::.psmCouplingOptimalPath(f, pR, TCO2 = 1000 / (44 / 12))

  expect_s3_class(got, "data.frame")
  expect_setequal(names(got), c("region", "year", "value"))
  # Broadcast across BOTH regions, since the anchor carries no region dimension.
  expect_setequal(unique(got$region), c("EUR", "USA"))
  expect_setequal(unique(got$year), c(2030L, 2050L))
  # The anchor, converted, not the ~0 capped price.
  v50 <- got$value[got$year == 2050]
  expect_equal(unname(v50), rep(0.2 * 1000 / (44 / 12), 2))
  expect_true(all(got$value > 1))          # the capped path would be ~1e-4
})

test_that("a region-free anchor is broadcast without recycling onto the wrong years", {
  skip_if_no_gt()
  skip_if_not_installed("gdx")
  f <- withr::local_tempfile(fileext = ".gdx")
  mkRunGdx(f, anchor = c("2030" = 0.1, "2050" = 0.9), regs = c("EUR", "USA", "CHA"))
  pR <- magclass::new.magpie(c("EUR", "USA", "CHA"), c(2030, 2050), fill = 0)
  got <- pfm:::.psmCouplingOptimalPath(f, pR, TCO2 = 1)
  # Every region must see the SAME value in a given year - the anchor is global.
  for (y in c(2030L, 2050L)) {
    expect_equal(length(unique(got$value[got$year == y])), 1L)
  }
  expect_equal(unique(got$value[got$year == 2030]), 0.1)
  expect_equal(unique(got$value[got$year == 2050]), 0.9)
})

test_that("a missing anchor warns loudly rather than falling back in silence", {
  skip_if_no_gt()
  skip_if_not_installed("gdx")
  f <- withr::local_tempfile(fileext = ".gdx")
  # Anchor present but all-zero: the fallback path must still fire.
  mkRunGdx(f, anchor = c("2030" = 0, "2050" = 0),
           capped = c(0.5, 0.5, 0.5, 0.5))
  pR <- magclass::new.magpie(c("EUR", "USA"), c(2030, 2050), fill = 0)
  expect_warning(pfm:::.psmCouplingOptimalPath(f, pR, TCO2 = 1),
                 "ALREADY\\s+CAPPED|ratchet")
})
