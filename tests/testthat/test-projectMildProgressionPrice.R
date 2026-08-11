# The "mild progression" price (Elmar variant). The properties that matter: the
# recursion is the one in the design note, the seed is honoured, the unbounded
# (S*-S)/S term cannot manufacture an infinite price, and the post-2100 hold applies.

mpFix <- function(S = 5, Sstar = 8, yrs = seq(2025, 2050, 5), regs = c("EUR", "USA")) {
  do.call(rbind, lapply(regs, function(r) {
    data.frame(region = r, year = yrs,
               feasibleIndex = rep(S, length(yrs)),
               ceilingIndex = rep(Sstar, length(yrs)),
               stringsAsFactors = FALSE)
  }))
}

test_that("the recursion is P(t+1) = P(t) (1 + lambda (S* - S)/S)", {
  d <- mpFix(S = 5, Sstar = 8)
  p <- projectMildProgressionPrice(d, c(EUR = 10, USA = 20), lambda = 0.1,
                                   maxGrowth = Inf)
  e <- p[p$region == "EUR", ]
  expect_equal(e$price[1], 10)                       # seed honoured exactly
  step <- 0.1 * (8 - 5) / 5                          # = 0.06
  expect_equal(e$gapRatio[2], 0.6, tolerance = 1e-12)
  expect_equal(e$price[2], 10 * (1 + step), tolerance = 1e-12)
  expect_equal(e$price[3], 10 * (1 + step)^2, tolerance = 1e-12)
  # Each region runs its own recursion off its own seed.
  expect_equal(p$price[p$region == "USA"][1], 20)
})

test_that("a closed gap leaves the price flat", {
  d <- mpFix(S = 8, Sstar = 8)
  p <- projectMildProgressionPrice(d, c(EUR = 10, USA = 10), lambda = 0.5)
  expect_true(all(abs(p$price - 10) < 1e-12))
  expect_true(all(p$growth[!is.na(p$growth)] == 0))
})

test_that("a near-zero stringency cannot manufacture an infinite price", {
  # (S* - S)/S -> Inf as S -> 0. This is the specification's real weak point.
  d <- mpFix(S = 1e-9, Sstar = 8)
  p <- projectMildProgressionPrice(d, c(EUR = 10, USA = 10), lambda = 1,
                                   maxGrowth = 1)
  expect_true(all(is.finite(p$price)))
  # The cap must BITE and must be reported, never applied silently.
  expect_true(any(p$capped))
  expect_gt(attr(p, "cappedShare"), 0)
  e <- p[p$region == "EUR", ]
  expect_equal(e$price[2], 20, tolerance = 1e-9)     # exactly one doubling
})

test_that("prices are held constant after holdAfter", {
  d <- mpFix(S = 5, Sstar = 8, yrs = seq(2090, 2110, 10))
  p <- projectMildProgressionPrice(d, c(EUR = 10, USA = 10), lambda = 0.1,
                                   seedYear = 2090, holdAfter = 2100)
  e <- p[p$region == "EUR", ]
  expect_gt(e$price[2], e$price[1])                  # still growing to 2100
  expect_equal(e$price[3], e$price[2])               # frozen after
})

test_that("the price never goes negative even with an overshooting gap", {
  d <- mpFix(S = 9, Sstar = 1)                       # far ABOVE the frontier
  p <- projectMildProgressionPrice(d, c(EUR = 10, USA = 10), lambda = 5,
                                   maxGrowth = Inf)
  expect_true(all(p$price >= 0))
})

test_that("bad inputs fail loudly", {
  d <- mpFix()
  expect_error(projectMildProgressionPrice(d[, 1:2], c(EUR = 10), lambda = 0.1),
               "missing column")
  expect_error(projectMildProgressionPrice(d, c(10, 20), lambda = 0.1), "NAMED")
  expect_error(projectMildProgressionPrice(d, c(EUR = 10), lambda = 0.1,
                                           maxGrowth = -1), "maxGrowth")
  expect_error(projectMildProgressionPrice(d, c(XXX = 10), lambda = 0.1),
               "no region has both")
})
