feasFixture <- function() {
  regs <- c("CHA", "EUR", "IND", "REF", "USA")
  do.call(rbind, lapply(c("Bulk", "Diffuse"), function(s) do.call(rbind, lapply(c(2030, 2050), function(y)
    data.frame(region = regs, year = y, sector = s,
               phi = if (s == "Bulk") c(1.00, 0.80, 0.70, 0.50, 0.66) else c(0.78, 0.90, 0.95, 0.60, 0.55),
               stringsAsFactors = FALSE)))))
}
withOverride <- function(lines, code) {
  d <- tempfile("grp"); dir.create(d)
  on.exit(unlink(d, recursive = TRUE))
  if (length(lines)) writeLines(lines, file.path(d, "phi-override.yml"))
  code(d)
}
quiet <- function(...) invisible(NULL)

test_that("no override file leaves feas untouched", {
  f <- feasFixture()
  withOverride(character(0), function(d) {
    out <- pfm:::.pfmApplyPhiOverride(f, d, quiet)
    expect_identical(out, f)
    expect_null(attr(out, "phiOverride"))
  })
})

test_that("uniform = mean sets every region to the sector mean, per sector", {
  f <- feasFixture()
  withOverride("mode: uniform", function(d) {
    out <- pfm:::.pfmApplyPhiOverride(f, d, quiet)
    expect_equal(unique(out$phi[out$sector == "Bulk"]), mean(c(1.00, 0.80, 0.70, 0.50, 0.66)))
    expect_equal(unique(out$phi[out$sector == "Diffuse"]), mean(c(0.78, 0.90, 0.95, 0.60, 0.55)))
    expect_equal(attr(out, "phiOverride")$mode, "uniform")
  })
})

test_that("uniform with a number uses it; out-of-range is refused", {
  f <- feasFixture()
  withOverride(c("mode: uniform", "value: 0.7"), function(d) {
    expect_true(all(pfm:::.pfmApplyPhiOverride(f, d, quiet)$phi == 0.7))
  })
  withOverride(c("mode: uniform", "value: 1.4"), function(d) {
    expect_error(pfm:::.pfmApplyPhiOverride(f, d, quiet), "not in \\[0, 1\\]")
  })
})

test_that("permute keeps each sector's set of values, moves them, and pairs the sectors", {
  f <- feasFixture()
  withOverride(c("mode: permute", "seed: 3"), function(d) {
    out <- pfm:::.pfmApplyPhiOverride(f, d, quiet)
    for (s in c("Bulk", "Diffuse")) {
      expect_equal(sort(unique(out$phi[out$sector == s])), sort(unique(f$phi[f$sector == s])))
    }
    expect_false(isTRUE(all.equal(out$phi, f$phi)))
    # the same donor region supplies both sectors of a region
    b <- f[f$year == 2030 & f$sector == "Bulk", ]; dd <- f[f$year == 2030 & f$sector == "Diffuse", ]
    ob <- out[out$year == 2030 & out$sector == "Bulk", ]; od <- out[out$year == 2030 & out$sector == "Diffuse", ]
    donorB <- b$region[match(ob$phi, b$phi)]; donorD <- dd$region[match(od$phi, dd$phi)]
    expect_identical(donorB, donorD)
    # constant over years, deterministic for a seed
    expect_equal(out$phi[out$year == 2030], out$phi[out$year == 2050])
    expect_identical(out$phi, pfm:::.pfmApplyPhiOverride(f, d, quiet)$phi)
  })
})

test_that("permute does not disturb the caller's random stream", {
  f <- feasFixture()
  withOverride(c("mode: permute", "seed: 1"), function(d) {
    set.seed(99); a <- stats::runif(1)
    set.seed(99); invisible(pfm:::.pfmApplyPhiOverride(f, d, quiet)); b <- stats::runif(1)
    expect_identical(a, b)
  })
})

test_that("set pins named regions and leaves the rest", {
  f <- feasFixture()
  withOverride(c("mode: set", "regions:", "  CHA:", "    Bulk: 0.862", "    Diffuse: 0.556"), function(d) {
    out <- pfm:::.pfmApplyPhiOverride(f, d, quiet)
    expect_true(all(out$phi[out$region == "CHA" & out$sector == "Bulk"] == 0.862))
    expect_true(all(out$phi[out$region == "CHA" & out$sector == "Diffuse"] == 0.556))
    expect_equal(out$phi[out$region != "CHA"], f$phi[f$region != "CHA"])
  })
  withOverride(c("mode: set", "regions:", "  XXX:", "    Bulk: 0.5"), function(d) {
    expect_error(pfm:::.pfmApplyPhiOverride(f, d, quiet), "not in this run's regions")
  })
})

test_that("an unknown mode is refused", {
  withOverride("mode: shuffle", function(d) {
    expect_error(pfm:::.pfmApplyPhiOverride(feasFixture(), d, quiet), "mode must be")
  })
})
