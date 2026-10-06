# nolint start
# The v6 coupling formulation (design note 0005 §1; Phase 1 acceptance properties).

gapsFixture <- function() {
  # three regions, two sectors, four years; R3 is the most constrained in Bulk
  expand <- expand.grid(region = c("R1", "R2", "R3"), year = c(2025, 2050, 2100, 2150),
                        sector = c("Bulk", "Diffuse"), stringsAsFactors = FALSE)
  base <- c(R1 = 0.10, R2 = 0.20, R3 = 0.40)
  grow <- c(`2025` = 1, `2050` = 1.5, `2100` = 2, `2150` = 3)
  expand$g <- base[expand$region] * grow[as.character(expand$year)] *
    ifelse(expand$sector == "Diffuse", 0.5, 1)
  expand
}
wReg <- c(R1 = 0.5, R2 = 0.3, R3 = 0.2)

test_that("k is the weighted mean gap normalised at t0, held after the hold year", {
  st <- pfm:::.pfmStrengthFromGaps(gapsFixture(), wReg, t0 = 2025, holdYear = 2100)
  b <- st[st$sector == "Bulk", ]
  expect_equal(b$G[b$year == 2025], sum(wReg * c(0.10, 0.20, 0.40)))
  expect_equal(b$k[b$year == 2025], 1)                          # D4: k(t0) = 1
  expect_equal(b$k[b$year == 2050], 1.5)
  expect_equal(b$k[b$year == 2150], b$k[b$year == 2100])        # D6: held after 2100
  expect_equal(b$kRaw[b$year == 2150], 3)                       # ... the raw path is kept
  # the spread is the weighted variance around G, normalised at t0
  g0 <- c(0.10, 0.20, 0.40); G0 <- sum(wReg * g0)
  expect_equal(b$D[b$year == 2025], sum(wReg * (g0 - G0)^2))
  expect_equal(b$d[b$year == 2050], 1.5^2)                      # gaps scaled by 1.5 -> variance 2.25
  # hold at 2060 = the sensitivity; a year that is not a period leaves the path unheld
  st60 <- pfm:::.pfmStrengthFromGaps(gapsFixture(), wReg, t0 = 2025, holdYear = 2050)
  expect_equal(st60$k[st60$sector == "Bulk" & st60$year == 2100], 1.5)
  expect_error(pfm:::.pfmStrengthFromGaps(gapsFixture(), wReg, t0 = 2030), "t0")
})

test_that("phi = 1 - theta k u with d = k; theta = 0 gives phi = 1; uniform u gives one share", {
  st <- pfm:::.pfmStrengthFromGaps(gapsFixture(), wReg, t0 = 2025)
  u <- data.frame(sector = rep(c("Bulk", "Diffuse"), each = 3), region = rep(c("R1", "R2", "R3"), 2),
                  u = rep(c(0, 1 / 3, 1), 2), stringsAsFactors = FALSE)
  sh <- pfm:::.pfmSharesFrom(u, wReg, st, theta = 0.5)
  b25 <- sh[sh$sector == "Bulk" & sh$year == 2025, ]
  expect_equal(b25$phi, 1 - 0.5 * c(0, 1 / 3, 1))               # phi(t0) = 1 - theta u
  b50 <- sh[sh$sector == "Bulk" & sh$year == 2050, ]
  expect_equal(b50$phi, 1 - 0.5 * 1.5 * c(0, 1 / 3, 1))
  # theta = 0: the null, phi = 1 everywhere and always
  expect_true(all(pfm:::.pfmSharesFrom(u, wReg, st, theta = 0)$phi == 1))
  # uniform ordering: every region at the weighted mean position, so one share per year
  shU <- pfm:::.pfmSharesFrom(u, wReg, st, theta = 0.5, ordering = "uniform")
  expect_true(all(tapply(shU$phi, paste(shU$sector, shU$year), function(p) diff(range(p))) < 1e-12))
  # reversed: the most constrained becomes the least
  shR <- pfm:::.pfmSharesFrom(u, wReg, st, theta = 0.5, ordering = "reversed")
  expect_equal(shR$u[shR$sector == "Bulk" & shR$year == 2025], c(1, 2 / 3, 0))
})

test_that("with d = k the ranking never changes over time; clipping is flagged", {
  st <- pfm:::.pfmStrengthFromGaps(gapsFixture(), wReg, t0 = 2025)
  u <- data.frame(sector = "Bulk", region = c("R1", "R2", "R3"), u = c(0.2, 0.9, 0.5), stringsAsFactors = FALSE)
  sh <- pfm:::.pfmSharesFrom(u, wReg, st, theta = 0.3)
  ranks <- tapply(seq_len(nrow(sh)), sh$year, function(i) paste(order(sh$phi[i]), collapse = ""))
  expect_length(unique(ranks), 1)
  # theta k u > 1 clips at 0 and is flagged (D12)
  shC <- pfm:::.pfmSharesFrom(u, wReg, st, theta = 0.9)
  hit <- shC$year == 2150 & shC$region == "R2"                  # 1 - 0.9 * 2 * 0.9 < 0
  expect_true(shC$clipped[hit] && shC$phi[hit] == 0)
  expect_error(pfm:::.pfmSharesFrom(u, wReg, st, theta = 1.5), "theta")
})
# nolint end
