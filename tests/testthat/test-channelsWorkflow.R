test_that("channelSpecs builds the expected suites", {
  g <- channelSpecs("guided")
  expect_length(g, 14) # WGI GovEff state capacity (V-Dem PCA dropped 2026-06-16)
  ex <- channelSpecs("exhaustive")
  # Base specs: 15 IQ x 2 AP x 8 controls x (3 FE levels + 1 hybridFD) = 960 (ADR 0011)
  # + 1 pureFD + 36 capture (R9) + 12 dif:EU (R7) + 4 lag + 720 context-dummies
  # block (ADR 0038: 15 IQ x 2 AP x 8 controls x 3 FE, levels only) = 1733 base;
  # every LEVELS base spec gets a saturating stringency-only twin (ADR 0028):
  # 720 + 36 + 12 + 4 + 720 = 1492 twins -> 3225 total.
  # + per-capita actor-power block (design-notes/0001, appended 2026-08-24):
  # 3 forms x 15 IQ x 8 controls x 3 FE = 1080 raw + 1080 twins = 2160 -> 5385.
  expect_length(ex, 5385)
  # The base grid is unchanged and the new forms are strictly additive - this is the
  # numbering-stability contract test-psmImprovements.R also pins.
  expect_length(channelSpecs("exhaustive", apPcForms = character(0)), 3225)
  # every spec has the fields the report schema needs
  needed <- c("name", "description", "actorPowerDrivers", "actorPowerIndex",
              "instQualityDrivers", "controlDrivers", "logisticTimeTrend",
              "panelTransform")
  for (s in c(g, ex)) expect_true(all(needed %in% names(s)))
  # names unique within each suite
  expect_false(anyDuplicated(vapply(g, `[[`, character(1), "name")) > 0)
  expect_false(anyDuplicated(vapply(ex, `[[`, character(1), "name")) > 0)
  # exhaustive transform split: 5144 levels (1492 base + 1492 satP twins
  # + 1080 per-capita + 1080 their twins) + 240 hybridFD + 1 pureFD. The per-capita
  # block is levels-only, so hybridFD and pureFD are untouched by it.
  tr <- vapply(ex, `[[`, character(1), "panelTransform")
  expect_equal(as.integer(table(tr)[c("levels", "hybridFD", "pureFD")]), c(5144L, 240L, 1L))
  trBase <- vapply(channelSpecs("exhaustive", apPcForms = character(0)), `[[`,
                   character(1), "panelTransform")
  expect_equal(as.integer(table(trBase)[c("levels", "hybridFD", "pureFD")]), c(2984L, 240L, 1L))
  # Option 2b: no hybrid AP form (no spec has split mains with a composite index)
  hybrid <- vapply(ex, function(s) {
    setequal(s$actorPowerDrivers, c("Innovator Power", "Incumbent Power")) &&
      identical(s$actorPowerIndex, "Actor Power Index")
  }, logical(1))
  expect_false(any(hybrid))
})

test_that("createChannelConfigs writes and respects overwrite", {
  skip_if_not_installed("yaml")
  d <- tempfile("cfg")
  p <- createChannelConfigs(d, "guided")
  expect_true(file.exists(p))
  cfg <- yaml::read_yaml(p)
  expect_length(cfg, 14)
  # second call without overwrite keeps the file
  mtime1 <- file.mtime(p)
  p2 <- createChannelConfigs(d, "guided", overwrite = FALSE)
  expect_identical(normalizePath(p), normalizePath(p2))
  expect_identical(file.mtime(p2), mtime1)
})

test_that("computeDeltaR2Theory strips theory terms and is family-aware", {
  set.seed(31)
  n <- 400
  df <- data.frame(
    Actor.Power.Index = rnorm(n),
    Rule.of.Law..VDem. = rnorm(n),
    GDP.per.Capita = rnorm(n),
    timeTrend = rep(1:20, 20)
  )
  df$Actor.Power.Index_x_Rule.of.Law..VDem. <-
    df$Actor.Power.Index * df$Rule.of.Law..VDem.
  lp <- 1.2 * df$Actor.Power.Index + 0.8 * df$Rule.of.Law..VDem. +
    0.2 * df$GDP.per.Capita
  df$adoption <- rbinom(n, 1, plogis(lp))
  fml <- adoption ~ Actor.Power.Index + Rule.of.Law..VDem. +
    Actor.Power.Index_x_Rule.of.Law..VDem. + GDP.per.Capita + timeTrend
  m <- glm(fml, data = df, family = binomial())
  fit <- list(model = m, formula = fml, data = df)
  d <- computeDeltaR2Theory(fit,
    actorPowerDrivers = "Actor Power Index", actorPowerIndex = "Actor Power Index",
    instQualityDrivers = "Rule of Law (VDem)", stage = "adoption"
  )
  expect_gt(d, 0.1) # theory terms carry the signal
  # gaussian-identity stringency model (FD case): baseline family must match
  df$ecp <- lp + rnorm(n, sd = 0.5)
  fmlS <- ecp ~ Actor.Power.Index + Rule.of.Law..VDem. +
    Actor.Power.Index_x_Rule.of.Law..VDem. + GDP.per.Capita + timeTrend
  mS <- glm(fmlS, data = df, family = gaussian())
  dS <- computeDeltaR2Theory(list(model = mS, formula = fmlS, data = df),
    actorPowerDrivers = "Actor Power Index", actorPowerIndex = "Actor Power Index",
    instQualityDrivers = "Rule of Law (VDem)", stage = "stringency"
  )
  expect_gt(dS, 0.3)
})
