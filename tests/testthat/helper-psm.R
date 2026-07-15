# Shared fixtures for the Policy Stringency Model (PSM) tests.
#
# Synthetic PSM panel: bounded 0-10 outcome generated from a known logit-mean DGP
# with positive Actor Power and Institutional Quality effects. The outcome at year
# t is generated from the drivers at t-1, matching preparePanelData's lag = 1
# (the first outcome year is NA and drops out).
makePSMagpie <- function(withBoundaries = FALSE) {
  set.seed(11)
  regions <- paste0("R", 1:12)
  years <- 2000:2019
  nR <- length(regions)
  nY <- length(years)
  vars <- c("Policy Stringency|Bulk", "Actor Power Index|Bulk", "Rule of Law (VDem)")
  m <- magclass::new.magpie(regions, years, vars, fill = NA)
  ap <- matrix(runif(nR * nY, -0.8, 0.1), nR, nY)
  iq <- matrix(runif(nR * nY), nR, nY)
  y <- matrix(NA_real_, nR, nY)
  for (t in 2:nY) {
    eta <- -0.5 + 2.0 * ap[, t - 1] + 1.5 * iq[, t - 1] + rnorm(nR, sd = 0.25)
    y[, t] <- 10 * plogis(eta)
  }
  if (withBoundaries) {
    y[, 3][1:10] <- 0
    y[, 4][1:5] <- 10
  }
  m[, , "Actor Power Index|Bulk"] <- as.vector(ap)
  m[, , "Rule of Law (VDem)"] <- as.vector(iq)
  m[, , "Policy Stringency|Bulk"] <- as.vector(y)
  m
}

# Scenario panel for the same DGP: future years, drivers only (no outcome), plus
# one region (R99) that was never in the estimation sample (out-of-coverage).
makePSMScenarioMagpie <- function() {
  set.seed(21)
  regions <- c(paste0("R", 1:12), "R99")
  years <- 2019:2030
  nR <- length(regions)
  nY <- length(years)
  vars <- c("Actor Power Index|Bulk", "Rule of Law (VDem)")
  m <- magclass::new.magpie(regions, years, vars, fill = NA)
  m[, , "Actor Power Index|Bulk"] <- runif(nR * nY, -0.8, 0.1)
  m[, , "Rule of Law (VDem)"] <- runif(nR * nY)
  m
}

# Two-sector sweep fixture: both PSM outcomes + sector-qualified Actor Power and
# two IQ channels, same lag-1 DGP per sector.
makePSMSweepMagpie <- function() {
  set.seed(31)
  regions <- paste0("R", 1:12)
  years <- 2000:2019
  nR <- length(regions)
  nY <- length(years)
  vars <- c("Policy Stringency|Bulk", "Policy Stringency|Diffuse",
            "Actor Power Index|Bulk", "Actor Power Index|Diffuse",
            "Rule of Law (VDem)", "Government Effectiveness (WGI)")
  m <- magclass::new.magpie(regions, years, vars, fill = NA)
  iq1 <- matrix(runif(nR * nY), nR, nY)
  iq2 <- matrix(runif(nR * nY), nR, nY)
  m[, , "Rule of Law (VDem)"] <- as.vector(iq1)
  m[, , "Government Effectiveness (WGI)"] <- as.vector(iq2)
  for (sec in c("Bulk", "Diffuse")) {
    ap <- matrix(runif(nR * nY, -0.8, 0.1), nR, nY)
    y <- matrix(NA_real_, nR, nY)
    for (t in 2:nY) {
      eta <- -0.5 + 2.0 * ap[, t - 1] + 1.2 * iq1[, t - 1] + 0.8 * iq2[, t - 1] +
        rnorm(nR, sd = 0.25)
      y[, t] <- 10 * plogis(eta)
    }
    m[, , paste0("Actor Power Index|", sec)] <- as.vector(ap)
    m[, , paste0("Policy Stringency|", sec)] <- as.vector(y)
  }
  m
}

makePSMSweepScenarioMagpie <- function() {
  set.seed(41)
  regions <- paste0("R", 1:12)
  years <- 2019:2030
  nR <- length(regions)
  nY <- length(years)
  vars <- c("Actor Power Index|Bulk", "Actor Power Index|Diffuse",
            "Rule of Law (VDem)", "Government Effectiveness (WGI)")
  m <- magclass::new.magpie(regions, years, vars, fill = NA)
  for (v in vars) {
    m[, , v] <- if (grepl("Actor Power", v)) runif(nR * nY, -0.8, 0.1) else runif(nR * nY)
  }
  m
}

psmFit <- function(m, modelDir = NULL, verbose = FALSE, ...) {
  suppressWarnings(estimatePolicyStringencyModel(
    m, sector = "Bulk",
    actorPowerDrivers = "Actor Power Index", actorPowerIndex = "Actor Power Index",
    instQualityDrivers = "Rule of Law (VDem)", controlDrivers = NULL,
    regionMappingFixedEffects = NULL, modelDir = modelDir, verbose = verbose, ...
  ))
}

psmTheoryTerms <- c("Actor.Power.Index", "Rule.of.Law..VDem.")

# Minimal PSM spec grid for sweep/post-processing tests (no region mapping files
# needed in the isolated test environment).
psmTestSpecs <- list(
  list(name = "psmA", actorPowerDrivers = "Actor Power Index",
       actorPowerIndex = "Actor Power Index",
       instQualityDrivers = "Rule of Law (VDem)",
       controlDrivers = NULL, regionMappingFixedEffects = NULL),
  list(name = "psmB", actorPowerDrivers = "Actor Power Index",
       actorPowerIndex = "Actor Power Index",
       instQualityDrivers = c("Rule of Law (VDem)", "Government Effectiveness (WGI)"),
       controlDrivers = NULL, regionMappingFixedEffects = NULL)
)

# Run a small PSM sweep into fresh tempdirs; returns the paths + result.
# tierGate = "Blue": the synthetic DGP has no interaction effect, so the fixture
# pool is all-Blue - the Green deployment gate (ADR 0039 default) would correctly
# select nothing (that behaviour has its own dedicated test).
psmTestSweep <- function(group, resultsDir, modelDir, scenarioData = NULL, ...) {
  suppressMessages(suppressWarnings(runPSMSweep(
    group = group, mode = "guided",
    resultsDir = resultsDir, modelDir = modelDir,
    panelData = makePSMSweepMagpie(), scenarioData = scenarioData,
    specs = psmTestSpecs, sectors = c("Bulk", "Diffuse"),
    selectFE = NULL, tierGate = "Blue", verbose = FALSE, ...
  )))
}

# Fit the satP engine persisted into `dir` and reload it as a PFMModel.
psmFitAndLoad <- function(m, dir, ...) {
  suppressMessages(psmFit(m, modelDir = dir, ...))
  files <- list.files(file.path(dir, "models"), pattern = "\\.rds$")
  stopifnot(length(files) == 1)
  loadPFMModel(sub("\\.rds$", "", files[[1]]), dir)
}

# Shift-share IV fixture: Incumbent Power declines from a heterogeneous base
# level as global VRE diffuses (strong first stage for z = inc0 x looVRE), and
# the outcome falls with incumbent power (true beta < 0).
makePSMIVMagpie <- function() {
  set.seed(61)
  regions <- paste0("R", 1:24)
  years <- 2000:2019
  nR <- length(regions)
  nY <- length(years)
  vars <- c("Policy Stringency|Bulk", "Incumbent Power|Bulk",
            "Rule of Law (VDem)", "VRE share")
  m <- magclass::new.magpie(regions, years, vars, fill = NA)
  inc0 <- runif(nR, 0.2, 0.95)
  vre <- outer(runif(nR, 0, 0.05), rep(1, nY)) +
    outer(rep(1, nR), seq(0, 0.5, length.out = nY)) +
    matrix(rnorm(nR * nY, sd = 0.02), nR, nY)
  vreGlobal <- colMeans(vre)
  inc <- outer(inc0, 1 - 0.8 * vreGlobal) + matrix(rnorm(nR * nY, sd = 0.03), nR, nY)
  iq <- matrix(runif(nR * nY), nR, nY)
  y <- matrix(NA_real_, nR, nY)
  for (t in 2:nY) {
    eta <- 0.5 - 2.0 * inc[, t - 1] + 1.0 * iq[, t - 1] + rnorm(nR, sd = 0.15)
    y[, t] <- 10 * plogis(eta)
  }
  m[, , "Incumbent Power|Bulk"] <- as.vector(inc)
  m[, , "Rule of Law (VDem)"] <- as.vector(iq)
  m[, , "VRE share"] <- as.vector(vre)
  m[, , "Policy Stringency|Bulk"] <- as.vector(y)
  m
}

# Event-history fixture: binary ratchet events from lag-1 drivers with known
# positive effects. The outcome is stored UNqualified ("Ratchet Event") to
# exercise estimatePolicyRatchetModel's sector-alias path.
makePSMEventMagpie <- function() {
  set.seed(71)
  regions <- paste0("R", 1:12)
  years <- 2000:2019
  nR <- length(regions)
  nY <- length(years)
  vars <- c("Ratchet Event", "Actor Power Index|Bulk", "Rule of Law (VDem)")
  m <- magclass::new.magpie(regions, years, vars, fill = NA)
  ap <- matrix(runif(nR * nY, -0.8, 0.1), nR, nY)
  iq <- matrix(runif(nR * nY), nR, nY)
  ev <- matrix(NA_real_, nR, nY)
  for (t in 2:nY) {
    ev[, t] <- rbinom(nR, 1, plogis(-1.2 + 2.0 * ap[, t - 1] + 1.5 * iq[, t - 1]))
  }
  m[, , "Actor Power Index|Bulk"] <- as.vector(ap)
  m[, , "Rule of Law (VDem)"] <- as.vector(iq)
  m[, , "Ratchet Event"] <- as.vector(ev)
  m
}
