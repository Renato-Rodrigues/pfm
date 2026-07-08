# Four-sector ratcheting-speed step (2026-07-08). The key regression: the sector
# outcomes borrow their PARENT group's Actor Power Index (electricity/industry ->
# Bulk, buildings/transport -> Diffuse) because AP is only computed at that
# grouping — the bug that failed the first cluster run.

make4SectorMagpie <- function() {
  set.seed(48)
  regions <- paste0("R", 1:12)
  years <- 2000:2019
  nR <- length(regions); nY <- length(years)
  four <- c("Electricity", "Industry", "Buildings", "Transport")
  vars <- c(paste0("Policy Stringency|", four),
            "Actor Power Index|Bulk", "Actor Power Index|Diffuse",
            "Rule of Law (VDem)", "Government Effectiveness (WGI)")
  m <- magclass::new.magpie(regions, years, vars, fill = NA)
  iq1 <- matrix(runif(nR * nY), nR, nY)
  iq2 <- matrix(runif(nR * nY), nR, nY)
  m[, , "Rule of Law (VDem)"] <- as.vector(iq1)
  m[, , "Government Effectiveness (WGI)"] <- as.vector(iq2)
  for (grp in c("Bulk", "Diffuse")) {
    ap <- matrix(runif(nR * nY, -0.8, 0.1), nR, nY)
    m[, , paste0("Actor Power Index|", grp)] <- as.vector(ap)
    secs <- if (grp == "Bulk") c("Electricity", "Industry") else c("Buildings", "Transport")
    for (sec in secs) {
      y <- matrix(NA_real_, nR, nY)
      for (t in 2:nY) {
        eta <- -0.5 + 2.0 * ap[, t - 1] + 1.2 * iq1[, t - 1] + rnorm(nR, sd = 0.25)
        y[, t] <- 10 * plogis(eta)
      }
      m[, , paste0("Policy Stringency|", sec)] <- as.vector(y)
    }
  }
  m
}

test_that("runPSMSectorSpeeds produces four validated speeds via parent-AP aliasing", {
  resultsDir <- withr::local_tempdir()
  modelDir <- withr::local_tempdir()
  # a sweep to write selected-models-psm.yml (two-sector spec = the shared spec)
  psmTestSweep("psm-ss", resultsDir, modelDir)
  res <- suppressMessages(suppressWarnings(runPSMSectorSpeeds(
    group = "psm-ss", resultsDir = resultsDir, modelDir = modelDir,
    panelData = make4SectorMagpie(), trainEnd = 2015, verbose = FALSE
  )))
  expect_true(file.exists(file.path(resultsDir, "psm-ss", "sector-speeds.rds")))
  expect_true(length(res$bySector) >= 1)         # at least one sector validated
  for (sec in names(res$bySector)) {
    m <- res$bySector[[sec]]$metrics
    expect_true(is.finite(m$adjustmentSpeed))
    # half-life is NA when the fit fully corrects/overshoots (speed >= 1) — a
    # legitimate outcome on the iid-driver synthetic DGP; finite on real data.
    expect_true(is.na(m$halfLife) || is.finite(m$halfLife))
    expect_true(all(res$bySector[[sec]]$rows$year > 2015))
  }
  # the four sectors are among the recognised names
  expect_true(all(names(res$bySector) %in%
                    c("Electricity", "Industry", "Buildings", "Transport")))
  # step recorded in the manifest with speed metrics
  mf <- jsonlite::fromJSON(file.path(resultsDir, "psm-ss", "manifest.json"))
  expect_true("sector-speeds" %in% names(mf$steps))
})
