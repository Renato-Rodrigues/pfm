# Helpers for the compute-workflow tests (ADR 0018/0019): a small synthetic panel and a
# matching spec, so runFitGrid / fitOneSpec can be exercised without the mrpfm data pipeline.

# Build a tiny Bulk+Diffuse panel with the variables a levels spec needs.
makeSyntheticPanel <- function(regs = paste0("R", 1:8), yrs = 2000:2014, seed = 7) {
  set.seed(seed)
  vars <- c("Effective Carbon Price|Bulk", "Innovator Power|Bulk",
            "Effective Carbon Price|Diffuse", "Innovator Power|Diffuse",
            "Rule of Law (VDem)", "GDP per Capita")
  m <- magclass::new.magpie(regs, yrs, vars, fill = 0)
  for (r in regs) {
    iq  <- stats::rnorm(length(yrs))
    gdp <- stats::rnorm(length(yrs))
    for (sg in c("Bulk", "Diffuse")) {
      x <- stats::rnorm(length(yrs))
      m[r, , paste0("Innovator Power|", sg)] <- x
      adopt <- stats::plogis(1.3 * x + 0.5 * iq) > stats::runif(length(yrs))
      m[r, , paste0("Effective Carbon Price|", sg)] <-
        ifelse(adopt, abs(stats::rnorm(length(yrs))) * 10 + 1, 0)
    }
    m[r, , "Rule of Law (VDem)"] <- iq
    m[r, , "GDP per Capita"]     <- gdp
  }
  m
}

# A normalised spec referencing only the synthetic panel's variables.
syntheticSpec <- function(name = "t1", logisticTimeTrend = FALSE) {
  list(
    name = name, description = name,
    actorPowerDrivers = "Innovator Power", actorPowerIndex = "Innovator Power",
    instQualityDrivers = "Rule of Law (VDem)", controlDrivers = NULL,
    includeLagged = FALSE, includeLaggedECP = FALSE, nickellCorrection = FALSE,
    interactRegionFE = FALSE, logisticTimeTrend = logisticTimeTrend, useMundlak = FALSE,
    gdpGovInteraction = FALSE, ridgeInteractions = FALSE,
    regionMappingFixedEffects = NULL, panelTransform = "levels"
  )
}

# A synthetic PFMModel for the persistence/index tests (no fitting required).
syntheticPFMModel <- function(id, sector = "Bulk", stage = "Adoption") {
  structure(list(
    id = id, id_full = paste0(id, "-full"), created_at = "2026-06-19", label = paste0("m-", id),
    pfm_version = "0.0.0", sector = sector, stage = stage, formula = adoption ~ x,
    family = "binomial", training_years = c(2000L, 2022L), useFirth = TRUE, data_hash = id,
    diagnostics = list(aic = 1, bic = 2, aicc = 3, pseudoR2 = 0.5, nObs = 100L,
                       nCountries = 50L, converged = TRUE, separation = FALSE,
                       vif = list(highVIF = FALSE)),
    projections = NULL
  ), class = "PFMModel")
}
