# nolint start
# Regression guard for the Run-Group v2 failure: all 2160 per-capita specs errored
# with "The following variables are missing from the data" even though the panel
# carried "Innovator Power pc|Bulk" etc. Cause: preparePanelData's `known_indices`
# listed only the share-based names, so the "|<sector>" suffix was never appended
# to the `*pc` variants and the lookup was done on the unqualified name.

apPanel <- function(vars) {
  magclass::new.magpie(
    c("AAA", "BBB"), c(2000, 2010, 2020),
    c("Effective Carbon Price|Bulk", "Rule of Law (VDem)", vars),
    fill = 1
  )
}

pcVars <- c("Innovator Power pc|Bulk", "Incumbent Power pc|Bulk")
shVars <- c("Innovator Power|Bulk", "Incumbent Power|Bulk")

test_that("per-capita actor power resolves its sector suffix like the share form", {
  m <- apPanel(c(shVars, pcVars))
  for (ap in list(
    c("Innovator Power", "Incumbent Power"),                          # splitAP
    c("Innovator Power pc", "Incumbent Power pc"),                    # splitAPpc
    c("Innovator Power", "Incumbent Power pc"),                       # mixedAP
    c("Innovator Power", "Incumbent Power", "Incumbent Power pc")     # bothIncAP
  )) {
    df <- expect_no_error(preparePanelData(
      m, sector = "Bulk",
      actorPowerDrivers = ap, actorPowerIndex = ap,
      instQualityDrivers = "Rule of Law (VDem)", controlDrivers = NULL,
      regionMappingFixedEffects = NULL
    ))
    expect_true(all(make.names(ap) %in% colnames(df)))
  }
})

test_that("a genuinely absent per-capita column still errors", {
  # The suffix fix must not turn a real missing-data bug into a silent pass.
  m <- apPanel(shVars)
  expect_error(
    preparePanelData(
      m, sector = "Bulk",
      actorPowerDrivers = c("Innovator Power pc", "Incumbent Power pc"),
      actorPowerIndex = c("Innovator Power pc", "Incumbent Power pc"),
      instQualityDrivers = "Rule of Law (VDem)", controlDrivers = NULL,
      regionMappingFixedEffects = NULL
    ),
    "missing from the data"
  )
})
# nolint end
