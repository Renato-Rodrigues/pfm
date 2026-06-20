# nolint start
mkRows <- function(stage, transform, maxVIF) {
  do.call(rbind, lapply(c("Bulk", "Diffuse"), function(sec) data.frame(
    model = "M1", sector = sec, stage = stage, panelTransform = transform,
    sigActorPower = 2L, sigInstQual = 1L, sigInteractions = 1L,
    deltaR2Theory = 0.2, pseudoR2 = 0.3, bic = 100, maxVIF = maxVIF,
    converged = TRUE, usesLagged = FALSE, stringsAsFactors = FALSE)))
}

test_that("returns NA selection when there are no hybridFD rows", {
  res <- mkRows("Adoption", "levels", 3)
  out <- selectDifferenceFirst(res, specByName = list(), panelData = NULL)
  expect_true(is.null(out[["Adoption"]]) || is.na(out[["Adoption"]]$chosen))
})

test_that("returns NA selection when no hybridFD spec passes the maximin hard gate", {
  res <- mkRows("Adoption", "hybridFD", 20)   # maxVIF 20 fails the VIF<10 gate
  out <- selectDifferenceFirst(res, specByName = list(M1 = list()), panelData = NULL)
  expect_true("Adoption" %in% names(out))
  expect_true(is.na(out[["Adoption"]]$chosen))
  expect_null(out[["Adoption"]]$chosenConfigLevels)
})
# nolint end
