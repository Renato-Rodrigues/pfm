# V-Dem's overall accountability as a fifth option of the accountability slot (v7; the PoliClim index):
# the grid, the panel key, the sweep option and the manifest record.

test_that("the default grid is unchanged and OvAcc only appends specs", {
  a <- channelSpecs("exhaustive")
  b <- channelSpecs("exhaustive", extraAccountability = "OvAcc")
  na <- vapply(a, `[[`, "", "name")
  nb <- vapply(b, `[[`, "", "name")
  names(b) <- nb
  expect_true(all(na %in% nb))
  expect_true(all(mapply(identical, a, b[na])))            # every existing spec, X-number included
  added <- b[setdiff(nb, na)]
  expect_true(length(added) > 0)
  expect_true(all(grepl("\\|OvAcc ", names(added))))
  expect_true(all(vapply(added, function(s) "Overall Accountability (VDem)" %in% unlist(s$instQualityDrivers),
                         logical(1))))
  # crossed like the three parts: the four sets with and without government effectiveness / rule of law
  sets <- unique(sub("^X-\\d+ (\\S+) .*", "\\1", names(added)))
  expect_setequal(sets, c("WGIge|RoL|OvAcc", "WGIge|noRoL|OvAcc", "noGE|RoL|OvAcc", "noGE|noRoL|OvAcc"))
  # and like VerAcc: the same number of specs per institution set (VerAcc's one pureFD demonstration aside)
  perSet <- function(nm, tok) sum(grepl(paste0("^X-\\d+ WGIge\\|RoL\\|", tok, " "), nm) & !grepl("pureFD", nm))
  expect_equal(perSet(nb, "OvAcc"), perSet(nb, "VerAcc"))
  expect_error(channelSpecs("exhaustive", extraAccountability = "bogus"), "unknown extraAccountability")
})

test_that("the panel definition carries the overall-accountability key, off by default", {
  expect_false(.pfmPanelDefNormalise(NULL)$overallAccountability)
  expect_false(.pfmPanelDefNormalise(list(years = c(2000, 2023)))$overallAccountability)
  d <- .pfmPanelDefNormalise(list(years = c(2000, 2023), overallAccountability = TRUE))
  expect_true(d$overallAccountability)
  expect_match(.pfmPanelDefLabel(d), "V-Dem overall accountability")
})

test_that("sweep options parse the extra accountability channels", {
  o <- .pfmSweepOptionsNormalise(list(extraAccountability = list("OvAcc")))
  expect_identical(o$extraAccountability, "OvAcc")
  expect_true("extraAccountability" %in% .pfmSweepOptionKeys)
  expect_match(.pfmSweepOptionsLabel(o), "extra accountability channels: OvAcc")
})

test_that("runPFMSweep records every panel-definition key in the manifest", {
  # a key left out of the record is read back as its default by every later step (the democracy weights
  # and the overall-accountability channel would vanish from the panels after the sweep)
  src <- paste(deparse(body(runPFMSweep)), collapse = "\n")
  expect_match(src, "democracy = pfmPanelDef()$democracy", fixed = TRUE)
  expect_match(src, "overallAccountability = pfmPanelDef()$overallAccountability", fixed = TRUE)
  expect_match(src, "extraAccountability = as.list(extraAccountability)", fixed = TRUE)
})
