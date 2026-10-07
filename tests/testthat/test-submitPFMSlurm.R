# submitPFM's slurmConfig: a start group whose rows set none must get one from the call, because
# start.R would otherwise ask on a terminal submitPFM captures (2026-10-07, EU21V371 waited unseen).

writeScen <- function(rows) {
  f <- withr::local_tempfile(fileext = ".csv", .local_envir = parent.frame())
  writeLines(c("title;start;slurmConfig;cm_taxCO2_regiDiff", rows), f)
  f
}

test_that("rows of the group without slurmConfig are found, coupled or not", {
  f <- writeScen(c("base;G1;;", "coupled;G1,G2;;11", "own;G1;--qos=standby;11", "other;G2;;"))
  expect_identical(pfm:::.pfmRowsWithoutSlurm(f, "G1"), c("base", "coupled"))
  expect_identical(pfm:::.pfmRowsWithoutSlurm(f, "G2"), c("coupled", "other"))
  expect_identical(pfm:::.pfmRowsWithoutSlurm(f, "G3"), character(0))
})

test_that("a missing slurmConfig is an error only when a row needs one", {
  expect_identical(pfm:::.pfmSlurmArg(NULL, character(0)), character(0))
  expect_error(pfm:::.pfmSlurmArg(NULL, c("a", "b")), "2 row\\(s\\).*slurmConfig = \"priority\"")
  expect_error(pfm:::.pfmSlurmArg("fast", "a"), "is not \"priority\"")
  expect_error(pfm:::.pfmSlurmArg("17", "a"), "is not")
})

test_that("names map to REMIND's choices and other forms pass through", {
  arg <- function(x) { a <- pfm:::.pfmSlurmArg(x, "a"); c(arg = as.character(a), value = attr(a, "value")) }
  expect_identical(arg("priority"), c(arg = paste0("slurmConfig=", shQuote("5")), value = "5"))
  expect_identical(arg("standby"), c(arg = paste0("slurmConfig=", shQuote("1")), value = "1"))
  expect_identical(arg("13"), c(arg = paste0("slurmConfig=", shQuote("13")), value = "13"))
  s <- "--qos=priority --nodes=1 --tasks-per-node=12"
  expect_identical(arg(s), c(arg = paste0("slurmConfig=", shQuote(s)), value = s))
})
