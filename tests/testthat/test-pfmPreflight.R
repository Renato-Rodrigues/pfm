# Design note 0005 F2 (pfmPreflight) and F3 (submitPFM).

skipNoGit <- function() if (!nzchar(Sys.which("git"))) testthat::skip("git not available")

gitRepo <- function(dir) {
  g <- function(...) suppressWarnings(system2("git", c("-C", shQuote(dir), ...), stdout = TRUE, stderr = TRUE))
  g("init", "-q")
  g("config", "user.email", "t@t")
  g("config", "user.name", "t")
  writeLines("a", file.path(dir, "f.txt"))
  g("add", "-A")
  g("commit", "-q", "-m", "init")
  g
}

test_that("repository state: clean and pushed, dirty, ahead, no upstream", {
  skipNoGit()
  remote <- withr::local_tempdir()
  suppressWarnings(system2("git", c("init", "-q", "--bare", shQuote(remote)), stdout = TRUE, stderr = TRUE))
  dir <- withr::local_tempdir()
  g <- gitRepo(dir)
  expect_match(pfm:::.pfmGitState(dir)$detail, "no upstream")
  g("remote", "add", "origin", remote)
  g("push", "-q", "-u", "origin", "HEAD")
  expect_true(pfm:::.pfmGitState(dir)$ok)
  writeLines("b", file.path(dir, "new.R"))
  s <- pfm:::.pfmGitState(dir)
  expect_false(s$ok)
  expect_match(s$detail, "1 uncommitted/untracked")
  g("add", "-A")
  g("commit", "-q", "-m", "two")
  s <- pfm:::.pfmGitState(dir)
  expect_false(s$ok)
  expect_match(s$detail, "1 commit\\(s\\) not pushed")
  g("push", "-q")
  expect_true(pfm:::.pfmGitState(dir)$ok)
  # another clone pushes: this one is now BEHIND, and only a fetch can see it
  other <- withr::local_tempdir()
  suppressWarnings(system2("git", c("clone", "-q", shQuote(remote), shQuote(other)), stdout = TRUE, stderr = TRUE))
  go <- function(...) suppressWarnings(system2("git", c("-C", shQuote(other), ...), stdout = TRUE, stderr = TRUE))
  go("config", "user.email", "t@t")
  go("config", "user.name", "t")
  writeLines("c", file.path(other, "g.txt"))
  go("add", "-A")
  go("commit", "-q", "-m", "three")
  go("push", "-q")
  expect_true(pfm:::.pfmGitState(dir, fetch = FALSE)$ok)   # the stale view passes
  s <- pfm:::.pfmGitState(dir)
  expect_false(s$ok)
  expect_match(s$detail, "1 commit\\(s\\) behind the remote")
})

writeConf <- function(path, df) utils::write.table(df, path, sep = ";", row.names = FALSE, quote = FALSE, na = "")

test_that("coupled rows: start group, copyConfigFrom, default SSP and the reference's SSP", {
  rd <- withr::local_tempdir()
  dir.create(file.path(rd, "config"))
  writeLines(c("$setglobal cm_GDPpopScen   SSP2  !! def = SSP2"), file.path(rd, "main.gms"))
  writeConf(file.path(rd, "config", "scenario_config.csv"),
            data.frame(title = c("SSP2-NPi", "SSP3-NPi"), start = c(0, 0), cm_GDPpopScen = c(NA, "SSP3")))
  pfmCsv <- file.path(rd, "config", "scenario_config_PFM.csv")
  conf <- data.frame(
    title = c("A", "B", "C", "D"), start = c("G1", "G1,G2", "G2", "G1"),
    copyConfigFrom = c(NA, "A", NA, NA), cm_taxCO2_regiDiff = c(11, NA, 11, "none"),
    cm_GDPpopScen = c(NA, NA, "SSP3", NA), path_gdx_ref = c("SSP2-NPi", "SSP2-NPi", "SSP2-NPi", NA),
    pfmGroup = c("v6", NA, "v6", NA)
  )
  writeConf(pfmCsv, conf)
  r <- pfm:::.pfmCoupledRows(pfmCsv, "G1", rd)
  expect_identical(r$title, c("A", "B"))                 # D is not coupled
  expect_identical(r$pfmGroup, c("v6", "v6"))            # B inherits from A
  expect_identical(r$ssp, c("SSP2", "SSP2"))             # main.gms default
  r2 <- pfm:::.pfmCoupledRows(pfmCsv, "G2", rd)
  expect_identical(r2$title, c("B", "C"))
  expect_identical(r2$ssp[2], "SSP3")
  expect_identical(r2$refSsp[2], "SSP2")                 # an SSP3 row on an SSP2 reference: the ssp check fails
  expect_identical(pfm:::.pfmRemindDefault(rd, "cm_GDPpopScen"), "SSP2")
})

test_that("Run-Group export: everything preparePFM copies, the panel named by the manifest", {
  gd <- withr::local_tempdir()
  expect_true("manifest.json" %in% pfm:::.pfmGroupExportMissing(gd))
  for (f in c("frontier.rds", "temporal-validation.rds", "donor-assignment-band-Bulk.rds",
              "donor-assignment-band-Diffuse.rds", "selected-models-pfm.yml")) writeLines("x", file.path(gd, f))
  jsonlite::write_json(list(panel_hash = "abc"), file.path(gd, "manifest.json"), auto_unbox = TRUE)
  expect_identical(pfm:::.pfmGroupExportMissing(gd), "the panel panel_abc.rds")
  dir.create(file.path(gd, "panels"))
  writeLines("x", file.path(gd, "panels", "panel_abc.rds"))
  expect_length(pfm:::.pfmGroupExportMissing(gd), 0)
  expect_identical(pfm:::.pfmGroupExportMissing(file.path(gd, "nope")), "the Run-Group folder")
})

test_that("pfmPreflight reports every failure and refuses", {
  rd <- withr::local_tempdir()
  dir.create(file.path(rd, "config"))
  pfmCsv <- file.path(rd, "config", "scenario_config_PFM.csv")
  writeConf(pfmCsv, data.frame(title = "A", start = "G1", cm_taxCO2_regiDiff = 11, path_gdx_ref = "missing-ref",
                               pfmGroup = "v6"))
  root <- withr::local_tempdir()
  writeLines("group: v6", file.path(root, "config.yml"))
  res <- suppressMessages(pfmPreflight(config = file.path(root, "config.yml"), startGroup = "G1",
                                       scenarioConfig = pfmCsv, remindDirs = rd,
                                       checks = c("groups", "ssp"), stopOnFail = FALSE, verbose = FALSE))
  expect_identical(res$check, c("groups", "ssp"))
  expect_false(any(res$ok))
  expect_error(suppressMessages(pfmPreflight(config = file.path(root, "config.yml"), startGroup = "G1",
                                             scenarioConfig = pfmCsv, remindDirs = rd,
                                             checks = "groups", verbose = FALSE)),
               "check\\(s\\) failed")
})

test_that("submitPFM: dry run checks and plans; a submission writes the batch manifest", {
  rd <- withr::local_tempdir()
  dir.create(file.path(rd, "config"))
  pfmCsv <- file.path(rd, "config", "scenario_config_PFM.csv")
  writeConf(pfmCsv, data.frame(title = "A", start = "G1", cm_taxCO2_regiDiff = 11, pfmGroup = "v6",
                               path_gdx_ref = NA))
  root <- withr::local_tempdir()
  writeLines("group: v6", file.path(root, "config.yml"))
  calls <- list()
  testthat::local_mocked_bindings(
    pfmPreflight = function(...) data.frame(check = "installed", target = "pfm", ok = TRUE, detail = "x"),
    .pfmRun = function(cmd, args, wd) {
      calls[[length(calls) + 1L]] <<- args
      list(out = "0 errors", status = 0L)
    }
  )
  # a row without slurmConfig needs one from the call: start.R would otherwise prompt unseen
  expect_error(suppressMessages(submitPFM("G1", remindDir = rd, config = file.path(root, "config.yml"), verbose = FALSE)),
               "set no slurmConfig")
  r <- suppressMessages(submitPFM("G1", remindDir = rd, config = file.path(root, "config.yml"),
                                  slurmConfig = "priority", verbose = FALSE))
  expect_false(r$submitted)
  expect_identical(r$rows$title, "A")
  expect_true(any(vapply(calls, function(a) "--test" %in% a, logical(1))))
  expect_true(any(vapply(calls, function(a) "--gamscompile" %in% a, logical(1))))   # compile = TRUE by default
  expect_false(dir.exists(file.path(root, "output")))
  r <- suppressMessages(submitPFM("G1", remindDir = rd, config = file.path(root, "config.yml"),
                                  dry = FALSE, slurmConfig = "priority", verbose = FALSE))
  expect_true(r$submitted)
  man <- jsonlite::read_json(r$manifest)
  expect_identical(man$startGroup, "G1")
  expect_identical(man$preflight, "passed")
  expect_true(file.exists(sub("[.]json$", ".log", r$manifest)))
  expect_identical(man$slurmConfig, "5")
  expect_identical(man$gamsCompile, "passed")
  expect_true(any(vapply(calls, function(a) "startgroup=G1" %in% a && !"--test" %in% a &&
                                            paste0("slurmConfig=", shQuote("5")) %in% a, logical(1))))
})

test_that("submitPFM stops on a GAMS compile FAIL", {
  rd <- withr::local_tempdir()
  dir.create(file.path(rd, "config"))
  pfmCsv <- file.path(rd, "config", "scenario_config_PFM.csv")
  writeConf(pfmCsv, data.frame(title = "A", start = "G1", cm_taxCO2_regiDiff = 11, pfmGroup = "v6",
                               path_gdx_ref = NA, slurmConfig = "--qos=standby"))
  root <- withr::local_tempdir()
  writeLines("group: v6", file.path(root, "config.yml"))
  testthat::local_mocked_bindings(
    pfmPreflight = function(...) data.frame(check = "installed", target = "pfm", ok = TRUE, detail = "x"),
    .pfmRun = function(cmd, args, wd) {
      if ("--gamscompile" %in% args) return(list(out = c("FAIL output/gamscompile/main_A.lst"), status = 1L))
      list(out = "0 errors", status = 0L)
    }
  )
  expect_error(suppressMessages(submitPFM("G1", remindDir = rd, config = file.path(root, "config.yml"), verbose = FALSE)),
               "GAMS compile failed")
  r <- suppressMessages(submitPFM("G1", remindDir = rd, config = file.path(root, "config.yml"), compile = FALSE,
                                  verbose = FALSE))
  expect_false(r$submitted)
})

test_that("mappings: the resolved H12 must be REMIND's own regions", {
  rd <- withr::local_tempdir()
  dir.create(file.path(rd, "config"))
  h12 <- utils::read.csv(system.file("extdata", "regional", "regionmappingH12.csv", package = "mrpfm"),
                         sep = ";", stringsAsFactors = FALSE)
  utils::write.table(h12, file.path(rd, "config", "regionmappingH12.csv"), sep = ";", row.names = FALSE, quote = FALSE)
  m <- Filter(function(x) x$name == "regionmappingH12.csv", pfm:::.pfmMappingMismatches(rd))[[1]]
  expect_true(m$ok)
  expect_match(m$detail, "REMIND's own regions")
  h12$RegionCode[h12$CountryCode == "UKR"] <- "NEU"                 # REMIND solving on other regions
  utils::write.table(h12, file.path(rd, "config", "regionmappingH12.csv"), sep = ";", row.names = FALSE, quote = FALSE)
  m <- Filter(function(x) x$name == "regionmappingH12.csv", pfm:::.pfmMappingMismatches(rd))[[1]]
  expect_false(m$ok)
  expect_match(m$detail, "/config")
})

test_that("the share-path switch must match the group's formulation (0005 Phase 3)", {
  rd <- withr::local_tempdir()
  dir.create(file.path(rd, "config"))
  pfmCsv <- file.path(rd, "config", "scenario_config_PFM.csv")
  writeConf(pfmCsv, data.frame(title = c("A", "B", "C", "D"), start = "G1", cm_taxCO2_regiDiff = 11,
                               copyConfigFrom = c(NA, "A", NA, NA), cm_pfmPhiPath = c(1, NA, NA, 1),
                               pfmGroup = c("g6", "g6", "g6", "g5"), path_gdx_ref = NA))
  r <- pfm:::.pfmCoupledRows(pfmCsv, "G1", rd)
  expect_identical(r$phiPath, c("1", "1", "0", "1"))     # B inherits from A, C takes the main.gms default
  ri <- withr::local_tempdir()
  for (g in c("g5", "g6")) {
    gd <- file.path(ri, g); dir.create(file.path(gd, "panels"), recursive = TRUE)
    for (f in c("frontier.rds", "temporal-validation.rds", "donor-assignment-band-Bulk.rds",
                "donor-assignment-band-Diffuse.rds", "selected-models-pfm.yml")) writeLines("x", file.path(gd, f))
    jsonlite::write_json(list(panel_hash = "h"), file.path(gd, "manifest.json"), auto_unbox = TRUE)
    writeLines("x", file.path(gd, "panels", "panel_h.rds"))
  }
  writeLines("x", file.path(ri, "g6", "phi-anchor.rds"))
  root <- withr::local_tempdir(); writeLines("group: v6", file.path(root, "config.yml"))
  res <- suppressMessages(pfmPreflight(config = file.path(root, "config.yml"), startGroup = "G1",
                                       scenarioConfig = pfmCsv, remindDirs = rd, remindInputs = ri,
                                       checks = "groups", stopOnFail = FALSE, verbose = FALSE))
  sw <- res[grepl("cm_pfmPhiPath", res$target), ]
  expect_false(sw$ok[sw$target == "g6 cm_pfmPhiPath"])
  expect_match(sw$detail[sw$target == "g6 cm_pfmPhiPath"], "must be 1 on: C")
  expect_false(sw$ok[sw$target == "g5 cm_pfmPhiPath"])
  expect_match(sw$detail[sw$target == "g5 cm_pfmPhiPath"], "must be 0 on: D")
})

test_that("the run renv: a pinned lockfile must list pfm and mrpfm (REMIND 3.7.1, 2026-10-07)", {
  rd <- withr::local_tempdir()
  dir.create(file.path(rd, "config")); dir.create(file.path(rd, "renv"))
  cfg <- function(line) writeLines(c("cfg <- list()", line), file.path(rd, "config", "default.cfg"))
  cfg("cfg$UseThisRenvLock <- NULL   # snapshot the checkout's renv")
  expect_null(pfm:::.pfmRunRenvLock(rd)$path)
  jsonlite::write_json(list(Packages = list(gdx2 = list(Package = "gdx2"), remind2 = list(Package = "remind2"))),
                       file.path(rd, "renv", "release.lock"), auto_unbox = TRUE)
  cfg('cfg$UseThisRenvLock <- "renv/release.lock"')
  l <- pfm:::.pfmRunRenvLock(rd)
  expect_identical(l$path, "renv/release.lock")
  expect_false(all(c("pfm", "mrpfm") %in% l$packages))
  jsonlite::write_json(list(Packages = list(pfm = list(Package = "pfm"), mrpfm = list(Package = "mrpfm"))),
                       file.path(rd, "renv", "ours.lock"), auto_unbox = TRUE)
  cfg("cfg$UseThisRenvLock <- 'renv/ours.lock'")
  expect_true(all(c("pfm", "mrpfm") %in% pfm:::.pfmRunRenvLock(rd)$packages))
  # the fork, if it is next to the package: NULL since 2026-10-07; REMIND's own release lockfile lacks pfm
  fork <- normalizePath(file.path(rprojroot::find_root("DESCRIPTION"), "..", "remind_pfm"), mustWork = FALSE)
  skip_if_not(dir.exists(fork))
  expect_null(pfm:::.pfmRunRenvLock(fork)$path)
  rel <- file.path(fork, "renv", "archive", "3.7.1_renv.lock")
  skip_if_not(file.exists(rel))
  expect_false("pfm" %in% names(jsonlite::read_json(rel)$Packages))
})
