# Run launcher + status checker plumbing (ADR 0020): core detection, the job-script literal
# serializer, cluster-detection guards, and runStatus reading a Run-Group manifest.

test_that(".pfmDetectCores honours SLURM_CPUS_PER_TASK then falls back", {
  withr::with_envvar(c(SLURM_CPUS_PER_TASK = "37"), {
    expect_equal(pfm:::.pfmDetectCores(), 37L)
  })
  withr::with_envvar(c(SLURM_CPUS_PER_TASK = ""), {
    n <- pfm:::.pfmDetectCores()
    expect_true(is.numeric(n) && n >= 1)
  })
})

test_that(".rlit serialises R values to literals", {
  expect_equal(pfm:::.rlit(NULL), "NULL")
  expect_equal(pfm:::.rlit(TRUE), "TRUE")
  expect_equal(pfm:::.rlit(8L), "8")
  expect_equal(pfm:::.rlit("a"), '"a"')
  expect_equal(pfm:::.rlit(c("H12", "OECDp")), 'c("H12", "OECDp")')
})

test_that("startRun(cluster='slurm') errors when sbatch is absent", {
  skip_if(nzchar(Sys.which("sbatch")), "sbatch present; cannot test the missing-sbatch guard")
  expect_error(
    startRun("g", resultsDir = withr::local_tempdir(), cluster = "slurm"),
    "sbatch"
  )
})

test_that("runStatus reads a Run-Group manifest and computes remaining steps", {
  skip_if_not_installed("jsonlite")
  resultsDir <- withr::local_tempdir()
  gd <- file.path(resultsDir, "g"); dir.create(gd, recursive = TRUE)
  saveRDS(1, file.path(gd, "sweep.rds"))  # an artifact
  man <- list(
    group = "g", mode = "guided",
    run = list(status = "running", startedAt = "2026-06-19 10:00:00",
               cluster = "local", nCores = 4, steps = list("sweep", "robustness")),
    steps = list(sweep = list(status = "completed", seconds = 12.3,
                              metrics = list(nJobs = 4, nNew = 4, nFailed = 0))),
    artifacts = list("sweep.rds", "manifest.json")
  )
  jsonlite::write_json(man, file.path(gd, "manifest.json"), pretty = TRUE, auto_unbox = TRUE)

  st <- runStatus("g", resultsDir = resultsDir, verbose = FALSE)
  expect_equal(st$manifestStatus, "running")
  expect_equal(st$nCores, 4)
  expect_equal(names(st$steps), "sweep")
  expect_equal(st$remaining, "robustness")        # requested minus completed
  expect_true("sweep.rds" %in% st$artifacts)
  expect_null(st$slurm)                             # no slurmJobId -> no live query
})

test_that("runStatus returns NULL when no run exists", {
  expect_null(runStatus("nope", resultsDir = withr::local_tempdir(), verbose = FALSE))
})
