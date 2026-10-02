# The project madrat cache: config.yml `madrat:` block (pfmResolveConfig) and its preparation
# (pfmPrepareCache) - copy what is missing from other caches, compute the rest, record it.

test_that("the madrat block resolves the {group} tag and the first EXISTING source candidate", {
  d <- withr::local_tempdir()
  dir.create(file.path(d, "here-sources"))
  dir.create(file.path(d, "other-cache"))
  writeLines(c("group: v9",
               "madrat:",
               '  cachefolder: "data/madrat/{group}"',
               '  sourcefolder: ["/no/such/cluster/path", "here-sources"]',
               '  cacheSources: ["other-cache", "/no/such/cache"]',
               "  compute: false"), file.path(d, "config.yml"))
  rc <- pfm::pfmResolveConfig(file.path(d, "config.yml"), verbose = FALSE)
  expect_equal(basename(rc$cachefolder), "v9")
  expect_equal(rc$madrat$tag, "v9")
  expect_equal(basename(rc$sourcefolder), "here-sources")
  expect_equal(basename(rc$madrat$cacheSources), "other-cache")
  expect_false(rc$madrat$compute)
  expect_true(rc$madrat$useMadratConfig)
  # the call's group wins over the config's
  expect_equal(basename(pfm::pfmResolveConfig(file.path(d, "config.yml"), group = "v10",
                                              verbose = FALSE)$cachefolder), "v10")
})

test_that("older configs with top-level cachefolder/sourcefolder still resolve", {
  d <- withr::local_tempdir()
  writeLines(c('cachefolder: "data/madrat"', 'sourcefolder: ""'), file.path(d, "config.yml"))
  rc <- pfm::pfmResolveConfig(file.path(d, "config.yml"), verbose = FALSE)
  expect_equal(rc$cachefolder, normalizePath(file.path(d, "data/madrat"), winslash = "/", mustWork = FALSE))
  expect_null(rc$sourcefolder)
})

test_that(".useMadratCache applies the source folder with the cache folder", {
  old <- madrat::getConfig(verbose = FALSE)
  on.exit(suppressMessages(madrat::setConfig(cachefolder = old$cachefolder, sourcefolder = old$sourcefolder,
                                             forcecache = old$forcecache, .verbose = FALSE)), add = TRUE)
  cf <- file.path(tempdir(), "sf-cache")
  sf <- file.path(tempdir(), "sf-src")
  dir.create(cf, showWarnings = FALSE)
  dir.create(sf, showWarnings = FALSE)
  suppressMessages(pfm:::.useMadratCache(cf, sourcefolder = sf))
  got <- madrat::getConfig(verbose = FALSE)
  expect_equal(normalizePath(got$sourcefolder), normalizePath(sf))
  expect_equal(normalizePath(got$cachefolder), normalizePath(cf))
  expect_true(isTRUE(got$forcecache))
})

test_that("pfmPrepareCache copies a miss from a cache source, records it, then reports ready", {
  mrLocalEnv()
  old <- madrat::getConfig(verbose = FALSE)
  withr::defer(suppressMessages(madrat::setConfig(cachefolder = old$cachefolder, forcecache = old$forcecache,
                                                  globalenv = old$globalenv, .verbose = FALSE)))
  calcPfmToy <- function() {
    list(x = magclass::new.magpie("GLO", 2000, "a", 1), weight = NULL,
         unit = "1", description = "toy", isocountries = FALSE)
  }
  assign("calcPfmToy", calcPfmToy, envir = globalenv())
  withr::defer(rm("calcPfmToy", envir = globalenv()))
  suppressMessages(madrat::setConfig(globalenv = TRUE, .verbose = FALSE))

  d <- withr::local_tempdir()
  src <- file.path(d, "src")
  dir.create(src)
  suppressMessages(madrat::setConfig(cachefolder = src, forcecache = TRUE, .verbose = FALSE))
  suppressWarnings(suppressMessages(madrat::calcOutput("PfmToy", aggregate = FALSE)))
  expect_length(list.files(src, "[.]rds$"), 1)

  writeLines(c("group: g1", "madrat:", '  cachefolder: "cache/{group}"', '  cacheSources: ["src"]',
               "  useMadratConfig: false", "  compute: false"), file.path(d, "config.yml"))
  toy <- function(...) {
    list(list(id = "toy", label = "toy", f = function() {
      madrat::calcOutput("PfmToy", aggregate = FALSE)
    }))
  }
  testthat::local_mocked_bindings(.cacheBuilders = toy)

  r <- suppressWarnings(pfm::pfmPrepareCache(file.path(d, "config.yml"), verbose = FALSE))
  expect_equal(r$status, "prepared")
  expect_equal(r$manifest$file, list.files(src, "[.]rds$"))
  expect_equal(r$manifest$origin, normalizePath(src, winslash = "/"))
  expect_equal(r$manifest$builders, "toy")
  expect_true(file.exists(file.path(d, "cache", "g1", "cache-manifest.tsv")))
  # the tracked record: same files and md5s, nothing machine-specific
  rec <- utils::read.delim(file.path(d, "records", "g1", "madrat-cache-manifest.tsv"), comment.char = "#")
  expect_equal(rec$md5, r$manifest$md5)
  expect_false("origin" %in% names(rec))
  expect_equal(suppressWarnings(pfm::pfmPrepareCache(file.path(d, "config.yml"), verbose = FALSE))$status, "ready")

  # the copy is gone and so is the source: check-only mode says so and writes no manifest
  unlink(file.path(d, "cache", "g1", r$manifest$file))
  unlink(src, recursive = TRUE)
  r3 <- suppressWarnings(pfm::pfmPrepareCache(file.path(d, "config.yml"), verbose = FALSE))
  expect_equal(r3$status, "incomplete")
  expect_match(r3$failed[["toy"]], "no cache has calcPfmToy")
})
