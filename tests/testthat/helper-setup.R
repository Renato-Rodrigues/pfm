library(madrat)
library(magclass)
library(pfm)

#' Path to the package test data directory
test_data_dir <- function() {
  testthat::test_path("testdata")
}

#' Set up an isolated madrat environment for the duration of one test.
mr_local_env <- function(src = NULL, env = parent.frame()) {
  oldConfig <- tryCatch(madrat::getConfig(), error = function(e) list())
  tmp <- withr::local_tempdir(.local_envir = env)
  sf <- if (!is.null(src)) file.path(test_data_dir(), src) else tmp
  suppressMessages(suppressWarnings(
    madrat::setConfig(sourcefolder = sf, mainfolder = tmp, verbosity = 0)
  ))
  withr::defer(
    suppressMessages(suppressWarnings(
      madrat::setConfig(
        sourcefolder = oldConfig$sourcefolder,
        mainfolder   = oldConfig$mainfolder,
        verbosity    = oldConfig$verbosity %||% 1
      )
    )),
    envir = env
  )
  invisible(tmp)
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
