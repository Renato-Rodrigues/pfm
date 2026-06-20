#!/usr/bin/env Rscript
# PFM run status checker (ADR 0020). Reads results/<group>/manifest.json (+ live SLURM when a
# job id is recorded) and prints the run's status, per-step timings, and remaining steps.
# Run from the pfm-reports working directory.
#   Rscript status.R --group=exhaustive
#   Rscript status.R --group=guided --resultsDir=/p/tmp/$USER/results
suppressMessages(library(pfm))
args <- commandArgs(trailingOnly = TRUE)
getArg <- function(name, default = NULL) {
  hit <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (length(hit)) sub(paste0("^--", name, "="), "", hit[[1]]) else default
}
cfg <- list()
if (file.exists("config.yml") && requireNamespace("yaml", quietly = TRUE)) {
  cfg <- tryCatch(yaml::read_yaml("config.yml"), error = function(e) list())
}
def    <- function(k, fb) { v <- cfg[[k]]; if (is.null(v) || !nzchar(as.character(v))) fb else v }
absify <- function(p) if (is.null(p) || grepl("^([A-Za-z]:|/|\\\\)", p)) p else file.path(getwd(), p)
invisible(pfm::runStatus(
  group      = getArg("group", def("group", "exhaustive")),
  resultsDir = absify(getArg("resultsDir", def("resultsDir", "results")))
))
