#!/usr/bin/env Rscript
# PFM run launcher (ADR 0020). Runs a model group locally or as a PIK SLURM job, optionally
# rendering the reports afterwards. Run from the pfm-reports working directory (where
# config.yml, cache/, results/, data/fulldata.gdx live).
#
# Examples:
#   Rscript start.R --group=exhaustive
#   Rscript start.R --group=exhaustive --nCores=128 --render
#   Rscript start.R --group=guided --steps=sweep,robustness --cluster=local
#   Rscript start.R --group=no-fe54 --mode=exhaustive --qos=medium --time=2-00:00:00 --selectFE=H12,OECDp,Mundlak
#
# Defaults come from ./config.yml when present (resultsDir, modelDir, gdxPath), else sensible
# fallbacks. SLURM vs local is auto-detected (override with --cluster=slurm|local).
suppressMessages(library(pfm))

args <- commandArgs(trailingOnly = TRUE)
getArg  <- function(name, default = NULL) {
  hit <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (length(hit)) sub(paste0("^--", name, "="), "", hit[[1]]) else default
}
hasFlag <- function(name) any(args == paste0("--", name))

cfg <- list()
if (file.exists("config.yml") && requireNamespace("yaml", quietly = TRUE)) {
  cfg <- tryCatch(yaml::read_yaml("config.yml"), error = function(e) list())
}
def    <- function(key, fb) { v <- cfg[[key]]; if (is.null(v) || !nzchar(as.character(v))) fb else v }
absify <- function(p) if (is.null(p) || grepl("^([A-Za-z]:|/|\\\\)", p)) p else file.path(getwd(), p)

nCoresArg <- getArg("nCores", NULL)
gdx <- absify(def("gdxPath", "data/fulldata.gdx"))
if (!is.null(gdx) && !file.exists(gdx)) gdx <- NULL
render   <- hasFlag("render")
selectFE <- getArg("selectFE", NULL)

callArgs <- list(
  group           = getArg("group", "exhaustive"),
  steps           = strsplit(getArg("steps", "sweep,robustness,temporal,subnational"), ",")[[1]],
  mode            = getArg("mode", "exhaustive"),
  selectionMethod = getArg("selectionMethod", "levels-first"),
  resultsDir      = absify(def("resultsDir", "results")),
  cacheDir        = absify(def("modelDir", "cache")),
  gdxFile         = gdx,
  nCores          = if (is.null(nCoresArg)) NULL else as.integer(nCoresArg),
  cluster         = getArg("cluster", "auto"),
  qos             = getArg("qos", "short"),
  partition       = getArg("partition", "standard"),
  time            = getArg("time", "24:00:00"),
  account         = getArg("account", NULL),
  mem             = getArg("mem", NULL),
  reportsDir      = if (render) getwd() else NULL,
  render          = render,
  forceRefit      = hasFlag("forceRefit")
)
if (!is.null(selectFE)) callArgs$selectFE <- strsplit(selectFE, ",")[[1]]  # forwarded to runSweep
do.call(pfm::startRun, callArgs)
