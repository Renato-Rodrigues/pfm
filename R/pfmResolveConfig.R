# nolint start
#' Resolve a run configuration and its scenario registry
#'
#' @description
#' Reads the YAML that tells a run WHERE things are: the madrat cache, the results
#' root, and above all the Policy Scenario Registry (ADR 0035) naming each scenario's
#' gdx and that gdx's own region mapping.
#'
#' This used to live in a launcher script outside the package, which meant a
#' caller going straight to \code{\link{startRun}} silently got NO scenarios: the
#' projection then fell back to a single legacy scenario read from a generic gdx and
#' wrote a projection of the wrong pathway, with nothing in the log to say so. Config
#' resolution belongs wherever the run is started from, so it lives here.
#'
#' Relative paths inside the file resolve against \strong{the config file's own
#' directory}, not the working directory, so a config can sit next to its data and be
#' passed from anywhere.
#'
#' @param config Path to the YAML. \code{NULL} looks for \code{config.yml} in the
#'   working directory and returns empty defaults if there is none.
#' @param group Run-Group. Fills the \code{\{group\}} tag of \code{madrat: cachefolder};
#'   \code{NULL} takes the config's own \code{group}.
#' @param verbose Logical.
#'
#' @section The madrat block:
#' \preformatted{
#' madrat:
#'   cachefolder: "data/madrat/{group}"   # the project cache; {group} -> one cache per Run-Group
#'   sourcefolder: [<dir>, <dir>]          # raw sources, first that EXISTS on this machine wins
#'   cacheSources: [<dir>, <dir>]          # caches to copy missing files from, in order
#'   useMadratConfig: true                 # also madrat's own cachefolder/sourcefolder, last
#'   compute: true                         # compute what no cache has, from the sources
#' }
#' Lists of candidate paths are how one committed config serves the workstation and the
#' cluster: a path that does not exist on this machine is skipped. The top-level
#' \code{cachefolder}/\code{sourcefolder} keys of older configs are still read.
#' See \code{\link{pfmPrepareCache}}.
#'
#' @section The panel block:
#' \preformatted{
#' panel:
#'   years: [2000, 2023]       # first and last panel year
#'   movingAverage: 5          # centred window; 1 = annual values
#'   ieaVersion: latest        # IEA World Energy Balances: default (2024 ed.) | latest (2025 ed.)
#'   groups:                   # per-Run-Group overrides of the keys above
#'     v6-annual: {movingAverage: 1}
#' }
#' It applies to a group that has not been swept yet; a swept group keeps the definition
#' recorded in its manifest (\code{\link{pfmPanelDef}}).
#'
#' @section The sweep block:
#' \preformatted{
#' sweep:
#'   apTransforms: [linear, saturating, saturating-innovator, saturating-incumbent]
#'   dropCompositeAP: true
#'   apExtrapolationGate: 0.275      # share of country-years; omit to leave the gate off
#'   apExtrapolationSd: 1
#'   apExtrapolationWindow: [2025, 2100]
#'   groups: {}                      # per-Run-Group overrides, as for panel
#' }
#' Arguments of \code{\link{runPFMSweep}} (design note 0005 D7). Like \code{panel}, it applies
#' only to a group not swept yet: the sweep records them in the manifest
#' (\code{sweepOptions}), and a group swept before the record keeps the v5 grid.
#'
#' @return List with \code{scenarios} (or \code{NULL}), \code{gdxFile} (the gating
#'   scenario's gdx, or \code{NULL}), \code{cachefolder}, \code{sourcefolder},
#'   \code{madrat} (the resolved madrat block), \code{panel} and \code{sweep} (the blocks
#'   resolved for \code{group}, or \code{NULL}), \code{group}, \code{recordsDir}, \code{resultsDir},
#'   \code{modelDir}, \code{path} and \code{dir}.
#' @author Renato Rodrigues
#' @export
pfmResolveConfig <- function(config = NULL, group = NULL, verbose = TRUE) {
  say <- function(...) if (isTRUE(verbose)) message("[config] ", ...)
  cfg <- list(); confDir <- getwd(); path <- NULL

  if (!is.null(config) && nzchar(config)) {
    if (!file.exists(config)) {
      stop("pfmResolveConfig: no such config file: ", config, call. = FALSE)
    }
    path <- normalizePath(config, winslash = "/", mustWork = TRUE)
    cfg <- tryCatch(yaml::read_yaml(path), error = function(e)
      stop("pfmResolveConfig: could not parse ", path, ": ", conditionMessage(e), call. = FALSE))
    confDir <- dirname(path)
    say("using ", path)
  } else if (file.exists("config.yml")) {
    path <- normalizePath("config.yml", winslash = "/", mustWork = TRUE)
    cfg <- tryCatch(yaml::read_yaml(path), error = function(e) list())
    confDir <- dirname(path)
    say("using ", path, " (found in the working directory)")
  } else {
    say("no config file — no scenario registry; ",
        "steps needing scenario gdxs will fall back or skip.")
  }

  absify <- function(p) {
    if (is.null(p) || !nzchar(p)) return(p)
    if (grepl("^([A-Za-z]:|/|\\\\)", p)) return(p)
    normalizePath(file.path(confDir, p), winslash = "/", mustWork = FALSE)
  }
  def <- function(key, fb) { v <- cfg[[key]]
    if (is.null(v) || !nzchar(as.character(v))) fb else v }

  scenReg <- parseScenarioRegistry(cfg, baseDir = confDir)
  scenarios <- if (length(scenReg$scenarios)) scenReg$scenarios else NULL
  gdxFile <- scenarioGatingGdx(scenReg) %||% absify(def("gdxPath", NULL))
  if (!is.null(gdxFile) && !file.exists(gdxFile)) {
    say("gating gdx not found (", gdxFile, ") — the Projection-Sanity gate will be skipped.")
    gdxFile <- NULL
  }
  if (!is.null(scenarios)) {
    say("scenario registry: ", length(scenarios), " scenario(s) [",
        paste(names(scenarios), collapse = ", "), "]; gating = ",
        scenReg$gating %||% "none")
  }

  # --- madrat: the project cache, where it is filled from, and the raw sources --------------
  md <- cfg[["madrat"]] %||% list()
  group <- group %||% def("group", NULL)
  template <- md[["cachefolder"]] %||% cfg[["cachefolder"]] %||% cfg[["cacheDir"]] %||% "data/cache"
  tag <- if (grepl("{group}", template, fixed = TRUE)) group %||% "default" else basename(template)
  cachefolder <- absify(gsub("{group}", tag, template, fixed = TRUE))
  paths <- function(v) {
    v <- as.character(unlist(v)); v <- v[!is.na(v) & nzchar(v)]
    vapply(v, function(p) absify(path.expand(p)), character(1), USE.NAMES = FALSE)
  }
  # Per MACHINE: a candidate that does not exist here is skipped, so the workstation and the
  # cluster paths can both be listed in the one committed file.
  sfCand <- paths(md[["sourcefolder"]] %||% cfg[["sourcefolder"]])
  sourcefolder <- sfCand[dir.exists(sfCand)][1]
  if (is.na(sourcefolder)) sourcefolder <- NULL
  csCand <- paths(md[["cacheSources"]])
  cacheSources <- unique(csCand[dir.exists(csCand)])
  cacheSources <- cacheSources[normalizePath(cacheSources, winslash = "/") !=
                                 normalizePath(cachefolder, winslash = "/", mustWork = FALSE)]
  madratBlock <- list(cachefolder = cachefolder, template = template, tag = tag,
                      sourcefolder = sourcefolder, sourcefolderCandidates = sfCand,
                      cacheSources = cacheSources, cacheSourceCandidates = csCand,
                      useMadratConfig = !isFALSE(md[["useMadratConfig"]]),
                      compute = !isFALSE(md[["compute"]]))

  # --- panel: years, smoothing, IEA edition; the group's override on top of the default ------
  panel <- cfg[["panel"]]
  if (!is.null(panel)) {
    override <- if (!is.null(group)) panel[["groups"]][[group]] else NULL
    panel[["groups"]] <- NULL
    if (!is.null(override)) panel <- utils::modifyList(panel, override)
    panel <- .pfmPanelDefNormalise(panel)
  }

  # --- sweep: the actor-power axes and the extrapolation gate (0005 D7), same override rule ---
  sweep <- cfg[["sweep"]]
  if (!is.null(sweep)) {
    override <- if (!is.null(group)) sweep[["groups"]][[group]] else NULL
    sweep[["groups"]] <- NULL
    if (!is.null(override)) sweep <- utils::modifyList(sweep, override)
    sweep <- .pfmSweepOptionsNormalise(sweep)
  }

  list(scenarios = scenarios, gdxFile = gdxFile,
       cachefolder = cachefolder, sourcefolder = sourcefolder, madrat = madratBlock,
       panel = panel, sweep = sweep, group = group,
       resultsDir = def("resultsDir", NULL), modelDir = def("modelDir", NULL),
       # Tracked reproduction records, one folder per Run-Group: the madrat pin and the list
       # of input files (pfmPrepareCache). Unlike output/, this folder is in git.
       recordsDir = absify(def("recordsDir", "records")),
       # The panel's spatial resolution. DECLARED here rather than left to each step
       # function's own default, because ~14 of them carry one and a partial change
       # produces a Run-Group fitted at one resolution and projected at another --
       # the failure class of PITFALLS 15/20/21. Every min-max normalised quantity
       # (u, and hence phi, and hence the theta anchor) is normalised over whatever
       # units are in frame, so this changes the fitted object, not the display.
       outputRegionMappingFile = cfg[["outputRegionMappingFile"]] %||% "country",
       path = path, dir = confDir)
}
# The sweep options (runPFMSweep arguments, design note 0005 D7) in their R types. Accepts the
# config form and the manifest form (lists, "off" for a disabled gate).
#' @keywords internal
.pfmSweepOptionsNormalise <- function(o) {
  keys <- c("apTransforms", "dropCompositeAP", "apExtrapolationGate", "apExtrapolationSd",
            "apExtrapolationWindow")
  unknown <- setdiff(names(o), keys)
  if (length(unknown)) {
    stop("sweep options: unknown key(s) ", paste(unknown, collapse = ", "), "; known: ",
         paste(keys, collapse = ", "), call. = FALSE)
  }
  if (!is.null(o$apTransforms)) {
    o$apTransforms <- match.arg(as.character(unlist(o$apTransforms)),
                                c("linear", "saturating", "saturating-innovator", "saturating-incumbent"),
                                several.ok = TRUE)
  }
  if (!is.null(o$dropCompositeAP)) o$dropCompositeAP <- isTRUE(as.logical(o$dropCompositeAP))
  if (!is.null(o$apExtrapolationGate)) {
    g <- o$apExtrapolationGate
    o$apExtrapolationGate <- if (identical(g, "off")) Inf else as.numeric(g)
  }
  if (!is.null(o$apExtrapolationSd)) o$apExtrapolationSd <- as.numeric(o$apExtrapolationSd)
  if (!is.null(o$apExtrapolationWindow)) {
    o$apExtrapolationWindow <- as.numeric(unlist(o$apExtrapolationWindow))
  }
  o
}

# A group's sweep options: the manifest record > none for a group swept before the record
# (runPFMSweep's defaults, the v5 grid) > config.yml sweep: > none. As .pfmPanelDefForGroup.
#' @keywords internal
.pfmSweepOptionsForGroup <- function(groupDir, configSweep = NULL) {
  mf <- file.path(groupDir, "manifest.json")
  man <- if (file.exists(mf)) tryCatch(jsonlite::fromJSON(mf, simplifyVector = FALSE),
                                       error = function(e) NULL) else NULL
  if (!is.null(man[["sweepOptions"]])) {
    return(structure(.pfmSweepOptionsNormalise(man[["sweepOptions"]]), source = "manifest"))
  }
  if (!is.null(man[["panel_hash"]])) {
    return(structure(list(), source = "legacy (swept before the record): the v5 grid"))
  }
  if (!is.null(configSweep)) return(structure(configSweep, source = "config"))
  structure(list(), source = "defaults: the v5 grid")
}

# One line for the plan.
#' @keywords internal
.pfmSweepOptionsLabel <- function(o) {
  tf <- o$apTransforms %||% c("linear", "saturating")
  gate <- o$apExtrapolationGate %||% Inf
  paste0("actor power ", paste(tf, collapse = "/"),
         if (isTRUE(o$dropCompositeAP)) ", no composite AP" else "",
         if (is.finite(gate)) {
           paste0(", AP-extrapolation gate ", gate, " (> ", o$apExtrapolationSd %||% 1, " SD, ",
                  paste(o$apExtrapolationWindow %||% c(2025, 2100), collapse = "-"), ")")
         } else ", no AP-extrapolation gate")
}
# nolint end
