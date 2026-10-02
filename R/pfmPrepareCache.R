# nolint start
#' Prepare the project's madrat cache: check it, fill it from other caches, compute the rest
#'
#' @description
#' Makes the project cache (config \code{madrat: cachefolder}) hold every madrat cache file
#' the pipeline reads, so that the historical panel, the estimation, the projection and the
#' coupling all read the SAME data versions from ONE folder. Called by
#' \code{tools/setup.sh} after installing, and by \code{\link{pfmRun}} before any step.
#'
#' \enumerate{
#'   \item \strong{Quick check.} When the folder's \code{cache-manifest.tsv} was written for
#'     the same request (resolution, years, coupling weights, pin, pfm version) and every file
#'     it lists is present and unchanged, nothing else happens. This is the normal case and
#'     takes well under a second.
#'   \item \strong{Pin.} A Run-Group that recorded what it read
#'     (\code{<recordsDir>/<group>/madrat-cache-used-pfm.tsv}, from
#'     \code{tools/listMadratCacheUsed.R}) gets EXACTLY those files, copied by
#'     name from the cache sources. This is what reproduces an existing group.
#'   \item \strong{Builders.} The pipeline's own data builders run against the project cache:
#'     the historical panel (two- and four-sector policy stringency), the scenario panel (on
#'     the first registry gdx present - its madrat calls do not depend on the gdx) and the
#'     coupling weights. Every madrat cache lookup they make is watched. On a miss, the cache
#'     sources are searched in order (config \code{cacheSources}, then madrat's own
#'     cachefolder) with madrat's own rule, and the file it would have read there is copied
#'     in. What no cache has is computed from the raw sources (config \code{sourcefolder})
#'     when \code{compute = TRUE}, and is an error otherwise.
#'   \item \strong{Manifest.} The files the builders read are written to
#'     \code{cache-manifest.tsv} in the cache folder, with size, md5, where each came from and
#'     which builder needs it. The REMIND export stages the coupling's subset from it.
#' }
#'
#' Why copy-on-miss and not "copy everything": under \code{forcecache} madrat accepts any file
#' whose argument hash matches and, among several, reads the NEWEST BY FILE TIME. A cache
#' holding several versions of one calculation therefore delivers whichever was touched
#' last, silently (\code{docs/PITFALLS.md}). A cache built from exactly what was read holds
#' one version of each, and says which.
#'
#' @param config Path to \code{config.yml}. \code{NULL}: \code{config.yml} in the working
#'   directory.
#' @param group Run-Group. Names the cache when its path carries \code{{group}}, and selects
#'   the pin. \code{NULL}: the config's \code{group}.
#' @param compute Logical. Compute what no cache has. \code{NULL}: config
#'   \code{madrat: compute} (default \code{TRUE}). \code{FALSE} is a check: a missing file
#'   stops the builder that needs it, and the result says which.
#' @param force Re-run the builders even when the quick check passes.
#' @param verify \code{"size"} (default) or \code{"md5"}: how the quick check compares files.
#' @param verbose Logical.
#'
#' @return Invisibly, a list: \code{status} (\code{"ready"}, \code{"prepared"} or
#'   \code{"incomplete"}), \code{cachefolder}, \code{tag}, \code{manifest} (data frame) and
#'   \code{failed} (named character: builder -> error).
#' @seealso \code{\link{pfmResolveConfig}} for the \code{madrat:} config block.
#' @author Renato Rodrigues
#' @export
pfmPrepareCache <- function(config = NULL, group = NULL, compute = NULL, force = FALSE,
                            verify = c("size", "md5"), verbose = TRUE) {
  verify <- match.arg(verify)
  say <- function(...) if (isTRUE(verbose)) message("[cache] ", ...)
  rc <- pfmResolveConfig(config, group = group, verbose = FALSE)
  m <- rc$madrat
  group <- rc$group
  cf <- m$cachefolder
  compute <- compute %||% m$compute
  rootPath <- function(p) if (is.null(p) || grepl("^([A-Za-z]:|/|\\\\)", p)) p else file.path(rc$dir, p)

  # madrat's own configuration, read BEFORE anything here changes it.
  old <- madrat::getConfig(verbose = FALSE)
  on.exit(suppressMessages(madrat::setConfig(cachefolder = old$cachefolder,
                                             sourcefolder = old$sourcefolder,
                                             forcecache = old$forcecache, .verbose = FALSE)),
          add = TRUE)
  sources <- m$cacheSources
  # An unconfigured madrat caches under tempdir(): nothing to copy from there.
  if (isTRUE(m$useMadratConfig) && length(old$cachefolder) && dir.exists(old$cachefolder) &&
      !startsWith(normalizePath(old$cachefolder, winslash = "/"), normalizePath(tempdir(), winslash = "/")) &&
      !identical(normalizePath(old$cachefolder, winslash = "/"),
                 normalizePath(cf, winslash = "/", mustWork = FALSE))) {
    sources <- unique(c(sources, normalizePath(old$cachefolder, winslash = "/")))
  }
  sourcefolder <- m$sourcefolder %||% (if (isTRUE(m$useMadratConfig)) old$sourcefolder)

  # The pin and the record live in the TRACKED records folder (config `recordsDir`), not in
  # output/: a fresh clone must be able to rebuild a group's exact cache.
  recordDir <- file.path(rc$recordsDir %||% file.path(rc$dir, "records"), group %||% "")
  pinFile <- file.path(recordDir, "madrat-cache-used-pfm.tsv")
  pin <- if (!is.null(group) && file.exists(pinFile)) .readCacheTable(pinFile)$file else NULL
  gdxs <- Filter(function(s) !is.null(s$gdx) && file.exists(s$gdx), rc$scenarios %||% list())
  spec <- .cacheBuilderSpec()
  builders <- .cacheBuilders(spec, rc$outputRegionMappingFile, gdxs)
  key <- digest::digest(list(spec = spec, res = rc$outputRegionMappingFile,
                             builders = vapply(builders, function(b) b$id, ""),
                             pfm = as.character(utils::packageVersion("pfm")),
                             gdx = vapply(gdxs, function(s) basename(dirname(s$gdx)), ""),
                             pin = if (length(pin)) unname(tools::md5sum(pinFile)) else ""))
  manifestFile <- file.path(cf, "cache-manifest.tsv")

  say("project cache: ", cf, "  [tag ", m$tag, "]")
  # --- 1. quick check -----------------------------------------------------------------------
  man <- if (file.exists(manifestFile)) .readCacheTable(manifestFile) else NULL
  if (!isTRUE(force) && !is.null(man)) {
    bad <- .cacheFilesChanged(cf, man, verify)
    if (identical(attr(man, "key"), key) && !length(bad)) {
      say("ready - all ", nrow(man), " files of the manifest present (", verify, " checked)")
      if (!is.null(group)) .writeCacheRecord(man, recordDir, key, m$tag)
      return(invisible(list(status = "ready", cachefolder = cf, tag = m$tag,
                            manifest = man, failed = character(0))))
    }
    say(if (!identical(attr(man, "key"), key)) "the request changed since the manifest was written"
        else paste0(length(bad), " manifest file(s) missing or changed: ", paste(utils::head(bad, 5), collapse = ", ")),
        " - preparing")
  } else if (is.null(man)) {
    say("no manifest yet - preparing")
  }
  say("copy from    : ", if (length(sources)) paste(sources, collapse = "\n                 ") else "(no other cache)")
  say("raw sources  : ", sourcefolder %||% "(none)", if (isTRUE(compute)) "" else "  [compute = FALSE: check only]")
  dir.create(cf, recursive = TRUE, showWarnings = FALSE)

  # --- 2. pin -------------------------------------------------------------------------------
  log <- list()
  if (length(pin)) {
    lost <- character(0)
    for (f in pin[!file.exists(file.path(cf, pin))]) {
      hit <- file.path(sources, f)[file.exists(file.path(sources, f))][1]
      if (is.na(hit)) { lost <- c(lost, f); next }
      file.copy(hit, file.path(cf, f), copy.date = TRUE)
      log[[length(log) + 1]] <- data.frame(file = f, origin = dirname(hit), builder = "pin")
    }
    say("pin          : ", length(pin), " files from ", pinFile,
        if (length(lost)) paste0(" - ", length(lost), " in NO source: ", paste(lost, collapse = ", ")) else " - all present")
  }

  # --- 3. builders, watched -----------------------------------------------------------------
  # mrpfm registers its calc* functions with madrat only when ATTACHED (.onAttach). Called as
  # pfm::pfmPrepareCache() from a bare Rscript nothing attaches it, and every builder failed
  # with madrat's 'Type "CarbonPrice" is not a valid output type' - found on a fresh clone,
  # 2026-10-01. library(pfm) attaches it (Depends).
  if (!"mrpfm" %in% madrat::getConfig("packages", verbose = FALSE)) {
    stop("pfmPrepareCache: mrpfm is not attached, so madrat cannot find its calculations. ",
         "Call library(pfm) first.", call. = FALSE)
  }
  suppressMessages(madrat::setConfig(cachefolder = cf, forcecache = TRUE, .verbose = FALSE))
  if (!is.null(sourcefolder)) suppressMessages(madrat::setConfig(sourcefolder = sourcefolder, .verbose = FALSE))
  if (!length(gdxs)) say("scenario panel: no registry gdx exists here - its files are not checked")
  runPass <- function(copy) {
    reads <- list(); failed <- character(0)
    for (b in builders) {
      st <- .cacheTraceStart(cf, if (copy) sources else character(0), stopOnMiss = !isTRUE(compute))
      ok <- tryCatch({ suppressWarnings(suppressMessages(b$f())); TRUE },
                     error = function(e) { failed[[b$id]] <<- conditionMessage(e); FALSE })
      ev <- .cacheTraceStop(st)
      if (nrow(ev)) reads[[length(reads) + 1]] <- cbind(ev, builder = b$id)
      say(sprintf("  %-9s %-34s %3d lookups, %d copied, %d computed", if (ok) "ok" else "FAILED",
                  b$label, nrow(ev), sum(ev$origin %in% sources), sum(ev$origin == "computed")))
    }
    list(reads = do.call(rbind, c(reads, list(.emptyTrace()))), failed = failed)
  }
  p1 <- runPass(copy = TRUE)
  # A computed file pulls its inputs through the cache too; those are not needed once the result
  # is cached. A second pass - now all hits - records exactly what the pipeline reads.
  p <- if (any(p1$reads$origin == "computed") && !length(p1$failed)) runPass(copy = FALSE) else p1
  for (id in names(p1$failed)) say("  ", id, ": ", .firstLine(p1$failed[[id]]))

  # --- 4. manifest --------------------------------------------------------------------------
  r <- p$reads[!is.na(p$reads$file), , drop = FALSE]
  prev <- man   # the previous manifest, if any: where a file already present came from
  firstOrigin <- function(f) {
    o <- p1$reads$origin[p1$reads$file == f & p1$reads$origin != "present"]
    o <- c(o, vapply(log, function(x) if (x$file == f) x$origin else NA_character_, ""))
    o <- c(o[!is.na(o)], if (!is.null(prev) && "origin" %in% names(prev)) prev$origin[prev$file == f])
    if (length(o)) o[1] else "present"
  }
  files <- unique(c(r$file, intersect(pin, list.files(cf))))
  man <- data.frame(
    file = files,
    size = unname(file.size(file.path(cf, files))),
    md5 = unname(tools::md5sum(file.path(cf, files))),
    origin = vapply(files, firstOrigin, "", USE.NAMES = FALSE),
    builders = vapply(files, function(f) paste(sort(unique(c(r$builder[r$file == f],
                                                              if (f %in% pin) "pin"))), collapse = ","),
                      "", USE.NAMES = FALSE),
    stringsAsFactors = FALSE)
  man <- man[order(man$file), , drop = FALSE]
  status <- if (length(p1$failed)) "incomplete" else "prepared"
  if (!length(p1$failed)) {
    .writeCacheTable(man, manifestFile, header = c(
      paste0("key: ", key), paste0("tag: ", m$tag), paste0("written: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
      paste0("pfm: ", utils::packageVersion("pfm")), paste0("pin: ", if (length(pin)) pinFile else "none"),
      paste0("sources: ", paste(sources, collapse = " | ")), paste0("rawSources: ", sourcefolder %||% "none")))
    if (!is.null(group)) .writeCacheRecord(man, recordDir, key, m$tag)
  }
  nNew <- function(o) sum(man$origin == o & !(man$file %in% prev$file))
  say(status, ": ", nrow(man), " files the pipeline reads - ",
      sum(vapply(sources, nNew, 0)), " copied now, ", nNew("computed"), " computed now, ",
      sum(man$file %in% prev$file), " already there",
      if (status == "prepared") paste0("; manifest ", manifestFile) else "; NO manifest written")
  invisible(list(status = status, cachefolder = cf, tag = m$tag, manifest = man, failed = p1$failed))
}

# The fixed arguments of the builders. They are what the pipeline calls, and they key the
# manifest: change one and the next run re-prepares.
.cacheBuilderSpec <- function() {
  list(histYears = 2000:2022, includePolicyStringency = TRUE,
       # psmCouplingWeights as iterativePFM calls it under default.cfg: weightYear 2025, SSP2.
       weightYear = 2025, weightScenario = "SSP2")
}

.cacheBuilders <- function(spec, res, gdxs) {
  b <- list(list(id = "historical-panel", label = paste0("historical panel (", res, ")"), f = function()
    panelDataHistorical(aggregate = TRUE, y = spec$histYears, outputRegionMappingFile = res,
                        includePolicyStringency = spec$includePolicyStringency)),
    # psm-sector-speeds builds its own panel at four-sector policy-stringency resolution, a
    # different calcPolicyStringency call (and cache file) from the two-sector one above.
    list(id = "historical-panel-four", label = paste0("historical panel, 4 sectors (", res, ")"), f = function()
      panelDataHistorical(aggregate = TRUE, y = spec$histYears, outputRegionMappingFile = res,
                          includePolicyStringency = TRUE, psSectorResolution = "four")))
  # The scenario panel's madrat calls do not depend on the gdx, so one gdx covers them.
  if (length(gdxs)) {
    g <- gdxs[[1]]
    b <- c(b, list(list(id = "scenario-panel", label = paste0("scenario panel (", g$id %||% basename(g$gdx), ")"),
                        f = function() panelDataScenario(gdxFile = g$gdx, aggregate = TRUE,
                                                         gdxRegionMappingFile = g$gdxRegionMapping %||% "regionmapping_21_EU11.csv",
                                                         outputRegionMappingFile = "country"))))
  }
  c(b, list(list(id = "coupling-weights", label = paste0("coupling weights (", spec$weightYear, ", ", spec$weightScenario, ")"),
                 f = function() psmCouplingWeights(year = spec$weightYear, scenario = spec$weightScenario))))
}

# --- watching madrat's cache lookups -------------------------------------------------------
# madrat has no hook for "the cache missed". cacheGet(prefix, type, args) is the one function
# every calcOutput/readSource lookup goes through, so it is traced: on entry, a miss in the
# project cache is looked up in each source with madrat's own cacheNames() (so the file chosen
# is the one madrat would have read there) and copied in; on exit, what was read - or that
# nothing was, and madrat is about to compute - is recorded. Internal API: if a madrat release
# changes it, the trace is skipped with a warning and the builders still run (no copying).
.cacheTraceState <- new.env(parent = emptyenv())

.emptyTrace <- function() data.frame(file = character(0), origin = character(0), builder = character(0))

.cacheTraceStart <- function(cachefolder, sources, stopOnMiss = FALSE) {
  st <- .cacheTraceState
  st$cachefolder <- cachefolder; st$sources <- sources; st$stopOnMiss <- stopOnMiss
  st$events <- list(); st$pending <- list()
  ns <- asNamespace("madrat")
  okApi <- exists("cacheGet", ns, inherits = FALSE) && exists("cacheNames", ns, inherits = FALSE) &&
    all(c("prefix", "type", "args") %in% names(formals(get("cacheGet", ns)))) &&
    all(c("prefix", "type", "args") %in% names(formals(get("cacheNames", ns))))
  if (!okApi) {
    warning("pfmPrepareCache: madrat ", utils::packageVersion("madrat"), " changed its cache internals; ",
            "lookups are not watched, so nothing is copied and no manifest is complete.", call. = FALSE)
    st$traced <- FALSE
    return(invisible(st))
  }
  suppressMessages(trace("cacheGet", where = ns, print = FALSE,
                         # the functions themselves are spliced into the calls, so the tracer
                         # needs no `:::` and works from a load_all() session too
                         tracer = as.call(list(.cacheTraceEnter, quote(prefix), quote(type), quote(args))),
                         exit = as.call(list(.cacheTraceExit, quote(prefix), quote(type), quote(args),
                                             quote(returnValue())))))
  st$traced <- TRUE
  invisible(st)
}

.cacheTraceStop <- function(st) {
  if (isTRUE(st$traced)) suppressMessages(untrace("cacheGet", where = asNamespace("madrat")))
  ev <- do.call(rbind, c(st$events, list(data.frame(file = character(0), origin = character(0)))))
  st$events <- list(); st$traced <- FALSE
  ev
}

.cacheTraceEnter <- function(prefix, type, args) {
  st <- .cacheTraceState
  cn <- get("cacheNames", asNamespace("madrat"))
  here <- cn(prefix = prefix, type = type, args = args)
  key <- paste(prefix, type, digest::digest(args))
  st$pending[[key]] <- "present"
  if (!is.null(here$read)) return(invisible())
  for (src in st$sources) {
    hit <- local({
      cur <- madrat::getConfig("cachefolder", verbose = FALSE)
      on.exit(suppressMessages(madrat::setConfig(cachefolder = cur, .verbose = FALSE)))
      suppressMessages(madrat::setConfig(cachefolder = src, .verbose = FALSE))
      cn(prefix = prefix, type = type, args = args)$read
    })
    if (!is.null(hit)) {
      file.copy(hit, file.path(st$cachefolder, basename(hit)), copy.date = TRUE)
      st$pending[[key]] <- normalizePath(src, winslash = "/")
      return(invisible())
    }
  }
  st$pending[[key]] <- "computed"
  if (isTRUE(st$stopOnMiss)) {
    stop("no cache has ", prefix, type, " for these arguments, and compute = FALSE (",
         basename(here$write), ")", call. = FALSE)
  }
  invisible()
}

.cacheTraceExit <- function(prefix, type, args, value) {
  st <- .cacheTraceState
  key <- paste(prefix, type, digest::digest(args))
  origin <- st$pending[[key]] %||% "present"
  # On a hit madrat sets attr "readFile"; on a miss only "id", the file it will WRITE.
  f <- attr(value, "readFile") %||% attr(value, "id")
  st$events[[length(st$events) + 1]] <- data.frame(file = basename(f %||% NA_character_), origin = origin)
  invisible()
}

# --- manifest files -------------------------------------------------------------------------
# The tracked copy of the manifest, <recordsDir>/<group>/madrat-cache-manifest.tsv: which input
# files the Run-Group reads, with md5. Portable on purpose - no machine paths, no origins - so
# the cluster and the workstation write the same file and git shows a diff only when the DATA
# changed. Rewritten only when its content would change.
.writeCacheRecord <- function(man, recordDir, key, tag) {
  rec <- man[, intersect(c("file", "size", "md5", "builders"), names(man)), drop = FALSE]
  path <- file.path(recordDir, "madrat-cache-manifest.tsv")
  if (file.exists(path)) {
    old <- tryCatch(.readCacheTable(path), error = function(e) NULL)
    if (!is.null(old) && identical(attr(old, "key"), key) &&
        identical(old$file, rec$file) && identical(old$md5, rec$md5)) return(invisible(path))
  }
  dir.create(recordDir, recursive = TRUE, showWarnings = FALSE)
  .writeCacheTable(rec, path, header = c(
    paste0("key: ", key), paste0("tag: ", tag), paste0("pfm: ", utils::packageVersion("pfm")),
    "The madrat cache files this Run-Group reads (pfm::pfmPrepareCache). Deposit them with the version."))
  invisible(path)
}

.readCacheTable <- function(path) {
  lines <- readLines(path, warn = FALSE)
  hdr <- sub("^#\\s*", "", grep("^#", lines, value = TRUE))
  tab <- utils::read.delim(path, comment.char = "#", stringsAsFactors = FALSE)
  for (h in hdr[grepl("^[A-Za-z]+: ", hdr)]) attr(tab, sub(":.*$", "", h)) <- sub("^[A-Za-z]+: ", "", h)
  tab
}

.writeCacheTable <- function(tab, path, header = character(0)) {
  writeLines(c(paste0("# ", header), paste(names(tab), collapse = "\t")), path)
  utils::write.table(tab, path, sep = "\t", quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
}

.cacheFilesChanged <- function(cachefolder, man, verify = "size") {
  p <- file.path(cachefolder, man$file)
  gone <- !file.exists(p)
  diff <- if (verify == "md5") unname(tools::md5sum(p)) != man$md5 else file.size(p) != man$size
  man$file[gone | (!gone & diff %in% TRUE)]
}

.firstLine <- function(x) substr(strsplit(gsub("\033\\[[0-9;]*m", "", x), "\n")[[1]][1], 1, 240)
# nolint end
