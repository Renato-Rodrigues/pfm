# nolint start
#' Check everything a coupled batch depends on, before submitting it
#'
#' @description
#' One call before any submission (design note 0005 F2). It checks what `PITFALLS.md`
#' sections 1-3 and 23 otherwise leave to memory, and refuses to proceed on any failure:
#' \enumerate{
#'   \item \strong{repositories}: every repository of the project present on this machine
#'     (the project itself and \code{tools/repos.txt}) is clean and has nothing unpushed. The
#'     cluster installs from the git remote, so an unpushed fix does not run (section 2);
#'   \item \strong{installed code}: the \code{pfm} and \code{mrpfm} that REMIND loads, seen from
#'     inside each REMIND checkout, are the code of the working tree. Compared by a
#'     fingerprint of every function, not by version number, which no longer moves with every
#'     change (section 23);
#'   \item \strong{Run-Groups}: every Run-Group a coupled row of the start group names exists
#'     in the REMIND inputs folder with everything \code{preparePFM.R} copies (spec file,
#'     manifest, frontier, temporal validation, donor bands, the panel named by the manifest);
#'   \item \strong{mappings}: every region mapping bundled with \code{mrpfm} resolves, through
#'     the resolver the model uses, to the same content; and where a REMIND checkout carries the
#'     mapping in its \code{config/} (H12, EU21), to the regions REMIND solves on (section 1);
#'   \item \strong{SSP}: each coupled row's SSP (\code{cm_GDPpopScen}) equals its reference
#'     run's (\code{path_gdx_ref}), as D9 requires; the anchor donor joins this check with the
#'     anchor artifact (F6);
#'   \item \strong{replay}: \code{\link{pfmReplayInterface}} and its negative control pass in
#'     each REMIND checkout.
#' }
#'
#' @param config Path to \code{config.yml}; its folder is the project root.
#' @param startGroup The scenario config's start group to be submitted (\code{start} column,
#'   as REMIND's \code{startgroup}). \code{NULL} checks every coupled row.
#' @param scenarioConfig The scenario config CSV. Default
#'   \code{config/scenario_config_PFM.csv} of the first REMIND checkout.
#' @param remindDirs REMIND checkouts to check. Default: those of \code{tools/repos.txt}
#'   (\code{remind_pfm*}) present on this machine.
#' @param remindInputs The REMIND inputs folder (one subfolder per Run-Group). Default
#'   \code{output/remind-inputs} under the project root.
#' @param checks Which checks to run; default all.
#' @param fetch Logical. \code{git fetch} each repository first, so "pushed" and "up to date"
#'   are judged against the remote as it is now. Default \code{TRUE}: without it a checkout that
#'   was never updated passes as "clean and pushed" against its stale view of the remote (the
#'   cluster, 2026-10-02). A fetch that fails is a failed check.
#' @param stopOnFail Logical. Raise an error when any check fails. Default \code{TRUE}.
#' @param verbose Logical.
#' @return Invisibly, a data frame with one row per check: \code{check}, \code{target},
#'   \code{ok}, \code{detail}.
#' @seealso \code{\link{submitPFM}}
#' @export
#' @author Renato Rodrigues
pfmPreflight <- function(config = "config.yml", startGroup = NULL, scenarioConfig = NULL,
                         remindDirs = NULL, remindInputs = NULL,
                         checks = c("repos", "installed", "groups", "mappings", "ssp", "replay"),
                         fetch = TRUE, stopOnFail = TRUE, verbose = TRUE) {
  checks <- match.arg(checks, several.ok = TRUE)
  say <- function(...) if (isTRUE(verbose)) message("[preflight] ", ...)
  root <- normalizePath(dirname(config), winslash = "/", mustWork = TRUE)
  repos <- .pfmProjectRepos(root)
  remindDirs <- remindDirs %||% repos$path[grepl("^remind_pfm", repos$name)]
  remindInputs <- remindInputs %||% file.path(root, "output", "remind-inputs")
  scenarioConfig <- scenarioConfig %||%
    (if (length(remindDirs)) file.path(remindDirs[1], "config", "scenario_config_PFM.csv") else NA_character_)
  res <- list()
  add <- function(check, target, ok, detail = "") {
    res[[length(res) + 1L]] <<- data.frame(check = check, target = target, ok = isTRUE(ok),
                                           detail = detail, stringsAsFactors = FALSE)
    say(if (isTRUE(ok)) "ok    " else "FAIL  ", check, " | ", target, if (nzchar(detail)) paste0(" | ", detail))
  }

  if ("repos" %in% checks) {
    for (i in seq_len(nrow(repos))) {
      r <- .pfmGitState(repos$path[i], fetch = fetch)
      add("repos", repos$name[i], r$ok, r$detail)
    }
  }
  if ("installed" %in% checks) {
    for (pkg in c("pfm", "mrpfm")) {
      src <- file.path(root, "models", pkg)
      fpSrc <- .pfmCodeFingerprintOf(pkg, source = src)
      for (rd in remindDirs) {
        fpInst <- .pfmCodeFingerprintOf(pkg, wd = rd)
        ok <- !is.na(fpSrc$hash) && identical(fpSrc$hash, fpInst$hash)
        detail <- if (ok) {
          paste0("matches the working tree (", fpInst$version, ")")
        } else if (is.na(fpInst$hash)) {
          paste0("not loadable from ", basename(rd), "'s library: ", fpInst$error)
        } else if (is.na(fpSrc$hash)) {
          paste0("the working tree could not be loaded: ", fpSrc$error)
        } else {
          paste0("installed ", fpInst$version, " is not the working tree's code (", fpSrc$version,
                 "): reinstall from the pushed commit")
        }
        add("installed", paste0(pkg, " @ ", basename(rd)), ok, detail)
      }
    }
    # A coupled run loads pfm from ITS OWN renv, built in the run folder from cfg$UseThisRenvLock when
    # that is set (REMIND 3.7.1 pins it to renv/archive/3.7.1_renv.lock), else from a snapshot of the
    # checkout's renv. A pinned lockfile without pfm gives "there is no package called 'pfm'" at the
    # first coupling call, after hours of queueing (2026-10-07) - and every check above passes.
    for (rd in remindDirs) {
      lock <- .pfmRunRenvLock(rd)
      ok <- is.null(lock$path) || all(c("pfm", "mrpfm") %in% lock$packages)
      add("installed", paste0("run renv @ ", basename(rd)), ok,
          if (is.null(lock$path)) "runs snapshot the checkout's renv (cfg$UseThisRenvLock = NULL)" else if (ok)
            paste0("pinned lockfile ", lock$path, " lists pfm and mrpfm") else
            paste0("cfg$UseThisRenvLock = '", lock$path, "' pins the run renv to a lockfile without ",
                   paste(setdiff(c("pfm", "mrpfm"), lock$packages), collapse = " and "),
                   ": a coupled run fails at its first PFM call. Set it to NULL in config/default.cfg"))
    }
  }
  rows <- NULL
  if (any(c("groups", "ssp") %in% checks)) {
    rows <- tryCatch(.pfmCoupledRows(scenarioConfig, startGroup, remindDirs[1]),
                     error = function(e) { add("groups", basename(scenarioConfig %||% "?"), FALSE, conditionMessage(e)); NULL })
    if (!is.null(rows) && !nrow(rows)) {
      add("groups", basename(scenarioConfig), FALSE,
          paste0("no coupled row (cm_taxCO2_regiDiff = 11) in start group '", startGroup %||% "*", "'"))
    }
  }
  if ("groups" %in% checks && length(rows) && nrow(rows)) {
    for (g in unique(rows$pfmGroup)) {
      miss <- .pfmGroupExportMissing(file.path(remindInputs, g))
      add("groups", g, !length(miss),
          if (length(miss)) paste("missing:", paste(miss, collapse = ", ")) else
            paste0(sum(rows$pfmGroup == g), " row(s)"))
      # The share path switch must match the group's formulation (0005 Phase 3): a v6 group (it
      # ships phi-anchor.rds) needs cm_pfmPhiPath = 1, or mode 1 and the rule-C rebuild run on the
      # t0 share alone; a v5 group must not set it, or GAMS waits for a path that never comes.
      if (!length(miss) && "phiPath" %in% names(rows)) {
        v6 <- file.exists(file.path(remindInputs, g, "phi-anchor.rds"))
        rg <- rows[rows$pfmGroup == g, , drop = FALSE]
        bad <- if (v6) rg$title[rg$phiPath != "1"] else rg$title[rg$phiPath == "1"]
        add("groups", paste0(g, " cm_pfmPhiPath"), !length(bad),
            if (length(bad)) paste0(if (v6) "v6 group, cm_pfmPhiPath must be 1 on: " else "v5 group, cm_pfmPhiPath must be 0 on: ",
                                    paste(bad, collapse = ", ")) else
              paste0(if (v6) "v6 (share path)" else "v5 (one share)", " on all ", nrow(rg), " row(s)"))
      }
    }
  }
  if ("mappings" %in% checks) {
    for (m in .pfmMappingMismatches(remindDirs)) add("mappings", m$name, m$ok, m$detail)
  }
  if ("ssp" %in% checks && length(rows) && nrow(rows)) {
    for (i in seq_len(nrow(rows))) {
      ok <- is.na(rows$refSsp[i]) || identical(rows$ssp[i], rows$refSsp[i])
      add("ssp", rows$title[i], ok && !is.na(rows$refSsp[i]),
          if (is.na(rows$refSsp[i])) paste0("reference '", rows$ref[i], "' not found in the scenario configs") else
            paste0(rows$ssp[i], " vs reference ", rows$ref[i], " ", rows$refSsp[i]))
    }
  }
  if ("replay" %in% checks) {
    for (rd in remindDirs) {
      r <- tryCatch(suppressMessages(pfmReplayInterface(remindDir = rd, quiet = TRUE)),
                    error = function(e) list(ok = FALSE, err = conditionMessage(e)))
      add("replay", basename(rd), isTRUE(r$ok) && is.null(r$skipped),
          r$err %||% (if (!is.null(r$skipped)) paste("skipped:", r$skipped) else "positive and negative control pass"))
    }
  }

  out <- do.call(rbind, res)
  nFail <- sum(!out$ok)
  say(if (nFail) paste0(nFail, " of ", nrow(out), " check(s) FAILED") else paste0("all ", nrow(out), " checks pass"))
  if (nFail && isTRUE(stopOnFail)) {
    stop("pfmPreflight: ", nFail, " check(s) failed: ",
         paste(unique(paste0(out$check[!out$ok], " (", out$target[!out$ok], ")")), collapse = "; "),
         call. = FALSE)
  }
  invisible(out)
}

# The project's repositories present here: the project itself plus tools/repos.txt.
.pfmProjectRepos <- function(root) {
  out <- data.frame(name = "pfm-workspace", path = root, stringsAsFactors = FALSE)
  f <- file.path(root, "tools", "repos.txt")
  if (file.exists(f)) {
    l <- trimws(readLines(f, warn = FALSE))
    l <- l[nzchar(l) & !startsWith(l, "#")]
    parts <- strsplit(l, "[[:space:]]+")
    rr <- data.frame(name = vapply(parts, `[`, "", 1), path = file.path(root, vapply(parts, `[`, "", 3)),
                     url = vapply(parts, `[`, "", 4), stringsAsFactors = FALSE)
    rr <- rr[dir.exists(file.path(rr$path, ".git")) & rr$url != "-", c("name", "path")]
    out <- rbind(out, rr)
  }
  out
}

# Clean and pushed? Untracked files count: a new source file not committed does not run either.
.pfmGitState <- function(path, fetch = TRUE) {
  git <- function(...) suppressWarnings(system2("git", c("-C", shQuote(path), ...), stdout = TRUE, stderr = TRUE))
  st <- git("status", "--porcelain")
  if (!is.null(attr(st, "status"))) return(list(ok = FALSE, detail = paste(st, collapse = " ")))
  up <- git("rev-parse", "--abbrev-ref", "@{u}")
  if (!is.null(attr(up, "status"))) return(list(ok = FALSE, detail = "no upstream branch"))
  if (isTRUE(fetch)) {
    f <- git("fetch", "--quiet")
    if (!is.null(attr(f, "status"))) {
      return(list(ok = FALSE, detail = paste("git fetch failed:", paste(f, collapse = " "))))
    }
  }
  cnt <- git("rev-list", "--left-right", "--count", "@{u}...HEAD")
  n <- as.integer(strsplit(trimws(cnt[1]), "[[:space:]]+")[[1]])
  branch <- git("rev-parse", "--abbrev-ref", "HEAD")[1]
  problems <- c(if (length(st)) paste0(length(st), " uncommitted/untracked file(s)"),
                if (n[2] > 0) paste0(n[2], " commit(s) not pushed"),
                if (n[1] > 0) paste0(n[1], " commit(s) behind the remote - update (setup.sh --update)"))
  list(ok = !length(problems),
       detail = if (length(problems)) paste(problems, collapse = "; ") else
         paste0(branch, ", clean, pushed and up to date", if (!isTRUE(fetch)) " (as of the last fetch)" else ""))
}

# A fingerprint of a package's code: every function's formals and body, deparsed without source
# references, in name order. Computed in a separate R process, either from the library seen from
# `wd` (a REMIND checkout loads its own library) or from a source tree via pkgload.
.pfmCodeFingerprintOf <- function(pkg, wd = NULL, source = NULL) {
  load <- if (!is.null(source)) {
    sprintf("suppressMessages(pkgload::load_all(%s, quiet = TRUE, export_all = FALSE, helpers = FALSE, attach_testthat = FALSE))",
            deparse(normalizePath(source, winslash = "/")))
  } else sprintf("suppressMessages(loadNamespace(%s))", deparse(pkg))
  code <- paste0(load, "; ns <- asNamespace(", deparse(pkg), "); ",
                 "fs <- sort(ls(ns, all.names = TRUE)); ",
                 "fs <- fs[vapply(fs, function(f) is.function(get(f, ns)), logical(1))]; ",
                 "ctl <- c('keepNA', 'keepInteger'); ",
                 "h <- digest::digest(lapply(fs, function(f) { g <- get(f, ns); ",
                 "list(f, deparse(formals(g), control = ctl), deparse(body(g), control = ctl)) })); ",
                 "cat('FP', h, as.character(utils::packageVersion(", deparse(pkg), ")), '\\n')")
  rs <- file.path(R.home("bin"), "Rscript")
  owd <- if (!is.null(wd)) setwd(wd) else NULL
  if (!is.null(owd)) on.exit(setwd(owd), add = TRUE)
  out <- suppressWarnings(system2(rs, c("-e", shQuote(code)), stdout = TRUE, stderr = TRUE))
  hit <- grep("^FP ", out, value = TRUE)
  if (!length(hit)) {
    err <- grep("^Error", out, value = TRUE)
    return(list(hash = NA_character_, version = NA_character_,
                error = if (length(err)) trimws(err[1]) else "no fingerprint returned"))
  }
  p <- strsplit(trimws(hit[length(hit)]), " ")[[1]]
  list(hash = p[2], version = p[3])
}

# The coupled rows (cm_taxCO2_regiDiff = 11) of a start group, with their Run-Group, SSP and the
# SSP of their reference run. copyConfigFrom is followed one level, as far as these columns go.
.pfmCoupledRows <- function(scenarioConfig, startGroup = NULL, remindDir = NULL) {
  if (is.na(scenarioConfig) || !file.exists(scenarioConfig)) {
    stop("scenario config not found: ", scenarioConfig, call. = FALSE)
  }
  rd <- function(f) utils::read.csv2(f, check.names = FALSE, stringsAsFactors = FALSE,
                                     comment.char = "#", na.strings = "")
  sc <- rd(scenarioConfig)
  main <- if (!is.null(remindDir) && file.exists(file.path(remindDir, "config", "scenario_config.csv"))) {
    rd(file.path(remindDir, "config", "scenario_config.csv"))
  } else sc[0, , drop = FALSE]
  fill <- function(df, col) {
    v <- if (col %in% names(df)) as.character(df[[col]]) else rep(NA_character_, nrow(df))
    if ("copyConfigFrom" %in% names(df)) {
      from <- match(df$copyConfigFrom, df$title)
      take <- is.na(v) & !is.na(from)
      v[take] <- if (col %in% names(df)) as.character(df[[col]][from[take]]) else NA_character_
    }
    v
  }
  defaultSsp <- .pfmRemindDefault(remindDir, "cm_GDPpopScen") %||% "SSP2"
  ssp <- function(df) { v <- sub("^gdp_", "", fill(df, "cm_GDPpopScen")); v[is.na(v)] <- defaultSsp; v }
  start <- as.character(sc$start %||% NA)
  inGroup <- if (is.null(startGroup) || identical(startGroup, "*")) !is.na(start) & start != "0" else
    grepl(paste0("(^|,)", startGroup, "($|,)"), start, perl = TRUE)
  coupled <- fill(sc, "cm_taxCO2_regiDiff") %in% "11"
  keep <- which(inGroup & coupled)
  pool <- rbind(data.frame(title = sc$title, ssp = ssp(sc), stringsAsFactors = FALSE),
                data.frame(title = main$title %||% character(0), ssp = if (nrow(main)) ssp(main) else character(0),
                           stringsAsFactors = FALSE))
  ref <- fill(sc, "path_gdx_ref")[keep]
  grp <- fill(sc, "pfmGroup")[keep]
  pp <- fill(sc, "cm_pfmPhiPath")[keep]
  pp[is.na(pp)] <- "0"   # main.gms default
  data.frame(title = sc$title[keep], pfmGroup = ifelse(is.na(grp), "(default)", grp),
             ssp = ssp(sc)[keep], ref = ref, refSsp = pool$ssp[match(ref, pool$title)],
             phiPath = trimws(pp), stringsAsFactors = FALSE)
}

# A $setglobal default from REMIND's main.gms.
# The lockfile a REMIND checkout's runs restore their renv from (cfg$UseThisRenvLock in
# config/default.cfg): list(path = NULL) when runs snapshot the checkout's renv, else the path and the
# packages it lists (character(0) when the file cannot be read).
.pfmRunRenvLock <- function(remindDir) {
  f <- file.path(remindDir, "config", "default.cfg")
  if (!file.exists(f)) return(list(path = NULL, packages = character(0)))
  l <- grep("^\\s*cfg\\$UseThisRenvLock\\s*<-", readLines(f, warn = FALSE), value = TRUE)
  if (!length(l)) return(list(path = NULL, packages = character(0)))
  v <- trimws(sub("#.*$", "", sub("^\\s*cfg\\$UseThisRenvLock\\s*<-", "", l[length(l)])))
  if (identical(v, "NULL") || !nzchar(v)) return(list(path = NULL, packages = character(0)))
  path <- gsub("^[\"']|[\"']$", "", v)
  lf <- if (grepl("^(/|[A-Za-z]:)", path)) path else file.path(remindDir, path)
  pk <- tryCatch(names(jsonlite::read_json(lf)$Packages), error = function(e) character(0))
  list(path = path, packages = pk %||% character(0))
}

.pfmRemindDefault <- function(remindDir, switch) {
  f <- if (!is.null(remindDir)) file.path(remindDir, "main.gms") else ""
  if (!file.exists(f)) return(NULL)
  l <- grep(paste0("^\\$setglobal +", switch, " "), readLines(f, warn = FALSE), value = TRUE)
  if (!length(l)) return(NULL)
  strsplit(trimws(sub(paste0("^\\$setglobal +", switch, " +"), "", l[1])), "[[:space:]]")[[1]][1]
}

# What preparePFM.R copies from a Run-Group's REMIND export and what is absent from it.
.pfmGroupExportMissing <- function(gd) {
  if (!dir.exists(gd)) return("the Run-Group folder")
  need <- c("manifest.json", "frontier.rds", "temporal-validation.rds",
            "donor-assignment-band-Bulk.rds", "donor-assignment-band-Diffuse.rds")
  miss <- need[!file.exists(file.path(gd, need))]
  if (!file.exists(.pfmSelectedModels(gd))) miss <- c(miss, "selected-models-pfm.yml")
  mf <- file.path(gd, "manifest.json")
  if (file.exists(mf)) {
    h <- tryCatch(jsonlite::read_json(mf)$panel_hash, error = function(e) NULL)
    p <- paste0("panel_", h, ".rds")
    if (is.null(h) || !any(file.exists(file.path(gd, c(p, file.path("panels", p)))))) {
      miss <- c(miss, paste0("the panel ", p))
    }
    # a group whose manifest records a completed pfm-anchor step must ship the artifact (0005 F2)
    anchored <- tryCatch(identical(jsonlite::read_json(mf)$steps[["pfm-anchor"]]$status, "completed"),
                         error = function(e) FALSE)
    if (anchored && !file.exists(file.path(gd, "phi-anchor.rds"))) miss <- c(miss, "phi-anchor.rds")
  }
  miss
}

# Every region mapping bundled with mrpfm, resolved the way the model resolves it, against the
# bundled copy and - where a REMIND checkout carries it in config/ - against REMIND's own copy,
# the regions REMIND solves on (decided 2026-10-02: the PFM follows REMIND's regions).
.pfmMappingMismatches <- function(remindDirs = character(0)) {
  dir <- system.file("extdata", "regional", package = "mrpfm")
  files <- list.files(dir, pattern = "^regionmapping.*[.]csv$")
  key <- function(m) {
    if (is.null(m) || !all(c("CountryCode", "RegionCode") %in% names(m))) return(NULL)
    m <- m[order(m$CountryCode), c("CountryCode", "RegionCode")]
    paste(m$CountryCode, m$RegionCode)
  }
  rd <- function(f) utils::read.csv(f, sep = ";", stringsAsFactors = FALSE)
  lapply(files, function(f) {
    got <- tryCatch(suppressMessages(mrpfm::toolPFMMapping(f, type = "regional", verbose = FALSE)),
                    error = function(e) NULL)
    bad <- character(0)
    remindChecked <- FALSE
    if (!identical(key(rd(file.path(dir, f))), key(got))) bad <- c(bad, "mrpfm's copy")
    for (rd0 in remindDirs) {
      rf <- file.path(rd0, "config", f)
      if (!file.exists(rf)) next
      remindChecked <- TRUE
      if (!identical(key(rd(rf)), key(got))) bad <- c(bad, paste0(basename(rd0), "/config"))
    }
    list(name = f, ok = !is.null(got) && !length(bad),
         detail = if (is.null(got)) "does not resolve" else if (length(bad)) {
           paste0("the resolved mapping differs from ", paste(bad, collapse = " and "),
                  " (PITFALLS section 1; tools/compareMappings.R lists the countries)")
         } else if (remindChecked) "resolves to mrpfm's copy and to REMIND's own regions" else
           "resolves to mrpfm's copy (not in REMIND's config/)")
  })
}
# nolint end
