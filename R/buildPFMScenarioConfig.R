# nolint start
#' Build the coupled scenario config from a scenario matrix
#'
#' @description
#' ADR 0055 (design note 0005 F1 / E16, D22). The coupled batch is described by a compact YAML
#' matrix; this function writes the REMIND scenario config from it. Every row is its canonical
#' REMIND parent plus named deltas and nothing else, so a family can never carry another
#' scenario's switches (the PkBudg750 incident of 2026-08). Section separators, start tags and the
#' \code{path_gdx*} chains are generated, not typed. The CSV is a build artifact: edit the matrix,
#' never the output.
#'
#' The matrix (format \code{pfm-scenario-matrix/1}) has these blocks:
#' \describe{
#'   \item{\code{output}, \code{canonical}}{the CSV to write and REMIND's canonical
#'     \code{scenario_config.csv}, both relative to \code{remindDir}.}
#'   \item{\code{version}}{appended as \code{-<version>} to every coupled title.}
#'   \item{\code{ssps}}{the SSPs to build (\code{SSP2}); the canonical parents are looked up as
#'     \code{<ssp>-<from>}.}
#'   \item{\code{thetaDefault}}{the θ that carries no title suffix; any other θ adds
#'     \code{Th<digits>} (0.325 → \code{Th325}).}
#'   \item{\code{resolutions}}{per resolution: \code{infix} (\code{SSP2-<infix>-...}, empty for
#'     none), \code{set} (switches on every row of that resolution), \code{baseStart} (start tags of
#'     its parents and uncoupled references).}
#'   \item{\code{parents}}{the uncoupled parents: \code{from} (canonical stem) and \code{set}.}
#'   \item{\code{coupled}}{switches on every coupled row.}
#'   \item{\code{rules}}{named deltas: \code{stem} (title part) and \code{set}.}
#'   \item{\code{runs}}{the batch, in order. Each entry: \code{key}, \code{wave}, \code{section},
#'     \code{parent}, \code{rule} or \code{stem}, \code{coupled} (default \code{TRUE}),
#'     \code{theta} (a value or a list, which expands), \code{markup} (\code{0} adds \code{Min}),
#'     \code{variant} (title suffix), \code{group} (\code{pfmGroup}), \code{institutions}
#'     (\code{pfmInstitutions}), \code{options} (\code{pfmPhi*} columns), \code{set},
#'     \code{warm} (\code{path_gdx}), \code{res} (resolutions), \code{start} (per-resolution tags
#'     replacing the wave tag \code{V<version>W<wave><res>}), \code{description}.}
#' }
#' A value \code{"@key"} is the title of the run (or parent) \code{key} of the same SSP and
#' resolution; \code{"@key"} of a θ-expanded run is its \code{thetaDefault} row.
#'
#' The generator never writes \code{cm_iteration_max} (the author's rule of 2026-10-01: a run that
#' does not converge is diagnosed or restarted from its gdx, never given a higher cap).
#'
#' @param matrix Path to the matrix YAML.
#' @param remindDir The REMIND checkout holding \code{canonical} and receiving \code{output}.
#' @param out The CSV to write; default the matrix's \code{output} under \code{remindDir}.
#'   \code{NULL} with \code{write = FALSE} builds without writing.
#' @param write Logical. Write the CSV.
#' @param verbose Logical.
#' @return Invisibly, a data frame of the rows (all character, separators included), with
#'   attribute \code{path} when written.
#' @export
#' @author Renato Rodrigues
buildPFMScenarioConfig <- function(matrix, remindDir, out = NULL, write = TRUE, verbose = TRUE) {
  say <- function(...) if (isTRUE(verbose)) message("[scenario-matrix] ", ...)
  m <- yaml::read_yaml(matrix)
  if (!identical(m$format, "pfm-scenario-matrix/1")) {
    stop("buildPFMScenarioConfig: ", matrix, " is not a pfm-scenario-matrix/1 file", call. = FALSE)
  }
  canonPath <- file.path(remindDir, m$canonical %||% "config/scenario_config.csv")
  canon <- .pfmReadScenarioCsv(canonPath)
  rows <- .pfmExpandMatrix(m, canon)
  .pfmCheckGenerated(rows)
  df <- .pfmMatrixFrame(rows, attr(canon, "header"))
  say(sum(!startsWith(df$title, "_")), " scenarios (", sum(nzchar(df$pfmGroup %||% "")), " coupled), ",
      ncol(df), " columns, from ", basename(matrix))
  if (isTRUE(write)) {
    out <- out %||% file.path(remindDir, m$output)
    .pfmWriteScenarioCsv(df, out)
    attr(df, "path") <- out
    say("written: ", out)
  }
  invisible(df)
}

# A REMIND scenario config as a named list of rows (each a named character vector), copyConfigFrom
# resolved. Attribute "header": the column order.
.pfmReadScenarioCsv <- function(path) {
  if (!file.exists(path)) stop("scenario config not found: ", path, call. = FALSE)
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  lines <- sub("^﻿", "", lines)
  lines <- lines[nzchar(trimws(lines)) & !startsWith(lines, "#")]
  split <- function(x) { f <- strsplit(paste0(x, ";\u0001"), ";", fixed = TRUE)[[1]]; f[-length(f)] }
  hdr <- split(lines[1])
  recs <- lapply(lines[-1], function(l) { f <- split(l); length(f) <- length(hdr); f[is.na(f)] <- ""
                                          stats::setNames(trimws(f), hdr) })
  names(recs) <- vapply(recs, `[[`, "", "title")
  if ("copyConfigFrom" %in% hdr) {
    for (t in names(recs)) {
      from <- recs[[t]][["copyConfigFrom"]]; seen <- t
      while (nzchar(from)) {
        if (!from %in% names(recs) || from %in% seen) stop("copyConfigFrom chain broken at ", t, call. = FALSE)
        src <- recs[[from]]; empty <- !nzchar(recs[[t]]) & names(recs[[t]]) != "copyConfigFrom"
        recs[[t]][empty] <- src[empty]; seen <- c(seen, from); from <- src[["copyConfigFrom"]]
      }
      recs[[t]][["copyConfigFrom"]] <- ""
    }
  }
  attr(recs, "header") <- setdiff(hdr, "copyConfigFrom")
  recs
}

.pfmThetaLabel <- function(theta, default) {
  if (identical(as.numeric(theta), as.numeric(default))) "" else paste0("Th", sub("^0?\\.", "", theta))
}

# Expand the matrix to an ordered list of rows. Each row: list(key, title, section, res, ssp,
# values = named character, coupled, separator).
.pfmExpandMatrix <- function(m, canon) {
  chr <- function(x) if (is.null(x)) NULL else stats::setNames(vapply(x, function(v) {
    if (is.logical(v)) (if (isTRUE(v)) "on" else "off") else as.character(v) }, ""), names(x))
  version <- as.character(m$version %||% "")
  thetaDefault <- as.character(m$thetaDefault %||% "0.50")
  out <- list()
  for (ssp in unlist(m$ssps %||% "SSP2")) for (resName in names(m$resolutions)) {
    res <- m$resolutions[[resName]]
    prefix <- paste(c(ssp, if (nzchar(res$infix %||% "")) res$infix), collapse = "-")
    resSet <- chr(res$set)
    titles <- list()     # key -> title, within this ssp x resolution
    block <- list()
    # 1. the uncoupled parents
    for (pk in names(m$parents)) {
      p <- m$parents[[pk]]
      src <- paste0(ssp, "-", p$from)
      if (!src %in% names(canon)) stop("canonical parent ", src, " not in the canonical scenario config", call. = FALSE)
      vals <- canon[[src]]
      vals[names(resSet)] <- resSet
      vals[names(chr(p$set))] <- chr(p$set)
      title <- paste0(prefix, "-", p$from)
      titles[[pk]] <- title
      block[[length(block) + 1]] <- list(key = pk, title = title, section = "parents", values = vals,
                                         start = unlist(res$baseStart), coupled = FALSE,
                                         description = paste0("Uncoupled parent: REMIND's ", src,
                                           if (length(resSet)) paste0(" at ", resName) else "",
                                           ", plus the matrix's declared settings (", paste(names(chr(p$set)), collapse = ", "),
                                           "). Generated by pfm::buildPFMScenarioConfig - edit the matrix, not this row."),
                                         parentKey = NA_character_)
    }
    # 2. the runs
    for (r in m$runs) {
      if (!resName %in% unlist(r$res %||% names(m$resolutions))) next
      thetas <- as.character(unlist(r$theta %||% list(NULL)))
      if (!length(thetas)) thetas <- NA_character_
      coupled <- !isFALSE(r$coupled)
      rule <- if (!is.null(r$rule)) m$rules[[r$rule]] else NULL
      if (!is.null(r$rule) && is.null(rule)) stop("run ", r$key, ": unknown rule ", r$rule, call. = FALSE)
      for (th in thetas) {
        if (is.null(r$parent) || !r$parent %in% names(m$parents)) {
          stop("run ", r$key, ": parent must be one of ", paste(names(m$parents), collapse = ", "), call. = FALSE)
        }
        vals <- canon[[paste0(ssp, "-", m$parents[[r$parent]]$from)]]
        vals[names(resSet)] <- resSet
        vals[names(chr(m$parents[[r$parent]]$set))] <- chr(m$parents[[r$parent]]$set)
        if (coupled) vals[names(chr(m$coupled))] <- chr(m$coupled)
        if (!is.null(rule)) vals[names(chr(rule$set))] <- chr(rule$set)
        if (!is.na(th)) vals[["cm_pfmTheta"]] <- th
        if (!is.null(r$markup)) vals[["cm_pfmSectorMarkup"]] <- as.character(r$markup)
        if (!is.null(r$group)) vals[["pfmGroup"]] <- as.character(r$group)
        if (!is.null(r$institutions)) vals[["pfmInstitutions"]] <- as.character(r$institutions)
        if (!is.null(r$options)) vals[names(chr(r$options))] <- chr(r$options)
        if (!is.null(r$set)) vals[names(chr(r$set))] <- chr(r$set)
        if (!is.null(r$warm)) vals[["path_gdx"]] <- as.character(r$warm)
        stem <- r$stem %||% rule$stem
        thLab <- if (is.na(th)) "" else .pfmThetaLabel(th, thetaDefault)
        title <- paste0(prefix, "-", m$parents[[r$parent]]$from, "-", stem, thLab,
                        if (identical(as.character(r$markup), "0")) "Min" else "",
                        if (!is.null(r$variant)) paste0("-", r$variant) else "",
                        if (coupled && nzchar(version)) paste0("-", version) else "")
        key <- paste0(r$key, thLab)
        if (!is.null(titles[[key]])) stop("duplicate run key ", key, " at ", resName, call. = FALSE)
        titles[[key]] <- title
        tag <- r$start[[resName]] %||% (if (coupled) paste0("V", sub("^v", "", version), "W", r$wave, resName) else unlist(res$baseStart))
        block[[length(block) + 1]] <- list(key = key, title = title, section = r$section %||% paste0("wave", r$wave),
                                           values = vals, start = unlist(tag), coupled = coupled,
                                           description = paste0(if (!is.null(r$wave)) paste0("[", version, " wave ", r$wave, "] ") else "",
                                                                r$description %||% ""),
                                           parentKey = r$parent)
      }
    }
    # 3. resolve @key references within this ssp x resolution
    for (i in seq_along(block)) {
      v <- block[[i]]$values
      at <- which(startsWith(v, "@"))
      for (j in at) {
        k <- substring(v[[j]], 2)
        if (is.null(titles[[k]])) stop(block[[i]]$title, ": ", names(v)[j], " = '@", k, "' names no run or parent at ", resName, call. = FALSE)
        v[[j]] <- titles[[k]]
      }
      block[[i]]$values <- v
      block[[i]]$ssp <- ssp; block[[i]]$res <- resName
    }
    # 4. separators: one per resolution, one per section
    sepRes <- paste0("_____", resName, "_", ssp, "_", version, "_____")
    out[[length(out) + 1]] <- list(title = sepRes, separator = TRUE)
    lastSection <- ""
    for (b in block) {
      if (!identical(b$section, lastSection)) {
        out[[length(out) + 1]] <- list(title = paste0("__________", gsub("[^A-Za-z0-9_-]", "_", b$section), "_", resName),
                                       separator = TRUE)
        lastSection <- b$section
      }
      b$separator <- FALSE
      out[[length(out) + 1]] <- b
    }
  }
  out
}

# Checks on the expanded rows, before anything is written.
.pfmCheckGenerated <- function(rows) {
  rr <- Filter(function(r) !isTRUE(r$separator), rows)
  titles <- vapply(rows, `[[`, "", "title")
  bad <- titles[!grepl("^[A-Za-z0-9_-]+$", titles)]
  if (length(bad)) stop("illegal title(s) for REMIND: ", paste(bad, collapse = ", "), call. = FALSE)
  dup <- titles[duplicated(titles)]
  if (length(dup)) stop("duplicate title(s): ", paste(unique(dup), collapse = ", "), call. = FALSE)
  for (r in rr) {
    if ("cm_iteration_max" %in% names(r$values) && nzchar(r$values[["cm_iteration_max"]])) {
      stop(r$title, ": the generator never writes cm_iteration_max (author's rule, 2026-10-01)", call. = FALSE)
    }
    if (any(grepl("[[:space:];]", r$start))) stop(r$title, ": a start tag contains whitespace or ';'", call. = FALSE)
    semi <- names(r$values)[grepl(";", r$values)]
    if (length(semi)) stop(r$title, ": ';' in ", paste(semi, collapse = ", "), " would split the row", call. = FALSE)
    if (grepl(";", r$description)) stop(r$title, ": ';' in the description would split the row", call. = FALSE)
    ref <- c("path_gdx", "path_gdx_ref", "path_gdx_refpolicycost", "path_gdx_carbonprice", "path_gdx_bau")
    for (cl in intersect(ref, names(r$values))) {
      v <- r$values[[cl]]
      if (nzchar(v) && !grepl("^[/~]|:", v) && !v %in% titles) {
        stop(r$title, ": ", cl, " = '", v, "' is not a scenario of the generated file", call. = FALSE)
      }
    }
    get <- function(k) if (k %in% names(r$values)) r$values[[k]] else ""
    if (r$coupled && startsWith(get("pfmGroup"), "v6") && !identical(get("cm_pfmPhiPath"), "1")) {
      stop(r$title, ": a v6 group needs cm_pfmPhiPath = 1 (PITFALLS.md 33)", call. = FALSE)
    }
  }
  invisible(TRUE)
}

# The rows as a data frame: title, start, then the canonical columns any row sets (canonical
# order), then the matrix's own columns (first-seen order), description last-but-PFM.
.pfmMatrixFrame <- function(rows, canonHeader) {
  rr <- Filter(function(r) !isTRUE(r$separator), rows)
  used <- unique(unlist(lapply(rr, function(r) names(r$values)[nzchar(r$values)])))
  first <- c("title", "start")
  canonCols <- setdiff(canonHeader[canonHeader %in% used], c(first, "description"))
  extra <- setdiff(used, c(first, canonCols, "description"))
  pfmCols <- extra[grepl("pfm|PFM", extra)]
  cols <- c(first, canonCols, setdiff(extra, pfmCols), "description", pfmCols)
  df <- as.data.frame(matrix("", nrow = length(rows), ncol = length(cols), dimnames = list(NULL, cols)),
                      stringsAsFactors = FALSE)
  for (i in seq_along(rows)) {
    r <- rows[[i]]
    df$title[i] <- r$title
    if (isTRUE(r$separator)) next
    v <- r$values[intersect(names(r$values), cols)]
    df[i, names(v)] <- v
    df$title[i] <- r$title
    df$start[i] <- paste(unique(r$start), collapse = ",")
    df$description[i] <- r$description
  }
  df
}

.pfmWriteScenarioCsv <- function(df, path) {
  lines <- c(paste(names(df), collapse = ";"), apply(df, 1, paste, collapse = ";"))
  con <- file(path, open = "wb")
  on.exit(close(con))
  writeBin(charToRaw(enc2utf8(paste0(paste(lines, collapse = "\r\n"), "\r\n"))), con)
  invisible(path)
}
# nolint end
