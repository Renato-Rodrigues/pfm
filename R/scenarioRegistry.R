# nolint start
#' Parse the Policy Scenario Registry (ADR 0035)
#'
#' @description
#' The PFM projects one shared [[Fitted Model]] onto several REMIND
#' \strong{policy scenarios} (e.g. National Policies vs Climate Ambition). The set
#' of scenarios is declared in \code{config.yml} as a \code{scenarios:} block plus a
#' top-level \code{gatingScenario:} field (the Scenario Registry, CONTEXT.md). This
#' helper normalises that declaration into a list of scenario descriptors used by
#' the projection fan-out (\code{\link{runProjection}}) and identifies the
#' \strong{gating scenario} whose projection drives model selection.
#'
#' Each registry entry is a named list with:
#' \describe{
#'   \item{id}{Stable identifier; the projection-artifact stem and report/figure key.
#'     If absent it is derived from the gdx run folder name with the trailing
#'     \code{_<date>_<time>} stamp stripped.}
#'   \item{name}{Human-readable display name for reports and the manuscript.}
#'   \item{gdx}{Path to the scenario's \code{fulldata.gdx} (resolved against \code{baseDir}).}
#'   \item{gdxRegionMapping}{Region mapping matching the gdx's native resolution
#'     (e.g. \code{regionmapping_21_EU11-without-missingH12.csv} for \code{EU21}
#'     runs). Defaults to \code{regionmappingH12.csv}.}
#'   \item{gating}{Logical; \code{TRUE} for the gating scenario.}
#' }
#'
#' Backward compatibility: when no \code{scenarios:} block is present but a single
#' \code{gdxPath}/\code{gdxFile} is, a one-scenario registry (id \code{"scenario"},
#' gating) is synthesised so the legacy single-projection pipeline keeps working.
#'
#' @param cfg A parsed \code{config.yml} as a named list (e.g. from
#'   \code{yaml::read_yaml}). May contain \code{scenarios}, \code{gatingScenario},
#'   and/or \code{gdxPath}/\code{gdxFile}.
#' @param baseDir Directory that relative gdx paths resolve against (the working
#'   directory of the run). Default \code{getwd()}.
#' @param requireExists Logical. If \code{TRUE} (default) scenarios whose gdx file
#'   is missing on disk are dropped with a warning rather than returned.
#'
#' @return A list with elements \code{scenarios} (a named list of normalised
#'   descriptors, keyed by id) and \code{gating} (the id of the gating scenario, or
#'   \code{NULL} if none resolved). Returns empty \code{scenarios} when nothing is
#'   declared.
#' @seealso \code{\link{runProjection}}, \code{\link{scenarioGatingGdx}}, ADR 0035
#' @author Renato Rodrigues
#' @export
parseScenarioRegistry <- function(cfg, baseDir = getwd(), requireExists = TRUE) {
  if (is.null(cfg)) cfg <- list()
  absify <- function(p) {
    if (is.null(p) || !nzchar(p)) return(NULL)
    if (grepl("^([A-Za-z]:|/|\\\\)", p)) p else normalizePath(file.path(baseDir, p), winslash = "/", mustWork = FALSE)
  }
  # id from a REMIND run folder: strip the trailing _<YYYY-MM-DD>_<HH.MM.SS> stamp.
  idFromGdx <- function(gdxPath) {
    folder <- basename(dirname(gdxPath))
    sub("_[0-9]{4}-[0-9]{2}-[0-9]{2}_[0-9]{2}[.:][0-9]{2}[.:][0-9]{2}$", "", folder)
  }

  raw <- cfg$scenarios
  scenarios <- list()

  if (!is.null(raw) && length(raw)) {
    for (i in seq_along(raw)) {
      e <- raw[[i]]
      if (is.null(e$gdx) && is.null(e$gdxPath)) next
      gdx <- absify(e$gdx %||% e$gdxPath)
      id  <- if (!is.null(e$id) && nzchar(e$id)) as.character(e$id) else idFromGdx(gdx)
      scenarios[[id]] <- list(
        id   = id,
        name = if (!is.null(e$name) && nzchar(e$name)) as.character(e$name) else id,
        gdx  = gdx,
        gdxRegionMapping = e$gdxRegionMapping %||% e$gdxRegionMappingFile %||% "regionmappingH12.csv",
        gating = isTRUE(e$gating)
      )
    }
  }

  # Backward-compatible single-scenario fallback.
  if (!length(scenarios)) {
    gdx <- absify(cfg$gdxPath %||% cfg$gdxFile)
    if (!is.null(gdx)) {
      scenarios[["scenario"]] <- list(
        id = "scenario", name = "Scenario", gdx = gdx,
        gdxRegionMapping = cfg$gdxRegionMapping %||% "regionmappingH12.csv", gating = TRUE)
    }
  }

  # Drop scenarios whose gdx is missing, if requested.
  if (isTRUE(requireExists) && length(scenarios)) {
    keep <- vapply(scenarios, function(s) !is.null(s$gdx) && file.exists(s$gdx), logical(1))
    if (any(!keep)) {
      for (s in scenarios[!keep]) warning("parseScenarioRegistry: gdx not found for scenario '",
                                          s$id, "' (", s$gdx %||% "NULL", ") - dropped.", call. = FALSE)
      scenarios <- scenarios[keep]
    }
  }

  # Resolve the gating scenario: explicit gatingScenario field -> any entry flagged gating ->
  # the single remaining scenario. An explicitly NAMED gating scenario that did not resolve
  # (e.g. its gdx is still missing) must NOT silently demote to another scenario — that would
  # screen model selection against the wrong stress-test pathway. Warn loudly and leave gating
  # unresolved instead (the launcher then skips the sanity gate with a clear upstream reason).
  gating <- NULL
  named <- cfg$gatingScenario %||% cfg$gating_scenario
  if (!is.null(named) && nzchar(named)) {
    if (!is.null(scenarios[[named]])) {
      gating <- named
    } else {
      warning("parseScenarioRegistry: gatingScenario '", named, "' is declared but did not ",
              "resolve (its gdx may be missing or still copying). Selection will be UNGATED ",
              "(no Projection-Sanity stress-test projection). Resolve that gdx before the ",
              "production run.", call. = FALSE)
    }
  } else {
    flagged <- names(Filter(function(s) isTRUE(s$gating), scenarios))
    if (length(flagged)) gating <- flagged[[1]]
    else if (length(scenarios) == 1) gating <- names(scenarios)[[1]]
  }
  for (id in names(scenarios)) scenarios[[id]]$gating <- identical(id, gating)

  list(scenarios = scenarios, gating = gating)
}

#' Gating-scenario gdx path from a registry (ADR 0035)
#'
#' Convenience accessor returning the \code{fulldata.gdx} path of the
#' \strong{gating scenario} — the gdx the sweep / model-selection sanity gate
#' should see. Use it to set \code{gdxFile} for \code{\link{startRun}} /
#' \code{\link{runSweep}} so selection is screened against the stress-test
#' (ambition) scenario while \emph{all} scenarios are projected at the end.
#'
#' @param registry The list returned by \code{\link{parseScenarioRegistry}}.
#' @return The gating scenario's gdx path, or \code{NULL} if none.
#' @seealso \code{\link{parseScenarioRegistry}}, ADR 0035
#' @export
scenarioGatingGdx <- function(registry) {
  if (is.null(registry) || is.null(registry$gating)) return(NULL)
  registry$scenarios[[registry$gating]]$gdx
}
# nolint end
