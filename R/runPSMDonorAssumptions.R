# nolint start
#' Donor-assumptions Run-Group step for countries CAPMF does not cover
#'
#' @description
#' Package port of the former \code{analysis/psm-donor-assumptions.R}. Building
#' the donor table from a loose script meant the repo root and the Run-Group were
#' script-level constants: a forgotten edit wrote new results into the OLD group,
#' silently. As a Run-Group step it takes the group as an argument, resolves the
#' panel from \code{manifest.json:panel_hash} like every other step, and can be
#' submitted to SLURM through the ordinary \code{\link{startRun}} path.
#'
#' What is donated is the \strong{relative gap} (efficiency ratio \eqn{S/S^*}),
#' never the ceiling: \eqn{S^*} is a function of drivers and already exists for
#' every country, so the slack is the only genuinely unobservable quantity. Each
#' recipient keeps its own ceiling and inherits only the assumption about how much
#' of it politics claims. Distance is measured in the model's own metric —
#' standardized drivers weighted by \eqn{|\beta|} — so "close" means close in the
#' directions the model says move policy.
#'
#' Static and exogenous by design: computed once, reviewed by hand, then read as an
#' assumption. It does not enter the iterative REMIND loop.
#'
#' Writes \code{<group>/coverage/donor-assumptions.{rds,csv}} and, per sector,
#' \code{<group>/donor-assignment-band-<sector>.rds}.
#'
#' @param group Run-Group name.
#' @param resultsDir,modelDir,cachefolder Standard Run-Group locations.
#' @param panelData Optional pre-built historical panel.
#' @param basisOverride Named character vector of hand-set bases, passed to
#'   \code{\link{computeDonorAssignment}}. The default pins \code{USA} to
#'   \code{"median"}: matched on drivers the USA lands next to JPN/GBR/KOR, which
#'   share its institutional capacity but not its willingness. Donor transfer
#'   assumes efficiency is a function of the drivers — if it were, we would predict
#'   it rather than transfer it — so matching fails exactly where capability and
#'   willingness diverge. US federal structure spans states with world-leading
#'   climate regulation and states with almost none, so the national outcome sits
#'   far below the frontier its institutions could support.
#' @param k Number of donors per recipient.
#' @param verbose Logical.
#'
#' @return Invisibly, the donor table (both sectors stacked).
#' @author Renato Rodrigues
#' @export
runPSMDonorAssumptions <- function(group,
                                   resultsDir = getOption("pfm.resultsDir", "output"),
                                   modelDir = getOption("pfm.modelDir", "output"),
                                   cachefolder = NULL,
                                   panelData = NULL,
                                   basisOverride = c(USA = "median"),
                                   k = 3,
                                   verbose = TRUE) {
  groupDir <- .resolveGroupDir(group, resultsDir, modelDir, cachefolder)
  say <- function(...) if (isTRUE(verbose)) message("[PSM-DONOR:", group, "] ", ...)
  t0 <- Sys.time()
  sectors <- c("Bulk", "Diffuse")

  selPath <- file.path(groupDir, "selected-models-psm.yml")
  frPath  <- file.path(groupDir, "frontier.rds")
  for (p in c(selPath, frPath)) {
    if (!file.exists(p)) {
      .recordStep(groupDir, group, "psm-donor", t0, status = "skipped",
                  metrics = list(reason = paste0("missing ", basename(p))))
      say("skipped: ", basename(p), " not found — run the sweep and psm-frontier first.")
      return(invisible(NULL))
    }
  }
  sel <- yaml::read_yaml(selPath)
  fr  <- readRDS(frPath)

  panel <- panelData %||% .psmHistPanel(groupDir, verbose = verbose)
  if (is.null(panel)) {
    .recordStep(groupDir, group, "psm-donor", t0, status = "failed",
                metrics = list(reason = "historical panel unavailable"))
    return(invisible(NULL))
  }

  norm <- function(s) {
    for (f in c("actorPowerDrivers", "actorPowerIndex", "instQualityDrivers", "controlDrivers"))
      if (!is.null(s[[f]])) s[[f]] <- unlist(s[[f]])
    s
  }

  # Strip the outcome so preparePanelData keeps ALL countries: it drops NA-outcome
  # rows, which would otherwise leave only the covered sample — and the covered
  # sample is precisely the set this step exists to look beyond.
  psVars <- grep("Policy Stringency", magclass::getNames(panel), value = TRUE)
  pAll <- panel[, , setdiff(magclass::getNames(panel), psVars)]

  outDir <- file.path(groupDir, "coverage")
  dir.create(outDir, showWarnings = FALSE, recursive = TRUE)

  tab <- do.call(rbind, lapply(sectors, function(sec) {
    hit <- Filter(function(x) identical(x$model_type, paste0("PolicyStringency: ", sec)), sel)
    if (!length(hit)) { say("no deployed spec for ", sec, " — skipped."); return(NULL) }
    cf <- norm(hit[[1]])
    fit <- estimatePolicyStringencyModel(
      data = panel, sector = sec, estimator = "satP",
      actorPowerDrivers = cf$actorPowerDrivers, actorPowerIndex = cf$actorPowerIndex,
      instQualityDrivers = cf$instQualityDrivers, controlDrivers = cf$controlDrivers,
      regionMappingFixedEffects = cf$regionMappingFixedEffects,
      logisticTimeTrend = isTRUE(cf$logisticTimeTrend),
      apTransform = cf$apTransform %||% "linear",
      modelDir = modelDir, updateIndex = FALSE, verbose = FALSE)
    sDf <- preparePanelData(
      data = pAll, sector = sec,
      actorPowerDrivers = cf$actorPowerDrivers, actorPowerIndex = cf$actorPowerIndex,
      instQualityDrivers = cf$instQualityDrivers, controlDrivers = cf$controlDrivers,
      regionMappingFixedEffects = NULL, driverScaling = fit$driverScaling,
      outcomeVar = "Policy Stringency")
    d <- computeDonorAssignment(fit, fr$bySector[[sec]]$scores, sDf, k = k,
                                basisOverride = basisOverride, sector = sec)
    say(sprintf("%-8s recipients %d | close %d / far %d / none %d | median inherited E %.3f",
                sec, nrow(d), sum(d$donorQuality == "close"), sum(d$donorQuality == "far"),
                sum(d$donorQuality == "none"), stats::median(d$efficiencyRatio, na.rm = TRUE)))
    d
  }))
  if (is.null(tab) || !nrow(tab)) {
    .recordStep(groupDir, group, "psm-donor", t0, status = "failed",
                metrics = list(reason = "no donor rows produced"))
    return(invisible(NULL))
  }

  # The RDS is what the pipeline reads; the CSV is the human review artifact. Write
  # the data FIRST so an Excel lock on the CSV cannot cost us the pipeline input.
  saveRDS(tab, file.path(outDir, "donor-assumptions.rds"))
  for (sec in sectors) {
    saveRDS(tab[tab$sector == sec, ],
            file.path(groupDir, paste0("donor-assignment-band-", sec, ".rds")))
  }

  hdr <- c(
    "# Donor assumptions for countries without CAPMF coverage (pfm::computeDonorAssignment).",
    "# DONATED QUANTITY: the relative gap (efficiencyRatio = S/S*), NOT the ceiling.",
    "#   Each recipient keeps its own driver-based ceiling and inherits only the",
    "#   assumption about how much of it politics claims.",
    "# DISTANCE: standardized drivers weighted by |beta| from the deployed model, so",
    "#   'close' means close in the directions the model says move policy stringency.",
    "# donorQuality is calibrated against how far COVERED countries sit from each",
    "#   other: close = within their median nearest-neighbour distance, far = within",
    "#   the 90th percentile, none = beyond anything the estimation sample spans.",
    "# A 'none' row is a flag, not a result: treat it as unmatched and fall back to",
    "#   the uncoupled assumption rather than quoting its inherited value.",
    paste0("# generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
  csv <- file.path(outDir, "donor-assumptions.csv")
  ok <- tryCatch({
    con <- file(csv, open = "wt"); on.exit(try(close(con), silent = TRUE), add = TRUE)
    writeLines(hdr, con); utils::write.csv(tab, con, row.names = FALSE)
    TRUE
  }, error = function(e) FALSE)
  if (!ok) {
    csv <- sub("[.]csv$", paste0("-", format(Sys.time(), "%H%M%S"), ".csv"), csv)
    con <- file(csv, open = "wt")
    writeLines(hdr, con); utils::write.csv(tab, con, row.names = FALSE); close(con)
    say("NOTE: the canonical CSV was locked; wrote ", basename(csv), " instead.")
  }
  say("wrote ", csv)

  .recordStep(groupDir, group, "psm-donor", t0, metrics = list(
    recipients = nrow(tab),
    close = sum(tab$donorQuality == "close"),
    far = sum(tab$donorQuality == "far"),
    none = sum(tab$donorQuality == "none")))
  invisible(tab)
}
# nolint end
