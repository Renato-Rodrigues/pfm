# nolint start
#' The anchor artifact of the v6 coupling, as a Run-Group step
#'
#' @description
#' Writes \code{<group>/phi-anchor.rds} (design note 0005 C6 / F6): everything the v6 coupling
#' needs about the past, computed once per Run-Group, so the call inside REMIND reads one small
#' file instead of re-deriving the ranking (no ECM, no \code{temporal-validation.rds}, no seed
#' panel). It holds
#' \itemize{
#'   \item per country and sector, the logit distance below the ceiling at the anchor year,
#'     \eqn{q_{c,s}}, with its basis (observed, donor, low band, median), from
#'     \code{\link{computeAnchorGap}};
#'   \item per resolution (EU21 and H12 by default), the regional ranking \eqn{u_{r,s}}, the
#'     regional weights and the provenance shares;
#'   \item per sector, a \emph{lean} frontier design: the spec, the deployed frontier
#'     coefficients, the frozen driver scaling (saturation parameters included), the guard ranges
#'     and the trend curve - what \eqn{\eta_{c,s}(t)} needs on a scenario panel, and nothing of the
#'     fitted model's data;
#'   \item provenance: the panel hash, the md5 of \code{frontier.rds}, the \code{pfm} version.
#' }
#' \code{\link{pfmAnchorFor}} turns it back into the object \code{\link{computeStrengthPath}} and
#' \code{\link{computeSharePath}} take.
#'
#' @param group Run-Group name.
#' @param resultsDir,modelDir,cachefolder Standard Run-Group locations.
#' @param resolutions Named vector of region mappings, one entry per coupling resolution.
#' @param ssp SSP of the coupling weights (final energy at \code{t0}; the SSPs coincide until 2029).
#' @param t0 The normalisation year of the strength factor. Default 2025.
#' @param verbose Logical.
#' @return Invisibly, the artifact (a list); \code{NULL} when a prerequisite is missing.
#' @seealso \code{\link{pfmAnchorFor}}, \code{\link{computeAnchorGap}}
#' @export
#' @author Renato Rodrigues
runPFMAnchor <- function(group, resultsDir = getOption("pfm.resultsDir", "output"),
                         modelDir = getOption("pfm.modelDir", "output"), cachefolder = NULL,
                         resolutions = c(EU21 = "regionmapping_21_EU11.csv", H12 = "regionmappingH12.csv"),
                         ssp = "SSP2", t0 = 2025, verbose = TRUE) {
  groupDir <- .resolveGroupDir(group, resultsDir, modelDir, cachefolder)
  say <- function(...) if (isTRUE(verbose)) message("[PFM-ANCHOR:", group, "] ", ...)
  tStart <- Sys.time()
  if (is.null(names(resolutions)) || any(!nzchar(names(resolutions)))) {
    stop("runPFMAnchor: 'resolutions' must be named, e.g. c(EU21 = \"regionmapping_21_EU11.csv\").", call. = FALSE)
  }
  need <- c(basename(.pfmSelectedModels(groupDir)), "manifest.json", "frontier.rds",
            "donor-assignment-band-Bulk.rds", "donor-assignment-band-Diffuse.rds")
  miss <- need[!file.exists(file.path(groupDir, need))]
  if (length(miss)) {
    .recordStep(groupDir, group, "pfm-anchor", tStart, status = "skipped",
                metrics = list(reason = paste("missing", paste(miss, collapse = ", "))))
    say("skipped: missing ", paste(miss, collapse = ", "), " - run the sweep, pfm-frontier and pfm-donor first.")
    return(invisible(NULL))
  }
  mf <- jsonlite::read_json(file.path(groupDir, "manifest.json"))
  hist <- loadTrainingPanel(mf$panel_hash, modelDir)
  if (is.null(hist)) {
    .recordStep(groupDir, group, "pfm-anchor", tStart, status = "failed",
                metrics = list(reason = paste0("training panel ", mf$panel_hash, " not in the Fit Cache")))
    stop("runPFMAnchor: training panel ", mf$panel_hash, " is not in the Fit Cache (", modelDir, ").", call. = FALSE)
  }
  weights <- pfmAssertSizeWeights(pfmCouplingWeights(year = t0, scenario = ssp), "runPFMAnchor")

  anchors <- lapply(resolutions, function(m) {
    say("anchor at ", m, " ...")
    computeAnchorGap(group, resultsDir = resultsDir, modelDir = modelDir, mapping = m, weights = weights,
                     ssp = ssp, t0 = t0, histData = hist, verbose = FALSE)
  })
  a1 <- anchors[[1]]
  # q is a country quantity: it must not depend on the resolution it was computed for.
  for (a in anchors[-1]) {
    if (!isTRUE(all.equal(a$country, a1$country))) {
      stop("runPFMAnchor: the country anchor differs between resolutions - it must not.", call. = FALSE)
    }
  }
  art <- list(
    format = "pfm-anchor/1", group = group, spec = a1$spec, rule = a1$rule,
    anchorYear = a1$anchorYear, t0 = t0, ssp = ssp,
    country = a1$country, weights = weights,
    designs = lapply(a1$designs, .pfmLeanDesign),
    byResolution = lapply(anchors, function(a) list(mapping = a$mapping, region = a$region,
                                                     regionWeights = a$regionWeights)),
    panelHash = mf$panel_hash,
    frontierMd5 = unname(tools::md5sum(file.path(groupDir, "frontier.rds"))),
    pfmVersion = as.character(utils::packageVersion("pfm")),
    created = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  out <- file.path(groupDir, "phi-anchor.rds")
  saveRDS(art, out)

  bases <- table(a1$country$basis[a1$country$sector == a1$country$sector[1]])
  floorOf <- function(r) vapply(split(r, r$sector), function(d) d$region[which.max(d$u)], character(1))
  floors <- lapply(art$byResolution, function(b) as.list(floorOf(b$region)))
  say("wrote ", out, " (anchor year ", art$anchorYear, "; ", paste(names(bases), bases, collapse = " "), ")")
  for (r in names(floors)) say("  ", r, ": most constrained region ", paste(names(floors[[r]]), unlist(floors[[r]]), sep = " ", collapse = ", "))
  .recordStep(groupDir, group, "pfm-anchor", tStart, metrics = list(
    anchorYear = art$anchorYear, resolutions = names(resolutions), countries = as.list(bases),
    mostConstrained = floors, kb = round(file.size(out) / 1024)))
  invisible(art)
}

#' The anchor of one resolution, from the anchor artifact
#'
#' @description
#' Rebuilds, from \code{phi-anchor.rds} (\code{\link{runPFMAnchor}}), the object
#' \code{\link{computeAnchorGap}} returns for one resolution, so \code{\link{computeStrengthPath}}
#' and \code{\link{computeSharePath}} run on it unchanged - without the Fit Cache, the training
#' panel or the donor files.
#'
#' @param x The artifact (a list) or the path to \code{phi-anchor.rds}, or a Run-Group folder
#'   that holds one.
#' @param resolution Name of the resolution (\code{"EU21"}, \code{"H12"}).
#' @return A list shaped like \code{computeAnchorGap()}'s output.
#' @export
#' @author Renato Rodrigues
pfmAnchorFor <- function(x, resolution = "EU21") {
  if (is.character(x)) {
    p <- if (dir.exists(x)) file.path(x, "phi-anchor.rds") else x
    if (!file.exists(p)) stop("pfmAnchorFor: no anchor artifact at ", p, call. = FALSE)
    x <- readRDS(p)
  }
  if (!identical(x$format, "pfm-anchor/1")) stop("pfmAnchorFor: not a pfm anchor artifact.", call. = FALSE)
  b <- x$byResolution[[resolution]]
  if (is.null(b)) {
    stop("pfmAnchorFor: no resolution '", resolution, "' in the artifact (has: ",
         paste(names(x$byResolution), collapse = ", "), ").", call. = FALSE)
  }
  list(group = x$group, rule = x$rule, anchorYear = x$anchorYear, t0 = x$t0, mapping = b$mapping,
       ssp = x$ssp, country = x$country, region = b$region, weights = x$weights,
       regionWeights = b$regionWeights, spec = x$spec, designs = x$designs)
}

# What .pfmFrontierEta() reads from a design, and nothing else: no estimation data, no fitted
# model. The formula's environment is cut, so the artifact does not drag in the fit's frame.
#' @keywords internal
.pfmLeanDesign <- function(d) {
  f <- d$fit
  fml <- f$formula
  environment(fml) <- baseenv()
  list(cfg = d$cfg, sector = d$sector, beta = d$beta, indexMax = d$indexMax, lastHist = d$lastHist,
       ranges = d$ranges, nSqueeze = d$nSqueeze, trend = d$trend,
       fit = list(driverScaling = f$driverScaling, outcomeVar = f$outcomeVar, formula = fml,
                  model = list(xlevels = list(regionFE = f$model$xlevels$regionFE))))
}
# nolint end
