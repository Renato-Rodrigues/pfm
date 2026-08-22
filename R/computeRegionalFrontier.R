# nolint start
#' Aggregate the fitted and projected ceiling to REMIND regions
#'
#' @description
#' The country-level frontier, summarised at the resolution the coupling actually assigns
#' \eqn{\varphi} on. Produces one row per region, year and sector with the final-energy-weighted
#' mean of the observed index, the fitted ceiling (historical years) and the projected ceiling
#' (scenario years), plus the share of the region's energy that is covered at all.
#'
#' Exists because `figures/` is a pure consumer of Run-Group artifacts and may not compute
#' weights, and weighting is not optional here: an unweighted regional mean gives Luxembourg the
#' same say as China. This is the same `psmCouplingWeights()` the coupling itself uses, so the
#' figure and the coupling aggregate the same way (`PITFALLS.md` §20).
#'
#' @section What this is and is not:
#' It is a **weighted mean of country ceilings**, not "the region's ceiling". The frontier is
#' non-linear — \eqn{\mathrm{index} = 10\,\mathrm{logit}^{-1}(\eta)} — so the mean of the
#' ceilings is not the ceiling of the mean, and no regional frontier was ever fitted. Label it
#' as a summary wherever it is shown.
#'
#' It is also **not a rescaled view of the country figure.** \eqn{u} is min–max normalised over
#' whatever units are in the frame, so aggregation changes the object rather than the units —
#' this is the same effect that moves the anchor from 0.744 over 48 countries to 0.396 over 21
#' regions (`MODEL.md` §5.3, `PITFALLS.md` §15). The two resolutions are complementary and
#' neither substitutes for the other.
#'
#' @section Coverage:
#' Only countries the frontier was FITTED on contribute. Out-of-coverage countries carry a
#' transferred relative gap rather than a measured ceiling, so including them would imply
#' evidence that does not exist. `coveredShare` reports what fraction of each region's weight
#' that leaves, and it is the honest caveat on every regional number: 16 of 20 regions are at or
#' above 73%, but LAM, OAS, MEA and SSA are thin, and **the USA has no in-coverage country at
#' all and is therefore absent by construction, not by omission.**
#'
#' @param group Run-Group name.
#' @param resultsDir,modelDir,cachefolder Standard Run-Group locations.
#' @param mapping Country-to-region mapping. Must be the resolution \eqn{\varphi} is assigned on.
#' @param scenarios Character vector of scenario ids to read from `<group>/projections/`.
#'   `NULL` reads every projection present.
#' @param weightYear,weightScenario Passed to \code{\link{psmCouplingWeights}}.
#' @param verbose Logical.
#'
#' @return Invisibly, a data.frame: `region, year, sector, scenario, kind, index, coveredShare,
#'   nCountries`. `kind` is `"observed"`, `"fitted"` or `"projected"`.
#' @author Renato Rodrigues
#' @export
computeRegionalFrontier <- function(group,
                                    resultsDir = getOption("pfm.resultsDir", "output"),
                                    modelDir = getOption("pfm.modelDir", "output"),
                                    cachefolder = NULL,
                                    mapping = "regionmapping_21_EU11.csv",
                                    scenarios = NULL,
                                    weightYear = 2025, weightScenario = "SSP2",
                                    verbose = TRUE) {
  groupDir <- .resolveGroupDir(group, resultsDir, modelDir, cachefolder)
  say <- function(...) if (isTRUE(verbose)) message("[PSM-REGFRONT:", group, "] ", ...)

  fr <- readRDS(file.path(groupDir, "frontier.rds"))
  wts <- psmCouplingWeights(year = weightYear, scaleBy = "gdp", scenario = weightScenario,
                            verbose = verbose)
  psmAssertSizeWeights(wts, "computeRegionalFrontier")

  map <- .psmReadRegionMapping(mapping)
  regOf <- function(iso) map$region[match(iso, map$iso)]

  # Denominator over ALL member countries, not just the fitted ones - otherwise `coveredShare`
  # would be 1 everywhere by construction, which is the exact mistake it exists to prevent.
  allw <- data.frame(iso = map$iso, region = map$region, stringsAsFactors = FALSE)
  allw$w <- as.numeric(wts[allw$iso]); allw$w[!is.finite(allw$w)] <- 0
  totW <- tapply(allw$w, allw$region, sum)

  wmean <- function(v, w) {
    ok <- is.finite(v) & is.finite(w) & w > 0
    if (!any(ok)) return(NA_real_)
    sum(v[ok] * w[ok]) / sum(w[ok])
  }
  agg <- function(df, valueCol, kind, sector, scenario = NA_character_,
                  scenarioName = NA_character_) {
    df$region2 <- regOf(df$region)
    df$w <- as.numeric(wts[df$region]); df$w[!is.finite(df$w)] <- 0
    df <- df[!is.na(df$region2), ]
    do.call(rbind, lapply(split(df, list(df$region2, df$year), drop = TRUE), function(x) {
      data.frame(region = x$region2[1], year = x$year[1], sector = sector,
                 scenario = scenario, scenarioName = scenarioName, kind = kind,
                 index = wmean(x[[valueCol]], x$w),
                 coveredShare = sum(x$w[x$w > 0]) / totW[[x$region2[1]]],
                 nCountries = sum(x$w > 0), stringsAsFactors = FALSE)
    }))
  }

  out <- list()
  for (sec in names(fr$bySector)) {
    h <- fr$bySector[[sec]]$scores
    out[[paste0(sec, ".obs")]] <- agg(h, "observedIndex", "observed", sec)
    out[[paste0(sec, ".fit")]] <- agg(h, "frontierIndex", "fitted", sec)
  }

  projDir <- file.path(groupDir, "projections")
  files <- if (dir.exists(projDir)) list.files(projDir, pattern = "[.]rds$", full.names = TRUE) else character(0)
  if (!is.null(scenarios)) {
    files <- files[tools::file_path_sans_ext(basename(files)) %in% scenarios]
  }
  for (f in files) {
    p <- as.data.frame(readRDS(f))
    id <- tools::file_path_sans_ext(basename(f))
    for (sec in unique(p$sector)) {
      # in-coverage ONLY, and only countries the frontier was fitted on, so the regional mean
      # never mixes a measured ceiling with a transferred one
      fitted <- unique(fr$bySector[[sec]]$scores$region)
      q <- p[p$sector == sec & !p$outOfCoverage & p$region %in% fitted, ]
      if (!nrow(q)) next
      # Carry the human-readable name through. Consumers must not have to parse an id,
      # and a figure that labels its series from the id ends up ordering them alphabetically -
      # which silently swaps the colours between two figures showing the same two scenarios.
      out[[paste0(sec, ".", id)]] <- agg(q, "index", "projected", sec, id,
                                         scenarioName = (q$scenarioName %||% id)[1])
    }
  }

  res <- do.call(rbind, c(out, list(make.row.names = FALSE)))
  res <- res[order(res$sector, res$region, res$kind, res$year), ]
  saveRDS(res, file.path(groupDir, "regional-frontier.rds"))
  say("wrote regional-frontier.rds: ", nrow(res), " rows, ",
      length(unique(res$region)), " regions, ",
      sum(!is.na(unique(res$scenario))), " scenario(s)")
  invisible(res)
}

# Region mapping reader, tolerant of the two column conventions madrat ships.
.psmReadRegionMapping <- function(mapping) {
  f <- if (file.exists(mapping)) mapping else {
    alt <- system.file("extdata", mapping, package = "madrat")
    if (!nzchar(alt)) alt <- system.file("extdata", sub("[.]csv$", "-columnname.csv", mapping),
                                         package = "madrat")
    alt
  }
  if (!nzchar(f) || !file.exists(f)) {
    stop("computeRegionalFrontier: region mapping '", mapping, "' not found. It must be the ",
         "resolution phi is assigned on - getting this wrong mis-assigns every region, ",
         "silently (PITFALLS.md).", call. = FALSE)
  }
  d <- utils::read.csv(f, sep = ";", stringsAsFactors = FALSE)
  isoCol <- intersect(c("ISOcountry", "CountryCode", "iso"), names(d))[1]
  regCol <- setdiff(names(d), c(isoCol, "RegionCode", "X"))
  regCol <- regCol[length(regCol)]
  if (is.na(isoCol)) stop("computeRegionalFrontier: no ISO column in ", f, call. = FALSE)
  data.frame(iso = d[[isoCol]], region = d[[regCol]], stringsAsFactors = FALSE)
}
# nolint end
