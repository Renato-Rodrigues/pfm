#' Apply a Run-Group's phi override to the per-sector regional shares
#'
#' A test instrument, not a model feature. It exists so a coupled run can be given feasibility
#' shares that carry NO estimated information, or one region's share held at another reading,
#' while everything else - the energy-system feedback, the severity, the bind mode, the level-cap
#' bound - is computed exactly as in the deployed run. It answers the paper's GP-24 (what does the
#' estimated ordering add to either coupled result?) and GP-23 (does the held-budget result depend
#' on China's 2035 reading?).
#'
#' The override is read from `phi-override.yml` in the Run-Group directory, if present, and applied
#' to `feas$phi` - the per-sector regional share - right after it is computed and BEFORE anything
#' is derived from it, so the economy-wide share, the per-market shares and the mode-2 price bound
#' all see the same overridden values. No file, no change: every existing Run-Group behaves exactly
#' as before.
#'
#' Modes (`mode:` in the yml):
#' \describe{
#'   \item{`uniform`}{Every region gets the same share in a sector. `value:` is a number, or `mean`
#'     (default) for the unweighted mean of that call's regional shares in that sector - so the
#'     level stays where the deployed run puts it and only the ordering is removed.}
#'   \item{`permute`}{The call's shares are reassigned across regions by a fixed random
#'     permutation (`seed:`, default 1) of the sorted region names. The same permutation is used
#'     for both sectors, so a region keeps the pairing of its two shares and the distribution of
#'     values is unchanged - only which region holds which value is broken.}
#'   \item{`set`}{Named regions get fixed shares per sector, e.g.
#'     `regions: {CHA: {Bulk: 0.862, Diffuse: 0.556}}`; every other region is left alone.}
#' }
#'
#' @param feas data.frame with at least `region`, `sector`, `phi` (one row per region, sector and,
#'   possibly, year; phi is constant over years).
#' @param dir the Run-Group directory to look for `phi-override.yml` in.
#' @param say a logging function; the applied override is always reported.
#' @return `feas` with `phi` overridden, and attribute `phiOverride` describing what was done
#'   (`NULL` attribute when there is no override file).
#' @keywords internal
.psmApplyPhiOverride <- function(feas, dir, say = message) {
  f <- file.path(dir, "phi-override.yml")
  if (!file.exists(f)) return(feas)
  ov <- yaml::read_yaml(f)
  mode <- tolower(as.character(ov$mode %||% ""))
  if (!mode %in% c("uniform", "permute", "set")) {
    stop("phi-override.yml in '", dir, "': mode must be uniform, permute or set, got '", mode, "'")
  }
  need <- c("region", "sector", "phi")
  if (!all(need %in% names(feas))) stop(".psmApplyPhiOverride: feas lacks ", paste(setdiff(need, names(feas)), collapse = ", "))
  reg <- as.character(feas$region); sec <- as.character(feas$sector)
  before <- feas$phi
  # one value per region and sector (phi does not vary over years)
  cur <- function(s) {
    x <- feas[sec == s & is.finite(feas$phi), c("region", "phi")]
    v <- tapply(x$phi, as.character(x$region), function(p) p[1])
    stats::setNames(as.numeric(v), names(v))
  }
  sectors <- sort(unique(sec))
  if (mode == "uniform") {
    val <- ov$value %||% "mean"
    for (s in sectors) {
      target <- if (identical(tolower(as.character(val)), "mean")) mean(cur(s)) else as.numeric(val)
      if (!is.finite(target) || target < 0 || target > 1) stop("phi override uniform: value for ", s, " is not in [0, 1]")
      feas$phi[sec == s] <- target
      say(sprintf("PHI OVERRIDE (uniform): %s share set to %.4f in every region", s, target))
    }
  } else if (mode == "permute") {
    seed <- as.integer(ov$seed %||% 1L)
    regs <- sort(unique(reg))
    old <- if (exists(".Random.seed", envir = globalenv())) get(".Random.seed", envir = globalenv()) else NULL
    set.seed(seed)
    perm <- stats::setNames(sample(regs), regs)       # region r takes the share of region perm[r]
    if (!is.null(old)) assign(".Random.seed", old, envir = globalenv())
    for (s in sectors) {
      v <- cur(s)
      take <- perm[names(v)]
      # value of the donor region, re-labelled to the receiving region (the names of v[take] are
      # the DONORS' - indexing by region with them would hand every region its own value back)
      newv <- stats::setNames(unname(v[take]), names(v))
      if (anyNA(newv)) stop("phi override permute: a region has no share in ", s)
      feas$phi[sec == s] <- unname(newv[reg[sec == s]])
    }
    say(sprintf("PHI OVERRIDE (permute, seed %d): %s", seed,
                paste(sprintf("%s<-%s", names(perm), perm), collapse = " ")))
  } else {
    for (r in names(ov$regions)) {
      if (!r %in% reg) stop("phi override set: region '", r, "' is not in this run's regions")
      for (s in names(ov$regions[[r]])) {
        if (!s %in% sectors) stop("phi override set: sector '", s, "' unknown")
        v <- as.numeric(ov$regions[[r]][[s]])
        if (!is.finite(v) || v < 0 || v > 1) stop("phi override set: ", r, "/", s, " not in [0, 1]")
        feas$phi[reg == r & sec == s] <- v
        say(sprintf("PHI OVERRIDE (set): %s %s share %.4f", r, s, v))
      }
    }
  }
  attr(feas, "phiOverride") <- list(mode = mode, file = f, maxChange = max(abs(feas$phi - before), na.rm = TRUE))
  feas
}
