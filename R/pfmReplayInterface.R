# nolint start
#' Replay the GAMS half of the PFM <-> REMIND gdx interface
#'
#' @description
#' Writes a coupling gdx with the real exporters, generates a GAMS stub that declares the
#' symbols \strong{by reading} \code{45_carbonprice/functionalForm/declarations.gms}, runs
#' the same \code{Execute_Loadpoint} statements \code{presolve.gms} runs, and asserts that
#' the values GAMS ends up holding are the ones R wrote.
#'
#' Promoted from the archived \code{replay-coupling.R} (\code{TODO.md} item 6). That script
#' replayed a whole coupling call from a finished \code{fulldata.gdx}; this is only its
#' \strong{interface} half, which is the part no R-side test can cover.
#'
#' @section Why asserting values is the point:
#' \code{execError} is \strong{not} sufficient. A symbol written with the right rank and the
#' right domain sets but the wrong index ORDER raises no GAMS error at all and loads as
#' \code{( ALL 0.000 )} - that was defect 4, and it reproduces one dimension up on the
#' rank-3 ADR 0042 symbols (verified 2026-08-17: the only execution error in the negative
#' control's listing is this function's own assertion). Only comparing the loaded cells
#' against what R wrote can catch it.
#'
#' @section The negative control:
#' With \code{negativeControl = TRUE} (the default) a second gdx is built \emph{by hand}
#' through gamstransfer - \code{\link{.psmCouplingSymMkt2d}} would refuse to produce it -
#' carrying \code{p45_pfmPriceBoundMkt} transposed to \code{(all_regi, ttot, all_emiMkt)},
#' and fed to the same stub. A run that does not ABORT there is a failure of the harness,
#' not a pass: it means the check is incapable of detecting the defect it exists for.
#'
#' @param remindDir Path to the REMIND fork carrying the module. Default: the
#'   \code{pfm.remindDir} option, else \code{"../remind_pfm"} relative to the working
#'   directory.
#' @param dir Working directory for the generated stub, gdx and listing. Default a session
#'   temporary directory; pass a real path to keep the artefacts.
#' @param negativeControl Also run the transposed-symbol control (default \code{TRUE}).
#' @param gams Path to the GAMS executable. Default \code{Sys.which("gams")}.
#' @param quiet Suppress progress messages.
#'
#' @return Invisibly, a list: \code{ok} (positive replay loaded cleanly AND the negative
#'   control was caught), \code{positive} / \code{negative} (each with \code{ok},
#'   \code{status}, \code{lst}, \code{verdict}), \code{dir}, \code{nDeclarations},
#'   \code{skipped} with a reason when GAMS or gamstransfer is unavailable.
#'
#' @seealso \code{\link{iterativePFM}}, \code{COUPLING.md} §7 and §11, ADR 0042
#' @export
#' @author Renato Rodrigues
pfmReplayInterface <- function(remindDir = getOption("pfm.remindDir", "../remind_pfm"),
                               dir = file.path(tempdir(), "pfm-replay-interface"),
                               negativeControl = TRUE,
                               gams = Sys.which("gams"),
                               quiet = FALSE) {
  say <- function(...) if (!quiet) message("[replay] ", ...)
  skip <- function(why) {
    say("SKIPPED: ", why)
    invisible(list(ok = NA, skipped = why, dir = dir))
  }
  if (!requireNamespace("gamstransfer", quietly = TRUE)) {
    return(skip("the 'gamstransfer' package is not installed"))
  }
  if (!nzchar(gams) || !file.exists(gams)) {
    return(skip("no GAMS executable found - pass gams= or put it on PATH"))
  }
  declGms <- file.path(remindDir, "modules", "45_carbonprice", "functionalForm",
                       "declarations.gms")
  if (!file.exists(declGms)) {
    return(skip(paste0("declarations.gms not found at ", declGms,
                       " - set options(pfm.remindDir=)")))
  }
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)

  regs <- c("EUR", "USA", "CHA")
  yrs  <- c(2030, 2050)
  mkts <- c("ETS", "ES", "other")
  ttot <- c(2005, 2010, 2020, yrs, 2100, 2150)
  iter <- 15

  fx <- .psmReplayFixture(regs, yrs)
  syms <- .psmReplaySymbols(fx, regs, iter)
  gdxFile <- file.path(dir, "p45_regiDiff_phi.gdx")
  .psmWriteCouplingGdx(gdxFile, syms)
  say("wrote ", length(syms), " symbols to ", basename(gdxFile))

  # Declarations are LIFTED from the module, never restated: a stub that declares its own
  # idea of a symbol would pass happily while the real model fails.
  decls <- .psmReplayDeclarations(declGms)
  say("lifted ", length(decls), " declarations verbatim from declarations.gms")

  stub <- .psmReplayStub(dir, decls, fx, regs, ttot, mkts, iter)
  pos <- .psmReplayRunGams(gams, dir, stub)
  say("positive replay: ", if (pos$ok) "OK" else paste0("FAILED - ", pos$verdict))

  neg <- NULL
  if (isTRUE(negativeControl)) {
    negDir <- file.path(dir, "negctl")
    dir.create(negDir, showWarnings = FALSE, recursive = TRUE)
    .psmReplayTransposedGdx(file.path(negDir, "p45_regiDiff_phi.gdx"), fx, regs, ttot,
                            mkts, iter)
    file.copy(stub, file.path(negDir, basename(stub)), overwrite = TRUE)
    neg <- .psmReplayRunGams(gams, negDir, file.path(negDir, basename(stub)))
    # Inverted on purpose: the control PASSES when GAMS aborts.
    neg$ok <- !isTRUE(neg$loadedCleanly)
    say("negative control: ", if (neg$ok) "OK - the transposed symbol was caught"
        else "FAILED - a transposed rank-3 symbol went UNDETECTED")
  }

  ok <- isTRUE(pos$ok) && (is.null(neg) || isTRUE(neg$ok))
  invisible(list(ok = ok, positive = pos, negative = neg, dir = dir,
                 nDeclarations = length(decls), skipped = NULL))
}

#' @keywords internal
#' @rdname pfmReplayInterface
.psmReplayFixture <- function(regs, yrs) {
  mkBnd <- function(base) {
    d <- expand.grid(year = yrs, region = regs, stringsAsFactors = FALSE)
    d$priceBound <- base + seq_len(nrow(d))   # every cell distinct, so a transposed
    d                                          # load cannot coincidentally match
  }
  bnd <- list(Bulk = mkBnd(100), Diffuse = mkBnd(200))
  list(
    phi = stats::setNames(c(0.7, 0.45, 0.9), regs),
    phiSector = list(Bulk    = stats::setNames(c(0.80, 0.60, 0.55), regs),
                     Diffuse = stats::setNames(c(0.50, 0.95, 0.70), regs)),
    lamSector = list(Bulk    = stats::setNames(rep(0.1023, length(regs)), regs),
                     Diffuse = stats::setNames(rep(0.0770, length(regs)), regs)),
    bndSector = bnd,
    mpSector  = lapply(bnd, function(d) { names(d)[names(d) == "priceBound"] <- "price"; d }),
    bnd = mkBnd(300))
}

#' @keywords internal
#' @rdname pfmReplayInterface
.psmReplaySymbols <- function(fx, regs, iter) {
  mp1 <- fx$mpSector$Bulk
  list(
    .psmCouplingSym1d("p45_regiDiff_phi", fx$phi),
    .psmCouplingSym1d("p45_pfmDelta",    stats::setNames(rep(0.004, length(regs)), regs)),
    .psmCouplingSym1d("p45_pfmIterSeen", stats::setNames(rep(iter, length(regs)), regs)),
    .psmCouplingSym2d("p45_pfmPriceBound", fx$bnd, "priceBound"),
    .psmCouplingSym2d("p45_pfmMPPrice",    mp1,    "price"),
    .psmCouplingSymMkt1d("p45_pfmPhiMkt",    fx$phiSector),
    .psmCouplingSymMkt1d("p45_pfmLambdaMkt", fx$lamSector),
    .psmCouplingSymMkt2d("p45_pfmPriceBoundMkt", fx$bndSector, "priceBound"),
    .psmCouplingSymMkt2d("p45_pfmMPPriceMkt",    fx$mpSector,  "price"))
}

#' @keywords internal
#' @rdname pfmReplayInterface
.psmReplayDeclarations <- function(declGms) {
  lines <- readLines(declGms, warn = FALSE)
  need <- c("p45_regiDiff_phi\\(", "p45_regiDiff_phi_aux", "p45_pfmDelta_aux",
            "p45_pfmIterSeen_aux", "p45_pfmPriceBound\\(", "p45_pfmPriceBound_aux",
            "p45_pfmMPPrice\\(", "p45_pfmMPPrice_aux",
            "p45_pfmPhiMkt\\(", "p45_pfmPhiMkt_aux",
            "p45_pfmLambdaMkt\\(", "p45_pfmLambdaMkt_aux",
            "p45_pfmPriceBoundMkt\\(", "p45_pfmPriceBoundMkt_aux",
            "p45_pfmMPPriceMkt\\(", "p45_pfmMPPriceMkt_aux")
  vapply(need, function(p) {
    hit <- grep(paste0("^\\s*", p), lines, value = TRUE)
    if (!length(hit)) {
      stop("pfmReplayInterface: '", sub("\\\\\\(", "", p),
           "' is not declared in declarations.gms. Either the symbol was renamed and this ",
           "replay was not updated, or the module is out of date.", call. = FALSE)
    }
    trimws(hit[1])
  }, character(1), USE.NAMES = FALSE)
}

#' @keywords internal
#' @rdname pfmReplayInterface
.psmReplayStub <- function(dir, decls, fx, regs, ttot, mkts, iter) {
  chk <- function(name, expr, want) {
    sprintf(paste0("s_chk = %s;\nif (abs(s_chk - %.10g) > 1e-6,\n",
                   "  display s_chk;\n  abort 'MISMATCH: %s';\n);"),
            expr, want, name)
  }
  b <- fx$bndSector$Bulk
  d <- fx$bndSector$Diffuse
  sumPhi <- sum(.psmCouplingSymMkt1d("x", fx$phiSector)$records$value)
  sumBnd <- sum(.psmCouplingSymMkt2d("x", fx$bndSector, "priceBound")$records$value)
  L <- c(
    "$title PFM coupling interface replay (generated by pfm::pfmReplayInterface)",
    "$offlisting",
    paste0("set all_regi / ", paste(regs, collapse = ", "), " /;"),
    paste0("set ttot / ", paste(ttot, collapse = ", "), " /;"),
    paste0("set all_emiMkt / ", paste(mkts, collapse = ", "), " /;"),
    "alias(all_regi, regi);",
    "alias(all_emiMkt, emiMkt);",
    "parameters",
    paste0("  ", decls),
    "  s_delta, s_iterSeen, s_chk",
    ";",
    "*** the exact statements presolve.gms executes after the Rscript call.",
    "*** The stamp is loaded FIRST, as presolve.gms does since 2026-08-17.",
    "Execute_Loadpoint 'p45_regiDiff_phi' p45_pfmIterSeen_aux = p45_pfmIterSeen;",
    "Execute_Loadpoint 'p45_regiDiff_phi' p45_regiDiff_phi_aux = p45_regiDiff_phi;",
    "Execute_Loadpoint 'p45_regiDiff_phi' p45_pfmDelta_aux = p45_pfmDelta;",
    "Execute_Loadpoint 'p45_regiDiff_phi' p45_pfmPriceBound_aux = p45_pfmPriceBound;",
    "Execute_Loadpoint 'p45_regiDiff_phi' p45_pfmMPPrice_aux = p45_pfmMPPrice;",
    "*** the ADR 0042 per-market companions - rank 2 and rank 3",
    "Execute_Loadpoint 'p45_regiDiff_phi' p45_pfmPhiMkt_aux = p45_pfmPhiMkt;",
    "Execute_Loadpoint 'p45_regiDiff_phi' p45_pfmLambdaMkt_aux = p45_pfmLambdaMkt;",
    "Execute_Loadpoint 'p45_regiDiff_phi' p45_pfmPriceBoundMkt_aux = p45_pfmPriceBoundMkt;",
    "Execute_Loadpoint 'p45_regiDiff_phi' p45_pfmMPPriceMkt_aux = p45_pfmMPPriceMkt;",
    "",
    "s_iterSeen = sum(regi, p45_pfmIterSeen_aux(regi)) / max(1, card(regi));",
    sprintf("if (abs(s_iterSeen - %d) > 0.5, abort 'STALE gdx: wrong iteration stamp');",
            iter),
    "",
    "*** VALUES, not just execError. A transposed symbol has the right rank, raises NO",
    "*** error, and loads as ( ALL 0.000 ) - only the cells can catch that.",
    chk("phiMkt EUR/ETS",   "p45_pfmPhiMkt_aux('EUR','ETS')",   fx$phiSector$Bulk[["EUR"]]),
    chk("phiMkt EUR/ES",    "p45_pfmPhiMkt_aux('EUR','ES')",    fx$phiSector$Diffuse[["EUR"]]),
    chk("phiMkt EUR/other", "p45_pfmPhiMkt_aux('EUR','other')", fx$phiSector$Diffuse[["EUR"]]),
    chk("lambdaMkt USA/ETS", "p45_pfmLambdaMkt_aux('USA','ETS')", fx$lamSector$Bulk[["USA"]]),
    chk("lambdaMkt USA/ES",  "p45_pfmLambdaMkt_aux('USA','ES')",  fx$lamSector$Diffuse[["USA"]]),
    chk("bndMkt 2050/CHA/ETS", "p45_pfmPriceBoundMkt_aux('2050','CHA','ETS')",
        b$priceBound[b$year == 2050 & b$region == "CHA"]),
    chk("bndMkt 2030/USA/ES", "p45_pfmPriceBoundMkt_aux('2030','USA','ES')",
        d$priceBound[d$year == 2030 & d$region == "USA"]),
    "*** totals, so a partial load that happens to get the probed cells right still fails",
    chk("phiMkt total", "sum((regi,emiMkt), p45_pfmPhiMkt_aux(regi,emiMkt))", sumPhi),
    chk("bndMkt total",
        "sum((ttot,regi,emiMkt), p45_pfmPriceBoundMkt_aux(ttot,regi,emiMkt))", sumBnd),
    "*** stated separately so an all-zero load is named as such in the listing",
    "if (sum((regi,emiMkt), p45_pfmPhiMkt_aux(regi,emiMkt)) < 1e-9,",
    "  abort 'ALL-ZERO: p45_pfmPhiMkt loaded nothing - rank/order mismatch';",
    ");",
    "if (sum((ttot,regi,emiMkt), p45_pfmMPPriceMkt_aux(ttot,regi,emiMkt)) < 1e-9,",
    "  abort 'ALL-ZERO: p45_pfmMPPriceMkt loaded nothing - rank/order mismatch';",
    ");",
    "",
    "if (execError > 0,",
    "  abort 'REPLAY FAILED: Execute_Loadpoint raised execution errors - see the .lst';",
    ");",
    "display 'REPLAY OK - GAMS loaded every symbol, including rank 3, with correct values';")
  f <- file.path(dir, "replay_interface.gms")
  writeLines(L, f)
  f
}

#' @keywords internal
#' @rdname pfmReplayInterface
.psmReplayTransposedGdx <- function(file, fx, regs, ttot, mkts, iter) {
  m  <- gamstransfer::Container$new()
  sr <- m$addSet("all_regi",   records = regs)
  st <- m$addSet("ttot",       records = as.character(ttot))
  sk <- m$addSet("all_emiMkt", records = mkts)
  one <- function(nm, v) m$addParameter(nm, domain = list(sr),
    records = data.frame(all_regi = regs, value = v, stringsAsFactors = FALSE))
  one("p45_regiDiff_phi", unname(fx$phi))
  one("p45_pfmDelta",     rep(0.004, length(regs)))
  one("p45_pfmIterSeen",  rep(iter, length(regs)))
  yrs <- sort(unique(fx$bnd$year))
  g2 <- expand.grid(ttot = as.character(yrs), all_regi = regs, stringsAsFactors = FALSE)
  m$addParameter("p45_pfmPriceBound", domain = list(st, sr),
                 records = cbind(g2, value = 300 + seq_len(nrow(g2))))
  m$addParameter("p45_pfmMPPrice", domain = list(st, sr),
                 records = cbind(g2, value = 100 + seq_len(nrow(g2))))
  gm <- expand.grid(all_regi = regs, all_emiMkt = mkts, stringsAsFactors = FALSE)
  gm$value <- ifelse(gm$all_emiMkt == "ETS", fx$phiSector$Bulk[gm$all_regi],
                     fx$phiSector$Diffuse[gm$all_regi])
  m$addParameter("p45_pfmPhiMkt", domain = list(sr, sk), records = gm)
  gl <- gm
  gl$value <- ifelse(gl$all_emiMkt == "ETS", fx$lamSector$Bulk[[1]], fx$lamSector$Diffuse[[1]])
  m$addParameter("p45_pfmLambdaMkt", domain = list(sr, sk), records = gl)
  g3 <- expand.grid(ttot = as.character(yrs), all_regi = regs, all_emiMkt = mkts,
                    stringsAsFactors = FALSE)
  m$addParameter("p45_pfmMPPriceMkt", domain = list(st, sr, sk),
                 records = cbind(g3, value = 100 + seq_len(nrow(g3))))
  # >>> THE DEFECT: right rank, right domain sets, wrong ORDER. GAMS reports nothing.
  bad <- expand.grid(all_regi = regs, ttot = as.character(yrs), all_emiMkt = mkts,
                     stringsAsFactors = FALSE)
  m$addParameter("p45_pfmPriceBoundMkt", domain = list(sr, st, sk),
                 records = cbind(bad, value = 100 + seq_len(nrow(bad))))
  m$write(file)
  invisible(file)
}

#' @keywords internal
#' @rdname pfmReplayInterface
.psmReplayRunGams <- function(gams, dir, stub) {
  old <- setwd(dir)
  on.exit(setwd(old), add = TRUE)
  lstName <- "replay_interface.lst"
  rc <- suppressWarnings(system2(gams, c(basename(stub), "lo=2", paste0("-o=", lstName)),
                                 stdout = TRUE, stderr = TRUE))
  status <- attr(rc, "status")
  if (is.null(status)) status <- 0L
  lst <- file.path(dir, lstName)
  lines <- if (file.exists(lst)) readLines(lst, warn = FALSE) else character(0)
  loadedCleanly <- any(grepl("REPLAY OK", lines, fixed = TRUE))
  verdict <- grep("MISMATCH|ALL-ZERO|STALE gdx|REPLAY FAILED|REPLAY OK", lines, value = TRUE)
  list(ok = identical(as.integer(status), 0L) && loadedCleanly,
       loadedCleanly = loadedCleanly, status = as.integer(status),
       lst = lst, verdict = utils::head(trimws(verdict), 6))
}
# nolint end
