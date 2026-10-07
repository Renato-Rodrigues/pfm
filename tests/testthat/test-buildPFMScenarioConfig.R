# The scenario matrix -> scenario config generator (ADR 0055). A row is its canonical parent plus the
# named deltas; references, theta suffixes, start tags and separators are generated.

localMatrix <- function(runs, extra = list(), env = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = env)
  dir.create(file.path(d, "config"))
  writeLines(c("title;start;cm_rcp_scen;regionmapping;carbonprice;cm_iterative_target_adj;path_gdx_ref;description",
               "SSP2-NPi2025;1;none;;NPi2025;;;canonical NPi",
               "SSP2-PkBudg1000;1;rcp26;;functionalForm;9;SSP2-NPi2025;canonical budget"),
             file.path(d, "config", "scenario_config.csv"))
  m <- utils::modifyList(list(
    format = "pfm-scenario-matrix/1", output = "config/out.csv", canonical = "config/scenario_config.csv",
    version = "v6", ssps = list("SSP2"), thetaDefault = "0.50",
    resolutions = list(EU21 = list(infix = "EU21", set = list(regionmapping = "./config/rm21.csv"), baseStart = list("EU21BASE")),
                       H12 = list(infix = "", set = list(), baseStart = list("H12BASE"))),
    parents = list(NPi2025 = list(from = "NPi2025", set = list()),
                   PkBudg1000 = list(from = "PkBudg1000", set = list(path_gdx_ref = "@NPi2025"))),
    coupled = list(cm_taxCO2_regiDiff = "11", cm_pfmPhiPath = "1", pfmGroup = "v6"),
    rules = list(gate = list(stem = "PFMgate", set = list(cm_pfmBindMode = "1", cm_pfmTheta = "0")),
                 Bfix = list(stem = "PFMlevelBfix", set = list(cm_iterative_target_adj = "0", cm_pfmBindMode = "2",
                                                              path_gdx_carbonprice = "@gate"))),
    runs = runs), extra)
  f <- file.path(d, "matrix.yml"); yaml::write_yaml(m, f)
  list(dir = d, matrix = f)
}
baseRuns <- list(
  list(key = "gate", wave = 1, section = "nulls", parent = "PkBudg1000", rule = "gate", start = list(EU21 = list("EU21V6GATE"))),
  list(key = "Bfix", wave = 1, section = "ruleB", parent = "PkBudg1000", rule = "Bfix", theta = list("0.50", "0.325"),
       warm = "@gate"),
  list(key = "Bheld", wave = 2, section = "held", parent = "PkBudg1000", rule = "Bfix", theta = "0.50", variant = "held",
       institutions = "hold", markup = 0, res = list("EU21"), options = list(pfmPhiHoldYear = "2060"), warm = "@Bfix"))

test_that("rows are parent + deltas, with titles, references and tags generated", {
  x <- localMatrix(baseRuns)
  df <- buildPFMScenarioConfig(x$matrix, x$dir, verbose = FALSE)
  row <- function(t) as.list(df[df$title == t, ])
  # parents carry the canonical switches, the resolution's set and their own set
  p <- row("SSP2-EU21-PkBudg1000")
  expect_identical(p$cm_rcp_scen, "rcp26")
  expect_identical(p$regionmapping, "./config/rm21.csv")
  expect_identical(p$path_gdx_ref, "SSP2-EU21-NPi2025")
  expect_identical(p$start, "EU21BASE")
  expect_identical(row("SSP2-PkBudg1000")$path_gdx_ref, "SSP2-NPi2025")    # H12: no infix
  # the gate: coupled deltas, rule, the per-resolution tag override, no theta suffix
  g <- row("SSP2-EU21-PkBudg1000-PFMgate-v6")
  expect_identical(c(g$cm_taxCO2_regiDiff, g$cm_pfmBindMode, g$cm_pfmTheta, g$cm_pfmPhiPath), c("11", "1", "0", "1"))
  expect_identical(g$start, "EU21V6GATE")
  expect_identical(row("SSP2-PkBudg1000-PFMgate-v6")$start, "V6W1H12")
  # theta expands; the default carries no suffix; @key resolves within the resolution
  b <- row("SSP2-EU21-PkBudg1000-PFMlevelBfix-v6"); b3 <- row("SSP2-EU21-PkBudg1000-PFMlevelBfixTh325-v6")
  expect_identical(c(b$cm_pfmTheta, b3$cm_pfmTheta), c("0.50", "0.325"))
  expect_identical(b$path_gdx, "SSP2-EU21-PkBudg1000-PFMgate-v6")
  expect_identical(b$path_gdx_carbonprice, "SSP2-EU21-PkBudg1000-PFMgate-v6")
  expect_identical(row("SSP2-PkBudg1000-PFMlevelBfix-v6")$path_gdx_carbonprice, "SSP2-PkBudg1000-PFMgate-v6")
  expect_identical(b$cm_iterative_target_adj, "0")
  # markup 0 -> Min, variant, institutions, options, res restriction
  h <- row("SSP2-EU21-PkBudg1000-PFMlevelBfixMin-held-v6")
  expect_identical(c(h$pfmInstitutions, h$pfmPhiHoldYear, h$cm_pfmSectorMarkup, h$path_gdx),
                   c("hold", "2060", "0", "SSP2-EU21-PkBudg1000-PFMlevelBfix-v6"))
  expect_identical(h$start, "V6W2EU21")
  expect_false(any(grepl("held", df$title[!grepl("EU21", df$title)])))
  # separators, and the file REMIND reads: CRLF, ';', one field per column
  expect_true(any(startsWith(df$title, "_____EU21_")))
  raw <- readBin(attr(df, "path"), "raw", file.size(attr(df, "path")))
  expect_true(grepl("\r\n", rawToChar(raw), fixed = TRUE))
  ln <- readLines(attr(df, "path"))
  expect_true(all(lengths(regmatches(ln, gregexpr(";", ln))) == ncol(df) - 1))
})

test_that("the generator refuses what would break or bend a batch", {
  x <- localMatrix(c(baseRuns, list(list(key = "bad", wave = 2, parent = "PkBudg1000", rule = "Bfix", theta = "0.50",
                                         variant = "cap", set = list(cm_iteration_max = "200")))))
  expect_error(buildPFMScenarioConfig(x$matrix, x$dir, write = FALSE, verbose = FALSE), "never writes cm_iteration_max")
  x <- localMatrix(list(list(key = "B", wave = 1, parent = "PkBudg1000", rule = "Bfix", warm = "@nothere")))
  expect_error(buildPFMScenarioConfig(x$matrix, x$dir, write = FALSE, verbose = FALSE), "names no run or parent")
  x <- localMatrix(list(list(key = "B", wave = 1, parent = "PkBudg1000", rule = "Bfix", description = "a; b"),
                        list(key = "gate", wave = 1, parent = "PkBudg1000", rule = "gate")))
  expect_error(buildPFMScenarioConfig(x$matrix, x$dir, write = FALSE, verbose = FALSE), "would split the row")
  x <- localMatrix(list(list(key = "gate", wave = 1, parent = "PkBudg1000", rule = "gate", set = list(cm_pfmPhiPath = "0"))))
  expect_error(buildPFMScenarioConfig(x$matrix, x$dir, write = FALSE, verbose = FALSE), "needs cm_pfmPhiPath = 1")
  x <- localMatrix(list(list(key = "gate", wave = 1, parent = "PkBudg1000", rule = "gate"),
                        list(key = "gate", wave = 1, parent = "PkBudg1000", rule = "gate", variant = "x")))
  expect_error(buildPFMScenarioConfig(x$matrix, x$dir, write = FALSE, verbose = FALSE), "duplicate run key")
})

test_that("the theta label", {
  expect_identical(pfm:::.pfmThetaLabel("0.50", "0.50"), "")
  expect_identical(pfm:::.pfmThetaLabel("0.5", "0.50"), "")
  expect_identical(pfm:::.pfmThetaLabel("0.325", "0.50"), "Th325")
  expect_identical(pfm:::.pfmThetaLabel("0.675", "0.50"), "Th675")
})
