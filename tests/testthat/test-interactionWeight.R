# The interaction weight (design note 0007, O10) and the edge-of-data gate.

makeWeightMagpie <- function() {
  set.seed(7)
  regions <- paste0("R", 1:12)
  years <- 2000:2015
  vars <- c("Effective Carbon Price|Bulk", "Incumbent Power|Bulk", "Rule of Law (VDem)",
            "Electoral Democracy (VDem)")
  m <- magclass::new.magpie(regions, years, vars, fill = NA)
  for (v in vars) m[, , v] <- runif(length(regions) * length(years))
  m[, , "Effective Carbon Price|Bulk"] <- runif(length(regions) * length(years)) * 50
  m
}

prepW <- function(m, weight = NULL, scaling = NULL) {
  preparePanelData(
    m, sector = "Bulk",
    actorPowerDrivers = "Incumbent Power", actorPowerIndex = "Incumbent Power",
    instQualityDrivers = "Rule of Law (VDem)", controlDrivers = NULL,
    regionMappingFixedEffects = NULL, driverScaling = scaling, interactionWeight = weight,
    excludeCountries = character(0)
  )
}

test_that("without a weight the interaction is the plain product and no weight column is added", {
  df <- prepW(makeWeightMagpie())
  expect_false("interactionWeight__" %in% colnames(df))
  expect_null(attr(attr(df, "driverScaling"), "interactionWeight"))
  expect_equal(df$Incumbent.Power_x_Rule.of.Law..VDem., df$Incumbent.Power * df$Rule.of.Law..VDem.)
})

test_that("a weight multiplies the interaction, is kept on its 0-1 scale and is recorded in the scaling", {
  m <- makeWeightMagpie()
  df <- prepW(m, weight = "Electoral Democracy (VDem)")
  expect_true("interactionWeight__" %in% colnames(df))
  expect_true(all(df$interactionWeight__ >= 0 & df$interactionWeight__ <= 1, na.rm = TRUE))
  expect_equal(df$Incumbent.Power_x_Rule.of.Law..VDem.,
               df$Incumbent.Power * df$Rule.of.Law..VDem. * df$interactionWeight__)
  expect_identical(attr(attr(df, "driverScaling"), "interactionWeight"), "Electoral Democracy (VDem)")
  # the weight is not standardised: it is not a driver
  expect_false("Electoral.Democracy..VDem." %in% names(attr(df, "driverScaling")))
})

test_that("apply mode takes the weight from the frozen scaling alone", {
  m <- makeWeightMagpie()
  fitDf <- prepW(m, weight = "Electoral Democracy (VDem)")
  applyDf <- prepW(m, scaling = attr(fitDf, "driverScaling"))
  expect_equal(applyDf$Incumbent.Power_x_Rule.of.Law..VDem., fitDf$Incumbent.Power_x_Rule.of.Law..VDem.)
})

test_that("'none' or an empty weight means no weight", {
  df <- prepW(makeWeightMagpie(), weight = "none")
  expect_false("interactionWeight__" %in% colnames(df))
})

test_that("a missing weight variable is reported like a missing driver", {
  m <- makeWeightMagpie()[, , "Electoral Democracy (VDem)", invert = TRUE]
  expect_error(prepW(m, weight = "Electoral Democracy (VDem)"), "missing from the data")
})

test_that("the driver guard keeps the weight when it recomputes the interactions", {
  df <- prepW(makeWeightMagpie(), weight = "Electoral Democracy (VDem)")
  ranges <- list(Incumbent.Power = c(min = -0.5, max = 0.5), Rule.of.Law..VDem. = c(min = -0.5, max = 0.5))
  g <- .pfmDriverGuard(df, ranges)$df
  expect_equal(g$Incumbent.Power_x_Rule.of.Law..VDem.,
               g$Incumbent.Power * g$Rule.of.Law..VDem. * g$interactionWeight__)
})

test_that("pfmSpecs adds a weighted twin of every spec with interactions, and only those", {
  specs <- list(
    list(name = "X-1 a", actorPowerDrivers = "Incumbent Power", actorPowerIndex = "Incumbent Power",
         instQualityDrivers = "Rule of Law (VDem)", controlDrivers = NULL),
    list(name = "X-2 b", actorPowerDrivers = "Incumbent Power", actorPowerIndex = NULL,
         instQualityDrivers = "Rule of Law (VDem)", controlDrivers = NULL))
  out <- pfmSpecs(specs, verbose = FALSE, apTransforms = "linear",
                  interactionWeights = c("none", "Electoral Democracy (VDem)"))
  nm <- vapply(out, `[[`, character(1), "name")
  expect_setequal(nm, c("X-1 a", "X-2 b", "X-1 a iw:poly"))
  tw <- out[[which(nm == "X-1 a iw:poly")]]
  expect_identical(tw$interactionWeight, "Electoral Democracy (VDem)")
  expect_true("interactionWeight" %in% names(.pfmSpecArgs(tw)))
  # one twin per weight, each with its own suffix
  out3 <- pfmSpecs(specs, verbose = FALSE, apTransforms = "linear",
                   interactionWeights = c("none", "Liberal Democracy (VDem)", "Freedom of Expression (VDem)"))
  expect_setequal(vapply(out3, `[[`, character(1), "name"), c("X-1 a", "X-2 b", "X-1 a iw:libdem", "X-1 a iw:fexp"))
  # the default adds nothing, as up to v6
  expect_length(pfmSpecs(specs, verbose = FALSE, apTransforms = "linear"), 2)
})

test_that("sweep options parse the weight axis and the edge gate", {
  o <- .pfmSweepOptionsNormalise(list(interactionWeights = list("none", "Electoral Democracy (VDem)"),
                                      edgeGate = list(share = 0.3)))
  expect_identical(o$interactionWeights, c("none", "Electoral Democracy (VDem)"))
  expect_equal(o$edgeGate$share, 0.3)
  expect_equal(o$edgeGate$effect, .pfmEdgeGateDefaults()$effect)
  expect_null(.pfmSweepOptionsNormalise(list(edgeGate = "off"))$edgeGate)
  expect_identical(.pfmSweepOptionsNormalise(list(edgeGate = TRUE))$edgeGate, .pfmEdgeGateDefaults())
  expect_error(.pfmSweepOptionsNormalise(list(edgeGate = list(bogus = 1))), "unknown edgeGate")
})

test_that("the panel definition carries the democracy key, off by default", {
  expect_false(.pfmPanelDefNormalise(NULL)$democracy)
  expect_true(.pfmPanelDefNormalise(list(democracy = TRUE))$democracy)
})

test_that("the edge gate flags a driver reversed by an extreme moderator and passes once it is weighted out", {
  # two countries far below the sample on the moderator reverse the fossil effect through the interaction
  set.seed(3)
  ctys <- c(paste0("C", 1:20), "CHN", "SAU")
  mkScen <- function(w) {
    d <- expand.grid(region = ctys, year = c(2025, 2100), stringsAsFactors = FALSE)
    d$Incumbent.Power <- ifelse(d$year == 2025, 1, -1)              # fossil dependence falls
    d$Innovator.Power <- ifelse(d$year == 2025, -1, 1)
    d$Vertical.Accountability..VDem. <- ifelse(d$region %in% c("CHN", "SAU"), -4, 0.2)
    d$interactionWeight__ <- if (w) ifelse(d$region %in% c("CHN", "SAU"), 0.05, 0.9) else 1
    d$Incumbent.Power_x_Vertical.Accountability..VDem. <- d$Incumbent.Power * d$Vertical.Accountability..VDem. * d$interactionWeight__
    d
  }
  train <- data.frame(Vertical.Accountability..VDem. = rnorm(200, 0, 0.5))
  beta <- c(Incumbent.Power = -0.15, Innovator.Power = 0.1, Incumbent.Power_x_Vertical.Accountability..VDem. = -0.54)
  gate <- list(mapping = NULL)
  # a toy mapping and weights stand in for the REMIND ones
  local_mocked_bindings(
    .pfmResolveCountryMap = function(mapping) stats::setNames(ifelse(ctys %in% c("CHN", "SAU"), "AUT", "DEM"), ctys),
    pfmCouplingWeights = function(...) stats::setNames(rep(1, length(ctys)), ctys))
  bad <- .pfmEdgeGate(beta, mkScen(FALSE), NULL, train, gate = gate)
  expect_false(bad$pass)
  expect_true("AUT" %in% bad$against$region)
  ok <- .pfmEdgeGate(beta, mkScen(TRUE), NULL, train, gate = gate)
  expect_true(is.null(ok$against))
})

test_that("the edge gate is not evaluable, not passed, when the final-energy weights are missing", {
  ctys <- c(paste0("C", 1:20), "CHN", "SAU")
  d <- expand.grid(region = ctys, year = c(2025, 2100), stringsAsFactors = FALSE)
  d$Incumbent.Power <- ifelse(d$year == 2025, 1, -1)
  d$Vertical.Accountability..VDem. <- ifelse(d$region %in% c("CHN", "SAU"), -4, 0.2)
  d$Incumbent.Power_x_Vertical.Accountability..VDem. <- d$Incumbent.Power * d$Vertical.Accountability..VDem.
  train <- data.frame(Vertical.Accountability..VDem. = stats::rnorm(200, 0, 0.5))
  beta <- c(Incumbent.Power = -0.15, Incumbent.Power_x_Vertical.Accountability..VDem. = -0.54)
  local_mocked_bindings(
    .pfmResolveCountryMap = function(mapping) stats::setNames(rep("DEM", length(ctys)), ctys),
    pfmCouplingWeights = function(...) stop("no madrat cache"))
  r <- .pfmEdgeGate(beta, d, NULL, train, gate = list(mapping = NULL))
  expect_true(is.na(r$pass))
  expect_match(r$detail, "not evaluable")
  expect_match(r$detail, "no madrat cache")
})

# --- the weight's scope: accountability and rule of law only (design note 0007 section 8, A7) ---------------

makeScopeMagpie <- function() {
  set.seed(11)
  regions <- paste0("R", 1:12)
  years <- 2000:2015
  vars <- c("Effective Carbon Price|Bulk", "Incumbent Power|Bulk", "Rule of Law (VDem)",
            "Government Effectiveness (WGI)", "Vertical Accountability (VDem)", "Liberal Democracy (VDem)")
  m <- magclass::new.magpie(regions, years, vars, fill = NA)
  for (v in vars) m[, , v] <- runif(length(regions) * length(years))
  m[, , "Effective Carbon Price|Bulk"] <- runif(length(regions) * length(years)) * 50
  m
}
prepScope <- function(m, weight = NULL, scaling = NULL) {
  preparePanelData(
    m, sector = "Bulk",
    actorPowerDrivers = "Incumbent Power", actorPowerIndex = "Incumbent Power",
    instQualityDrivers = c("Government Effectiveness (WGI)", "Rule of Law (VDem)", "Vertical Accountability (VDem)"),
    controlDrivers = NULL, regionMappingFixedEffects = NULL, driverScaling = scaling, interactionWeight = weight,
    excludeCountries = character(0)
  )
}

test_that("a scoped weight multiplies the accountability and rule-of-law interactions only", {
  m <- makeScopeMagpie()
  df <- prepScope(m, weight = "Liberal Democracy (VDem)@accRoL")
  w <- df$interactionWeight__
  expect_true(all(df$interactionWeightScope__ == 1))
  expect_equal(df$Incumbent.Power_x_Rule.of.Law..VDem., df$Incumbent.Power * df$Rule.of.Law..VDem. * w)
  expect_equal(df$Incumbent.Power_x_Vertical.Accountability..VDem.,
               df$Incumbent.Power * df$Vertical.Accountability..VDem. * w)
  expect_equal(df$Incumbent.Power_x_Government.Effectiveness..WGI.,
               df$Incumbent.Power * df$Government.Effectiveness..WGI.)
  # the weight itself is the one an unscoped spec reads
  expect_equal(w, prepScope(m, weight = "Liberal Democracy (VDem)")$interactionWeight__)
  expect_identical(attr(attr(df, "driverScaling"), "interactionWeight"), "Liberal Democracy (VDem)@accRoL")
  # apply mode keeps the scope
  ap <- prepScope(m, scaling = attr(df, "driverScaling"))
  expect_equal(ap$Incumbent.Power_x_Government.Effectiveness..WGI., df$Incumbent.Power_x_Government.Effectiveness..WGI.)
  expect_equal(ap$Incumbent.Power_x_Rule.of.Law..VDem., df$Incumbent.Power_x_Rule.of.Law..VDem.)
  expect_error(prepScope(m, weight = "Liberal Democracy (VDem)@bogus"), "unknown scope")
})

test_that("the driver guard keeps the scope when it recomputes the interactions", {
  df <- prepScope(makeScopeMagpie(), weight = "Liberal Democracy (VDem)@accRoL")
  ranges <- list(Incumbent.Power = c(min = -0.5, max = 0.5), Government.Effectiveness..WGI. = c(min = -0.5, max = 0.5),
                 Rule.of.Law..VDem. = c(min = -0.5, max = 0.5))
  g <- .pfmDriverGuard(df, ranges)$df
  expect_equal(g$Incumbent.Power_x_Government.Effectiveness..WGI., g$Incumbent.Power * g$Government.Effectiveness..WGI.)
  expect_equal(g$Incumbent.Power_x_Rule.of.Law..VDem., g$Incumbent.Power * g$Rule.of.Law..VDem. * g$interactionWeight__)
})

test_that("pfmSpecs names a scoped twin by weight and scope", {
  specs <- list(list(name = "X-1 a", actorPowerDrivers = "Incumbent Power", actorPowerIndex = "Incumbent Power",
                     instQualityDrivers = "Rule of Law (VDem)", controlDrivers = NULL))
  out <- pfmSpecs(specs, verbose = FALSE, apTransforms = "linear",
                  interactionWeights = c("Liberal Democracy (VDem)", "Liberal Democracy (VDem)@accRoL"))
  nm <- vapply(out, `[[`, character(1), "name")
  expect_setequal(nm, c("X-1 a", "X-1 a iw:libdem"))   # rule of law only: the scoped twin is the unscoped one
})

test_that("a scoped twin is added only where it differs from the unweighted and the unscoped twin", {
  mk <- function(nm, inst) list(name = nm, actorPowerDrivers = "Incumbent Power", actorPowerIndex = "Incumbent Power",
                                instQualityDrivers = inst, controlDrivers = NULL)
  specs <- list(mk("X-1 mixed", c("Government Effectiveness (WGI)", "Vertical Accountability (VDem)")),
                mk("X-2 allIn", c("Rule of Law (VDem)", "Vertical Accountability (VDem)")),
                mk("X-3 noneIn", "Government Effectiveness (WGI)"))
  out <- pfmSpecs(specs, verbose = FALSE, apTransforms = "linear",
                  interactionWeights = c("none", "Electoral Democracy (VDem)", "Electoral Democracy (VDem)@accRoL"))
  nm <- vapply(out, `[[`, character(1), "name")
  expect_true("X-1 mixed iw:poly@accRoL" %in% nm)
  expect_false(any(c("X-2 allIn iw:poly@accRoL", "X-3 noneIn iw:poly@accRoL") %in% nm))
  expect_true(all(c("X-2 allIn iw:poly", "X-3 noneIn iw:poly") %in% nm))
})

# --- found on v7's first sweep (2026-10-22): the gate never ran -----------------------------------------------

test_that("the job script writes the gate's thresholds as a named list", {
  g <- list(share = 0.25, effect = 0.25, capShift = 0.5, cap = c(0.05, 0.95), mapping = "regionmapping_21_EU11.csv")
  expect_identical(eval(parse(text = .rlit(g))), g)
  expect_identical(eval(parse(text = .rlit(c(2025, 2100)))), c(2025, 2100))   # atomic vectors as before
  expect_error(.pfmEdgeGate(c(Incumbent.Power = 1), data.frame(), NULL, data.frame(), gate = unlist(g)),
               "named list")
})

test_that("the gate reads the direction on the per-person forms when the spec has no share form", {
  set.seed(3)
  ctys <- c(paste0("C", 1:20), "CHN", "SAU")
  d <- expand.grid(region = ctys, year = c(2025, 2100), stringsAsFactors = FALSE)
  d$Incumbent.Power.pc <- ifelse(d$year == 2025, 1, -1)
  d$Innovator.Power.pc <- ifelse(d$year == 2025, -1, 1)
  d$Vertical.Accountability..VDem. <- ifelse(d$region %in% c("CHN", "SAU"), -4, 0.2)
  d$Incumbent.Power.pc_x_Vertical.Accountability..VDem. <- d$Incumbent.Power.pc * d$Vertical.Accountability..VDem.
  train <- data.frame(Vertical.Accountability..VDem. = rnorm(200, 0, 0.5))
  beta <- c(Incumbent.Power.pc = -0.15, Innovator.Power.pc = 0.1,
            Incumbent.Power.pc_x_Vertical.Accountability..VDem. = -0.54)
  local_mocked_bindings(
    .pfmResolveCountryMap = function(mapping) stats::setNames(ifelse(ctys %in% c("CHN", "SAU"), "AUT", "DEM"), ctys),
    pfmCouplingWeights = function(...) stats::setNames(rep(1, length(ctys)), ctys))
  bad <- .pfmEdgeGate(beta, d, NULL, train, gate = list(mapping = NULL))
  expect_false(bad$pass)
  expect_true("AUT" %in% bad$against$region)
  expect_true(all(bad$against$driver == "Incumbent.Power.pc"))
})
