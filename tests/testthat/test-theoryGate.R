# The theory gate (design note 0007 section 15): H1 structural power, H2 contingency, H3 coherence.

coefRows <- function(model, sector, terms) {
  data.frame(model = model, sector = sector, term = names(terms), estimate = vapply(terms, `[[`, 0, 1),
             pValue = vapply(terms, `[[`, 0, 2), stringsAsFactors = FALSE)
}

test_that("H1 needs a negative, significant fossil effect in every sector; H2 a fossil x institution term past Bonferroni", {
  good <- list(Incumbent.Power = c(-0.6, 0.01), Incumbent.Power_x_Rule.of.Law..VDem. = c(0.7, 0.001),
               Incumbent.Power_x_Vertical.Accountability..VDem. = c(-0.5, 0.2), Innovator.Power = c(-0.2, 0.3))
  cf <- rbind(coefRows("A", "Bulk", good), coefRows("A", "Diffuse", good),
              # B: fossil positive in one sector (H1)
              coefRows("B", "Bulk", good), coefRows("B", "Diffuse", replace(good, "Incumbent.Power", list(c(0.1, 0.01)))),
              # C: interactions only at p 0.03 with two terms -> 0.03 > 0.05 / 2 (H2)
              coefRows("C", "Bulk", good),
              coefRows("C", "Diffuse", list(Incumbent.Power = c(-0.6, 0.01), Incumbent.Power_x_Rule.of.Law..VDem. = c(0.7, 0.03),
                                            Incumbent.Power_x_Vertical.Accountability..VDem. = c(-0.5, 0.03))),
              # D: per-person form only, which then carries H1 and H2
              coefRows("D", "Bulk", list(Incumbent.Power.pc = c(-0.3, 0.02), Incumbent.Power.pc_x_Rule.of.Law..VDem. = c(1, 0.001))),
              coefRows("D", "Diffuse", list(Incumbent.Power.pc = c(-0.3, 0.02), Incumbent.Power.pc_x_Rule.of.Law..VDem. = c(1, 0.001))))
  v <- .pfmTheoryScreen(cf, c("A", "B", "C", "D", "E"), c("Bulk", "Diffuse"))
  expect_identical(v$pass, c(TRUE, FALSE, FALSE, TRUE, FALSE))
  expect_match(v$reason[2], "Diffuse: H1 fossil \\+0.10")
  expect_match(v$reason[3], "Diffuse: H2")
  expect_match(v$reason[5], "not fitted")
  # the sign of the interaction is free; switching H2 off passes C
  expect_true(.pfmTheoryScreen(cf, "C", c("Bulk", "Diffuse"), list(contingency = FALSE))$pass)
  # both fossil terms count for H2: a per-person interaction past Bonferroni carries a share-form spec
  e <- list(Incumbent.Power = c(-0.6, 0.01), Incumbent.Power_x_Rule.of.Law..VDem. = c(0.7, 0.2),
            Incumbent.Power.pc = c(0.4, 0.01), Incumbent.Power.pc_x_Government.Effectiveness..WGI. = c(-0.5, 0.001))
  cfE <- rbind(coefRows("E", "Bulk", e), coefRows("E", "Diffuse", e))
  expect_true(.pfmTheoryScreen(cfE, "E", c("Bulk", "Diffuse"))$pass)
  # the clean-energy term carries no requirement (A's is negative and not significant)
})

test_that("H3 is the share of rows where the fossil effect, interactions included and weighted, is negative", {
  df <- data.frame(Incumbent.Power = 0, Rule.of.Law..VDem. = c(-2, -1, 0, 1, 2), interactionWeight__ = 1)
  b <- c(Incumbent.Power = -0.5, Incumbent.Power_x_Rule.of.Law..VDem. = 0.4)
  expect_equal(.pfmTheoryCoherence(b, df)$share, 0.8)          # -1.3, -0.9, -0.5, -0.1, +0.3
  df$interactionWeight__ <- c(1, 1, 1, 1, 0)                    # the weight switches the last row's interaction off
  expect_equal(.pfmTheoryCoherence(b, df)$share, 1)
  expect_identical(.pfmTheoryCoherence(c(Incumbent.Power.pc = -1), data.frame(Incumbent.Power.pc = 1))$driver, "Incumbent.Power.pc")
})

test_that("sweep options parse the theory gate; the job script keeps it; the walk flags H3", {
  o <- .pfmSweepOptionsNormalise(list(theoryGate = list(coherence = 0.8)))
  expect_equal(o$theoryGate$coherence, 0.8)
  expect_equal(o$theoryGate$p, 0.05)
  expect_null(.pfmSweepOptionsNormalise(list(theoryGate = "off"))$theoryGate)
  expect_error(.pfmSweepOptionsNormalise(list(theoryGate = list(bogus = 1))), "unknown theoryGate")
  expect_true("theoryGate" %in% .pfmSweepOptionKeys)
  expect_match(.pfmSweepOptionsLabel(o), "theory gate \\(p 0.05")
  expect_identical(eval(parse(text = .rlit(o$theoryGate))), o$theoryGate)
  src <- paste(deparse(body(.pfmSanitySelect)), collapse = "\n")
  expect_match(src, "theoryCoherence", fixed = TRUE)
  expect_match(paste(deparse(body(runPFMSweep)), collapse = "\n"), ".pfmTheoryScreen(grid$coefficients", fixed = TRUE)
})
