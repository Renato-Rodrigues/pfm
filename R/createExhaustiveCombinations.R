#' @title createExhaustiveCombinations
#' @description Generates all combinations of base model specifications,
#' institutional quality drivers, and regional fixed-effects strategies for
#' exhaustive model selection sweeps. Returns a flat list of model config
#' objects in the same format consumed by the model-selection report.
#'
#' Default parameter values reproduce the exhaustive.yml model-selection
#' configuration (V-Dem only, no WGI institutional quality variables).
#'
#' @param baseVariants Named list of base model specs. Each element must
#'   contain: \code{name} (character), \code{actorPowerDrivers} (character
#'   vector), \code{actorPowerIndex} (character vector),
#'   \code{controlDrivers} (character vector), \code{logisticTimeTrend}
#'   (logical), \code{includeLagged} (logical), and
#'   \code{gdpGovInteraction} (logical). Defaults to 15 variants covering
#'   Composite/Split AP × Linear/Logistic trend × Standard/GDP-Q/EKC income.
#' @param iqVariants Named list of institutional quality specs. Each element
#'   must contain: \code{name} (character) and \code{instQualityDrivers}
#'   (character vector). Defaults to 8 V-Dem variants (CSP, Meritocracy,
#'   CSP+Meritocracy, State Cap Full, PC1, PC1+PC2, Full Accountability,
#'   No IQ).
#' @param feVariants Named list of fixed-effects strategies. Each element
#'   must contain: \code{name} (character),
#'   \code{regionMappingFixedEffects} (character or NULL), and
#'   \code{useMundlak} (logical). Defaults to 5 strategies: H12, EU/OECD+,
#'   No FE, Mundlak, 54 Regions.
#' @param ridgeInteractions Logical. Passed to every generated config as
#'   the \code{ridgeInteractions} field. Default: \code{FALSE}.
#'
#' @return A named list of model config lists, where each element has
#'   all fields required by \code{estimateAdoptionModel} and
#'   \code{estimatePriceStringencyModel}: \code{name}, \code{description},
#'   \code{actorPowerDrivers}, \code{actorPowerIndex},
#'   \code{instQualityDrivers}, \code{controlDrivers},
#'   \code{includeLagged}, \code{interactRegionFE},
#'   \code{regionMappingFixedEffects}, \code{useMundlak},
#'   \code{logisticTimeTrend}, \code{gdpGovInteraction},
#'   \code{ridgeInteractions}.
#'
#' @examples
#' \dontrun{
#' # Generate all ~600 combinations with default settings
#' configs <- createExhaustiveCombinations()
#' length(configs)  # 15 * 8 * 5 = 600
#'
#' # Use only split-AP logistic base specs
#' splitLogistic <- .exhaustiveDefaultBaseVariants()[
#'   grepl("Split.*Logistic", names(.exhaustiveDefaultBaseVariants()))]
#' configs_subset <- createExhaustiveCombinations(baseVariants = splitLogistic)
#'
#' # Test with ridgeInteractions enabled
#' configs_ridge <- createExhaustiveCombinations(ridgeInteractions = TRUE)
#' }
#'
#' @author Renato Rodrigues
#' @export
createExhaustiveCombinations <- function(
    baseVariants  = .exhaustiveDefaultBaseVariants(),
    iqVariants    = .exhaustiveDefaultIQVariants(),
    feVariants    = .exhaustiveDefaultFEVariants(),
    ridgeInteractions = FALSE) {

  configs <- list()

  for (p1 in baseVariants) {
    for (p2 in iqVariants) {
      for (p3 in feVariants) {
        cfg_name <- paste(p1$name, "+", p2$name, "+", p3$name)
        cfg <- list(
          name = cfg_name,
          description = paste0(
            "Auto-generated: ", p1$name,
            " | IQ: ", p2$name,
            " | FE: ", p3$name
          ),
          actorPowerDrivers        = p1$actorPowerDrivers,
          actorPowerIndex          = p1$actorPowerIndex,
          instQualityDrivers       = p2$instQualityDrivers,
          controlDrivers           = p1$controlDrivers,
          includeLagged            = isTRUE(p1$includeLagged),
          interactRegionFE         = FALSE,
          regionMappingFixedEffects = p3$regionMappingFixedEffects,
          useMundlak               = isTRUE(p3$useMundlak),
          logisticTimeTrend        = isTRUE(p1$logisticTimeTrend),
          gdpGovInteraction        = isTRUE(p1$gdpGovInteraction),
          ridgeInteractions        = isTRUE(ridgeInteractions)
        )
        configs <- c(configs, list(cfg))
      }
    }
  }

  configs
}

# ── Default variant definitions ───────────────────────────────────────────────

#' @keywords internal
.exhaustiveDefaultBaseVariants <- function() {
  std    <- c("GDP per Capita", "Hydro Nuclear Share")
  gdpq   <- c("GDP per Capita (Q-centred)", "Hydro Nuclear Share")
  nohydro <- "GDP per Capita"
  ekc    <- c("GDP per Capita", "GDP per Capita Sq", "Hydro Nuclear Share")
  composite <- list(actorPowerDrivers = "Actor Power Index",
                    actorPowerIndex   = "Actor Power Index")
  split     <- list(actorPowerDrivers = c("Innovator Power", "Incumbent Power"),
                    actorPowerIndex   = c("Innovator Power", "Incumbent Power"))

  list(
    # Group A: Composite AP, Linear trend
    "A1 Composite Linear Standard" = c(composite, list(
      name = "A1 Composite Linear Standard",
      controlDrivers = std, logisticTimeTrend = FALSE,
      includeLagged = FALSE, gdpGovInteraction = FALSE)),
    "A2 Composite Linear No-Hydro" = c(composite, list(
      name = "A2 Composite Linear No-Hydro",
      controlDrivers = nohydro, logisticTimeTrend = FALSE,
      includeLagged = FALSE, gdpGovInteraction = FALSE)),
    "A3 Composite Linear EKC" = c(composite, list(
      name = "A3 Composite Linear EKC",
      controlDrivers = ekc, logisticTimeTrend = FALSE,
      includeLagged = FALSE, gdpGovInteraction = FALSE)),
    "A4 Composite Linear GDP-Q" = c(composite, list(
      name = "A4 Composite Linear GDP-Q",
      controlDrivers = gdpq, logisticTimeTrend = FALSE,
      includeLagged = FALSE, gdpGovInteraction = FALSE)),
    "A5 Composite Linear Lagged" = c(composite, list(
      name = "A5 Composite Linear Lagged",
      controlDrivers = std, logisticTimeTrend = FALSE,
      includeLagged = TRUE, gdpGovInteraction = FALSE)),

    # Group B: Split AP, Linear trend
    "B1 Split AP Linear Standard" = c(split, list(
      name = "B1 Split AP Linear Standard",
      controlDrivers = std, logisticTimeTrend = FALSE,
      includeLagged = FALSE, gdpGovInteraction = FALSE)),
    "B2 Split AP Linear GDP-Q" = c(split, list(
      name = "B2 Split AP Linear GDP-Q",
      controlDrivers = gdpq, logisticTimeTrend = FALSE,
      includeLagged = FALSE, gdpGovInteraction = FALSE)),
    "B3 Split AP Linear Lagged" = c(split, list(
      name = "B3 Split AP Linear Lagged",
      controlDrivers = std, logisticTimeTrend = FALSE,
      includeLagged = TRUE, gdpGovInteraction = FALSE)),

    # Group C: Composite AP, Logistic trend
    "C1 Composite Logistic Standard" = c(composite, list(
      name = "C1 Composite Logistic Standard",
      controlDrivers = std, logisticTimeTrend = TRUE,
      includeLagged = FALSE, gdpGovInteraction = FALSE)),
    "C2 Composite Logistic GDP-Q" = c(composite, list(
      name = "C2 Composite Logistic GDP-Q",
      controlDrivers = gdpq, logisticTimeTrend = TRUE,
      includeLagged = FALSE, gdpGovInteraction = FALSE)),
    "C3 Composite Logistic No-Hydro" = c(composite, list(
      name = "C3 Composite Logistic No-Hydro",
      controlDrivers = nohydro, logisticTimeTrend = TRUE,
      includeLagged = FALSE, gdpGovInteraction = FALSE)),
    "C4 Composite Logistic Lagged" = c(composite, list(
      name = "C4 Composite Logistic Lagged",
      controlDrivers = std, logisticTimeTrend = TRUE,
      includeLagged = TRUE, gdpGovInteraction = FALSE)),

    # Group D: Split AP, Logistic trend
    "D1 Split AP Logistic Standard" = c(split, list(
      name = "D1 Split AP Logistic Standard",
      controlDrivers = std, logisticTimeTrend = TRUE,
      includeLagged = FALSE, gdpGovInteraction = FALSE)),
    "D2 Split AP Logistic GDP-Q" = c(split, list(
      name = "D2 Split AP Logistic GDP-Q",
      controlDrivers = gdpq, logisticTimeTrend = TRUE,
      includeLagged = FALSE, gdpGovInteraction = FALSE)),
    "D3 Split AP Logistic No-Hydro" = c(split, list(
      name = "D3 Split AP Logistic No-Hydro",
      controlDrivers = nohydro, logisticTimeTrend = TRUE,
      includeLagged = FALSE, gdpGovInteraction = FALSE))
  )
}

#' @keywords internal
.exhaustiveDefaultIQVariants <- function() {
  list(
    "VDem CSP" = list(
      name = "VDem CSP",
      instQualityDrivers = "Civil Service Professionalism (VDem)"),
    "VDem Meritocracy" = list(
      name = "VDem Meritocracy",
      instQualityDrivers = "Meritocracy Index (VDem)"),
    "VDem CSP+Meritocracy" = list(
      name = "VDem CSP+Meritocracy",
      instQualityDrivers = c("Civil Service Professionalism (VDem)",
                             "Meritocracy Index (VDem)")),
    "VDem State Cap Full" = list(
      name = "VDem State Cap Full",
      instQualityDrivers = c("Civil Service Professionalism (VDem)",
                             "Policy Implementation (VDem)",
                             "Rule Predictability (VDem)",
                             "Absence of Corruption (VDem)",
                             "Meritocracy Index (VDem)")),
    "VDem PC1" = list(
      name = "VDem PC1",
      instQualityDrivers = "State Capacity PC1 (VDem)"),
    "VDem PC1+PC2" = list(
      name = "VDem PC1+PC2",
      instQualityDrivers = c("State Capacity PC1 (VDem)",
                             "State Capacity PC2 (VDem)")),
    "VDem Accountability" = list(
      name = "VDem Accountability",
      instQualityDrivers = c("Rule of Law (VDem)",
                             "Vertical Accountability (VDem)",
                             "Horizontal Accountability (VDem)",
                             "Diagonal Accountability (VDem)")),
    "No IQ" = list(
      name = "No IQ",
      instQualityDrivers = character(0))
  )
}

#' @keywords internal
.exhaustiveDefaultFEVariants <- function() {
  list(
    "H12" = list(
      name = "H12",
      regionMappingFixedEffects = "regionmappingH12.csv",
      useMundlak = FALSE),
    "EU/OECD+" = list(
      name = "EU/OECD+",
      regionMappingFixedEffects = "regionmapping_EU_OECDp.csv",
      useMundlak = FALSE),
    "No FE" = list(
      name = "No FE",
      regionMappingFixedEffects = NULL,
      useMundlak = FALSE),
    "Mundlak" = list(
      name = "Mundlak",
      regionMappingFixedEffects = NULL,
      useMundlak = TRUE),
    "54 Regions" = list(
      name = "54 Regions",
      regionMappingFixedEffects = "regionmapping_54.csv",
      useMundlak = FALSE)
  )
}
