#' Calculate Actor Power Index
#'
#' @description
#' Calculates the Actor Power Index. The index evaluates the net "Green Push"
#' (Innovator Power) minus the "Legacy Power" (Incumbent Power).
#' Drivers are extracted dynamically from `downscaleREMINDResults`.
#'
#' @param data A `magpie` object with calculated drivers.
#' @param coeff List with weights for Innovator and Incumbent index calculation.
#' Default weights apply the predefined Bulk and Diffuse schema.
#'
#' @return A `magpie` object with the calculated indices.
#' @author Renato Rodrigues
#'
#' @importFrom magclass new.magpie getRegions getYears getNames mbind setNames
#'
#' @export
actorPowerIndex <- function(
    data,
    coeff = list(
      bulk = list(
        actor_power = list(innov = 1, incumb = 1),
        innovators_power = list(vre = 1, elec = 0.6),
        incumbents_power = list(coal = 1, oilgas = 1, fossilInd = 0.5)
      ),
      diffuse = list(
        actor_power = list(innov = 1, incumb = 1),
        innovators_power = list(vre = 0.5, elec = 1),
        incumbents_power = list(coal = 0.2, oilgas = 0.2, fossilInd = 1)
      )
    )) {
  if (is.null(data)) {
    return(NULL)
  }

  # Extract individual components
  coal <- data[, , "Coal primary energy share"]
  oilgas <- data[, , "Oil/Gas primary energy share"]
  fossilInd <- data[, , "Fossil share in Industry"]
  vre <- data[, , "VRE share"]
  elec <- data[, , "Electrification"]

  # Pre-allocate output arrays for indices across Sectors ("Bulk", "Diffuse")
  outNames <- c(
    "Actor Power Index|Bulk", "Actor Power Index|Diffuse",
    "Innovator Power|Bulk", "Innovator Power|Diffuse",
    "Incumbent Power|Bulk", "Incumbent Power|Diffuse"
  )
  out <- new.magpie(
    cells_and_regions = getRegions(data), years = getYears(data),
    names = outNames, fill = 0
  )

  sumInnovBulk <- coeff$bulk$innovators_power$vre + coeff$bulk$innovators_power$elec
  sumIncumbBulk <- coeff$bulk$incumbents_power$coal + coeff$bulk$incumbents_power$oilgas +
    coeff$bulk$incumbents_power$fossilInd

  sumInnovDiffuse <- coeff$diffuse$innovators_power$vre + coeff$diffuse$innovators_power$elec
  sumIncumbDiffuse <- coeff$diffuse$incumbents_power$coal + coeff$diffuse$incumbents_power$oilgas +
    coeff$diffuse$incumbents_power$fossilInd

  # --- Calculate Innovator Power ---
  out[, , "Innovator Power|Bulk"] <-
    ((coeff$bulk$innovators_power$vre * vre) + (coeff$bulk$innovators_power$elec * elec)) /
      sumInnovBulk
  out[, , "Innovator Power|Diffuse"] <-
    ((coeff$diffuse$innovators_power$vre * vre) + (coeff$diffuse$innovators_power$elec * elec)) /
      sumInnovDiffuse

  # --- Calculate Incumbent Power ---
  out[, , "Incumbent Power|Bulk"] <-
    ((coeff$bulk$incumbents_power$coal * coal) + (coeff$bulk$incumbents_power$oilgas * oilgas) +
      (coeff$bulk$incumbents_power$fossilInd * fossilInd)) / sumIncumbBulk
  out[, , "Incumbent Power|Diffuse"] <-
    ((coeff$diffuse$incumbents_power$coal * coal) + (coeff$diffuse$incumbents_power$oilgas * oilgas) +
      (coeff$diffuse$incumbents_power$fossilInd * fossilInd)) / sumIncumbDiffuse

  # --- Calculate overall Actor Power Index ---
  out[, , "Actor Power Index|Bulk"] <-
    coeff$bulk$actor_power$innov * out[, , "Innovator Power|Bulk"] -
    coeff$bulk$actor_power$incumb * out[, , "Incumbent Power|Bulk"]
  out[, , "Actor Power Index|Diffuse"] <-
    coeff$diffuse$actor_power$innov * out[, , "Innovator Power|Diffuse"] -
    coeff$diffuse$actor_power$incumb * out[, , "Incumbent Power|Diffuse"]

  return(out)
}
