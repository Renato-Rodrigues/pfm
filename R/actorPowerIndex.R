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
#' @param energyPerCapita Optional `magpie` object holding total primary energy per
#' capita, aligned to `data`. When supplied, per-capita variants of both indices are
#' emitted **in addition to** the shares, named `"Innovator Power pc|<sector>"` and
#' `"Incumbent Power pc|<sector>"`. A specification selects between share and
#' per-capita by naming the drivers it wants; nothing downstream changes. See
#' `design-notes/0001-actor-power-share-vs-level.md` for why both are carried.
#'
#' @return A `magpie` object with the calculated indices.
#' @author Renato Rodrigues
#'
#' @importFrom magclass new.magpie getRegions getYears getNames mbind setNames
#'
#' @export
actorPowerIndex <- function(
    data,
    energyPerCapita = NULL,
    coeff = list(
      bulk = list(
        actor_power = list(innov = 1, incumb = 1),
        innovators_power = list(vre = 1, elec = 0.6),
        incumbents_power = list(coal = 1, oilgas = 1, fossilInd = 0.5)
      ),
      diffuse = list(
        actor_power = list(innov = 1, incumb = 1),
        innovators_power = list(vre = 0.5, elec = 1, biofuel = 0.4),
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
  biofuel <- data[, , "Biofuel Displacement"]

  # Pre-allocate output arrays for indices across Sectors ("Bulk", "Diffuse")
  outNames <- c(
    "Actor Power Index|Bulk", "Actor Power Index|Diffuse",
    "Innovator Power|Bulk", "Innovator Power|Diffuse",
    "Incumbent Power|Bulk", "Incumbent Power|Diffuse"
  )
  out <- new.magpie(
    cells_and_regions = magclass::getItems(data, dim = 1), years = getYears(data),
    names = outNames, fill = 0
  )

  sumInnovBulk <- coeff$bulk$innovators_power$vre + coeff$bulk$innovators_power$elec
  sumIncumbBulk <- coeff$bulk$incumbents_power$coal + coeff$bulk$incumbents_power$oilgas +
    coeff$bulk$incumbents_power$fossilInd

  sumInnovDiffuse <- coeff$diffuse$innovators_power$vre + coeff$diffuse$innovators_power$elec +
    coeff$diffuse$innovators_power$biofuel
  sumIncumbDiffuse <- coeff$diffuse$incumbents_power$coal + coeff$diffuse$incumbents_power$oilgas +
    coeff$diffuse$incumbents_power$fossilInd

  # --- Calculate Innovator Power ---
  out[, , "Innovator Power|Bulk"] <-
    ((coeff$bulk$innovators_power$vre * vre) + (coeff$bulk$innovators_power$elec * elec)) / sumInnovBulk
  out[, , "Innovator Power|Diffuse"] <-
    ((coeff$diffuse$innovators_power$vre * vre) + (coeff$diffuse$innovators_power$elec * elec) +
     (coeff$diffuse$innovators_power$biofuel * biofuel)) / sumInnovDiffuse

  # --- Calculate Incumbent Power ---
  out[, , "Incumbent Power|Bulk"] <-
    ((coeff$bulk$incumbents_power$coal * coal) + (coeff$bulk$incumbents_power$oilgas * oilgas) +
     (coeff$bulk$incumbents_power$fossilInd * fossilInd)) / sumIncumbBulk
  out[, , "Incumbent Power|Diffuse"] <-
    ((coeff$diffuse$incumbents_power$coal * coal) + (coeff$diffuse$incumbents_power$oilgas * oilgas) +
     (coeff$diffuse$incumbents_power$fossilInd * fossilInd)) / sumIncumbDiffuse

  # --- Per-capita variants (design-notes/0001, 2026-08-24) -------------------
  # The indices above are SHARES of the energy system, so they operationalise
  # STRUCTURAL power (dependence, lock-in) and fall when a rival grows even if the
  # sector is untouched. The per-capita variants multiply by total primary energy
  # per person, giving the sector's absolute weight in the polity - INSTRUMENTAL
  # power (rents, employment, lobbying resources).
  #
  # This matters for projection, not only for theory: measured on v1, a share-based
  # incumbent index is driven ~5 SD below its estimation range by 2100 because a
  # mitigation scenario takes the share to a floor by construction; the per-capita
  # level moves ~1 SD. Both are extrapolation; one is five times further out.
  #
  # Emitted alongside the shares (never instead), so a specification chooses between
  # them by NAMING the drivers it wants - no estimator or projection change. Skipped
  # when `energyPerCapita` is NULL, which keeps every existing caller unchanged.
  if (!is.null(energyPerCapita)) {
    epc <- energyPerCapita[getItems(out, dim = 1), getYears(out), ]
    pcNames <- c("Innovator Power pc|Bulk", "Innovator Power pc|Diffuse",
                 "Incumbent Power pc|Bulk", "Incumbent Power pc|Diffuse")
    pc <- new.magpie(magclass::getItems(out, dim = 1), getYears(out), pcNames, fill = NA)
    for (s in c("Bulk", "Diffuse")) {
      for (a in c("Innovator", "Incumbent")) {
        pc[, , paste0(a, " Power pc|", s)] <-
          setNames(out[, , paste0(a, " Power|", s)] * epc, paste0(a, " Power pc|", s))
      }
    }
    out <- mbind(out, pc)
  }

  # --- Calculate overall Actor Power Index ---
  out[, , "Actor Power Index|Bulk"] <-
    coeff$bulk$actor_power$innov * out[, , "Innovator Power|Bulk"] -
    coeff$bulk$actor_power$incumb * out[, , "Incumbent Power|Bulk"]
  out[, , "Actor Power Index|Diffuse"] <-
    coeff$diffuse$actor_power$innov * out[, , "Innovator Power|Diffuse"] -
    coeff$diffuse$actor_power$incumb * out[, , "Incumbent Power|Diffuse"]

  return(out)
}
