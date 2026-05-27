#' Calculated drivers
#'
#' @param data A [`magpie`][magclass::magclass] object.
#' @return A [`magpie`][magclass::magclass] object with calculated drivers
#' @author Renato Rodrigues
#'
#' @importFrom magclass getNames<- getYears getRegions new.magpie ndata clean_magpie
#'
#' @export
#'
iamCalculatedDrivers <- function(data) {
  # add calculated drivers
  driverList <- c(
    "Coal primary energy share", "Oil/Gas primary energy share",
    "Fossil share in Industry", "VRE share", "Electrification"
  )
  result <- new.magpie(
    cells_and_regions = getRegions(data), # nolint: undesirable_function_linter.
    years = getYears(data), names = driverList, fill = 0
  )
  # Safe division helper
  safe_div <- function(num, den) {
    val <- num / den
    val[!is.finite(val)] <- 0
    return(val)
  }
  
  clamp01 <- function(x, varName) {
    arr   <- as.array(x)
    below <- which(is.finite(arr) & arr < 0, arr.ind = TRUE)
    above <- which(is.finite(arr) & arr > 1, arr.ind = TRUE)
    if (nrow(below) + nrow(above) > 0) {
      dn  <- dimnames(arr)
      fmt <- function(idx, vals) {
        paste(sprintf("  %s | %s : %.4f",
          dn[[1]][idx[, 1]], gsub("^y", "", dn[[2]][idx[, 2]]), vals),
          collapse = "\n")
      }
      msg <- paste0("Driver '", varName, "' has out-of-range values (expected [0,1]) — clamping:\n")
      if (nrow(below) > 0) msg <- paste0(msg, " Below 0:\n", fmt(below, arr[below]), "\n")
      if (nrow(above) > 0) msg <- paste0(msg, " Above 1:\n", fmt(above, arr[above]), "\n")
      warning(msg, call. = FALSE)
    }
    pmin(pmax(x, 0), 1)
  }

  result[, , "Coal primary energy share"] <-
    clamp01(safe_div(data[, , "pecoal"], data[, , "petotal"]), "Coal primary energy share")
  result[, , "Oil/Gas primary energy share"] <-
    clamp01(safe_div(data[, , "pegas"] + data[, , "peoil"], data[, , "petotal"]), "Oil/Gas primary energy share")
  result[, , "Fossil share in Industry"] <-
    clamp01(safe_div(data[, , "fe_indst_fossil"], data[, , "fe_indst"]), "Fossil share in Industry")
  result[, , "VRE share"] <-
    clamp01(safe_div(data[, , "wind"] + data[, , "solar"], data[, , "seel"]), "VRE share")
  result[, , "Electrification"] <-
    clamp01(safe_div(data[, , "fe_seel"], data[, , "fe_total"]), "Electrification")

  return(result)
}
