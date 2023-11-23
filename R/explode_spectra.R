#' Explode Spectra Data
#'
#' This function takes a wide format dataframe and unnests (explodes) it into a long format.
#' The unnested table contains the columns rt (retention time), scanid (id of each scan),
#' energy_level (low or high), mz (m/z values) and intensities.
#'
#' @param wide_df A wide format dataframe with columns 'masses', 'intensities', and 'scan_size'.
#' @return A long format dataframe with the exploded spectral data.
#' @export
#'

explode_spectra <- function(wide_df) {
  # defining data.table variables locally to avoid R cmd check NOTES due to NSE
  energy_level = rt = scanid = intensities = masses = scan_size = NULL

  spectra <- as.data.table(wide_df)
  spectra[, scanid := seq_len(.N), by = energy_level]

  unnestmasses <- spectra[, unlist(masses), by = .(rt, scanid, energy_level)]
  unnestintensities <- spectra[, unlist(intensities)]
  unnestdt <- cbind(unnestmasses, unnestintensities)
  setnames(unnestdt, c("rt", "scanid", "energy_level", "mz", "intensities"))

  # Adding bin number to each line/mz value
  scansizes <- spectra[, unlist(scan_size), by = scanid]
  setnames(scansizes, "V1", "scan_size")
  scansizes$bin <- rep(1:200, times = nrow(spectra))
  scansizedf <- tidytable::uncount(scansizes, scan_size, .remove = F)

  unnestdt <- cbind(unnestdt, bin = scansizedf$bin)

  return(unnestdt)
}
