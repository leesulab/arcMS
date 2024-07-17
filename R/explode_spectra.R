#' Explode Spectra Data
#'
#' This function takes a wide format dataframe and unnests (explodes) it into a long format.
#' The unnested table contains the columns rt (retention time), scanid (id of each scan),
#' energy_level (low or high), mz (m/z values), bin (drift time bin) and intensities.
#'
#' @param wide_df A wide format dataframe with columns 'masses', 'intensities', and 'scan_size'.
#' @return A long format dataframe with the exploded spectral data.
#' @keywords internal


explode_spectra <- function(wide_df) {
  # defining data.table variables locally to avoid R cmd check NOTES due to NSE
  energy_level = rt = scanid = intensities = masses = scan_size = bin = NULL

  spectra <- as.data.table(wide_df)
  rm(wide_df)

  spectra[, scanid := seq_len(.N), by = energy_level]

  unnestmasses <- spectra[, .(mz = unlist(masses)), by = .(rt, scanid, energy_level)]
  unnestintensities <- spectra[, .(intensity = unlist(intensities)), by = .(scanid)]
  unnestdt = unnestmasses[, intensities := unnestintensities$intensity]
  rm(unnestmasses,unnestintensities)

  scansizes <- spectra[, .(scan_size = unlist(scan_size)), by = .(rt, scanid, energy_level)]

  scansizes[, bin := rep(1:200, each = .N / 200), by = .(rt, scanid, energy_level)]

  manual_uncount <- function(dt, count_col) {
    setDT(dt)
    rep_indices <- rep(seq_len(nrow(dt)), dt[[count_col]])
    dt_repeated <- dt[rep_indices, ]
    return(dt_repeated)
  }
  scansizedf <- manual_uncount(scansizes, "scan_size")
  unnestdt = unnestdt[, bin := scansizedf$bin]
  rm(scansizedf)
  return(unnestdt)
}
