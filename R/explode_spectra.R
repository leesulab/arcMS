#' Explode Spectra Data
#'
#' This function takes a wide format dataframe and unnests (explodes) it into a long format.
#' The unnested table contains the columns rt (retention time), scanid (id of each scan),
#' mslevel (1 or 2, i.e. low or high energy for MSe data), mz (m/z values), bin (drift time bin) and intensities.
#'
#' @param wide_df A wide format dataframe with columns 'mz', 'intensity', and 'scan_size'.
#' @return A long format dataframe with the exploded spectral data.
#' @keywords internal


explode_spectra <- function(wide_df) {
  # defining data.table variables locally to avoid R cmd check NOTES due to NSE
  mslevel = rt = scanid = intensity = mz = scan_size = bin = NULL

  #spectra <- as.data.table(wide_df)
  spectra = wide_df
  rm(wide_df)

  spectra[, scanid := seq_len(.N), by = mslevel]

  unnestmasses <- spectra[, .(mz = unlist(mz)), by = .(rt, scanid, mslevel)]
  unnestintensities <- spectra[, .(intensity = unlist(intensity)), by = .(scanid, mslevel)]
  unnestdt = unnestmasses[, intensity := unnestintensities$intensity]
  rm(unnestmasses,unnestintensities)
  gc(reset = T)

  manual_uncount <- function(dt, count_col) {
    setDT(dt)
    rep_indices <- rep(seq_len(nrow(dt)), dt[[count_col]])
    dt_repeated <- dt[rep_indices, ]
    return(dt_repeated)
  }
  # test if has ion mobility data (200 scan size values)
  if(length(spectra[["scan_size"]][[1]]) == 200) {
    # Adding bin number to each line/mz value
     # scansizes <- spectra[, .(scan_size = unlist(scan_size)), by = .(rt, scanid, mslevel)]
     # scansizes$bin <- rep(1:200, times = nrow(spectra))
     # scansizedf <- tidytable::uncount(scansizes, scan_size, .remove = F)

    # unnestdt <- cbind(unnestdt, bin = scansizedf$bin)

    scansizes <- spectra[, .(scan_size = unlist(scan_size)), by = .(rt, scanid, mslevel)]
    scansizes[, bin := rep(1:200, each = .N / 200), by = .(rt, scanid, mslevel)]

    scansizedf <- manual_uncount(scansizes, "scan_size")
    unnestdt = unnestdt[, bin := scansizedf$bin]
    rm(scansizedf)
    gc(reset = T)
  }


  return(unnestdt)
}
