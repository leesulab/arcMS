#' Convert data from all samples in an Analysis
#'
#' The function collects all samples data from an Analysis using the collect_one_sample_data() function and save them on disk.
#'
#' @param connection_params Connection parameters to the Unifi API - url and token
#' @param analysis_id The id of the analysis
#' @param format The format chosen for the exported filed (parquet or HDF5)
#'
#' @return A dataframe for each sample is saved in parquet or HDF5 format in a folder named after the Analysis.
#' @export

convert_all_samples_data <- function(connection_params, analysis_id, format = 'parquet', path = NULL, num_spectra = NULL) {

      if (!format %in% c('parquet', 'hdf5')) {
    stop("The format must be parquet or hdf5")
  }
  samples_list = get_samples_list(connection_params, analysis_id)

  X = 1:nrow(samples_list)
  p <- progressor(along = X)
  for (i in X) {
    p(message(glue::glue("-------- Starting conversion of sample {i}/{nrow(samples_list)} -------- \n")))
    sample_id = samples_list$id[i]

    convert_one_sample_data(connection_params, sample_id, format = format, path = path, num_spectra = num_spectra)
  }
  message("All done!")

}
