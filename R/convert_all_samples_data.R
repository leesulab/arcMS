#' Convert data from all samples in an Analysis
#'
#' The function collects all samples data from an Analysis using the collect_one_sample_data() function and save them on disk.
#'
#' @param analysis_id The id of the analysis
#' @param connection_params OPTIONAL: Connection parameters object created by the
#' \code{\link{create_connection_params}} function. If not provided, the
#' \code{\link{get_connection_params}} will look for such object in the global environment
#' @param format The format chosen for the exported file (Parquet or HDF5)
#' @param path OPTIONAL The destination path for exported files
#' @param overwrite OPTIONAL overwrite the sample if already present on disk
#' @param num_spectras OPTIONAL A number of spectra to be collected and converted
#' (if no need to convert whole samples, e.g. for testing purposes)
#'
#' @return A dataframe for each sample is saved in Parquet or HDF5 format
#' in a folder named after the Analysis.
#' @export

convert_all_samples_data <- function(analysis_id, connection_params = NULL, format = 'parquet', path = NULL, overwrite = T, num_spectras = NULL) {

      if (!format %in% c('parquet', 'hdf5')) {
    stop("The format must be parquet or hdf5")
  }
  if(is.null(connection_params))
    connection_params = get_connection_params(parent.frame())

  samples_list = get_samples_list(analysis_id, connection_params)

  X = 1:nrow(samples_list)
  p <- progressor(along = X)
  for (i in X) {
    p(message(glue::glue("-------- Starting conversion of sample {i}/{nrow(samples_list)} -------- \n")))
    sample_id = samples_list$id[i]

    convert_one_sample_data(sample_id, connection_params = connection_params, format = format, path = path, overwrite = overwrite, num_spectras = num_spectras)
  }
  message("All done!")

}
