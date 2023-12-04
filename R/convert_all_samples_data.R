#' Convert data from all samples in an Analysis
#'
#' The function collects all samples data from an Analysis using the collect_one_sample_data() function and save them on disk.
#'
#' @param connection_params Connection parameters to the Unifi API - url and token
#' @param analysis_id The id of the analysis
#' @param format The format chosen for the exported filed (parquet or HDF5)
#'
#' @return A ataframe for each sample is saved in parquet or HDF5 format in a folder named after the Analysis.
#' @export

convert_all_samples_data <- function(connection_params, analysis_id, format = 'parquet') {

      if (!format %in% c('parquet', 'HDF5')) {
    stop("The format must be parquet or HDF5")
  }

  samples_list = get_samples_list(connection_params, analysis_id)
  for (i in 1:(nrow(samples_list))) {
    print(glue::glue("-------- Number of samples collected {i}/{nrow(samplelist)} -------- \n"))
    sample_id = samples_list$id[i]

    convert_one_sample_data(connection_params, sample_id, format = format)
  }
  printf("All done!\n")

}
