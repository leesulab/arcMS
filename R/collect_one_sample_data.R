#' collect_one_sample_data
#'
#' The function collects spectral data from a sample in a Unifi Analysis.
#' First, asynchronous queries are performed for each sample.
#' Then, the data is deserialized by the function deserialize_data().
#' The S3 object containing the spectral data is adapted into a dataframe by the function outputlist_to_df().
#' Finally, the data is saved on disk to the parquet or HDF5 format.
#'
#' @param connection_params Connection parameters to the Unifi API - url and token
#' @param sample_id The id of the sample to be collected
#' @param sample_name The sample name (to be used for naming the saved file)
#' @param analysis_name The name of the Analysis (to be used for naming the folder)
#' @param format The format chosen for the exported filed (parquet or HDF5)
#'
#' @return A dataframe of the sample's deserialized spectral data is saved in parquet or HDF5 format in the analysis name folder.
#' @export

collect_one_sample_data <- function(connection_params, sample_id, sample_name, analysis_name, format = 'parquet', num_spectras = NULL){

    if (!format %in% c('parquet', 'hdf5')) {
    stop("The format must be parquet or hdf5")
  }

  printf("Downloading sample '%s'...\n", sample_name)

  hostUrl = connection_apihosturl(connection_params)
  token = connection_token(connection_params)
  # RT collect
  sampleUrl = glue::glue("{hostUrl}/sampleresults({sample_id})")

  httpClientPlain = function(url) {
    httr::GET(url, add_headers(
                                     Accept="text/plain",
                                     "Authorization"=paste("Bearer", token)))
  }

  httpClientOctet = function(url) {
    httr::GET(url,
              add_headers("Content-Type"="application/x-www-form-urlencoded",
                          Accept="application/octet-stream",
                "Authorization"=paste("Bearer", token)))
  }

  spectrumEndpoint = function(skip, top) { glue::glue(sampleUrl, "/spectra/mass.mse?$skip=", skip, "&$top=", top) }
  request = function(skip, top) {httr::content(httpClientOctet(spectrumEndpoint(skip, top)))}

  spectrumCountEndpoint = glue::glue(sampleUrl, "/spectra/mass.mse/$count")
  spectrumCount = httr::content(httpClientPlain(spectrumCountEndpoint), "text", encoding = "utf-8")

  numSpectra = as.numeric(iconv(spectrumCount, 'utf-8', 'ascii', sub=''))
  numLogicalSpectra =  numSpectra * 200
  printf("Number of spectra: %s\n", numSpectra)
  chunkSize = 500 #default value in msconvert = 20
  nchunks = ceiling(numSpectra / chunkSize)
  # nchunks = 5
  skip = 0
  skips = list()
  for (i in 1:nchunks) {
    skips[[i]] = skip
    skip = skip + chunkSize
  }
  ParallelDownloadQueue = function(skips, chunkSize, p) {
   res = request(skips, chunkSize)
   p()
   skip = skips + chunkSize
   return(res)
 }

future::plan(multisession)
 resp = function(skips) {
  p <- progressor(along = skips)
  response = future.apply::future_lapply(skips, ParallelDownloadQueue, chunkSize = chunkSize, p = p)
  return(response)
}
response = with_progress(resp(skips))

printf("Saving sample '%s' in folder '%s'...\n", sample_name, analysis_name)

deseria = lapply(response, deserialize_data)
output = lapply(deseria, outputlist_to_df)
data_all = data.table::rbindlist(output)

# defining data.table variable locally to avoid R cmd check NOTES due to NSE
energy_level = NULL

data_all$energy_level <- factor(data_all$energy_level, levels = c("Low", "High"))
data_all <- data_all[order(data_all$energy_level),]

explode_data = explode_spectra(data_all)
explode_data_with_dt = add_drift_time(connection_params = connection_params, unnestdt = explode_data, sample_id = sample_id)
spectrum_infos = get_spectrum_infos(connection_params = connection_params, sample_id = sample_id)

path = analysis_name
if (!file.exists(path))
  dir.create(path, showWarnings = TRUE, recursive = FALSE, mode = "0777")

#save data
if (format == "parquet") {
  arrow::write_parquet(explode_data_with_dt, glue("{path}/{sample_name}.parquet"), compression = "gzip")
  arrow::write_parquet(spectrum_infos$spectrum_infos, glue("{path}/{sample_name}metadata.parquet"), compression = "gzip")

  printf("Sample '%s' saved as parquet file! \n", sample_name)
} else {
  hdf5_file <- glue("{path}/{sample_name}.h5")
  rhdf5::h5createFile(hdf5_file)
  low_data <- subset(explode_data_with_dt, energy_level == "Low")
  high_data <- subset(explode_data_with_dt, energy_level == "High")
  rhdf5::h5write(obj = low_data, file = hdf5_file, name = "ms1")
  rhdf5::h5write(obj = high_data, file = hdf5_file, name = "ms2")
  rhdf5::h5write(obj = spectrum_infos$spectrum_infos, file = hdf5_file, name = "metadata")

  printf("Sample '%s' saved as HDF5 file! \n", sample_name)
}
}
