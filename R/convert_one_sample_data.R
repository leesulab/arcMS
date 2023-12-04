#' Convert one sample
#'
#' The function collects spectral data from a sample in a Unifi Analysis.
#' First, asynchronous queries are performed for each sample.
#' Then, the data is deserialized by the \code{\link{deserialize_data}} function.
#' The S3 object containing the spectral data is adapted into a dataframe by the \code{\link{outputlist_to_df}} function.
#' Finally, the data is saved on disk to the Parquet or HDF5 format with the \code{\link{save_one_sample_data}} function.
#'
#' @param connection_params Connection parameters to the Unifi API - url and token
#' @param sample_id The id of the sample to be collected
#' @param num_spectras Number of spectras to be downloaded (OPTIONAL, only if whole sample data not needed)
#' @param format The format chosen for the exported file (parquet or hdf5)
#'
#' @return Datatables of the sample's spectral data and metadata are saved in Parquet or HDF5 format in the analysis name folder.
#' @seealso \code{\link{collect_one_sample_data}} for only collecting data by dowloading from the API into the R environment, and \code{\link{save_one_sample_data}} to save collected data from the R environment to Parquet or HDF5 files.
#' @export

convert_one_sample_data <- function(connection_params, sample_id, format = 'parquet', num_spectras = NULL){

  if (!format %in% c('parquet', 'hdf5')) {
  stop("The format argument must be either 'parquet' or 'hdf5'")
}

sample_infos = get_sample_infos(connection_params, sample_id)
sample_name = get_sample_name(sample_infos)
analysis_name = get_analysis_name(sample_infos)

collected_data = collect_one_sample_data(connection_params, sample_id, num_spectras)
save_one_sample_data(collected_data, sample_name, analysis_name, format = format)
}


#' Collect data from one sample
#'
#' The function collects spectral data from a sample in a Unifi Analysis.
#' First, asynchronous queries are performed for each sample.
#' Then, the data is deserialized by the function deserialize_data().
#' The S3 object containing the spectral data is adapted into a dataframe by the function outputlist_to_df().
#'
#' @param connection_params Connection parameters to the Unifi API - url and token
#' @param sample_id The id of the sample to be collected
#' @param num_spectras Number of spectras to be downloaded (OPTIONAL, only if whole sample data not needed)
#'
#' @return A list of dataframes of the sample's deserialized spectral data and metadata.
#' @seealso \code{\link{save_one_sample_data}} to save collected data from the R environment to Parquet or HDF5 files, and \code{\link{convert_one_sample_data}} to both collect data and saving to files.
#' @export

collect_one_sample_data <- function(connection_params, sample_id, num_spectras = NULL){

  sample_infos = get_sample_infos(connection_params, sample_id)
  sample_name = get_sample_name(sample_infos)
  analysis_name = get_analysis_name(sample_infos)
  sample_metadata = get_sample_metadata(sample_infos)

  printf("Downloading sample '%s'...\n", sample_name)

  hostUrl = connection_apihosturl(connection_params)
  token = connection_token(connection_params)
  # RT collect
  sampleUrl = glue::glue("{hostUrl}/sampleresults({sample_id})")

  spectrumEndpoint = function(skip, top) { glue::glue(sampleUrl, "/spectra/mass.mse?$skip=", skip, "&$top=", top) }
  request = function(skip, top) {httr::content(httpClientOctet(spectrumEndpoint(skip, top), token))}

  spectrumCountEndpoint = glue::glue(sampleUrl, "/spectra/mass.mse/$count")
  spectrumCount = httr::content(httpClientPlain(spectrumCountEndpoint, token), "text", encoding = "utf-8")

  if (!is.null(num_spectras)) {
    numSpectra = num_spectras
  } else{
    numSpectra = as.numeric(iconv(spectrumCount, 'utf-8', 'ascii', sub=''))
  }
  numLogicalSpectra =  numSpectra * 200
  printf("Number of spectra to download: %s\n", numSpectra)
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

printf("Deserializing '%s' sample data...\n", sample_name)

deseria = lapply(response, deserialize_data)
output = lapply(deseria, outputlist_to_df)
data_all = data.table::rbindlist(output)

# defining data.table variable locally to avoid R cmd check NOTES due to NSE
energy_level = NULL

data_all$energy_level <- factor(data_all$energy_level, levels = c("Low", "High"))
data_all <- data_all[order(data_all$energy_level),]
explode_data = explode_spectra(data_all)
explode_data_with_dt = add_drift_time(connection_params = connection_params, unnestdt = explode_data, sample_id = sample_id)
spectrum_infos = get_spectrum_metadata(sample_infos)

collecteddata <- list("data" = explode_data_with_dt, "samplemetadata" = sample_metadata, "spectrummetadata" = spectrum_infos$spectrum_infos)

return(collecteddata)
}

#' Save collected data from one sample
#'
#' The function saves data collected from one sample with the \code{\link{collect_one_sample_data}} function.
#' Datatables of spectra and metadata are saved in either two Parquet files
#' or one HDF5 file.
#'
#' @param collected_data Collected data from sample
#' @param sample_name The sample name (to be used for naming the saved file)
#' @param analysis_name The name of the Analysis (to be used for naming the folder)
#' @param format The format chosen for the exported file (parquet or hdf5)
#'
#' @return Datatables of the sample's spectral data and metadata are saved in Parquet or HDF5 format in the analysis name folder.
#' @seealso \code{\link{collect_one_sample_data}} for only collecting data by dowloading from the API into the R environment, and \code{\link{convert_one_sample_data}} to both collect data and saving to files.
#' @export

save_one_sample_data <- function(collected_data, sample_name = NULL, analysis_name = NULL, format = 'parquet'){

    if (!format %in% c('parquet', 'hdf5')) {
    stop("The format argument must be either 'parquet' or 'hdf5'")
  }
  if (!is.null(sample_name)) {
    sample_name = sample_name
  } else {
    sample_name = collected_data$samplemetadata$sampleName
  }
  if (!is.null(analysis_name)) {
    analysis_name = analysis_name
  } else {
    analysis_name = collected_data$samplemetadata$analysisName
  }
printf("Saving sample '%s' to folder '%s'...\n", sample_name, analysis_name)

path = analysis_name
if (!file.exists(path))
  dir.create(path, showWarnings = TRUE, recursive = FALSE, mode = "0777")

#save data
if (format == "parquet") {
  metadatalist = list("sampleinfos" = collected_data$samplemetadata, "spectruminfos" = collected_data$spectrummetadata)
  metadatajson = toJSON(metadatalist, pretty = T)
  arrow::write_parquet(collected_data$data, glue("{path}/{sample_name}.parquet"), compression = "gzip")
  write(metadatajson, glue("{path}/{sample_name}-metadata.json"))

  printf("Sample '%s' saved as parquet file! \n", sample_name)
} else {
  hdf5_file <- glue::glue("{path}/{sample_name}.h5")
  rhdf5::h5createFile(hdf5_file)
  # defining data.table variable locally to avoid R cmd check NOTES due to NSE
  energy_level = NULL
  low_data <- subset(collected_data$data, energy_level == "Low")
  high_data <- subset(collected_data$data, energy_level == "High")
  rhdf5::h5write(obj = low_data, file = hdf5_file, name = "ms1")
  rhdf5::h5write(obj = high_data, file = hdf5_file, name = "ms2")
  rhdf5::h5write(obj = collected_data$samplemetadata, file = hdf5_file, name = "samplemetadata")
  rhdf5::h5write(obj = collected_data$spectrummetadata, file = hdf5_file, name = "spectrummetadata")

  printf("Sample '%s' saved as HDF5 file! \n", sample_name)
}
}
