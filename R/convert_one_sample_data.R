#' Convert one sample
#'
#' The function collects spectral data from a sample in a UNIFI Analysis.
#' First, asynchronous queries are performed for each sample.
#' Then, the data is deserialized by the \code{\link{deserialize_data}} function.
#' The S3 object containing the spectral data is adapted into a dataframe by the \code{\link{outputlist_to_df}} function.
#' Finally, the data is saved on disk to the Parquet or HDF5 format with the \code{\link{save_one_sample_data}} function.
#'
#' @param sample_id The id of the sample to be collected
#' @param connection_params OPTIONAL: Connection parameters object created by the
#' \code{\link{create_connection_params}} function. If not provided, the
#' \code{\link{get_connection_params}} will look for such object in the global environment
#' @param format The format chosen for the exported file (Parquet or HDF5)
#' @param path OPTIONAL The destination path for the exported file
#' @param overwrite OPTIONAL overwrite the sample if already present on disk
#' @param num_spectras OPTIONAL Number of spectras to be downloaded (OPTIONAL, only if whole sample data not needed, e.g. for testing purposes)
#'
#' @return Datatables of the sample's spectral data and metadata are saved in Parquet or HDF5 format in the analysis name folder.
#' @seealso \code{\link{collect_one_sample_data}} for only collecting data by dowloading from the API into the R environment, and \code{\link{save_one_sample_data}} to save collected data from the R environment to Parquet or HDF5 files.
#' @export

convert_one_sample_data <- function(sample_id, connection_params = NULL, format = 'parquet', path = NULL, overwrite = T, num_spectras = NULL){
  if (!format %in% c('parquet', 'hdf5')) {
  stop("The format argument must be either 'parquet' or 'hdf5'")
  } else {
    if(is.null(connection_params))
      connection_params = get_connection_params(parent.frame())
    sample_infos = get_sample_infos(sample_id, connection_params)
    sample_name = fs::path_sanitize(get_sample_name(sample_infos))
    analysis_name = get_analysis_name(sample_infos)

    if(!is.null(path)) {
      path2 = paste0(path, "/", analysis_name)
    } else {
      path2 = analysis_name
    }
    if (file.exists(glue("{path2}/{sample_name}.parquet")) & overwrite == F) {
      message(glue::glue("File '{sample_name}' already exists..."))
    } else {
      collected_data = collect_one_sample_data(sample_id, connection_params, num_spectras)
      printf("Start Saving")
      save_one_sample_data(collected_data, sample_name, analysis_name, path = path, format = format)
      printf("End Saving")
      rm(collected_data)
    }
  }
}

#' Collect data from one sample
#'
#' The function collects spectral data from a sample in a UNIFI Analysis.
#' First, asynchronous queries are performed for each sample.
#' Then, the data is deserialized by the function deserialize_data().
#' The S3 object containing the spectral data is adapted into a dataframe by the function outputlist_to_df().
#'
#' @param sample_id The id of the sample to be collected
#' @param connection_params OPTIONAL: Connection parameters object created by the
#' \code{\link{create_connection_params}} function. If not provided, the
#' \code{\link{get_connection_params}} will look for such object in the global environment
#' @param num_spectras OPTIONAL Number of spectras to be downloaded (OPTIONAL, only if whole sample data not needed, e.g. for testing purposes)
#'
#' @return A \code{\link{sample_dataset}} object, containing the sample data,
#' sample metadata and spectrum metadata datatables.
#' @seealso \code{\link{save_one_sample_data}} to save collected data from the R environment to Parquet or HDF5 files, and \code{\link{convert_one_sample_data}} to both collect data and saving to files.
#' @export

collect_one_sample_data <- function(sample_id, connection_params = NULL, num_spectras = NULL){

  if(is.null(connection_params))
    connection_params = get_connection_params(parent.frame())
  sample_infos = get_sample_infos(sample_id, connection_params)
  sample_name = get_sample_name(sample_infos)
  analysis_name = get_analysis_name(sample_infos)
  sample_metadata = get_sample_metadata(sample_infos)

  message(glue::glue("Downloading sample '{sample_name}'..."))

  hostUrl = connection_apihosturl(connection_params)
  token = connection_token(connection_params)
  # RT collect
  sampleUrl = glue::glue("{hostUrl}/sampleresults({sample_id})")

  spectrumEndpoint = function(skip, top) { glue::glue(sampleUrl, "/spectra/mass.mse?$skip=", skip, "&$top=", top) }
  request = function(skip, top) {
    httr::set_config(httr::config(ssl_verifypeer = 0L, ssl_verifyhost = 0L))
    httr::content(httpClientOctet(spectrumEndpoint(skip, top), token))
  }

  spectrumCountEndpoint = glue::glue(sampleUrl, "/spectra/mass.mse/$count")
  spectrumCount = httr::content(httpClientPlain(spectrumCountEndpoint, token), "text", encoding = "utf-8")

  if (!is.null(num_spectras)) {
    numSpectra = num_spectras
  } else{
    numSpectra = as.numeric(iconv(spectrumCount, 'utf-8', 'ascii', sub=''))
  }
  numLogicalSpectra =  numSpectra * 200
  message(glue::glue("Number of spectra to download: {numSpectra}"))
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

# future::plan(multisession)
 resp = function(skips) {
  p <- progressor(along = skips)
  response = future.apply::future_lapply(skips, ParallelDownloadQueue, chunkSize = chunkSize, p = p)
  return(response)
}
response = with_progress(resp(skips))
message(glue::glue("Deserializing '{sample_name}' sample data..."))

deseria = lapply(response, deserialize_data)
output = lapply(deseria, outputlist_to_df)
rm(deseria)

data_all = data.table::rbindlist(output)
rm(output)


# defining data.table variable locally to avoid R cmd check NOTES due to NSE
mslevel = NULL

data_all <- data_all[ , mslevel := as.factor(mslevel)]
data_all <- data_all[order(data_all$mslevel),]

long_data = explode_spectra(data_all)

if("bin" %in% colnames(long_data)) {
  long_data = add_drift_time(connection_params = connection_params, unnestdt = long_data, sample_id = sample_id)
} else {
  # add bin and dt columns even for data without IMS, for compatibility with viz app
  long_data[, "bin" := 1]
  long_data[, "dt" := 0]
}
spectrum_infos = get_spectrum_metadata(sample_infos)
sample_metadata_json = as.character(get_sample_metadata_json(sample_infos))
spectrum_metadata_json = as.character(get_spectrum_metadata_json(sample_infos))

collecteddata <- sample_dataset(
    sample_data = long_data,
    sample_metadata = sample_metadata,
    spectrum_metadata = spectrum_infos,
    sample_metadata_json = sample_metadata_json,
    spectrum_metadata_json = spectrum_metadata_json
  )

return(collecteddata)

}

#' Save collected data from one sample
#'
#' The function saves data collected from one sample with the \code{\link{collect_one_sample_data}} function.
#' Datatables of spectra and metadata are saved in either two Parquet files
#' or one HDF5 file.
#'
#' @param sample_dataset Collected data from a sample, stored in a \code{\link{sample_dataset}} object
#' @param sample_name OPTIONAL The sample name (to be used for naming the saved file)
#' @param analysis_name OPTIONAL The name of the Analysis (to be used for naming the folder)
#' @param path OPTIONAL The destination path for the exported file
#' @param format The format chosen for the exported file (parquet or hdf5)
#'
#' @return Datatables of the sample's spectral data and metadata are saved in Parquet or HDF5 format in the analysis name folder.
#' @seealso \code{\link{collect_one_sample_data}} for only collecting data by downloading from the API into the R environment, and \code{\link{convert_one_sample_data}} to both collect data and saving to files.
#' @export

save_one_sample_data <- function(sample_dataset, sample_name = NULL, analysis_name = NULL, path = NULL, format = 'parquet'){

    if (!format %in% c('parquet', 'hdf5')) {
    stop("The format argument must be either 'parquet' or 'hdf5'")
  } else {
  if (!is.null(sample_name)) {
    sample_name = fs::path_sanitize(sample_name)
  } else {
    sample_name = fs::path_sanitize(get_sample_name(sample_dataset))
  }
  if (!is.null(analysis_name)) {
    analysis_name = analysis_name
  } else {
    analysis_name = get_analysis_name(sample_dataset)
  }
message(glue::glue("Saving sample '{sample_name}' to folder '{analysis_name}'..."))
if(!is.null(path)) {
  path = paste0(path, "/", analysis_name)
} else {
  path = analysis_name
}
if (!file.exists(path))
  dir.create(path, showWarnings = TRUE, recursive = FALSE, mode = "0777")

sample_data = get_sample_data(sample_dataset)
sample_metadata = get_sample_metadata(sample_dataset)
spectrum_metadata = get_spectrum_metadata(sample_dataset)

attr(sample_data, "sample_metadata") = sample_metadata
attr(sample_data, "spectrum_metadata") = spectrum_metadata
attr(sample_data, "sample_metadata_json") = toJSON(sample_metadata, pretty = T)
attr(sample_data, "spectrum_metadata_json") = toJSON(spectrum_metadata, pretty = T)
attr(sample_data, "rtmin") = min(sample_data$rt)
attr(sample_data, "rtmax") = max(sample_data$rt)
attr(sample_data, "mzmin") = min(sample_data$mz)
attr(sample_data, "mzmax") = max(sample_data$mz)
attr(sample_data, "binmin") = min(sample_data$bin)
attr(sample_data, "binmax") = max(sample_data$bin)
attr(sample_data, "dims") = dim(sample_data)

sample_data_arrow = arrow::arrow_table(sample_data)

#save data
if (format == "parquet") {
  metadatalist = list("sampleinfos" = sample_metadata, "spectruminfos" = spectrum_metadata)
  metadatajson = toJSON(metadatalist, pretty = T)
  arrow::write_parquet(sample_data_arrow, glue("{path}/{sample_name}.parquet"), compression = "gzip")
  write(metadatajson, glue("{path}/{sample_name}-metadata.json"))

  message(glue::glue("Sample '{sample_name}' saved as parquet file!"))
} else {
  hdf5_file <- glue::glue("{path}/{sample_name}.h5")
  rhdf5::h5createFile(hdf5_file)
  # defining data.table variable locally to avoid R cmd check NOTES due to NSE
  mslevel = NULL
  ms1data <- subset(sample_data, mslevel == "1")
  ms2data <- subset(sample_data, mslevel == "2")
  rhdf5::h5write(obj = ms1data, file = hdf5_file, name = "ms1")
  rhdf5::h5write(obj = ms2data, file = hdf5_file, name = "ms2")
  rhdf5::h5write(obj = sample_metadata, file = hdf5_file, name = "samplemetadata")
  rhdf5::h5write(obj = spectrum_metadata, file = hdf5_file, name = "spectrummetadata")

  message(glue::glue("Sample '{sample_name}' saved as HDF5 file!"))
}
}
}
