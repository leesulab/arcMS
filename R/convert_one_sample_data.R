#' @include main.R
#' @include sample_infos.R
NULL

#' Class containing a sample data and metadata
#'
#' Contains sample data, metadata and spectrum metadata in table and json formats.
#'
#' Objects for this class are returned by \code{\link{collect_one_sample_data}}.
#'
#' @slot sample_data Contains a \code{datatable} with the sample data.
#' @slot sample_metadata Contains a \code{datatable} with the sample metadata.
#' @slot spectrum_metadata Contains a \code{datatable} with the spectrum metadata.
#' @slot sample_metadata_json Contains a \code{character} with the sample metadata.
#' @slot spectrum_metadata_json Contains a \code{character} with the spectrum metadata.
#'
#' @section Use the \code{\link{collect_one_sample_data}} to:
#'   store the sample data, metadata and spectrum metadata, in table and json formats.
#'
#' @param obj The \code{\link{sample_dataset}} object to access.
#'
#' @export

sample_dataset <- setClass("sample_dataset",
                        slots = c(sample_data = "data.table"),
                        contains = "sample_infos")
# initialize method during object instantiation
setMethod("initialize", signature = "sample_dataset",
          definition = function(.Object, sample_data, ...)
          {
            .Object@sample_data <- sample_data
            .Object <- callNextMethod(.Object, ...)
            return(.Object)
          } )

#' @describeIn sample_dataset Accessor method to obtain the sample_data table.
#' @return \code{get_sample_data} returns a data.table object containing the sample data.
#' @aliases get_sample_data
#' @export
setMethod("get_sample_data", "sample_dataset", function(obj) obj@sample_data)

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
  } else {

  sample_infos = get_sample_infos(connection_params, sample_id)
  sample_name = get_sample_name(sample_infos)
  analysis_name = get_analysis_name(sample_infos)

  collected_data = collect_one_sample_data(connection_params, sample_id, num_spectras)
  save_one_sample_data(collected_data, sample_name, analysis_name, format = format)
  }
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
#' @return A \code{\link{sample_dataset}} object, containing the sample data,
#' sample metadata and spectrum metadata datatables.
#' @seealso \code{\link{save_one_sample_data}} to save collected data from the R environment to Parquet or HDF5 files, and \code{\link{convert_one_sample_data}} to both collect data and saving to files.
#' @export

collect_one_sample_data <- function(connection_params, sample_id, num_spectras = NULL){

  sample_infos = get_sample_infos(connection_params, sample_id)
  sample_name = get_sample_name(sample_infos)
  analysis_name = get_analysis_name(sample_infos)
  sample_metadata = get_sample_metadata(sample_infos)

  message(glue::glue("Downloading sample '{sample_name}'..."))

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

future::plan(multisession)
 resp = function(skips) {
  p <- progressor(along = skips)
  response = future.apply::future_lapply(skips, ParallelDownloadQueue, chunkSize = chunkSize, p = p)
  return(response)
}
response = with_progress(resp(skips))

message(glue::glue("Deserializing '{sample_name}' sample data..."))

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
sample_metadata_json = as.character(get_sample_metadata_json(sample_infos))
spectrum_metadata_json = as.character(get_spectrum_metadata_json(sample_infos))

collecteddata <- sample_dataset(
    sample_data = explode_data_with_dt,
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
#' @param sample_name The sample name (to be used for naming the saved file)
#' @param analysis_name The name of the Analysis (to be used for naming the folder)
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
    sample_name = sample_name
  } else {
    sample_name = get_sample_name(sample_dataset)
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
#save data
if (format == "parquet") {
  metadatalist = list("sampleinfos" = sample_metadata, "spectruminfos" = spectrum_metadata)
  metadatajson = toJSON(metadatalist, pretty = T)
  arrow::write_parquet(sample_data, glue("{path}/{sample_name}.parquet"), compression = "gzip")
  write(metadatajson, glue("{path}/{sample_name}-metadata.json"))

  message(glue::glue("Sample '{sample_name}' saved as parquet file!"))
} else {
  hdf5_file <- glue::glue("{path}/{sample_name}.h5")
  rhdf5::h5createFile(hdf5_file)
  # defining data.table variable locally to avoid R cmd check NOTES due to NSE
  energy_level = NULL
  low_data <- subset(sample_data, energy_level == "Low")
  high_data <- subset(sample_data, energy_level == "High")
  rhdf5::h5write(obj = low_data, file = hdf5_file, name = "ms1")
  rhdf5::h5write(obj = high_data, file = hdf5_file, name = "ms2")
  rhdf5::h5write(obj = sample_metadata, file = hdf5_file, name = "samplemetadata")
  rhdf5::h5write(obj = spectrum_metadata, file = hdf5_file, name = "spectrummetadata")

  message(glue::glue("Sample '{sample_name}' saved as HDF5 file!"))
}
}
}
