#' Convert mzML (or mzXML) file to data.table in long format
#'
#' The function collects spectral data and metadata from an mzML file with mzR package.
#' Data are transformed to data.table in long format.
#'
#' @param filename Path name of the netCDF, mzXML or mzML file to read/write.
#' @param chunksize size (number of scans) for each chunk to be retrieved from the mzML file.
#'
#' @return A \code{\link{data.table}} object containing the spectra
#' @seealso \code{\link{mzml_to_sample_dataset}} to convert mzML data to sample_dataset object.
mzml_to_dt <- function(filename, chunksize = 10000) {
  aa <- mzR::openMSfile(filename, backend = "pwiz")
  scanCount = length(aa)
  chk = 1
  chunks = list()
  nchunks = ceiling(scanCount / chunksize)
  for (i in 1:nchunks) {
    chunks[[i]] = chk
    chk = chk + chunksize
  }

  retrieveHeaders <- function(chunks, chunksize, p) {
    start = chunks
    end = chunks + chunksize - 1

    if(end>scanCount) {
      end = scanCount
    }
    res = header(aa, start:end)
    p()
    return(res)
  }

  respheaders = function(chunks) {
    p <- progressor(along = chunks)
    response = lapply(chunks, retrieveHeaders, chunksize = chunksize, p = p)
    return(response)
  }
  responseheaders = with_progress(respheaders(chunks))

  # defining data.table variables locally to avoid R cmd check NOTES due to NSE
  scanid = NULL
  retentionTime = NULL
  bin = NULL
  freq = NULL
  ionMobilityDriftTime = NULL

  hds = rbindlist(responseheaders)
  hds[ , scanid := .GRP, by = retentionTime]
  hds[, bin := seq_len(.N), by = scanid]
  hds.expanded <- hds[ ,list(freq=rep(1,peaksCount)),by=c("scanid", "msLevel", "retentionTime","ionMobilityDriftTime", "bin")]
  hds.expanded[,freq := NULL]
  hds.expanded[,dt:= ifelse(is.na(ionMobilityDriftTime), 0, ionMobilityDriftTime)]
  hds.expanded[, ionMobilityDriftTime := NULL]

  retrievePeaks <- function(chunks, chunksize, p) {
    start = chunks
    end = chunks + chunksize - 1

    if(end>scanCount) {
      end = scanCount
    }
    res = peaks(aa, start:end)
    p()
    return(res)
  }
  resppeaks = function(chunks) {
    p <- progressor(along = chunks)
    response = lapply(chunks, retrievePeaks, chunksize = chunksize, p = p)
    return(response)
  }
  responsepeaks = with_progress(resppeaks(chunks))

  dt = unlist(responsepeaks, recursive = F)
  dt2 = do.call(rbind, dt)
  rm(dt)
  dt = as.data.table(dt2)
  dt = cbind(dt, hds.expanded)
  setnames(dt, old=c("mz", "intensity", "scanid", "msLevel", "retentionTime", "bin", "dt"), new=c("mz", "intensity", "scanid", "mslevel", "rt", "bin", "dt"))
  setcolorder(dt, c("scanid", "rt", "mslevel", "bin", "dt", "mz", "intensity"))
  return(dt)
}

#' Convert mzML (or mzXML) file to sample_dataset
#'
#' The function collects spectral data and metadata from an mzML file with mzR package.
#' Data are transformed to data.table and saved in a sample_dataset object.
#' Metadata are arranged as a data.frame and passed to sample_metadata slot.
#'
#' @param filename Path name of the netCDF, mzXML or mzML file to read/write.
#' @param chunksize size (number of scans) for each chunk to be retrieved from the mzML file.
#'
#' @return A \code{\link{sample_dataset}} object containing the sample spectra
#' and sample metadata datatables.
#' @seealso \code{\link{convert_mzml_to_parquet}} to directly save converted data to Parquet file.
#' @export

mzml_to_sample_dataset <- function(filename, chunksize = 10000){

  dat = mzml_to_dt(filename = filename, chunksize = chunksize)

  mzmlfile <- openMSfile(filename, backend = "pwiz")
  # spectrum_metadata = runInfo(mzmlfile) # quite long and getting barely useful info
  iinfo = as.data.table(instrumentInfo(mzmlfile))
  iinfo$sampleName = tools::file_path_sans_ext(basename(filename))

  converteddata <- sample_dataset(
    sample_data = dat,
    sample_metadata = iinfo,
    sample_metadata_json = as.character(jsonlite::toJSON(iinfo))
  )

  return(converteddata)
}

#' Convert mzML (or mzXML) file to parquet file
#'
#' The function collects spectral data and metadata from an mzML file with mzR package.
#' Data are transformed to data.table, passed in a sample_dataset object and saved as parquet file.
#'
#' @param filename Path name of the netCDF, mzXML or mzML file to read/write.
#' @param chunksize size (number of scans) for each chunk to be retrieved from the mzML file.
#' @param path OPTIONAL The destination path for the exported file. Default is current working directory.
#' @param overwrite OPTIONAL overwrite the sample if already present on disk
#'
#' @seealso \code{\link{save_one_sample_data}} to save collected/converted data from the R environment to Parquet or HDF5 files.
#' @export
convert_mzml_to_parquet <- function(filename, chunksize = 10000, path = ".", overwrite = T){

  sd = mzml_to_sample_dataset(filename = filename, chunksize = chunksize)
  sample_name = get_sample_name(sd)

  if (file.exists(glue("{path}/{sample_name}.parquet")) & overwrite == F) {
    message(glue::glue("File '{sample_name}' already exists..."))
  } else {
  save_one_sample_data(sd, analysis_name = path)
  }
}
