#' @include main.R
#' @include sample_infos.R
NULL

#' Class containing a sample data and metadata
#'
#' Contains sample data, metadata and spectrum metadata in table and json formats.
#'
#' Objects for this class are returned by \code{\link{collect_one_sample_data}}
#' and \code{\link{create_sample_dataset}}.
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

#' Create a sample dataset from a Parquet file
#'
#' The function creates a sample dataset object from data imported from a Parquet file.
#''
#' @param file A character file name or URI of Parquet file.
#' @param method Whether to import the data in RAM or keep it on-disk
#'
#' @return A \code{\link{sample_dataset}} object, containing the sample data,
#' sample metadata and spectrum metadata datatables.
#' @seealso \code{\link{save_one_sample_data}} to save collected data from the R environment to Parquet or HDF5 files, and \code{\link{convert_one_sample_data}} to both collect data and saving to files.
#' @export

create_sample_dataset <- function(file, method = "inram"){
          data = read_parquet(file)
          sample_metadata = attributes(data)$sample_metadata
          spectrum_metadata = attributes(data)$spectrum_metadata
          sample_metadata_json = as.character(attributes(data)$sample_metadata_json)
          spectrum_metadata_json = as.character(attributes(data)$spectrum_metadata_json)

          attr(data, "sample_metadata") = NULL
          attr(data, "spectrum_metadata") = NULL
          attr(data, "sample_metadata_json") = NULL
          attr(data, "spectrum_metadata_json") = NULL
          
          collecteddata <- sample_dataset(
              sample_data = data,
              sample_metadata = sample_metadata,
              spectrum_metadata = spectrum_metadata,
              sample_metadata_json = sample_metadata_json,
              spectrum_metadata_json = spectrum_metadata_json
            )

          return(collecteddata)

          }
