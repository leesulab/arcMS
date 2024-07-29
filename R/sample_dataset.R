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
setClassUnion("dataframeOrDatatable", c("data.frame", "data.table"))
sample_dataset <- setClass("sample_dataset",
                        slots = c(sample_data = "dataframeOrDatatable"),
                        contains = "sample_infos")
setMethod("initialize", signature = "sample_dataset",
          definition = function(.Object, ...)
          {
            .Object <- callNextMethod()
              if (length(.Object@sample_data) == 0L)
                  warning("zero-length 'sample_data' slot")
            return(.Object)
          })
# sample_dataset <- function(sample_data, )

#' @describeIn sample_dataset Accessor method to obtain the sample_data table.
#' @return \code{get_sample_data} returns a data.table object containing the sample data.
#' @aliases get_sample_data
#' @export
setMethod("get_sample_data", "sample_dataset", function(obj) obj@sample_data)

#' Create a sample dataset from a Parquet file created by arcMS
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

          sample_metadata = spectrum_metadata = data.table(NULL)
          sample_metadata_json = spectrum_metadata_json = character(0)

          if (!is.null(attr(data, "sample_metadata"))) {
                    sample_metadata = attributes(data)$sample_metadata
                    attr(data, "sample_metadata") = NULL
          }
          if (!is.null(attr(data, "spectrum_metadata"))) {
                    spectrum_metadata = attributes(data)$spectrum_metadata
                    attr(data, "spectrum_metadata") = NULL
          }
          if (!is.null(attr(data, "sample_metadata_json"))) {
                    sample_metadata_json = as.character(attributes(data)$sample_metadata_json)
                    attr(data, "sample_metadata_json") = NULL
          }
          if (!is.null(attr(data, "spectrum_metadata_json"))) {
                    spectrum_metadata_json = as.character(attributes(data)$spectrum_metadata_json)
                    attr(data, "spectrum_metadata_json") = NULL
          }

          collecteddata <- sample_dataset(
              sample_data = data,
              sample_metadata = as.data.table(sample_metadata),
              spectrum_metadata = as.data.table(spectrum_metadata),
              sample_metadata_json = sample_metadata_json,
              spectrum_metadata_json = spectrum_metadata_json
            )

          return(collecteddata)

          }

#' Class containing a sample data as pointer to Parquet file, and metadata
#'
#' Contains sample data as a pointer to a Parquet file, and sample metadata
#' and spectrum metadata in table and json formats.
#'
#' Objects for this class can be created by \code{\link{create_sample_dataset}}
#' with the ondisk method.
#'
#' @slot sample_data Contains an \code{Arrow Dataset} R6 object pointing to the Parquet file.
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

sample_dataset_ondisk <- setClass("sample_dataset_ondisk",
                        slots = c(sample_data = c("FileSystemDataset")),
                        contains = "sample_infos")
# initialize method during object instantiation
setMethod("initialize", signature = "sample_dataset_ondisk",
          definition = function(.Object, sample_data, ...)
          {
            .Object@sample_data <- sample_data
            .Object <- callNextMethod(.Object, ...)
            return(.Object)
          } )
