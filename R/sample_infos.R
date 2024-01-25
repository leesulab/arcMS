#' @include main.R
NULL

#' Class containing a sample's information
#'
#' Contains sample metadata and spectrum metadata in table and json formats.
#'
#' Objects for this class are returned by \code{\link{get_sample_infos}}.
#'
#' @slot sample_metadata Contains a \code{datatable} with the sample metadata.
#' @slot spectrum_metadata Contains a \code{datatable} with the spectrum metadata.
#' @slot sample_metadata_json Contains a \code{character} with the sample metadata.
#' @slot spectrum_metadata_json Contains a \code{character} with the spectrum metadata.
#'
#' @section Use the \code{\link{get_sample_infos}} to:
#'   store the sample metadata and spectrum metadata, in table and json formats.
#'
#' @param obj The \code{\link{sample_infos}} object to access.
#'
#' @export
sample_infos <- setClass("sample_infos",
                        slots = c(sample_metadata = "data.table",
                                  spectrum_metadata = "data.table",
                                  sample_metadata_json = "character",
                                  spectrum_metadata_json = "character"))
# initialize method during object instantiation
setMethod("initialize", signature = "sample_infos",
          definition = function(.Object, sample_metadata, spectrum_metadata, sample_metadata_json, spectrum_metadata_json)
          {
                    .Object@sample_metadata <- sample_metadata
                    .Object@spectrum_metadata <- spectrum_metadata
                    .Object@sample_metadata_json <- sample_metadata_json
                    .Object@spectrum_metadata_json <- spectrum_metadata_json
            return(.Object)
          } )


#' @describeIn sample_infos Accessor method to obtain the sample_infos table.
#' @return \code{get_sample_metadata} returns a data.table object containing the sample metadata.
#' @aliases get_sample_metadata
#' @export
setMethod("get_sample_metadata", "sample_infos", function(obj) obj@sample_metadata)

#' @describeIn sample_infos Accessor method to obtain the sample name.
#' @return \code{get_sample_infos} returns a character object containing the sample name.
#' @aliases get_sample_name
#' @export
setMethod("get_sample_name", "sample_infos", function(obj) obj@sample_metadata$sampleName)

#' @describeIn sample_infos Accessor method to obtain the analysis name.
#' @return \code{get_analysis_name} returns a character object containing the analysis name.
#' @aliases get_analysis_name
#' @export
setMethod("get_analysis_name", "sample_infos", function(obj) obj@sample_metadata$analysisName)

#' @describeIn sample_infos Accessor method to obtain the sample metadata in json format.
#' @return \code{get_sample_metadata_json} returns a json character object containing the sample metadata.
#' @aliases get_sample_metadata_json
#' @export
setMethod("get_sample_metadata_json", "sample_infos", function(obj) jsonlite::prettify(obj@sample_metadata_json))

#' @describeIn sample_infos Accessor method to obtain the spectrum_metadata table.
#' @return \code{get_spectrum_metadata} returns a data.table object containing the spectrum metadata of a sample.
#' @aliases get_spectrum_metadata
#' @export
setMethod("get_spectrum_metadata", "sample_infos", function(obj) obj@spectrum_metadata)

#' @describeIn sample_infos Accessor method to obtain the spectrum metadata in json format.
#' @return \code{get_spectrum_metadata_json} returns a json character object containing the spectrum metadata of a sample.
#' @aliases get_spectrum_metadata_json
#' @export
setMethod("get_spectrum_metadata_json", "sample_infos", function(obj) jsonlite::prettify(obj@spectrum_metadata_json))

#' Retrieve Sample Information from UNIFI API with a sample id
#'
#' This function retrieves sample metadata and spectrum information from the UNIFI API for a specified sample result using the provided connection parameters.
#' It extracts detailed spectrum information associated with the given sample result identifier.
#' The function returns two dataframes containing the sample information and spectrum informationn, and two formatted JSON strings of the same data.
#'
#' @param sample_id The identifier of the sample result for which spectrum information is to be retrieved.
#' @param connection_params OPTIONAL: Connection parameters object created by the
#' \code{\link{create_connection_params}} function. If not provided, the
#' \code{\link{get_connection_params}} will look for such object in the global environment
#'
#' @return A \code{\link{sample_infos}} object, consisting of a list containing two elements: a dataframe with detailed sample information from the UNIFI API, and a formatted JSON string of the sample information.
#'
#' Example usage:
#' con_params <- create_connection_params()
#' results <- get_sample_infos(sample_result_id, con_params)
#'
#' @export

get_sample_infos <- function(sample_id, connection_params = NULL) {
if(is.null(connection_params))
  connection_params = get_connection_params(parent.frame())
hostUrl = connection_apihosturl(connection_params)
token = connection_token(connection_params)

# get sample name and analysis name for creating folder and file on disk
parentAnalysisEndpoint = glue::glue("{hostUrl}/sampleresults({sample_id})/analyses")
parentAnalysis = httr::content(httpClientPlain(parentAnalysisEndpoint, token), "text", encoding = "utf-8")
parentAnalysisInfo = jsonlite::fromJSON(parentAnalysis)
parentAnalysisId = parentAnalysisInfo$value$id
# save sample list with custom names avoiding duplicates
samplelist = get_samples_list(parentAnalysisId, connection_params)
# defining data.table variable locally to avoid R cmd check NOTES due to NSE
id = NULL
sample_metadata = samplelist[id %in% sample_id, ]
# convert all empty or NA/NaN values to NaN character
sample_metadata[sample_metadata==""] = "NaN"
sample_metadata[is.na(sample_metadata)] = "NaN"
sample_metadata_json = jsonlite::toJSON(sample_metadata, pretty = T)
sample_metadata_json = as.character(sample_metadata_json)

spectrumInfosEndpoint <- glue::glue("{hostUrl}/sampleresults({sample_id})/spectruminfos")
rg <- httpClientPlain(spectrumInfosEndpoint, token)
json_string <- httr::content(rg, "text", encoding = "UTF-8")
spectrum_infos <- jsonlite::fromJSON(json_string)
spectrum_metadata_json <- jsonlite::toJSON(spectrum_infos, pretty = TRUE, auto_unbox = TRUE)
spectrum_metadata_json = as.character(spectrum_metadata_json)

spectrum_metadata = data.table(spectrum_infos$value)
spectrum_metadata = tidytable::unnest(spectrum_metadata, names_sep = ".")
spectrum_metadata = as.data.table(spectrum_metadata)
# convert all empty or NA/NaN values to NaN character
spectrum_metadata[spectrum_metadata==""] = "NaN"
spectrum_metadata[is.na(spectrum_metadata)] = "NaN"

ret = sample_infos(
          sample_metadata = sample_metadata,
          spectrum_metadata = spectrum_metadata,
          sample_metadata_json = sample_metadata_json,
          spectrum_metadata_json = spectrum_metadata_json
)
return(ret)
}
