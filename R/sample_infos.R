#' @include main.R
NULL

#' Class containing a sample's information
#'
#' Contains sample metadata in table and json formats.
#'
#' Objects for this class are returned by \code{\link{get_sample_infos}}.
#'
#' @slot sample_metadata Contains a \code{dataframe} with the sample metadata.
#' @slot formatted_json Contains a \code{character} with the sample metadata.
#'
#' @section Use the \code{\link{get_sample_infos}} to:
#'   store the sample metadata, in table and json formats.
#'
#' @param obj The \code{\link{sample_infos}} object to access.
#'
#' @export
sample_infos <- setClass("sample_infos",
                        slots = c(sample_metadata = "data.frame", formatted_json = "character"))
# initialize method during object instantiation
setMethod("initialize", signature = "sample_infos",
          definition = function(.Object, sample_metadata, formatted_json)
          {
              .Object@sample_metadata <- sample_metadata
              .Object@formatted_json <- formatted_json
            return(.Object)
          } )

#' Retrieve Sample Information from Unifi API
#'
#' This function retrieves spectrum information from the Unifi API for a specified sample result using the provided connection parameters.
#' It extracts detailed spectrum information associated with the given sample result identifier.
#' The function returns both a dataframe containing the sample information and a formatted JSON string of the same data.
#'
#' @param connection_params Connection parameters to the Unifi API, including the API host URL and access token.
#' @param sample_id The identifier of the sample result for which spectrum information is to be retrieved.
#' @return A \code{\link{sample_infos}} object, consisting of a list containing two elements: a dataframe with detailed sample information from the Unifi API, and a formatted JSON string of the sample information.
#'
#' Example usage:
#' con_params <- create_connection_params(api_host_url, api_token)
#' results <- get_sample_infos(con_params, sample_result_id)
#'
#' @export

get_sample_infos <- function(connection_params, sample_id) {

hostUrl = connection_apihosturl(connection_params)
token = connection_token(connection_params)

# get sample name and analysis name for creating folder and file on disk
parentAnalysisEndpoint = glue::glue("{hostUrl}/sampleresults({sample_id})/analyses")
parentAnalysis = httr::content(httpClientPlain(parentAnalysisEndpoint, token), "text", encoding = "utf-8")
parentAnalysisInfo = jsonlite::fromJSON(parentAnalysis)
parentAnalysisId = parentAnalysisInfo$value$id
samplelist = get_sample_list(connection_params, parentAnalysisId)
# defining data.table variable locally to avoid R cmd check NOTES due to NSE
id = NULL
sample_metadata = samplelist[id %in% sample_id, ]
# convert all empty or NA/NaN values to NaN character
sample_metadata[sample_metadata==""] = "NaN"
sample_metadata[is.na(sample_metadata)] = "NaN"
formatted_json = jsonlite::toJSON(sample_metadata, pretty = T)
formatted_json = as.character(formatted_json)

ret = sample_infos(
          sample_metadata = sample_metadata,
          formatted_json = formatted_json
)
return(ret)
}

#' @describeIn sample_infos accessor method to obtain the sample_metadata table.
#' @return \code{get_sample_metadata} returns a data.frame object containing the sample metadata.
#' @aliases get_sample_metadata
#' @export
setMethod("get_sample_metadata", "sample_infos", function(obj) obj@sample_metadata)

#' @describeIn sample_infos accessor method to obtain the sample name.
#' @return \code{get_sample_metadata} returns a character object containing the sample name.
#' @aliases get_sample_name
#' @export
setMethod("get_sample_name", "sample_infos", function(obj) obj@sample_metadata$sampleName)

#' @describeIn sample_infos accessor method to obtain the analysis name.
#' @return \code{get_analysis_name} returns a character object containing the analysis name.
#' @aliases get_analysis_name
#' @export
setMethod("get_analysis_name", "sample_infos", function(obj) obj@sample_metadata$analysisName)

#' @describeIn sample_infos accessor method to obtain the sample metadata in json format.
#' @return \code{get_sample_metadata_json} returns a json character object containing the sample metadata.
#' @aliases get_sample_metadata_json
#' @export
setMethod("get_sample_metadata_json", "sample_infos", function(obj) jsonlite::prettify(obj@formatted_json))
