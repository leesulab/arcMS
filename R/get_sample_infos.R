#' Retrieve Sample Information from Unifi API
#'
#' This function retrieves spectrum information from the Unifi API for a specified sample result using the provided connection parameters.
#' It extracts detailed spectrum information associated with the given sample result identifier.
#' The function returns both a dataframe containing the sample information and a formatted JSON string of the same data.
#'
#' @param connection_params Connection parameters to the Unifi API, including the API host URL and access token.
#' @param sample_id The identifier of the sample result for which spectrum information is to be retrieved.
#' @return A list containing two elements: a dataframe with detailed sample information from the Unifi API,
#'         and a formatted JSON string of the sample information.
#'
#' Example usage:
#' con_params <- create_connection_params(api_host_url, api_token)
#' results <- get_sample_infos(con_params, sample_result_id)
#' sample_info <- results$sample_metadata_df
#' sample_info_json <- results$formatted_json
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

return(list(sample_metadata_df = sample_metadata, formatted_json = formatted_json))
}
