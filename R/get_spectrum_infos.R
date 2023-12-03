#' Retrieve Spectrum Information from Unifi API
#'
#' This function retrieves spectrum information from the Unifi API for a specified sample result using the provided connection parameters.
#' It extracts detailed spectrum information associated with the given sample result identifier.
#' The function returns both a dataframe containing the spectrum information and a formatted JSON string of the same data.
#'
#' @param connection_params Connection parameters to the Unifi API, including the API host URL and access token.
#' @param sample_id The identifier of the sample result for which spectrum information is to be retrieved.
#' @return A list containing two elements: a dataframe with detailed spectrum information from the Unifi API,
#'         and a formatted JSON string of the spectrum information.
#'
#' Example usage:
#' con_params <- create_connection_params(api_host_url, api_token)
#' results <- get_spectrum_infos(con_params, sample_result_id)
#' spectrum_infos_df <- results$spectrum_infos
#' formatted_json <- results$formatted_json
#'
#' @export


get_spectrum_infos <- function(connection_params, sample_id) {
  url <- connection_apihosturl(connection_params)
  token <- connection_token(connection_params)

  url2 <- glue::glue("{url}/sampleresults({sample_id})/spectruminfos")
  rg <- httr::GET(url2,
                  add_headers("Content-Type"="application/x-www-form-urlencoded",
                              Accept="application/json",
                              "Authorization"=paste("Bearer", token)))

  json_string <- httr::content(rg, "text", encoding = "UTF-8")
  spectrum_infos <- jsonlite::fromJSON(json_string)
  formatted_json <- jsonlite::toJSON(spectrum_infos, pretty = TRUE, auto_unbox = TRUE)
  spectrum_infos_df = data.table(spectrum_infos$value)
  spectrum_infos_df = tidytable::unnest(spectrum_infos_df, names_sep = ".")
  # convert all empty or NA/NaN values to NaN character
  spectrum_infos_df[spectrum_infos_df==""] = "NaN"
  spectrum_infos_df[is.na(spectrum_infos_df)] = "NaN"
  return(list(spectrum_infos = spectrum_infos_df, formatted_json = formatted_json))
}
