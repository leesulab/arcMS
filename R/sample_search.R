#' Retrieve Sample Results from Unifi API
#'
#' This function retrieves samples result information from the Unifi API for a specified analysis using the provided connection parameters.
#'
#' @param connection_params Connection parameters to the Unifi API, including the API host URL and access token.
#' @param analysis_id The identifier of the analysis for which sample results are to be retrieved.
#' @return A data frame containing samples result information from the Unifi API,
#'         with superfluous columns omitted for clarity.
#'
#' Example usage:
#' con_params <- create_connection_params(api_host_url, api_token)
#' sample_results <- sample_search(con_params, analysis_id)
#'
#' @export

sample_search <- function(connection_params, analysis_id) {
  url <- connection_apihosturl(connection_params)
  token <- connection_token(connection_params)

  url2 <- glue::glue("{url}/analyses({analysis_id})/sampleresults")
  rg <- httr::GET(url2,
                  add_headers("Content-Type"="application/x-www-form-urlencoded",
                              Accept="text/plain",
                              "Authorization"=paste("Bearer", token)))

  json_string <- httr::content(rg, "text", encoding = "UTF-8")
  samples <- jsonlite::fromJSON(json_string)
  samples = data.frame(samples$value)
  samples = samples[, -which(names(samples) %in% c("sample", "description", "components.odata.navigationLink",
  "spectra.odata.navigationLink", "spectrumInfos.odata.navigationLink",
  "chromatogramInfos.odata.navigationLink"))]
  return(samples)
}
