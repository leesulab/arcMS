#' Retrieve Folders from Unifi API
#'
#' This function retrieves folder information from the Unifi API using the provided connection parameters.
#'
#' @param connection_params Connection parameters to the Unifi API, including the API host URL and access token.
#' @return A data frame containing folder information from the Unifi API.
#' @export

folders_search <- function(connection_params) {
  hostUrl <- connection_apihosturl(connection_params)
  token <- get_unifi_api_token()
  # token <- connection_token(connection_params)

  foldersEndpoint <- glue::glue("{hostUrl}/folders")

  rg <- httpClientPlain(foldersEndpoint, token)

  json_string <- httr::content(rg, "text", encoding = "UTF-8")
  folders <- jsonlite::fromJSON(json_string)
  folders <- data.frame(folders$value)
  return(folders)
}
