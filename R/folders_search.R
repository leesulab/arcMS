#' Retrieve Folders from Unifi API
#'
#' This function retrieves folder information from the Unifi API using the provided connection parameters.
#'
#' @param connection_params Connection parameters to the Unifi API, including the API host URL and access token.
#' @return A data frame containing folder information from the Unifi API.
#' @export
#'


folders_search <- function(connection_params) {
  url <- connection_apihosturl(connection_params)
  token <- connection_token(connection_params)

  url2 <- glue::glue("{url}/folders")

  rg <- httr::GET(url2,
                  add_headers("Content-Type"="application/x-www-form-urlencoded",
                              Accept="text/plain",
                              "Authorization"=paste("Bearer", token)))


  json_string <- httr::content(rg, "text", encoding = "UTF-8")
  folders <- jsonlite::fromJSON(json_string)
  folders <- data.frame(folders$value)

  return(folders)
}
