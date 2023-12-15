#' Retrieve Analyses from Unifi API
#'
#' This function retrieves items information from the Unifi API for a specified Unifi folder using the provided connection parameters.
#' The function filters and returns analysis items based on their type.
#'
#' @param connection_params Connection parameters to the Unifi API, including the API host URL and access token.
#' @param folder_id The identifier of the folder for which items are to be retrieved.
#' @return A data frame containing analysis item information from the Unifi API,
#'         with unnecessary columns removed for clarity.
#'
#' Example usage:
#' con_params <- create_connection_params(api_host_url, api_token)
#' analysis_items <- analysis_search(con_params, folder_id)
#'
#' @export

analysis_search <- function(folder_id) {
  print(exists(con))
  url <- connection_apihosturl(connection_params)
  token <- get_unifi_api_token()
  # token <- connection_token(connection_params)

  itemsEndpoint <- glue::glue("{url}/folders({folder_id})/items")

  rg <- httpClientPlain(itemsEndpoint, token)


  json_string <- httr::content(rg, "text", encoding = "UTF-8")
  items <- jsonlite::fromJSON(json_string)
  if(length(items$value) != 0) {
  analyses = items$value[items$value$type == 'Analysis',]
  analyses <- data.frame(analyses)

  analyses = analyses[, -which(names(analyses) %in% c("type", "autoExportStatus", "version", "creatorFullName",
    "modifiedAt", "modifierFullName", "dataOnline", "imported", "status",
    "restoredAt", "restoredByFullName", "dataType", "remark", "summary", "lockedByFullName"))]

  return(analyses)
} else {
  stop("There is no Analysis in this folder")
}

}
