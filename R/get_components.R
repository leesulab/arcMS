#' Retrieve Components from a Sample Result in UNIFI API
#'
#' This function retrieves component information from the UNIFI API for a specified
#' sample result. Components include identified and candidate markers with their
#' associated chromatographic, mass spectrometric, and ion mobility data.
#'
#' @param sample_id The identifier of the sample result for which components are to be retrieved.
#' @param connection_params OPTIONAL: Connection parameters object created by the
#' \code{\link{create_connection_params}} function. If not provided, the
#' \code{\link{get_connection_params}} will look for such object in the global environment.
#'
#' @return A data.table containing component information from the UNIFI API,
#'   including columns such as name, id, componentStatus, chromatographic.retentionTime,
#'   ms.massToChargeRatio, ims.driftTime, ims.collisionCrossSection, etc.
#'
#' @export

get_components <- function(sample_id, connection_params = NULL) {
  if (is.null(connection_params))
    connection_params <- get_connection_params(parent.frame())
  hostUrl <- connection_apihosturl(connection_params)
  token <- connection_token(connection_params)

  componentsEndpoint <- paste0(hostUrl, "/sampleresults(", sample_id, ")/components")

  rg <- quote(httpClientPlain(componentsEndpoint, token))
  req <- send_request(rg, connection_params)
  json_string <- httr::content(req, "text", encoding = "UTF-8")
  components_json <- jsonlite::fromJSON(json_string, flatten = TRUE)

  if (length(components_json$value) == 0) {
    stop("No components found for this sample result.")
  }

  components <- data.table::as.data.table(components_json$value)
  return(components)
}
