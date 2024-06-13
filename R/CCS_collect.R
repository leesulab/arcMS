#' Convert bin data to CCS (Collision Cross Sections)
#'
#' @param sample_id The id of the sample to be collected.
#' @param unnestdt A data frame containing the sample data, expected to include columns `bin`, `mz`, and `rt`.
#' @param connection_params OPTIONAL: Connection parameters object created by the
#' \code{\link{create_connection_params}} function. If not provided, the
#' \code{\link{get_connection_params}} will look for such an object in the global environment.
#'
#' @return A data frame with an additional column `CCS` containing the converted values.

#' @export
convert_bin_to_ccs <- function(sample_id, unnestdt, connection_params = NULL) {
    # Check if connection parameters were provided, if not, attempt to retrieve them
    if (is.null(connection_params)) {
        # Attempt to retrieve connection parameters from the global environment or other default
        connection_params <- get_connection_params()  # Assumes this function can handle fetching or creating parameters
        if (is.null(connection_params)) {
            stop("Connection parameters are required but were not provided or found.")
        }
    }
    
    # Validate connection parameters
    if (!inherits(connection_params, "connection_params")) {
        stop("Invalid 'connection_params': Must be an object of class 'connection_params'.")
    }

    # Ensure the unnestdt contains the required columns
    if (!all(c("bin", "mz", "rt") %in% names(unnestdt))) {
        stop("The data frame 'unnestdt' must contain 'bin', 'mz', and 'rt' columns.")
    }
    
    # Retrieve the API host URL and token from the connection parameters
    hostUrl <- connection_apihosturl(connection_params)
    token <- connection_token(connection_params)
    
    # Prepare the API URL and request body
    sampleUrl <- glue::glue("{hostUrl}/sampleresults({sample_id})/spectra/mass.mse/convertbintoccs")
    body <- jsonlite::toJSON(list(
        bins = unnestdt$bin,
        mzs = unnestdt$mz,
        charges = rep(1, nrow(unnestdt)),
        retentiontimes = unnestdrt
    ))
    
    # Make the API request
    response <- httr::POST(
        url = sampleUrl,
        body = body,
        httr::add_headers(
            "Content-Type" = "application/json",
            "Authorization" = paste("Bearer", token)
        )
    )
    
    # Check the response status
    if (httr::status_code(response) != 200) {
        stop("Error in API request: ", httr::content(response, "text", encoding = "UTF-8"))
    }
    
    # Parse the JSON response and update the data frame with CCS values
    ccs_values <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
    unnestdt$CCS <- ccs_values$value
    
    return(unnestdt)
}
