#' Convert bin value to CCS (Collision Cross Section) value
#' This function converts bin values to CCS values for the given sample.
#' This conversion should be performed for a limited number of detected peaks
#' and not on raw data to avoid too many requests and useless CCS calculations
#' for all m/z peaks and fragments.
#' Peak lists can be obtained after peak detection, for example using the DEIMoS Python library.
#' For more information on DEIMoS, see \url{https://deimos.readthedocs.io/en/latest/}.
#'
#' @param data An Arrow table object containing the sample data.
#'   This data should include the necessary columns: `bin`, `mz`, and `rt`.
#'   The metadata of this Arrow table should contain the `id` of the sample under `data$metadata$id`.
#' @param connection_params OPTIONAL: Connection parameters object created by the
#' \code{\link{create_connection_params}} function. If not provided, the
#' \code{\link{get_connection_params}} will look for such an object in the global environment.
#'
#' @return An Arrow table that includes the original data along with an additional column `CCS` containing the CCS values.
#'   The returned Arrow table will also retain the original metadata.
#' @export


convert_bin_to_ccs <- function(data, connection_params = NULL) {
    # Check if sample_id is accessible
    if (!"id" %in% names(data$metadata)) {
        stop("Sample ID is missing.")
    }
    sample_id <- data$metadata$id

    # Check connection parameters
    if (is.null(connection_params)) {
        connection_params <- get_connection_params()
        if (is.null(connection_params)) {
            stop("Connection parameters are required but were not provided or found.")
        }
    }

    if (!inherits(connection_params, "connection_params")) {
        stop("Invalid 'connection_params': Must be an object of class 'connection_params'.")
    }

    unnestdt <- dplyr::collect(data) %>% as.data.frame()

    # Add the rt column if necessary
    if ("retention_time" %in% names(unnestdt)) {
        unnestdt$rt <- unnestdt$retention_time
    }

    # Check for required columns
    if (!all(c("bin", "mz", "rt") %in% names(unnestdt))) {
        stop("The data frame 'unnestdt' must contain 'bin', 'mz', and 'rt' columns.")
    }

    # Prepare the API call
    hostUrl <- connection_apihosturl(connection_params)
    token <- connection_token(connection_params)
    sampleUrl <- glue::glue("{hostUrl}/sampleresults({sample_id})/spectra/mass.mse/convertbintoccs")

    body <- jsonlite::toJSON(list(
        bins = unnestdt$bin,
        mzs = unnestdt$mz,
        charges = rep(1, nrow(unnestdt)),
        retentiontimes = unnestdt$rt
    ))

    # Make API call
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

    # Update unnestdt with CCS values
    ccs_values <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
    unnestdt$CCS <- ccs_values$value

    arrow_table <- arrow::as_arrow_table(unnestdt)
    arrow_table$metadata <- data$metadata

    return(arrow_table)
}
