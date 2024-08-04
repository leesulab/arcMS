#' Convert bin value to CCS (Collision Cross Section) value
#' This function converts bin values to CCS values for the given sample.
#' This conversion should be performed for a limited number of detected peaks
#' and not on raw data to avoid too many requests and useless CCS calculations
#' for all m/z peaks and fragments.
#' Peak lists can be obtained after peak detection, for example using the DEIMoS Python library.
#' For more information on DEIMoS, see \url{https://deimos.readthedocs.io/en/latest/}.
#'
#' @param sample_dataset A sample_dataset object containing the sample data.
#'   This data should include the necessary columns: `bin`, `mz`, and `rt`.
#'   The `id` of the sample should be present in the `sample_metadata` slot of the sample_dataset object.
#' @param connection_params OPTIONAL: Connection parameters object created by the
#' \code{\link{create_connection_params}} function. If not provided, the
#' \code{\link{get_connection_params}} will look for such an object in the global environment.
#'
#' @return A sample_dataset object including the original data and an additional column `CCS` containing the CCS values.
#'
#' @export


convert_bin_to_ccs <- function(sample_dataset, connection_params = NULL) {
    # Check if sample_id is accessible
    sample_metadata = get_sample_metadata(sample_dataset)
    if (!"id" %in% names(sample_metadata)) {
        stop("Sample ID is missing.")
    }
    sample_id <- sample_metadata$id

    # Check connection parameters
    if (is.null(connection_params)) {
        connection_params <- get_connection_params(parent.frame())
        if (is.null(connection_params)) {
            stop("Connection parameters are required but were not provided or found.")
        }
    }

    if (!inherits(connection_params, "connection_params")) {
        stop("Invalid 'connection_params': Must be an object of class 'connection_params'.")
    }

    sample_data = get_sample_data(sample_dataset)

    # Change rt column name if necessary
    if ("retention_time" %in% names(sample_data)) {
      setnames(sample_data, "retention_time", "rt")
    }

    # Check for required columns
    if (!all(c("bin", "mz", "rt") %in% names(sample_data))) {
        stop("The data must contain 'bin', 'mz', and 'rt' columns.")
    }

    # Prepare the API call
    hostUrl <- connection_apihosturl(connection_params)
    token <- connection_token(connection_params)
    sampleUrl <- glue::glue("{hostUrl}/sampleresults({sample_id})/spectra/mass.mse/convertbintoccs")

    body <- jsonlite::toJSON(list(
        bins = sample_data$bin,
        mzs = sample_data$mz,
        charges = rep(1, nrow(sample_data)),
        retentiontimes = sample_data$rt
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

    # Update data with CCS values
    ccs_values <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
    sample_data$ccs <- ccs_values$value

    sample_dataset@sample_data <- sample_data

    return(sample_dataset)
}
