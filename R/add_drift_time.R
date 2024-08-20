#' Add Drift Time to Unnested Spectral Data
#'
#' This function takes a long format table with spectral data and adds drift time
#'
#' @param connection_params Connection parameters to the UNIFI API - url and token
#' @param long_data An unnested (long format) data table (in RAM or on disk)
#' containing at least a column 'bin'.
#' @param sample_id The sample ID to be used in the API call.
#' @return The same table with an additional column 'dt' for drift time.
#' @keywords internal

add_drift_time <- function(connection_params, long_data, sample_id) {

    url = connection_apihosturl(connection_params)
    token = connection_token(connection_params)

    url2 <- glue::glue("{url}/sampleresults({sample_id})/spectra/mass.mse/convertbintodrifttime")
    rg <- quote(httr::POST(url2,
        body = jsonlite::toJSON(list(bins = 1:200)),
        add_headers("Content-Type" = "application/json",
        "Authorization" = paste("Bearer", token))))
    req = send_request(rg, connection_params)
    json_string <- httr::content(req, "text", encoding = "UTF-8")
    infos <- jsonlite::fromJSON(json_string)

    driftvalues <- jsonlite::fromJSON(json_string)
    driftvalues <- driftvalues$value

    bin = NULL #needed to avoid R CMD check NOTES due to NSE
    if (inherits(long_data, "FileSystemDataset")) {
      bin_dt_table = long_data |> to_duckdb() |> select(bin) |> collect() |> mutate(dt = driftvalues[bin])
      long_data = long_data |> as_arrow_table() |> cbind(dt = bin_dt_table$dt)
      long_data |> arrow::write_parquet("long_data.parquet")
      long_data = open_dataset("long_data.parquet")
      #long_data |> mutate(dt = bin_dt_table$dt) |> write_parquet("long_with_dt.parquet")
    } else {
      long_data |> dplyr::mutate(dt = driftvalues[bin])
    }

    return(long_data)
}
