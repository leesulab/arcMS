#' Add Drift Time to Unnested Spectral Data
#'
#' This function takes an unnested data table with spectral data and adds drift time
#'
#' @param connection_params Connection parameters to the Unifi API - url and token
#' @param unnestdt An unnested data table that contains at least the columns 'bin'.
#' @param sampleid The sample ID to be used in the API call.
#' @return The unnested data table with an additional column 'dt' for drift time.
#' @export

add_drift_time <- function(connection_params, unnestdt, sampleid) {

    url = connection_apihosturl(connection_params)
    token = connection_token(connection_params)

    url2 <- glue::glue("{url}/sampleresults({sampleid})/spectra/mass.mse/convertbintodrifttime")
    rg <- httr::POST(url2,
        body = jsonlite::toJSON(list(bins = 1:200)),
        add_headers("Content-Type" = "application/json",
        "Authorization" = paste("Bearer", token)))

    json_string <- httr::content(rg, "text", encoding = "UTF-8")
    infos <- jsonlite::fromJSON(json_string)

    unnestf = function(unnestdt){
        bin = NULL #needed to avoid R CMD check NOTES due to NSE
        dplyr::mutate(unnestdt, dt = infos$value[bin])
    }
    unnestdt = unnestf(unnestdt)

    return(unnestdt)
}
