#' Convert Drift Time to Bin Number
#'
#' This function converts drift time values to bin numbers by querying the
#' UNIFI API endpoint \code{convertdrifttimetobin}.
#'
#' @param sample_id The identifier of the sample result.
#' @param drift_times A numeric vector of drift time values (in milliseconds) to convert.
#' @param connection_params OPTIONAL: Connection parameters object created by the
#' \code{\link{create_connection_params}} function.
#'
#' @return An integer vector of bin numbers corresponding to the input drift times.
#'
#' @export

convert_drifttime_to_bin <- function(sample_id, drift_times, connection_params = NULL) {
  if (is.null(connection_params))
    connection_params <- get_connection_params(parent.frame())
  hostUrl <- connection_apihosturl(connection_params)
  token <- connection_token(connection_params)

  url <- paste0(hostUrl, "/sampleresults(", sample_id, ")/spectra/mass.mse/convertdrifttimetobin")

  body <- jsonlite::toJSON(list(drifttimes = drift_times), auto_unbox = FALSE)
  response <- httr::POST(url,
    body = body,
    httr::add_headers(
      "Content-Type" = "application/json",
      "Authorization" = paste("Bearer", token)
    )
  )

  if (httr::http_error(response)) {
    stop("Error converting drift time to bin: ",
         httr::content(response, "text", encoding = "UTF-8"))
  }

  result <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
  return(result$value)
}


#' Convert Bin Numbers to Drift Time
#'
#' This function converts bin numbers to drift time values by querying the
#' UNIFI API endpoint \code{convertbintodrifttime}.
#'
#' @param sample_id The identifier of the sample result.
#' @param bins An integer vector of bin numbers to convert.
#' @param connection_params OPTIONAL: Connection parameters object created by the
#' \code{\link{create_connection_params}} function.
#'
#' @return A numeric vector of drift time values (in milliseconds) corresponding to the input bins.
#'
#' @export

convert_bin_to_drifttime <- function(sample_id, bins, connection_params = NULL) {
  if (is.null(connection_params))
    connection_params <- get_connection_params(parent.frame())
  hostUrl <- connection_apihosturl(connection_params)
  token <- connection_token(connection_params)

  url <- paste0(hostUrl, "/sampleresults(", sample_id, ")/spectra/mass.mse/convertbintodrifttime")

  body <- jsonlite::toJSON(list(bins = as.integer(bins)), auto_unbox = FALSE)
  response <- httr::POST(url,
    body = body,
    httr::add_headers(
      "Content-Type" = "application/json",
      "Authorization" = paste("Bearer", token)
    )
  )

  if (httr::http_error(response)) {
    stop("Error converting bin to drift time: ",
         httr::content(response, "text", encoding = "UTF-8"))
  }

  result <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
  return(result$value)
}


#' Convert Bin Numbers to Aligned Drift Time
#'
#' This function converts bin numbers to m/z-aligned drift time values by querying the
#' UNIFI API endpoint \code{convertbintoaligneddrifttime}. The alignment depends on the
#' m/z value provided.
#'
#' @param sample_id The identifier of the sample result.
#' @param bins An integer vector of bin numbers to convert.
#' @param mzs A numeric vector of m/z values for alignment (same length as bins, or a single value
#'   that will be recycled).
#' @param connection_params OPTIONAL: Connection parameters object created by the
#' \code{\link{create_connection_params}} function.
#'
#' @return A numeric vector of aligned drift time values corresponding to the input bins and m/z values.
#'
#' @export

convert_bin_to_aligned_drifttime <- function(sample_id, bins, mzs, connection_params = NULL) {
  if (is.null(connection_params))
    connection_params <- get_connection_params(parent.frame())
  hostUrl <- connection_apihosturl(connection_params)
  token <- connection_token(connection_params)

  # Recycle mzs if a single value

  if (length(mzs) == 1 && length(bins) > 1)
    mzs <- rep(mzs, length(bins))

  url <- paste0(hostUrl, "/sampleresults(", sample_id, ")/spectra/mass.mse/convertbintoaligneddrifttime")

  body <- jsonlite::toJSON(list(bins = as.integer(bins), mzs = mzs), auto_unbox = FALSE)
  response <- httr::POST(url,
    body = body,
    httr::add_headers(
      "Content-Type" = "application/json",
      "Authorization" = paste("Bearer", token)
    )
  )

  if (httr::http_error(response)) {
    stop("Error converting bin to aligned drift time: ",
         httr::content(response, "text", encoding = "UTF-8"))
  }

  result <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
  return(result$value)
}


#' Find the Best Bin for a Given Drift Time and m/z
#'
#' This function finds the bin number whose aligned drift time is closest to the target
#' drift time for a given m/z value. It uses the \code{convertbintoaligneddrifttime}
#' endpoint to get m/z-dependent aligned drift times for all 200 bins in a single API
#' call, then selects the bin with the closest aligned DT to the target.
#'
#' Additionally, it retrieves the raw bin from \code{convertdrifttimetobin} as a
#' reference point for comparison.
#'
#' @param sample_id The identifier of the sample result.
#' @param target_dt The target drift time value (in milliseconds), as reported by the
#'   UNIFI components table (\code{ims.driftTime}).
#' @param mz The m/z value for alignment.
#' @param connection_params OPTIONAL: Connection parameters object created by the
#' \code{\link{create_connection_params}} function.
#'
#' @return A list with elements:
#'   \item{best_bin}{The bin number closest to the target aligned drift time.}
#'   \item{best_dt}{The aligned drift time of the best bin.}
#'   \item{raw_bin}{The raw bin obtained from \code{convertdrifttimetobin} (for reference).}
#'   \item{bin_dt_table}{A data.table with columns \code{bin}, \code{aligned_dt} and
#'     \code{raw_dt} for all valid bins (positive aligned DT).}
#'
#' @export

find_best_bin <- function(sample_id, target_dt, mz, connection_params = NULL) {
  if (is.null(connection_params))
    connection_params <- get_connection_params(parent.frame())

  all_bins <- 1L:200L

  # Step 1: Get aligned drift times for all bins in a single API call
  aligned_dts <- convert_bin_to_aligned_drifttime(
    sample_id, bins = all_bins, mzs = mz, connection_params = connection_params
  )

  # Step 2: Get raw drift times for all bins in a single API call
  raw_dts <- convert_bin_to_drifttime(
    sample_id, bins = all_bins, connection_params = connection_params
  )

  # Step 3: Also get the raw bin from the target DT directly
  raw_bin <- tryCatch(
    convert_drifttime_to_bin(sample_id, target_dt, connection_params = connection_params),
    error = function(e) NA_integer_
  )

  # Build full lookup table
  bin_dt_table <- data.table::data.table(
    bin = all_bins,
    aligned_dt = aligned_dts,
    raw_dt = raw_dts
  )

  # Keep only bins with positive aligned DT
  valid_table <- bin_dt_table[aligned_dt > 0]

  if (nrow(valid_table) == 0) {
    # Fallback: use raw bin if aligned DTs are all invalid
    best_bin <- if (!is.na(raw_bin)) raw_bin else 1L
    return(list(
      best_bin = best_bin,
      best_dt = NA_real_,
      raw_bin = raw_bin,
      bin_dt_table = bin_dt_table
    ))
  }

  # Step 4: Find the bin whose aligned DT is closest to the target DT
  idx <- which.min(abs(valid_table$aligned_dt - target_dt))
  best_bin <- valid_table$bin[idx]
  best_dt  <- valid_table$aligned_dt[idx]

  return(list(
    best_bin = best_bin,
    best_dt = best_dt,
    raw_bin = raw_bin,
    bin_dt_table = bin_dt_table
  ))
}
