#' Retrieve Individual MS Spectra from UNIFI API
#'
#' This function retrieves individual mass spectra from the UNIFI API for a given
#' sample, filtered by retention time range and energy level (Low for MS1,
#' High for MS2/MSe). The spectra are returned as protobuf binary data and
#' deserialized into R data structures.
#'
#' This is the R equivalent of the JavaScript \code{loadSpectra} function from the
#' UNIFI explorer app, which queries the
#' \code{sampleresults({id})/spectra/mass.mse} endpoint.
#'
#' @param sample_id The identifier of the sample result.
#' @param rt_min Minimum retention time (in minutes) for the filter.
#' @param rt_max Maximum retention time (in minutes) for the filter.
#' @param energy_level Energy level to filter: \code{"Low"} for MS1, \code{"High"} for MS2.
#' @param connection_params OPTIONAL: Connection parameters object created by the
#' \code{\link{create_connection_params}} function.
#'
#' @return A list of deserialized spectrum objects, each containing masses, intensities,
#'   scan sizes, retention time, energy level, and ionization polarity.
#'
#' @export

get_spectra_by_rt <- function(sample_id, rt_min, rt_max, energy_level = "Low",
                              connection_params = NULL) {
  if (is.null(connection_params))
    connection_params <- get_connection_params(parent.frame())
  hostUrl <- connection_apihosturl(connection_params)
  token <- connection_token(connection_params)

  energy_filter <- match.arg(energy_level, c("Low", "High"))

  url <- glue::glue(
    "{hostUrl}/sampleresults({sample_id})/spectra/mass.mse",
    "?$filter=energyLevel eq '{energy_filter}'",
    " and retentiontime ge {rt_min}",
    " and retentiontime le {rt_max}"
  )

  rg <- quote(httpClientOctet(url, token))
  req <- send_request(rg, connection_params)

  rg_content <- httr::content(req)

  if (is.null(rg_content) || length(rg_content) == 0) {
    return(list())
  }

  spectra_list <- deserialize_data(rg_content)
  return(spectra_list)
}


#' Count Available Spectra for a Sample
#'
#' @param sample_id The identifier of the sample result.
#' @param connection_params OPTIONAL: Connection parameters object.
#'
#' @return The total number of spectra available for the sample.
#'
#' @export

get_spectra_count <- function(sample_id, connection_params = NULL) {
  if (is.null(connection_params))
    connection_params <- get_connection_params(parent.frame())
  hostUrl <- connection_apihosturl(connection_params)
  token <- connection_token(connection_params)

  url <- glue::glue("{hostUrl}/sampleresults({sample_id})/spectra/mass.mse/$count")
  rg <- quote(httpClientPlain(url, token))
  req <- send_request(rg, connection_params)

  count_text <- httr::content(req, "text", encoding = "UTF-8")
  count <- as.numeric(iconv(count_text, "utf-8", "ascii", sub = ""))
  return(count)
}


#' Extract a Spectrum for a Specific Drift Time Bin from Protobuf Data
#'
#' Given a deserialized spectrum (a single element from the list returned by
#' \code{\link{get_spectra_by_rt}}), this function extracts the m/z and intensity
#' values corresponding to a specific drift time bin.
#'
#' This is the R equivalent of the JavaScript \code{filterSpectrum} function.
#'
#' @param spectrum A single deserialized spectrum object (one element from the
#'   output of \code{\link{get_spectra_by_rt}} or \code{\link{deserialize_data}}).
#' @param dt_bin The drift time bin number (1-200) to extract.
#'
#' @return A data.table with columns \code{masses} and \code{intensities} for
#'   the selected drift time bin, plus \code{retentionTime}, \code{energyLevel}
#'   and \code{ionizationPolarity} metadata.
#'
#' @export

extract_spectrum_at_bin <- function(spectrum, dt_bin) {
  scan_sizes <- spectrum$MassSpectrum$ScanSize
  masses <- spectrum$MassSpectrum$Masses
  intensities <- spectrum$Intensities
  rt <- spectrum$MassSpectrum$MSeMassSpectrum$RetentionTime
  energy <- spectrum$MassSpectrum$MSeMassSpectrum$EnergyLevel
  polarity <- spectrum$MassSpectrum$MSeMassSpectrum$IonizationPolarity

  if (length(scan_sizes) == 0 || dt_bin > length(scan_sizes)) {
    return(data.table::data.table(
      masses = numeric(0), intensities = numeric(0),
      retentionTime = numeric(0), energyLevel = integer(0),
      ionizationPolarity = integer(0)
    ))
  }

  # Calculate start index (sum of scan sizes before the target bin)
  if (dt_bin > 1) {
    start_idx <- sum(scan_sizes[1:(dt_bin - 1)]) + 1
  } else {
    start_idx <- 1
  }
  selected_size <- scan_sizes[dt_bin]

  if (selected_size == 0) {
    return(data.table::data.table(
      masses = numeric(0), intensities = numeric(0),
      retentionTime = rt, energyLevel = energy,
      ionizationPolarity = polarity
    ))
  }

  end_idx <- start_idx + selected_size - 1

  data.table::data.table(
    masses = masses[start_idx:end_idx],
    intensities = intensities[start_idx:end_idx],
    retentionTime = rt,
    energyLevel = energy,
    ionizationPolarity = polarity
  )
}


#' Combine Multiple Spectra Across Bins or Scans
#'
#' Combines spectra from multiple bins and/or scans by summing intensities of
#' peaks with similar m/z values (rounded to 2 decimal places), retaining the
#' exact m/z of the most intense peak within each group.
#'
#' @param spectra_list A list of data.tables, each with columns \code{masses}
#'   and \code{intensities}, as returned by \code{\link{extract_spectrum_at_bin}}.
#'
#' @return A data.table with columns \code{round_masses}, \code{total_intensities},
#'   \code{exact_masses}, and \code{num} (number of peaks combined).
#'
#' @export

combine_spectra <- function(spectra_list) {
  # Remove NULL or empty elements
  spectra_list <- Filter(function(x) !is.null(x) && nrow(x) > 0, spectra_list)
  if (length(spectra_list) == 0) {
    return(data.table::data.table(
      round_masses = numeric(0), total_intensities = numeric(0),
      exact_masses = numeric(0), num = integer(0)
    ))
  }

  combined <- data.table::rbindlist(spectra_list, fill = TRUE)
  combined <- combined[intensities != 0]
  combined <- combined[order(masses)]

  # Keep the exact mass of the highest intensity peak for each rounded mass group
  exact_mass_ref <- combined[, .SD[which.max(intensities)], by = .(round(masses, digits = 2))]
  exact_mass_ref <- exact_mass_ref[!duplicated(round)]
  data.table::setnames(exact_mass_ref, "round", "rounded")

  # Sum intensities by rounded mass
  combined[, round_masses := round(masses, digits = 2)]
  result <- combined[, .(total_intensities = sum(intensities), num = .N), by = round_masses]
  result[, exact_masses := exact_mass_ref$masses]

  return(result)
}


#' Retrieve Individual Spectra of a Marker from UNIFI
#'
#' This is the main high-level function that retrieves MS1 (Low energy) and MS2
#' (High energy) spectra for a specific marker, identified by its retention time,
#' drift time, and m/z value. It handles the full workflow:
#' \enumerate{
#'   \item Finding the best drift time bin via aligned drift time conversion
#'   \item Querying spectra filtered by retention time range
#'   \item Extracting spectra at the target bin
#'   \item Optionally combining spectra across bins (DT tolerance) and scans (RT scans)
#' }
#'
#' This is the R equivalent of the entire \code{loadSpectra} JavaScript function
#' from the UNIFI explorer app.
#'
#' @param sample_id The identifier of the sample result.
#' @param rt Retention time of the marker (in seconds, will be converted to minutes internally).
#' @param dt Drift time of the marker (in milliseconds).
#' @param mz m/z value of the marker.
#' @param rt_tolerance Retention time tolerance (in seconds, default 0.05).
#'   The range will be \code{[rt/60 - rt_tolerance, rt/60 + rt_tolerance]} in minutes.
#' @param dt_tolerance Drift time bin tolerance (number of bins around the best bin, default 1).
#' @param combine_scans Logical. If \code{TRUE}, combine all RT scans within the window (default \code{TRUE}).
#' @param combine_bins Logical. If \code{TRUE}, combine all DT bins within tolerance (default \code{FALSE}).
#' @param use_ims Logical. If \code{TRUE}, apply IMS filtering (default \code{TRUE}).
#' @param connection_params OPTIONAL: Connection parameters object created by the
#' \code{\link{create_connection_params}} function.
#'
#' @return A list with elements:
#'   \item{low}{A data.table with MS1 (low energy) spectrum data (\code{masses} and \code{intensities}).}
#'   \item{high}{A data.table with MS2 (high energy) spectrum data (\code{masses} and \code{intensities}).}
#'   \item{low_raw}{The raw deserialized list of low energy spectra.}
#'   \item{high_raw}{The raw deserialized list of high energy spectra.}
#'   \item{best_bin}{The best matching bin number.}
#'   \item{bin_dt_table}{A data.table mapping bins to aligned drift times.}
#'   \item{scan_count}{Number of scans in the RT window.}
#'
#' @export

get_marker_spectra <- function(sample_id, rt, dt, mz,
                               rt_tolerance = 0.05,
                               dt_tolerance = 1L,
                               combine_scans = TRUE,
                               combine_bins = FALSE,
                               use_ims = TRUE,
                               connection_params = NULL) {
  if (is.null(connection_params))
    connection_params <- get_connection_params(parent.frame())

  # Convert RT from seconds to minutes (UNIFI stores RT in minutes in the API filter)
  rt_min_value <- rt / 60
  rt_low <- rt_min_value - rt_tolerance
  rt_high <- rt_min_value + rt_tolerance

  # Get raw spectra (Low = MS1, High = MS2)
  spectra_low <- get_spectra_by_rt(sample_id, rt_low, rt_high, "Low", connection_params)
  spectra_high <- get_spectra_by_rt(sample_id, rt_low, rt_high, "High", connection_params)

  scan_count <- length(spectra_low)

  if (scan_count == 0) {
    return(list(
      low = data.table::data.table(masses = numeric(0), intensities = numeric(0)),
      high = data.table::data.table(masses = numeric(0), intensities = numeric(0)),
      low_raw = list(), high_raw = list(),
      best_bin = NA_integer_, bin_dt_table = data.table::data.table(),
      scan_count = 0L
    ))
  }

  # Find best bin if IMS is used
  best_bin <- NA_integer_
  bin_dt_table <- data.table::data.table()

  if (use_ims) {
    bin_info <- find_best_bin(sample_id, dt, mz, connection_params)
    best_bin <- bin_info$best_bin
    bin_dt_table <- bin_info$bin_dt_table

    dt_bin_min <- max(1L, best_bin - dt_tolerance)
    dt_bin_max <- min(200L, best_bin + dt_tolerance)

    if (combine_scans && combine_bins) {
      # Combine across both RT scans and DT bins
      all_low <- list()
      all_high <- list()
      for (scan_idx in seq_along(spectra_low)) {
        for (bin in dt_bin_min:dt_bin_max) {
          all_low[[length(all_low) + 1]] <- extract_spectrum_at_bin(spectra_low[[scan_idx]], bin)
          all_high[[length(all_high) + 1]] <- extract_spectrum_at_bin(spectra_high[[scan_idx]], bin)
        }
      }
      result_low <- combine_spectra(all_low)
      result_high <- combine_spectra(all_high)

    } else if (combine_scans) {
      # Combine across RT scans at the best bin
      all_low <- lapply(spectra_low, extract_spectrum_at_bin, dt_bin = best_bin)
      all_high <- lapply(spectra_high, extract_spectrum_at_bin, dt_bin = best_bin)
      result_low <- combine_spectra(all_low)
      result_high <- combine_spectra(all_high)

    } else if (combine_bins) {
      # Combine across DT bins at first scan
      all_low <- lapply(dt_bin_min:dt_bin_max, function(bin) {
        extract_spectrum_at_bin(spectra_low[[1]], bin)
      })
      all_high <- lapply(dt_bin_min:dt_bin_max, function(bin) {
        extract_spectrum_at_bin(spectra_high[[1]], bin)
      })
      result_low <- combine_spectra(all_low)
      result_high <- combine_spectra(all_high)

    } else {
      # Single scan, single bin
      result_low <- extract_spectrum_at_bin(spectra_low[[1]], best_bin)
      result_high <- extract_spectrum_at_bin(spectra_high[[1]], best_bin)
    }

  } else {
    # No IMS: just use masses/intensities directly
    if (combine_scans) {
      all_low <- lapply(spectra_low, function(sp) {
        data.table::data.table(masses = sp$MassSpectrum$Masses, intensities = sp$Intensities)
      })
      all_high <- lapply(spectra_high, function(sp) {
        data.table::data.table(masses = sp$MassSpectrum$Masses, intensities = sp$Intensities)
      })
      result_low <- combine_spectra(all_low)
      result_high <- combine_spectra(all_high)
    } else {
      result_low <- data.table::data.table(
        masses = spectra_low[[1]]$MassSpectrum$Masses,
        intensities = spectra_low[[1]]$Intensities
      )
      result_high <- data.table::data.table(
        masses = spectra_high[[1]]$MassSpectrum$Masses,
        intensities = spectra_high[[1]]$Intensities
      )
    }
  }

  # Normalize column names for combined results
  if ("exact_masses" %in% names(result_low)) {
    low_final <- data.table::data.table(masses = result_low$exact_masses, intensities = result_low$total_intensities)
    high_final <- data.table::data.table(masses = result_high$exact_masses, intensities = result_high$total_intensities)
  } else {
    low_final <- result_low[, .(masses, intensities)]
    high_final <- result_high[, .(masses, intensities)]
  }

  # Filter out noise (keep above 0.1% of max intensity)
  if (nrow(low_final) > 0) {
    low_final <- low_final[intensities > 0.001 * max(intensities)]
    low_final <- low_final[order(-intensities)]
  }
  if (nrow(high_final) > 0) {
    high_final <- high_final[intensities > 0.001 * max(intensities)]
    high_final <- high_final[order(-intensities)]
  }

  return(list(
    low = low_final,
    high = high_final,
    low_raw = spectra_low,
    high_raw = spectra_high,
    best_bin = best_bin,
    bin_dt_table = bin_dt_table,
    scan_count = scan_count
  ))
}


#' Retrieve Chromatogram Information from a Sample Result
#'
#' @param sample_id The identifier of the sample result.
#' @param connection_params OPTIONAL: Connection parameters object.
#'
#' @return A data.table with chromatogram metadata (id, name, type, etc.).
#'
#' @export

get_chromatogram_infos <- function(sample_id, connection_params = NULL) {
  if (is.null(connection_params))
    connection_params <- get_connection_params(parent.frame())
  hostUrl <- connection_apihosturl(connection_params)
  token <- connection_token(connection_params)

  url <- glue::glue("{hostUrl}/sampleresults({sample_id})/chromatograminfos")
  rg <- quote(httpClientPlain(url, token))
  req <- send_request(rg, connection_params)
  json_string <- httr::content(req, "text", encoding = "UTF-8")
  result <- jsonlite::fromJSON(json_string, flatten = TRUE)

  if (length(result$value) == 0) {
    return(data.table::data.table())
  }

  return(data.table::as.data.table(result$value))
}


#' Retrieve Chromatogram Data from a Sample Result
#'
#' @param sample_id The identifier of the sample result.
#' @param chromatogram_id The identifier of the chromatogram.
#' @param connection_params OPTIONAL: Connection parameters object.
#'
#' @return A data.table with columns \code{retentionTimes} and \code{intensities}.
#'
#' @export

get_chromatogram_data <- function(sample_id, chromatogram_id, connection_params = NULL) {
  if (is.null(connection_params))
    connection_params <- get_connection_params(parent.frame())
  hostUrl <- connection_apihosturl(connection_params)
  token <- connection_token(connection_params)

  url <- glue::glue("{hostUrl}/sampleresults({sample_id})/chromatograminfos({chromatogram_id})/data")
  rg <- quote(httpClientPlain(url, token))
  req <- send_request(rg, connection_params)
  json_string <- httr::content(req, "text", encoding = "UTF-8")
  result <- jsonlite::fromJSON(json_string)

  data.table::data.table(
    retentionTimes = result$retentionTimes,
    intensities = result$intensities
  )
}
