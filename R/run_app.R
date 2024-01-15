#' Run Shiny application to use the conversion package
#'
#' The function launches the Shiny app. The app can be used to connect to the UNIFI API,
#' navigate in folders and analyses available in UNIFI, select samples or analysis to convert,
#' and run the conversion. The destination folder of converted files
#' and their format (Parquet or HDF5) can be chosen.
#'
#' @export
run_app <- function() {
  appDir <- system.file("shiny-app", package = "arcMS")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing arcMS.", call. = FALSE)
  }

  shiny::runApp(appDir)
}
