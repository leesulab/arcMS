#' @export
run_app <- function() {
  appDir <- system.file("shiny-app", package = "parquetMS")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing parquetMS.", call. = FALSE)
  }

  shiny::runApp(appDir)
}
