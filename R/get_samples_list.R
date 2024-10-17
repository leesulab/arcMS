#' Retrieve the list of Samples Results from an Analysis in UNIFI API
#'
#' The function connects to a UNIFI Analysis and gets the list of all samples.
#' It returns a dataframe with sample names and IDs, along with the analysis name.
#'
#' @param analysis_id The identifier of the Analysis
#' @param connection_params OPTIONAL: Connection parameters object created by the
#' \code{\link{create_connection_params}} function. If not provided, the
#' \code{\link{get_connection_params}} will look for such object in the global environment
#'
#' @return A dataframe wih sample names and their IDs, along with the analysis name and sample metadata.
#'
#' @export

get_samples_list <- function(analysis_id, connection_params = NULL) {
  if(is.null(connection_params))
    connection_params = get_connection_params(parent.frame())
  hostUrl = connection_apihosturl(connection_params)
  token = connection_token(connection_params)

  url1 = glue::glue("{hostUrl}/analyses({analysis_id})")
  rg <- quote(httpClientPlain(url1, token))
  req = send_request(rg, connection_params)
  json_string <- httr::content(req, "text", encoding = "UTF-8")
  infos = jsonlite::fromJSON(json_string)
  analysis_name = infos$name

  url2 = glue::glue("{hostUrl}/analyses({analysis_id})/sampleresults")
  rg2 <- quote(httpClientPlain(url2, token))
  req2 = send_request(rg2, connection_params)
  json_string <- httr::content(req2, "text", encoding = "UTF-8")
  samplelistjson = jsonlite::fromJSON(json_string)
  samplelist = as.data.table(samplelistjson$value)
  samplelist = make_unique_sample_names(samplelist)
  samplelist = cbind(samplelist, analysisName = rep(analysis_name))

  return(samplelist)
}

# create unique sample names
# duplicate names are changed by UNIFI when generating the marker table, e.g. (A1, A1, A1, B1) gives (A1, A1_2, A1_3, B1)
# so we must use a custom make.unique function to append a suffix starting with 2 at the second occurence
custom_make_unique <- function(v1, sep = '.') {
    vec <- as.numeric(ave(v1, v1, FUN = seq_along))
    inds <- vec > 1
    v1[inds] <- paste(v1[inds], vec[inds], sep = sep)
    v1
  }

make_unique_sample_names <- function(samples_datatable){
  # sample names in marker tables combine the sample name and the replicate number, and add a suffix in case of duplicates (function custom_make_unique) (e.g. A_replicate_1 several times)
  sampleName = NULL
  samples_datatable[, new := do.call(paste, c(.SD, sep = "_replicate_")), .SDcols = c("name","sample.replicateNumber")]
  samples_datatable[, sampleName := unlist(custom_make_unique(as.character(samples_datatable$new), sep = '_'))]
  samples_datatable[,new:=NULL]
  return(samples_datatable)
}
