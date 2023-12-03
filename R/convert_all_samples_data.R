#' Get the list of samples from an Analysis
#'
#'The function connects to a Unifi Analysis and gets the list of all samples. It returns a dataframe with sample names and IDs, along with the analysis name.
#'
#' @param connection_params - Connection parameters to the Unifi API - url and token
#' @param analysis_id - The id of the analysis
#'
#' @return A dataframe wih sample names and IDs, along with the analysis name.

get_sample_list <- function(connection_params, analysis_id) {

  hostUrl = connection_apihosturl(connection_params)
  token = connection_token(connection_params)

  url1 = glue::glue("{hostUrl}/analyses({analysis_id})")
  rg <- httpClientPlain(url1, token)
  json_string <- httr::content(rg, "text", encoding = "UTF-8")
  infos = jsonlite::fromJSON(json_string)
  analysis_name = infos$name

  url2 = glue::glue("{hostUrl}/analyses({analysis_id})/sampleresults")
  rg2 <- httpClientPlain(url2, token)
  json_string <- httr::content(rg2, "text", encoding = "UTF-8")
  samplelistjson = jsonlite::fromJSON(json_string)
  samplelist = as.data.table(samplelistjson$value)
  samplelist = make_unique_sample_names(samplelist)
  samplelist = cbind(samplelist, analysisName = rep(analysis_name))
  return(samplelist)
}

#' Convert data from all samples in an Analysis
#'
#' The function collects all samples data from an Analysis using the collect_one_sample_data() function and save them on disk.
#'
#' @param connection_params Connection parameters to the Unifi API - url and token
#' @param analysis_id The id of the analysis
#' @param format The format chosen for the exported filed (parquet or HDF5)
#'
#' @return A ataframe for each sample is saved in parquet or HDF5 format in a folder named after the Analysis.
#' @export

convert_all_samples_data <- function(connection_params, analysis_id, format = 'parquet') {

      if (!format %in% c('parquet', 'HDF5')) {
    stop("The format must be parquet or HDF5")
  }

  samplelist = get_sample_list(connection_params, analysis_id)
  for (i in 1:(nrow(samplelist))) {
    print(glue::glue("-------- Number of samples collected {i}/{nrow(samplelist)} -------- \n"))
    sample_id = samplelist$id[i]

    convert_one_sample_data(connection_params, sample_id, format = format)
  }
  printf("All done!\n")

}

# create unique sample names
# duplicate names are changed by Unifi when generating the marker table, e.g. (A1, A1, A1, B1) gives (A1, A1_2, A1_3, B1)
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
