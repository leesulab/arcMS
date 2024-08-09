#### Dependencies
#' @import methods
#' @import data.table
#' @import dtplyr
#' @import bitops
#' @import withr
#' @importFrom utils head tail modifyList setTxtProgressBar txtProgressBar write.csv write.table read.csv data getFromNamespace
#' @importFrom stats ave
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate select collect
#' @importFrom tidytable uncount unnest
#' @importFrom RProtoBuf readProtoFiles
#' @importFrom httr GET add_headers content POST
#' @importFrom jsonlite fromJSON toJSON prettify
#' @importFrom glue glue
#' @importFrom future plan multisession
#' @importFrom future.apply future_lapply
#' @importFrom progressr progressor with_progress withProgressShiny
#' @importFrom arrow write_parquet read_parquet open_dataset
#' @importFrom rhdf5 h5write h5createFile
NULL # need this for doc generation

printf <- function(...) cat(sprintf(...), sep = "")

# Need to import these as functions generated with checkmate/withr don't always take namespace in to account
#' @importFrom checkmate makeAssertion vname
#' @importFrom withr defer
NULL

# For Rcpp
#' @importFrom Rcpp evalCpp
NULL

#### Generics

#' @include generics.R
NULL

### data.table . compatibility - see https://stackoverflow.com/questions/43662416/when-using-data-table-in-a-package-r-cmd-check-notes-no-visible-global-functio
`.` <- list

#### Document generation

#' UNIFI MS converter to Parquet and HDF5 formats
#'
#' \Sexpr[results=text,echo=FALSE]{packageDescription("arcMS", fields = "Description")}
#'
"_PACKAGE"
NULL
setOldClass("FileSystemDataset")
setClassUnion("dataframeOrDatatable", c("data.frame", "data.table"))
setClassUnion("dataframeOrDatatableOrArrowdataset", c("data.frame", "data.table", "FileSystemDataset"))
