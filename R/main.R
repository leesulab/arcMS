#### Dependencies

#' @importFrom utils head tail modifyList setTxtProgressBar txtProgressBar write.csv write.table read.csv data getFromNamespace
#' @importFrom stats ave
#' @import methods
#' @import data.table
#' @import tidytable
#' @import dplyr
#' @import dtplyr
#' @import bitops
#' @import RProtoBuf
#' @import httr
#' @import jsonlite
#' @import glue
#' @import future
#' @import future.apply
#' @import progressr
#' @import arrow
#' @import withr
#' @import rhdf5
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
