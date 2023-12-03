#' @include main.R
NULL

#' Class containing Unifi API connection parameters
#'
#' Contains url and token for connection to Unifi API.
#'
#' Objects for this class are returned by \code{\link{create_connection_params}}.
#'
#' @slot identityurl Contains a \code{character} with the URL for connection to the Unifi identity server.
#' @slot username Contains a \code{character} with the user name for connection to the Unifi identity server (e.g. administrator).
#' @slot password Contains a \code{character} with the user password for connection to the Unifi identity server (e.g. administrator).
#' @slot apihosturl Contains a \code{character} with the URL for connection to the Unifi API server (host).
#' @slot token A \code{character} with the token sent back from the Unifi API.
#'
#' @section Use the \code{\link{create_connection_params}} to:
#'   store the connection parameters,
#'   using the default parameters or replacing them with the needed URLs,
#'   username and password to the Unifi API. It will request a connection
#'   token that will be used by other functions to retrieve data.
#'
#' @param obj The \code{\link{connection_params}} object to access.
#'
#' @export
connection_params <- setClass("connection_params",
                        slots = c(identityurl = "character", username = "character", password = "character", apihosturl = "character", token = "character"))

# initialize method during object instantiation
setMethod("initialize", signature = "connection_params",
          definition = function(.Object, identityurl, username, password, apihosturl, token)
          {
              .Object@identityurl <- identityurl
              .Object@username <- username
              .Object@password <- password
              .Object@apihosturl <- apihosturl
              .Object@token <- token
            return(.Object)
          } )
#' Create connection parameters
#'
#' @details \code{create_connection_params} is an utility function that returns the url and token for Unifi API connection
#'
#' @param identityurl The \code{url} to connect to the Unifi API identity server.
#' @param username The \code{username} to connect to the Unifi API identity server.
#' @param password The \code{password} to connect to the Unifi API identity server.
#' @param apihosturl The \code{url} to connect to the Unifi API server (host).
#'
#' @return A list containing all parameters needed for the connection, in a \code{\link{connection_params}} object.
#' @export

create_connection_params <- function(identityurl = "http://localhost:50333/identity/connect/token", username = "administrator", password = "administrator", apihosturl = "http://localhost:50034/unifi/v1")
{
    r <- POST(identityurl,
          config = list(),
          body = list(
            grant_type="password",
            client_id="resourceownerclient",
            client_secret="secret",
            scope="unifi",
            username=username,
            password=password
          ),
          encode = "form"
      )
    c = content(r)
    bearer_token = c$access_token

    ret <- connection_params(identityurl = identityurl,
                            username = username,
                            password = password,
                            apihosturl = apihosturl,
                            token = bearer_token
            )
    return(ret)
}

#' @describeIn connection_params Accessor method to obtain the connection url.
#' @return \code{connection_apihosturl} returns a character object containing the connection url.
#' @aliases connection_apihosturl
#' @export
setMethod("connection_apihosturl", "connection_params", function(obj) obj@apihosturl)

#' @describeIn connection_params Accessor method to obtain the connection token.
#' @return \code{connection_token} returns a character object containing the connection token.
#' @aliases connection_token
#' @export
setMethod("connection_token", "connection_params", function(obj) obj@token)


# Helper functions to connect to API with URL, either collecting plain text or binary response
httpClientPlain = function(url, token) {
  httr::GET(url, add_headers(
                                   Accept="text/plain",
                                   "Authorization"=paste("Bearer", token)))
}

httpClientOctet = function(url, token) {
  httr::GET(url,
            add_headers("Content-Type"="application/x-www-form-urlencoded",
                        Accept="application/octet-stream",
              "Authorization"=paste("Bearer", token)))
}
