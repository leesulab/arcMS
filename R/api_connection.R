#' @include main.R
NULL

#' Class containing UNIFI API connection parameters
#'
#' Contains url and token for connection to UNIFI API.
#'
#' Objects for this class are returned by \code{\link{create_connection_params}}.
#'
#' @slot identityurl Contains a \code{character} with the URL for connection to the UNIFI identity server.
#' @slot username Contains a \code{character} with the user name for connection to the UNIFI identity server (e.g. administrator).
#' @slot password Contains a \code{character} with the user password for connection to the UNIFI identity server (e.g. administrator).
#' @slot apihosturl Contains a \code{character} with the URL for connection to the UNIFI API server (host).
#' @slot token A \code{character} with the token sent back from the UNIFI API.
#'
#' @section Use the \code{\link{create_connection_params}} to:
#'   store the connection parameters,
#'   using the default parameters or replacing them with the needed URLs,
#'   username and password to the UNIFI API. It will request a connection
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
#' @details \code{create_connection_params} is an utility function that returns the url and token for UNIFI API connection
#'
#' @param identityurl The \code{url} to connect to the UNIFI API identity server.
#' @param username The \code{username} to connect to the UNIFI API identity server.
#' @param password The \code{password} to connect to the UNIFI API identity server.
#' @param apihosturl The \code{url} to connect to the UNIFI API server (host).
#' @param install if TRUE, will install the token in your \code{.Renviron} file for use in future sessions.  Defaults to TRUE.
#' @param verbose if TRUE, will print messages.  Defaults to TRUE.
#' @return A list containing all parameters needed for the connection, in a \code{\link{connection_params}} object.
#' @export

create_connection_params <- function(identityurl = "http://localhost:50333/identity/connect/token", username = "administrator", password = "administrator", apihosturl = "http://localhost:50034/unifi/v1", install = TRUE, verbose = TRUE)
{
    parsed_url = httr::parse_url(apihosturl)
    api_version = if (parsed_url$port == 50034) 3 else 4
    client_scope = if (api_version == 3) "unifi" else "webapi"
    # don't check certificate for LAN access with IP address:
    httr::set_config(httr::config(ssl_verifypeer = 0L, ssl_verifyhost = 0L))

    # with httr2
    # req <- request("https://example.com") %>%
    #   req_options(ssl_verifypeer = 0, ssl_verifyhost = 0)
    r <- POST(identityurl,
          config = list(),
          body = list(
            grant_type="password",
            client_id="resourceownerclient",
            client_secret="secret",
            scope=client_scope,
            username=username,
            password=password
          ),
          encode = "form"
      )

    c = content(r)
    bearer_token = c$access_token

    store_unifi_api_token(bearer_token, install = install, overwrite = TRUE, verbose = verbose)

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

#' @describeIn connection_params Accessor method to obtain the connection token.
#' @return \code{connection_identityurl} returns a character object containing the connection identity url.
#' @aliases connection_identityurl
#' @export
setMethod("connection_identityurl", "connection_params", function(obj) obj@identityurl)

#' @describeIn connection_params Accessor method to obtain the connection username.
#' @return \code{connection_username} returns a character object containing the connection username.
#' @aliases connection_username
#' @export
setMethod("connection_username", "connection_params", function(obj) obj@username)

#' @describeIn connection_params Accessor method to obtain the connection password.
#' @return \code{connection_password} returns a character object containing the connection password.
#' @aliases connection_password
#' @export
setMethod("connection_password", "connection_params", function(obj) obj@password)

# Helper functions to connect to API with URL, either collecting plain text or binary response
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


#' Save the API token in the \code{.Renviron} file for repeated use
#' @description This function will add the UNIFI API token to your \code{.Renviron} file so it can be called securely without being stored
#' in your code. After you have installed your key, it can be called any time by typing \code{Sys.getenv("UNIFI_API_TOKEN")} and can be
#' used in package functions by simply typing UNIFI_API_TOKEN
#' If you do not have an \code{.Renviron} file, the function will create on for you.
#' If you already have an \code{.Renviron} file, the function will append the key to your existing file, while making a backup of your
#' original file for disaster recovery purposes.
#' Function obtained and adapted from the tidycensus package
#' @param token The API token retrieved from UNIFI formated in quotes.
#' @param install if TRUE, will install the token in your \code{.Renviron} file for use in future sessions.  Defaults to FALSE.
#' @param overwrite If this is set to TRUE, it will overwrite an existing UNIFI_API_TOKEN that you already have in your \code{.Renviron} file.
#' @importFrom utils write.table read.table
#' @examples
#'
#' \dontrun{
#' unifi_api_token("111111abc", install = TRUE)
#' # First time, reload your environment so you can use the key without restarting R.
#' readRenviron("~/.Renviron")
#' # You can check it with:
#' Sys.getenv("UNIFI_API_TOKEN")
#' }
#'
#' \dontrun{
#' # If you need to overwrite an existing key:
#' unifi_api_token("111111abc", overwrite = TRUE, install = TRUE)
#' # First time, relead your environment so you can use the key without restarting R.
#' readRenviron("~/.Renviron")
#' # You can check it with:
#' Sys.getenv("UNIFI_API_TOKEN")
#' }
#' @export

store_unifi_api_token <- function(token, overwrite = FALSE, install = FALSE, verbose = TRUE){

  if (install) {
    home <- Sys.getenv("HOME")
    renv <- file.path(home, ".Renviron")
    if(file.exists(renv)){
      # Backup original .Renviron before doing anything else here.
      file.copy(renv, file.path(home, ".Renviron_backup"))
    }
    if(!file.exists(renv)){
      file.create(renv)
    }
    else{
      if(isTRUE(overwrite)){
          if(verbose)
            message("Your original .Renviron will be backed up and stored in your R HOME directory if needed.")
          oldenv=read.table(renv, stringsAsFactors = FALSE)
          newenv <- oldenv[-grep("UNIFI_API_TOKEN", oldenv),]
          write.table(newenv, renv, quote = FALSE, sep = "\n",
                    col.names = FALSE, row.names = FALSE)
      }
      else{
        tv <- readLines(renv)
        if(any(grepl("UNIFI_API_TOKEN",tv))){
          stop("A UNIFI_API_TOKEN already exists. You can overwrite it with the argument overwrite=TRUE", call.=FALSE)
        }
      }
    }

    tokenconcat <- paste0("UNIFI_API_TOKEN='", token, "'")
    # Append API key to .Renviron file
    write(tokenconcat, renv, sep = "\n", append = TRUE)
    if(verbose)
        message('Your API token has been stored in your .Renviron and can be accessed by Sys.getenv("UNIFI_API_TOKEN"). \nTo use now, restart R or run `readRenviron("~/.Renviron")`')
    return(token)
  } else {
    message("To install your API token for use in future sessions, run this function with `install = TRUE`.")
    Sys.setenv(UNIFI_API_TOKEN = token)
  }

}

#' Retrieve the API token in the environment variable
#' @description This function will retrieve the UNIFI API token from the OS environment

get_unifi_api_token <- function(){

  if (Sys.getenv("UNIFI_API_TOKEN") == "") {
    stop("No UNIFI API token found. \nTo obtain one, use the `create_connection_params` function.")
    return(NULL)
  } else {
return(Sys.getenv('UNIFI_API_TOKEN'))
  }

}

#' Retrieve the connection parameters object
#' @description This function will retrieve the connection parameters object from
#' the global environment, created by the \code{\link{create_connection_params}} function
#' @param envir The environment to look for object containing connection parameters
#' @export
get_connection_params <- function(envir = parent.frame()){
  con_object_name = Filter(function(x) inherits(get(x), "connection_params"), ls(envir))
  if (identical(con_object_name, character(0))) {
    stop("No connection parameters found in the environment. \nTo create them, use the `create_connection_params` function.")
    return(NULL)
  } else {
    if(length(con_object_name) > 1) {
      stop("Several connection objects found. \nKeep only one of them, or select one of them as the `connection_params` argument of the selected function.")
    } else {
      con_object = get(con_object_name)
      return(con_object)
    }
  }

}

#' Check if the connection is still active before current request then return request
#' @description This function will check if the connection is still active
#' and the current token is valid before retrieving request results
#' @param endpoint The endpoint of request to be executed during/after checking
#' @param connection_params The current connection parameters object (with current token)
send_request <- function(request, connection_params){
    token = connection_token(connection_params)
    # check if connection still active, else get a new token
    # ifelse(type == "plain", request = httpClientPlain(endpoint, token), request = httpClientOctet(endpoint, token))
    # eval(request)
    e = parent.frame()
    if(httr::http_error(eval(request, envir = e))) {
       message("Regenerating connection")
       # get previous connection parameters
       identityUrl = connection_identityurl(connection_params)
       username = connection_username(connection_params)
       password = connection_password(connection_params)
       # issue new connection request and retrieve new token
       connection_params = create_connection_params(identityurl = identityUrl, username = username, password = password, apihosturl = hostUrl, verbose = FALSE)
       e$token = connection_token(connection_params)
       # ifelse(type == "plain", request = httpClientPlain(endpoint, token), request = httpClientOctet(endpoint, token))
   }
   return(eval(request, envir = e))
}
