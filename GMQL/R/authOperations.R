#' Login to GMQL
#'
#' Login to GMQL REST services suite as a registered user, specifiyng username and password,
#' or as guest
#'
#' @import httr
#'
#' @param url single string url of server: it must contain the server address and base url;
#' service name will be added automatically
#' @param username single string name used during signup
#' @param password single string password used during signup
#'
#' @seealso  \code{\link{register.GMQL}} \code{\link{logout.GMQL}}
#'
#' @details
#' if both username and password are NULL you will be logged as guest
#' After login you will receive an authentication token.
#' As token remains vaild on server (until the next login / registration) a user can safely use a token for
#' a previous session as a convenience, this token is saved in Global environment to perform
#' subsequent REST call even on complete R restart (if is environemnt has been saved, of course ...)
#' If error occured a specific error will be printed
#'
#'
#'
#' @examples
#'
#' \dontrun{
#'
#' url <- <http_server_address>
#' login.GMQL(url)
#' login.GMQL(url, username="pippo",password="baudo")
#' }
#'
login.GMQL <- function(url,username = NULL, password = NULL)
{
  as_guest <- TRUE

  if(!is.null(username) || !is.null(password))
    as_guest <- FALSE

  if(as_guest)
  {
    h <- c('Accept' = "Application/json")
    URL <- paste0(url,"/guest")
    req <- httr::GET(URL,httr::add_headers(h))
  }
  else
  {
    h <- c('Accept' = "Application/json",'Content-Type' = 'Application/json')
    URL <- paste0(url,"/login")
    body <- list('username'=username,'password'=password)
    req <- httr::POST(URL,httr::add_headers(h),body = body,encode = "json")
  }

  content <- httr::content(req)

  if(req$status_code !=200)
    stop(content$errorString)
  else
  {
    authToken <<- content$authToken
    print(paste("your Token is",authToken))
  }
}


#' Logout from GMQL
#'
#' Logout from GMQL REST services suite
#'
#' @import httr
#'
#' @param url single string url of server: it must contain the server address and base url;
#' service name will be added automatically
#'
#' @seealso \code{\link{register.GMQL}} \code{\link{logout.GMQL}}
#'
#' @details
#' After logout the authentication token will be invalidated.
#' The authentication token is removed from Global environment
#' If error occured a specific error will be printed
#'
#' @examples
#'
#' \dontrun{
#'
#' url <- <http_server_address>)
#' login.GMQL(url, username="pippo",password="baudo")
#' logout.GMQL()
#' }
logout.GMQL <- function(url)
{
  URL <- paste0(url,"/logout")
  h <- c('X-Auth-Token' = authToken)
  req <- httr::GET(URL, httr::add_headers(h))
  content <- httr::content(req)
  if(req$status_code !=200)
    stop(content$error)
  else
  {
    print(content)
    #delete token from environment
    rm(authToken, envir = .GlobalEnv)
  }
}

#' Sign up to GMQL
#'
#' Sign up to GMQL rest services suite
#'
#' @import httr
#'
#' @param url single string url of server: it must contain the server address and base url;
#' service name will be added automatically
#' @param name single string first name of the user (can contain space)
#' @param lastname single string last name of the user (can contain space)
#' @param mail single string email (as spacified in RFC format)
#' @param username single string name you want to authenticate with
#' @param password single string password you want to authenticate with
#'
#'
#' @seealso \code{\link{register.GMQL}} \code{\link{logout.GMQL}}
#'
#' @details
#' After registration you receive an authentication token.
#' As token remains vaild on server (until the next login / registration) a user can safely use a token for
#' a previous session as a convenience, this token is saved in Global environment to perform
#' subsequent REST call even on complete R restart (if is environemnt has been saved, of course ...)
#' If error occured a specific error will be printed
#'
#'
#' @examples
#'
#' \dontrun{
#'
#' url <- <http_server_address>)
#' register.GMQL(url = url,"jonh","Doe","jonh@doe.com","JonhDoe46","password")
#' }

register.GMQL <- function(url, name, lastname, mail, username, password)
{
  info <- list('firstName'=name,
               'lastName'=lastname,
               'username'=username,
               'email'=mail,
               'password'=password
               )
  URL <- paste0(url,"/register")
  h <- c('Accept' = 'Application/json','Content-Type' = 'Application/json')
  req <<- httr::POST(URL,body = info ,httr::add_headers(h),encode = "json")

  content <- httr::content(req)

  if(req$status_code !=200)
    stop(content)
  else
  {
    authToken <<- content$authToken
    print(paste("your Token is",authToken))
  }
}



