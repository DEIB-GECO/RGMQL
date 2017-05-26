#' GMQL API web Service
#'
#' Log in to GMQL web service as guest or registered user
#' trough username and password
#'
#'
#'
#' @param url server address
#' @param username user name used on registration
#' @param password password used on registration
#'
#' @seealso  \code{\link{register.GMQL}} \code{\link{logout.GMQL}}
#' @examples
#'
#' @details
#' if username and password are NULL you will log in as user guest
#' After login you receive an authenticated token.
#' This token is saved in Global environment and used to perform the other REST call
#'
#'
#' \dontrun{
#' login.GMQL(url = <http_server_address>)
#' url <- <http_server_address>)
#' login.GMQL(url, username="pippo",password="baudo")
#' }

login.GMQL <- function(url,username = NULL, password = NULL)
{
  as_guest <- TRUE

  if(!is.null(username) || !is.null(password))
    as_guest <- FALSE

  if(as_guest)
  {
    h <- c('Accept' = "Application/json")
    URL <- paste0(url,"/guest")
    #req <- GET(url, add_headers(h),verbose(data_in = TRUE,info = TRUE))
    req <- GET(URL, add_headers(h))
  }
  else
  {
    h <- c('Accept' = "Application/json",'Content-Type' = 'Application/json')
    URL <- paste0(url,"/login")
    body <- list('username'=username,'password'=password)
   # req <- POST(url,add_headers(h),body = body,verbose(data_in = T,info = T),encode = "json")
    req <- POST(URL,add_headers(h),body = body,encode = "json")
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


#' GMQL API web Service
#'
#' Log out to GMQL web service
#'
#'
#' @param url server address
#'
#' @seealso  \code{\link{register.GMQL}} \code{\link{logout.GMQL}}
#'
#' @details
#' After logout you will not be able to call any services.
#' The authneticated token is removed from Global environment
#'
#' @examples
#'
#' \dontrun{
#' url <- <http_server_address>)
#' login.GMQL(url, username="pippo",password="baudo")
#' logout.GMQL()
#' }
logout.GMQL <- function(url)
{
  URL <- paste0(url,"/logout")
  h <- c('X-Auth-Token' = authToken)
  req <- GET(URL, add_headers(h))
  #req <- GET(url, add_headers(h),verbose(data_in = TRUE,info = TRUE))
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

#' GMQL API web Service
#'
#' Register to Web Service Provider
#'
#'
#' @param url server address
#' @param name first name
#' @param lastname lastname
#' @param mail email
#' @param username username
#' @param password password
#'
#'
#' @seealso \code{\link{register.GMQL}} \code{\link{logout.GMQL}}
#'
#' @details
#' After registration you receive an authenticated token.
#' This token is saved in Global environment and used to perform the other REST call
#'
#' @examples
#'
#' \dontrun{
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
  #req <<- POST(url,body = info ,add_headers(h),encode = "multipart",verbose(data_in = TRUE,info = TRUE))
  req <<- POST(URL,body = info ,add_headers(h),encode = "json")

  content <- httr::content(req)

  if(req$status_code !=200)
    stop(content)
  else
  {
    authToken <<- content$authToken
    print(paste("your Token is",authToken))
  }
}



