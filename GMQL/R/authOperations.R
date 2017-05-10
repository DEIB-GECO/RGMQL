
GenomicPolimiUrl <- "http://genomic.elet.polimi.it/gmql-rest"


#' GMQL API web Service
#'
#' Allow access to web service GMQL as guest or registered user
#' with username and password
#'
#'
#'
#' @param url server address
#' @param username user name
#' @param password user password
#' @return autentication Token
#' @import httr
#' @examples
#' GMQLlogin(url = http://...../...)
#' GMQLlogin(url, username="pippo",password="baudo")
#'

GMQLlogin <- function(url,username = NULL, password = NULL)
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
#' Allow access to web service GMQL as guest or registered user
#' with username and password
#'
#'
#' @param url server address
#' @import httr
#' @examples
#' GMQLlogin(url = http://...../...)
#' GMQLlogin(url, username="pippo",password="baudo")
#'
GMQLlogout <- function(url)
{
  h <- c('X-Auth-Token' = authToken)
  req <- GET(url, add_headers(h))
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
#' Register to web Service Provider
#'
#'
#' @param url server address
#' @param name user name
#' @param lastname user lastname
#' @param mail user mail
#' @param username user username
#' @param password user password
#'
#' @return autentication Token
#' @import httr
#' @examples
#' GMQLregister(url = http://...../...)
#'

GMQLregister <- function(url, name, lastname, mail, username, password)
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



