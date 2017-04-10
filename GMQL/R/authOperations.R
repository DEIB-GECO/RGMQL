#' GMQL API web Service
#'
#' Allow access to web service GMQL as guest or registered user
#' with username and password
#'
#'
#'
#'
#' @param username user name
#' @param password user password


GMQLlogin <- function(username = NULL, password = NULL)#,as_guest = TRUE)
{
  as_guest <- TRUE

  if(!is.null(username) || !is.null(password))
    as_guest <- FALSE

  if(as_guest)
  {
    h <- c('Accept' = "Application/json")
    url <- "http://genomic.elet.polimi.it/gmql-rest/guest"
    #req <- GET(url, add_headers(h),verbose(data_in = TRUE,info = TRUE))
    req <- GET(url, add_headers(h))
  }
  else
  {
    h <- c('Accept' = "Application/json",'Content-Type' = 'Application/json')
    url <- "http://genomic.elet.polimi.it/gmql-rest/login"
    body <- list('username'=username,'password'=password)
   # req <- POST(url,add_headers(h),body = body,verbose(data_in = T,info = T),encode = "json")
    req <- POST(url,add_headers(h),body = body,encode = "json")
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
#'
#'

GMQLlogout <- function()
{
  url <- "http://genomic.elet.polimi.it/gmql-rest/logout"
  h <- c('X-Auth-Token' = authToken)
  req <- GET(url, add_headers(h))
  #req <- GET(url, add_headers(h),verbose(data_in = TRUE,info = TRUE))
  content <- httr::content(req)
  if(req$status_code !=200)
    stop(content$error)
  else
    print(content)
}

#' GMQL API web Service
#'
#' Allow access to web service GMQL as guest or registered user
#' with username and password
#'
#'
#'
#'

GMQLregister <- function(name,lastname,mail,username,password)
{
  url <- "http://genomic.elet.polimi.it/gmql-rest/register"
  info <- list('firstName'=name,
               'lastName'=lastname,
               'username'=username,
               'email'=mail,
               'password'=password
               )
  h <- c('Accept' = 'Application/json','Content-Type' = 'Application/json')
  #req <<- POST(url,body = info ,add_headers(h),encode = "multipart",verbose(data_in = TRUE,info = TRUE))
  req <<- POST(url,body = info ,add_headers(h),encode = "json")

  content <- httr::content(req)

  if(req$status_code !=200)
    stop(content)
  else
  {
    authToken <<- content$authToken
    print(paste("your Token is",authToken))
  }
}



