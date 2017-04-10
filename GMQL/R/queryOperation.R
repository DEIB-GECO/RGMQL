
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

allJobs <- function(datasetName)
{
  url <- paste0("http://genomic.elet.polimi.it/gmql-rest/jobs")
  h <- c('X-Auth-Token' = authToken)
  #req <- GET(url, add_headers(h),verbose(data_in = TRUE,info = TRUE))
  req <- GET(url, add_headers(h))
  content <- httr::content(req,"parsed")
  if(req$status_code !=200)
    stop(content$error)
  else
    return(content)
}

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

logJob <- function()
{

}

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

stopJob <- function()
{

}

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

traceJob <- function()
{}


#' GMQL API web Service
#'
#' Allow access to web service GMQL as guest or registered user
#' with username and password
#'
#' si predilige il testo della query
#'
#'
#' @param username user name
#' @param password user password
#' @return authentication token



runQuery <- function(fileName,query, output)
{

  out <- check(output,flag)

  url <- paste0("http://genomic.elet.polimi.it/gmql-rest/queries/run",fileName,"/",out)
  h <- c('Accept' = "Application/json",
         'Content-Type' = 'text/plain','X-Auth-Token' = authToken)

  req <<- POST(url,body = query ,add_headers(h),encode = "json")
  #req <- GET(url,add_headers(h),verbose(data_in = TRUE,info = TRUE))
  req <- GET(url,add_headers(h))
  content <- httr::content(req,"parsed")
  if(req$status_code !=200)
    stop(content$error)
  else
    return(content)
}

#' GMQL API web Service
#'
#' Allow access to web service GMQL as guest or registered user
#' with username and password
#'
#' si predilige il testo della query
#'
#'
#' @param username user name
#' @param password user password
#' @return authentication token


compileQuery <- function(query)
{
  url <- paste0("http://genomic.elet.polimi.it/gmql-rest/queries/compile")
  h <- c('Accept' = "Application/json",
         'Content-Type' = 'text/plain','X-Auth-Token' = authToken)

  #req <- GET(url, add_headers(h),verbose(data_in = TRUE,info = TRUE))
  req <<- POST(url,body = query ,add_headers(h),encode = "json")
  content <- httr::content(req,"parsed")
  if(req$status_code !=200)
    stop(content$error)
  else
    return(content)
}
