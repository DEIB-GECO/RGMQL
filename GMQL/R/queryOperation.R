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

allJobs <- function(url ="http://genomic.elet.polimi.it/gmql-rest/jobs" , datasetName)
{
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



runQuery <- function(url = "http://genomic.elet.polimi.it/gmql-rest/queries/run",fileName,query,output)
{
  out <- check(fileOutput,output)

  URL <- paste0(url,"/",fileName,"/",out)
  h <- c('Accept' = "Application/json",
         'Content-Type' = 'text/plain','X-Auth-Token' = authToken)

  req <- POST(URL,body = query ,add_headers(h),encode = "json",verbose(data_in = T,info = T))
  #req <- GET(url,add_headers(h),verbose(data_in = TRUE,info = TRUE))
  #req <- GET(URL,add_headers(h))
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


compileQuery <- function(url ="http://genomic.elet.polimi.it/gmql-rest/queries/compile" ,query)
{
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
