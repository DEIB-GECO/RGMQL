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

showJobs <- function(url, dataset_name)
{
  URL <- paste0(url,"/jobs")
  h <- c('X-Auth-Token' = authToken)
  #req <- GET(url, add_headers(h),verbose(data_in = TRUE,info = TRUE))
  req <- GET(URL, add_headers(h))
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

showJobLog <- function(url,job_id)
{
  URL <- paste0(url,"/jobs/",job_id,"/log")
  h <- c('X-Auth-Token' = authToken,'Accept'= 'Application/json')
  #req <- GET(url, add_headers(h),verbose(data_in = TRUE,info = TRUE))
  req <- GET(URL, add_headers(h))
  content <- httr::content(req,"parsed")
  if(req$status_code !=200)
    stop(content$error)
  else
    return(unlist(content))
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

stopJob <- function(url,job_id)
{
  URL <- paste0(url,"/jobs/",job_id,"/stop")
  h <- c('X-Auth-Token' = authToken,'Accept'= 'text/plain')
  #req <- GET(url, add_headers(h),verbose(data_in = TRUE,info = TRUE))
  req <- GET(URL, add_headers(h))
  content <- httr::content(req,"parsed")
  if(req$status_code !=200)
    stop(content)
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

traceJob <- function(url, job_id)
{
  URL <- paste0(url,"/jobs/",job_id,"/trace")
  h <- c('X-Auth-Token' = authToken,'Accept'= 'Application/json')
  #req <- GET(url, add_headers(h),verbose(data_in = TRUE,info = TRUE))
  req <- GET(URL, add_headers(h))
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



runQuery <- function(url,fileName,query,output_gtf = T)
{
  if(output_gtf)
    out <- "GTF"
  else
    out <- "TAB"

  URL <- paste0(url,"/queries/run/",fileName,"/",out)
  h <- c('Accept' = "Application/json",
         'Content-Type' = 'text/plain','X-Auth-Token' = authToken)

  req <- POST(URL,body = query ,add_headers(h),encode = "json",verbose(data_in = T,data_out = T))
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


compileQuery <- function(url ,query)
{
  h <- c('Accept' = "Application/json",
         'Content-Type' = 'text/plain','X-Auth-Token' = authToken)
  URL <- paste0(url,"/queries/compile")

  #req <- GET(url, add_headers(h),verbose(data_in = TRUE,info = TRUE))
  req <<- POST(URL,body = query ,add_headers(h),encode = "json")
  content <- httr::content(req,"parsed")
  if(req$status_code !=200)
    stop(content$error)
  else
    return(content)
}
