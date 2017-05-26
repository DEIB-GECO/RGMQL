#' GMQL API web Service
#'
#' It show all Jobs
#'
#' @param url server address
#'
#'
#'
showJobs <- function(url)
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
#' It show a log for specific job
#'
#' @param url server address
#' @param job_id id of jobs
#'
#'
#'
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
#' It stops a specific job
#'
#' @param url server address
#' @param job_id id of jobs
#'
#'
#'
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
#' It traces a specific job
#'
#' @param url server address
#' @param job_id id of jobs
#'
#'
#'
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
#' It runs a query
#'
#' @param url server address
#' @param fileName name of file
#' @param query text of query
#' @param output_gtf logical: file format for sample generating from query
#' the possiblities are: GTF or TAB
#'
#'
#'
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
#'
#' It runs a query from file
#'
#' @param url server address
#' @param fileName name of file
#' @param filePath path of txt files where you wrote a query
#' @param output_gtf logical: file format for sample generating from query
#' the possiblities are: GTF or TAB
#'
#'
#'
runQuery.fromfile <- function(url,fileName,filePath,output_gtf = T)
{
  if(!file.exists(filePath))
    stop("file does not exist")

  query <- read.table(filePath)
  runQuery(url,fileName,query,output_gtf)
}

#' GMQL API web Service
#'
#' It compile a query
#'
#' @param url server address
#' @param query txt query
#'
#'
#'
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

#' GMQL API web Service
#'
#' It compile a query from file
#'
#' @param url server address
#' @param filePath path of txt files where you wrote a query
#'
#'
compileQuery.fromfile <- function(url ,filePath)
{
  if(!file.exists(filePath))
   stop("file does not exist")

  query <- read.table(filePath)
  compileQuery(url,query)
}
