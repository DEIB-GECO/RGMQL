#' Show all jobs
#'
#' It show all Jobs (runnig, succeded or failed) run by user
#'
#' @import httr
#' @param url single string url of server: it must contain the server address and base url;
#' service name will be added automatically
#' @return list of jobs
#' Every job in the list is identified by:
#' \itemize{
#' \item{id: id of job used for log, trace and stop}
#' }
#'
#' @seealso \code{\link{showJobLog}} @seealso \code{\link{stopJob}} @seealso \code{\link{traceJob}}
#'
#' @details
#' If error occured a specific error will be printed
#'
#'
showJobs <- function(url)
{
  URL <- paste0(url,"/jobs")
  h <- c('X-Auth-Token' = authToken)
  #req <- GET(url, add_headers(h),verbose(data_in = TRUE,info = TRUE))
  req <- httr::GET(URL, httr::add_headers(h))
  content <- httr::content(req,"parsed")
  if(req$status_code !=200)
    stop(content$error)
  else
    return(content)
}

#' Show a job log
#'
#' It show a job log for specific job
#'
#'
#' @import httr
#' @param url single string url of server: it must contain the server address and base url;
#' service name will be added automatically
#' @param job_id single string id of the job
#'
#' @return log text
#'
#'
#' @details
#' If error occured a specific error will be printed
#'
showJobLog <- function(url,job_id)
{
  URL <- paste0(url,"/jobs/",job_id,"/log")
  h <- c('X-Auth-Token' = authToken,'Accept'= 'Application/json')
  #req <- GET(url, add_headers(h),verbose(data_in = TRUE,info = TRUE))
  req <- httr::GET(URL, httr::add_headers(h))
  content <- httr::content(req,"parsed")
  if(req$status_code !=200)
    stop(content$error)
  else
    return(unlist(content,use.names = F))
}

#' Stop a job
#'
#' It stops a specific current job
#'
#' @import httr
#' @param url single string url of server: it must contain the server address and base url;
#' service name will be added automatically
#' @param job_id single string id of the job
#'
#'
#' @details
#' If error occured a specific error will be printed
#'
stopJob <- function(url,job_id)
{
  URL <- paste0(url,"/jobs/",job_id,"/stop")
  h <- c('X-Auth-Token' = authToken,'Accept'= 'text/plain')
  #req <- GET(url, add_headers(h),verbose(data_in = TRUE,info = TRUE))
  req <- httr::GET(URL, httr::add_headers(h))
  content <- httr::content(req,"parsed")
  if(req$status_code !=200)
    stop(content)
  else
    return(content)
}

#' Trace a job
#'
#' It traces a specific current job
#'
#' @import httr
#' @param url single string url of server: it must contain the server address and base url;
#' service name will be added automatically
#' @param job_id single string id of the job
#'
#'
#' @details
#' If error occured a specific error will be printed
#'
traceJob <- function(url, job_id)
{
  URL <- paste0(url,"/jobs/",job_id,"/trace")
  h <- c('X-Auth-Token' = authToken,'Accept'= 'Application/json')
  #req <- GET(url, add_headers(h),verbose(data_in = TRUE,info = TRUE))
  req <- httr::GET(URL, httr::add_headers(h))
  content <- httr::content(req,"parsed")
  if(req$status_code !=200)
    stop(content$error)
  else
    return(content)

}


#' Run GMQL query
#'
#' It runs a GMQL query
#'
#'
#' @import httr
#' @param url single string url of server: it must contain the server address and base url;
#' service name will be added automatically
#' @param fileName name of file
#' @param query text of query
#' @param output_gtf logical: file format for sample generating from query
#' the possiblities are: GTF or TAB
#'
#'
#' @details
#' If error occured a specific error will be printed
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

  #req <- httr::POST(URL,body = query ,httr::add_headers(h),encode = "json",verbose(data_in = T,data_out = T))
  req <- httr::POST(URL,body = query ,httr::add_headers(h),encode = "json")
  #req <- GET(URL,add_headers(h))
  content <- httr::content(req,"parsed")
  if(req$status_code !=200)
    stop(content$error)
  else
    return(content)
}

#' Run GMQL query
#'
#' It runs a GMQL query from file
#'
#'
#' @import httr
#' @param url single string url of server: it must contain the server address and base url;
#' service name will be added automatically
#' @param fileName name of file
#' @param filePath path of txt files where you wrote a query
#' @param output_gtf logical: file format for sample generating from query
#' the possiblities are: GTF or TAB
#'
#'
#' @details
#' If error occured a specific error will be printed
#'
runQuery.fromfile <- function(url,fileName,filePath,output_gtf = T)
{
  if(!file.exists(filePath))
    stop("file does not exist")

  query <- readLines(filePath)
  runQuery(url,fileName,query,output_gtf)
}

#' Compile GMQL query
#'
#' It compiles a GMQL query
#'
#' @import httr
#' @param url single string url of server: it must contain the server address and base url;
#' service name will be added automatically
#' @param query text query
#'
#'
#'
compileQuery <- function(url ,query)
{
  h <- c('Accept' = "Application/json",
         'Content-Type' = 'text/plain','X-Auth-Token' = authToken)
  URL <- paste0(url,"/queries/compile")

  #req <- GET(url, add_headers(h),verbose(data_in = TRUE,info = TRUE))
  req <- httr::POST(URL,body = query ,httr::add_headers(h),encode = "json")
  content <- httr::content(req,"parsed")
  if(req$status_code !=200)
    stop(content$error)
  else
    return(content)
}

#' Compile GMQL query from file
#'
#' It compiles a GMQL query from file
#'
#' @param url single string url of server: it must contain the server address and base url;
#' service name will be added automatically
#' @param filePath path of txt files where you wrote a query
#'
#' @details
#' If error occured a specific error will be printed
#'
compileQuery.fromfile <- function(url ,filePath)
{
  if(!file.exists(filePath))
   stop("file does not exist")

  query <- readLines(filePath)
  compileQuery(url,query)
}
