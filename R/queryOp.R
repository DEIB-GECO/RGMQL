#' Show all jobs
#'
#' It show all Jobs (runnig, succeded or failed) run by user
#'
#' @import httr
#' @param url single string url of server: it must contain the server address and base url;
#' service name is added automatically
#'
#' @return list of jobs
#' Every job in the list is identified by:
#' \itemize{
#' \item{id: id of job used for log, trace and stop}
#' }
#'
#' @seealso \code{\link{showJobLog}} @seealso \code{\link{stopJob}} @seealso \code{\link{traceJob}}
#'
#' @details
#' If error occures a specific error is printed
#'
#' @examples
#'
#' PolimiUrl = "http://130.186.13.219/gmql-rest"
#' login.GMQL(PolimiUrl)
#' list_jobs <- showJobs(PolimiUrl)
#'
#' @export
#'
showJobs <- function(url)
{
  URL <- paste0(url,"/jobs")
  h <- c('X-Auth-Token' = authToken)
  #req <- GET(url, add_headers(h),verbose(data_in = TRUE,info = TRUE))
  req <- httr::GET(URL, httr::add_headers(h))
  content <- httr::content(req,"parsed")
  if(req$status_code !=200)
    print(content$error)
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
#' service name is added automatically
#' @param job_id single string id of the job
#'
#' @return log text
#'
#'
#' @details
#' If error occures a specific error is printed
#'
#' @examples
#'
#' \dontrun{
#' ## login with test user
#' PolimiUrl = "http://130.186.13.219/gmql-rest"
#' login.GMQL(PolimiUrl,"test101","test")
#' ## list all jobs
#' list_jobs <- showJobs(PolimiUrl)
#' jobs_1 <- list_jobs$jobs[[1]]
#' ## show log
#' showJobLog(PolimiUrl,jobs_1)
#' }
#' 
#' @export
#'
showJobLog <- function(url,job_id)
{
  URL <- paste0(url,"/jobs/",job_id,"/log")
  h <- c('X-Auth-Token' = authToken,'Accept'= 'Application/json')
  #req <- GET(url, add_headers(h),verbose(data_in = TRUE,info = TRUE))
  req <- httr::GET(URL, httr::add_headers(h))
  content <- httr::content(req,"parsed")
  if(req$status_code !=200)
    print(content$error)
  else
    print(unlist(content,use.names = FALSE))
}

#' Stop a job
#'
#' It stops a specific current job
#'
#' @import httr
#' @param url single string url of server: it must contain the server address and base url;
#' service name is added automatically
#' @param job_id single string id of the job
#'
#' @return None
#'
#' @details
#' If error occures a specific error is printed
#'
#'
#' @examples
#'
#' \dontrun{
#' PolimiUrl = "http://130.186.13.219/gmql-rest"
#' login.GMQL(PolimiUrl,"test101","test")
#' list_jobs <- showJobs(PolimiUrl)
#' jobs_1 <- list_jobs$jobs[[1]]
#' stopJob(PolimiUrl,jobs_1)
#' }
#' 
#' @export
#'
stopJob <- function(url,job_id)
{
  URL <- paste0(url,"/jobs/",job_id,"/stop")
  h <- c('X-Auth-Token' = authToken,'Accept'= 'text/plain')
  #req <- GET(url, add_headers(h),verbose(data_in = TRUE,info = TRUE))
  req <- httr::GET(URL, httr::add_headers(h))
  content <- httr::content(req,"parsed")
  if(req$status_code !=200)
    print(content)
  else
    print(content)
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
#' @return text trace log
#'
#' @details
#' If error occures a specific error is printed
#'
#' @examples
#' \dontrun{
#' PolimiUrl = "http://130.186.13.219/gmql-rest"
#' login.GMQL(PolimiUrl,"test101","test")
#' list_jobs <- showJobs(PolimiUrl)
#' jobs_1 <- list_jobs$jobs[[1]]
#' traceJob(PolimiUrl,jobs_1)
#' }
#' 
#'
#' @export
#'
traceJob <- function(url, job_id)
{
  URL <- paste0(url,"/jobs/",job_id,"/trace")
  h <- c('X-Auth-Token' = authToken,'Accept'= 'Application/json')
  req <- httr::GET(URL, httr::add_headers(h))
  content <- httr::content(req,"parsed")
  if(req$status_code !=200)
    print(content$error)
  else
    return(content)

}


#' Run GMQL query
#'
#' It runs a GMQL query as single string
#'
#'
#' @import httr
#' @param url single string url of server: it must contain the server address and base url;
#' service name is added automatically
#' @param fileName single string name of the file
#' @param query single string text of the query
#' @param output_gtf logical: file format for samples generated from query
#' the possiblities are: GTF or TAB
#'
#' @return None
#'
#' @details
#' If error occures a specific error is printed
#'
#'
#' @examples
#'
#' PolimiUrl = "http://130.186.13.219/gmql-rest"
#' login.GMQL(PolimiUrl)
#' runQuery(PolimiUrl, "query_1", "DATA_SET_VAR = SELECT() HG19_TCGA_dnaseq;
#' MATERIALIZE DATA_SET_VAR INTO RESULT_DS;", output_gtf = FALSE)
#'
#' @export
#'
runQuery <- function(url,fileName,query,output_gtf = TRUE)
{
  if(output_gtf)
    out <- "GTF"
  else
    out <- "TAB"

  URL <- paste0(url,"/queries/run/",fileName,"/",out)
  h <- c('Accept' = "Application/json",
         'Content-Type' = 'text/plain','X-Auth-Token' = authToken)

  req <- httr::POST(URL,body = query ,httr::add_headers(h),encode = "json")
  content <- httr::content(req,"parsed")
  if(req$status_code !=200)
    print(content$error)
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
#' service name is added automatically
#' @param fileName single string name of the file
#' @param filePath single string path of txt files containing a GMQL query
#' @param output_gtf logical: file format for sample generating from query
#' the possiblities are: GTF or TAB
#'
#' @return None
#'
#' @details
#' If error occures a specific error is printed
#'
#' @examples
#'
#' ## run query: output GTF
#'
#' PolimiUrl = "http://130.186.13.219/gmql-rest"
#' login.GMQL(PolimiUrl)
#' test_path <- system.file("example",package = "GMQL")
#' test_query <- file.path(test_path, "query1.txt")
#' runQuery(PolimiUrl, "query_1", test_query, output_gtf = FALSE)
#'
#' ## run query: output GDM (tabulated)
#'
#' login.GMQL(PolimiUrl)
#' test_path <- system.file("example",package = "GMQL")
#' test_query <- file.path(test_path, "query1.txt")
#' runQuery(PolimiUrl, "query_1", test_query, output_gtf = TRUE)
#'
#' @export
#'
runQuery.fromfile <- function(url,fileName,filePath,output_gtf = TRUE)
{
  if(!file.exists(filePath))
    stop("file does not exist")

  query <- readLines(filePath)
  runQuery(url,fileName,query,output_gtf)
}

#' Compile GMQL query
#'
#' It compiles a GMQL query as single string using the proper GMQL web service available 
#' on a remote server
#'
#' @import httr
#' @param url single string url of server: it must contain the server address and base url;
#' service name is added automatically
#' @param query single string text of the query
#'
#'
#' @return None
#'
#' @examples
#'
#' PolimiUrl = "http://130.186.13.219/gmql-rest"
#' login.GMQL(PolimiUrl)
#' compileQuery(PolimiUrl, "DATA_SET_VAR = SELECT() HG19_TCGA_dnaseq;
#' MATERIALIZE DATA_SET_VAR INTO RESULT_DS;")
#'
#' @export
#'
compileQuery <- function(url ,query)
{
  h <- c('Accept' = "Application/json",
         'Content-Type' = 'text/plain','X-Auth-Token' = authToken)
  URL <- paste0(url,"/queries/compile")
  req <- httr::POST(URL,body = query ,httr::add_headers(h),encode = "json")
  content <- httr::content(req,"parsed")
  if(req$status_code !=200)
    print(content$error)
  else
    return(content)
}

#' Compile GMQL query from file
#'
#' It compiles a GMQL query from file using the proper GMQL web service available 
#' on a remote server
#'
#' @param url single string url of server: it must contain the server address and base url;
#' service name is added automatically
#' @param filePath single string path of txt files containing a GMQL query
#'
#' @return None
#'
#' @details
#' If error occures a specific error is printed
#'
#' @examples
#'
#' PolimiUrl = "http://130.186.13.219/gmql-rest"
#' login.GMQL(PolimiUrl)
#' test_path <- system.file("example",package = "GMQL")
#' test_query <- file.path(test_path, "query1.txt")
#' compileQuery.fromfile(PolimiUrl,test_query)
#'
#' @export
#'
compileQuery.fromfile <- function(url ,filePath)
{
  if(!file.exists(filePath))
   stop("file does not exist")

  query <- readLines(filePath)
  compileQuery(url,query)
}



serializeQuery <- function(output_gtf,base64)
{
  if(output_gtf)
    out <- "GTF"
  else
    out <- "TAB"
  
  URL <- paste0(url,"/queries/dag/",out)
  h <- c('Accept' = "Application/json",
         'Content-Type' = 'text/plain','X-Auth-Token' = authToken)
  
  req <- httr::POST(URL,body = base64 ,httr::add_headers(h),encode = "json")
  content <- httr::content(req,"parsed")
  if(req$status_code !=200)
    print(content$error)
  else
    return(content)
}
