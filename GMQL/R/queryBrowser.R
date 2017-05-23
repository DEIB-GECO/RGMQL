#' GMQL API web Service
#'
#' It show all the query saved to repository
#'
#'
#' @param url server address
#' @return list of queries
#'
#' @seealso \code{\link{saveQuery}}
#'
#' @details
#' if error occured print the content error
#'
#' @examples
#'
#' \dontrun{
#' url <- <http_server_address>)
#' login.GMQL(url, username="pippo",password="baudo")
#' list <- showQueries(url)
#' logout.GMQL()
#' }
#'
showQueries <- function(url)
{
  URL <- paste0(url,"/query")
  h <- c('Accept' = 'Application/json', 'X-Auth-Token' = authToken)
  #req <<- GET(URL, add_headers(h),verbose(data_in = TRUE,data_out = TRUE))
  req <- GET(URL, add_headers(h))
  content <- httr::content(req,"parsed") #JSON
  if(req$status_code==200)
  {
    #listSample <- Reduce(c, content)
    return(content)
  }
  else
    stop(content$error)
}

#' GMQL API web Service
#'
#' It save the GMQL queries to repository
#'
#' @param url server address
#' @param queryName name of query
#' @param queryTxt text of GMQL queries
#'
#' @seealso \code{\link{showQueries}} \code{\link{saveQuery.fromfile}}
#'
#' @details
#' if error occured print the content error
#'
#' @examples
#'
#' \dontrun{
#' url <- <http_server_address>)
#' login.GMQL(url, username="pippo",password="baudo")
#' saveQuery(url,"prova1","<query_text>")
#' logout.GMQL()
#' }
#'
saveQuery <- function(url,queryName,queryTxt)
{
  URL <- paste0(url,"/query/",queryName,"/save")
  h <- c('Accept' = 'text/plain', 'X-Auth-Token' = authToken, 'Content-Type' = 'text/plain')
  #req <<- POST(url, add_headers(h),verbose(data_in = TRUE,info = TRUE),body = queryTxt)
  req <- POST(URL, add_headers(h),body = queryTxt)
  content <- httr::content(req)

  if(req$status_code==200)
    print(content) # print Saved
  else
    stop(content$error)
}

#' GMQL API web Service
#'
#' It save the GMQL queries to repository
#'
#' @param url server address
#' @param queryName name of query
#' @param filePath local file path where you write GMQL query
#'
#' @seealso \code{\link{saveQuery}}
#'
#' @details
#' if error occured print the content error
#'
#' @examples
#'
#' \dontrun{
#' url <- <http_server_address>)
#' login.GMQL(url, username="pippo",password="baudo")
#' saveQuery.fromfile(url,"prova1","<query_file_path>")
#' logout.GMQL()
#' }
#'
saveQuery.fromfile <- function(url,queryName,filePath)
{
  if(!file.exists(filePath))
    stop("file does not exist")

  queryTxt <- read.table(filePath)
  saveQuery(url,queryName,queryTxt)
}
