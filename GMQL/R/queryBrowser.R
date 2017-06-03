#' Shows all Queries
#'
#' It shows all the GMQL query saved to repository
#'
#' @import httr
#'
#' @param url single string url of server: it must contain the server address and base url;
#' service name will be added automatically
#'
#' @return list of queries
#' Every query in the list is identified by:
#' \itemize{
#' \item{name: name of query}
#' \item{text: text of GMQL query}
#' }
#' @seealso \code{\link{saveQuery}}
#'
#' @details
#' if error occured print the content error
#'
#' @examples
#'
#' \dontrun{
#'
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
  req <- httr::GET(URL, httr::add_headers(h))
  content <- httr::content(req,"parsed") #JSON
  if(req$status_code==200)
  {
    #listSample <- Reduce(c, content)
    return(content)
  }
  else
    stop(content$error)
}

#' Save GMQL query
#'
#' It saves the GMQL query into repository
#'
#' @import httr
#' @param url single string url of server: it must contain the server address and base url;
#' service name will be added automatically
#' @param queryName single string name of query
#' @param queryTxt single string text of GMQL query
#'
#' @seealso \code{\link{showQueries}} \code{\link{saveQuery.fromfile}}
#'
#' @details
#' if error occured print the content error
#'
#' @examples
#'
#' \dontrun{
#'
#' url <- <http_server_address>
#' login.GMQL(url)
#' saveQuery(url,"prova1","<query_text>")
#' logout.GMQL()
#' }
#'
saveQuery <- function(url,queryName,queryTxt)
{
  URL <- paste0(url,"/query/",queryName,"/save")
  h <- c('Accept' = 'text/plain', 'X-Auth-Token' = authToken, 'Content-Type' = 'text/plain')
  #req <<- POST(url, add_headers(h),verbose(data_in = TRUE,info = TRUE),body = queryTxt)
  req <- httr::POST(URL, httr::add_headers(h),body = queryTxt)
  content <- httr::content(req)

  if(req$status_code==200)
    print(content) # print Saved
  else
    stop(content$error)
}

#' Save GMQL query from file
#'
#' It saves the GMQL query into repository taken from file
#'
#'
#' @param url single string url of server: it must contain the server address and base url;
#' service name will be added automatically
#' @param queryName name of GMQL query
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
#'
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

  queryTxt <- readLines(filePath)
  saveQuery(url,queryName,queryTxt)
}
