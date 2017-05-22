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
#' showQueries(url)
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
#' @seealso \code{\link{showQueries}}
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
#' showQueries(url)
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
