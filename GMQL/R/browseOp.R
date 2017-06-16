#' Shows all Queries
#'
#' It shows all the GMQL query saved on repository
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
#' PolimiUrl = "http://genomic.elet.polimi.it/gmql-rest"
#' login.GMQL(PolimiUrl)
#' list <- showQueries(PolimiUrl)
#'
#' @export
#'
showQueries <- function(url)
{
  URL <- paste0(url,"/query")
  h <- c('Accept' = 'Application/json', 'X-Auth-Token' = authToken)
  #req <- GET(URL, add_headers(h),verbose(data_in = TRUE,data_out = TRUE))
  req <- httr::GET(URL, httr::add_headers(h))
  content <- httr::content(req,"parsed") #JSON
  if(req$status_code==200)
    return(content)
  else
    print(content$error)
}

#' Save GMQL query
#'
#' It saves the GMQL query into repository
#'
#' @import httr
#'
#' @param url single string url of server: it must contain the server address and base url;
#' service name will be added automatically
#' @param queryName single string name of query
#' @param queryTxt single string text of GMQL query
#'
#' @return no object return
#'
#' @seealso \code{\link{showQueries}} \code{\link{saveQuery.fromfile}}
#'
#' @references
#' Please, read for writing a GMQL query
#' \url{http://www.bioinformatics.deib.polimi.it/genomic_computing/GMQL/doc/GMQLUserTutorial.pdf}
#'
#' @details
#' #' if you save a query with the same name of an other query already stored in repository
#' you will overwrite it
#' if error occured print the content error
#' if no error occured print "Saved" otherwise print the content error
#'
#' @examples
#'
#'
#' PolimiUrl = "http://genomic.elet.polimi.it/gmql-rest"
#' login.GMQL(PolimiUrl)
#' saveQuery(PolimiUrl,"try_1", queryTxt = "DATA_SET_VAR = SELECT() HG19_TCGA_dnaseq;
#' MATERIALIZE DATA_SET_VAR INTO RESULT_DS;")
#'
#' @export
#'
saveQuery <- function(url,queryName,queryTxt)
{
  URL <- paste0(url,"/query/",queryName,"/save")
  h <- c('Accept' = 'text/plain', 'X-Auth-Token' = authToken, 'Content-Type' = 'text/plain')
  #req <- POST(url, add_headers(h),verbose(data_in = TRUE,info = TRUE),body = queryTxt)
  req <- httr::POST(URL, httr::add_headers(h),body = queryTxt)
  content <- httr::content(req)

  if(req$status_code==200)
    print(content) # print Saved
  else
    print(content$error)
}

#' Save GMQL query from file
#'
#' It saves the GMQL query into repository taken from file
#'
#'
#' @param url single string url of server: it must contain the server address and base url;
#' service name will be added automatically
#' @param queryName single string name of the GMQL query
#' @param filePath single string local file path where you write GMQL query
#'
#' @return no object return
#'
#' @references
#' Please, read for writing a GMQL query
#' \url{http://www.bioinformatics.deib.polimi.it/genomic_computing/GMQL/doc/GMQLUserTutorial.pdf}
#'
#'
#' @seealso \code{\link{saveQuery}}
#'
#' @details
#' if you save a query with the same name of an other query already stored in repository
#' you will overwrite it
#' if no error occured print "Saved" otherwise print the content error#'
#'
#' @examples
#'
#' test_path <- system.file("example",package = "GMQL")
#' test_query <- file.path(test_path, "query1.txt")
#'
#' PolimiUrl = "http://genomic.elet.polimi.it/gmql-rest"
#' login.GMQL(PolimiUrl)
#' saveQuery.fromfile(PolimiUrl,"query1", test_query)
#'
#' @export
#'
saveQuery.fromfile <- function(url,queryName,filePath)
{
  if(!file.exists(filePath))
    stop("file does not exist")

  queryTxt <- readLines(filePath)
  saveQuery(url,queryName,queryTxt)
}
