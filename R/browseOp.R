#' Shows all Queries
#'
#' It shows all the GMQL query saved on repository 
#' using the proper GMQL web service available on a remote server
#' 
#' @import httr
#'
#' @param url string url of server: It must contain the server address 
#' and base url; service name is added automatically
#'
#' @return list of queries
#' Every query in the list is identified by:
#' \itemize{
#' \item{name: name of query}
#' \item{text: text of GMQL query}
#' }
#' @seealso \code{\link{save_query}}
#'
#' @details
#' if error occures, a specific error is printed
#'
#' @examples 
#' remote_url = "http://130.186.13.219/gmql-rest"
#' 
#' \dontrun{
#' login_gmql(remote_url)
#' list <- show_queries_list(remote_url)
#' }
#' @export
#'
show_queries_list <- function(url)
{
    URL <- paste0(url,"/query")
    h <- c('Accept' = 'Application/json', 'X-Auth-Token' = authToken)
    req <- httr::GET(URL, httr::add_headers(h))
    content <- httr::content(req,"parsed")
    if(req$status_code==200)
        return(content)
    else
        stop(content$error)
}

#' Save GMQL query
#'
#' It saves the GMQL query into repository
#' using the proper GMQL web service available on a remote server
#' 
#' @import httr
#'
#' @param url string url of server: It must contain the server address 
#' and base url; service name is added automatically
#' @param queryName string name of query
#' @param queryTxt string text of GMQL query
#'
#' @return None
#'
#' @seealso \code{\link{show_queries_list}} \code{\link{save_query_fromfile}}
#'
#'
#' @details
#' if you save a query with the same name of an other query already stored 
#' in repository you will overwrite it,
#' if no error occures print "Saved" otherwise print the content error
#'
#' @examples
#' remote_url = "http://130.186.13.219/gmql-rest"
#' \dontrun{
#' 
#' login_gmql(remote_url)
#' save_query(remote_url, "dna_query", "DATASET = SELECT() HG19_TCGA_dnaseq; 
#' MATERIALIZE DATASET INTO RESULT_DS;")
#' }
#' @export
#'
save_query <- function(url, queryName, queryTxt)
{
    URL <- paste0(url,"/query/",queryName,"/save")
    h <- c('Accept' = 'text/plain', 'X-Auth-Token' = authToken,
            'Content-Type' = 'text/plain')
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
#' using the proper GMQL web service available on a remote server
#'
#' 
#' 
#' @param url string url of server: It must contain the server address 
#' and base url; service name is added automatically
#' @param queryName string name of the GMQL query
#' @param filePath string local file path of txt file containing a GMQL query
#'
#' @return None
#'
#' @seealso \code{\link{save_query}}
#'
#' @details
#' if you save a query with the same name of an other query already stored
#' in repository you will overwrite it,
#' if no error occures print "Saved" otherwise print the content error
#'
#' @examples
#' test_path <- system.file("example", package = "RGMQL")
#' test_query <- file.path(test_path, "query1.txt")
#' remote_url = "http://130.186.13.219/gmql-rest"
#' \dontrun{
#' 
#' login_gmql(remote_url)
#' save_query_fromfile(remote_url, "query1", test_query)
#' }
#' @export
#'
save_query_fromfile <- function(url, queryName, filePath)
{
    if(!file.exists(filePath))
        stop("file does not exist")

    queryTxt <- readLines(filePath)
    save_query(url,queryName,queryTxt)
}
