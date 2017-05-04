#' GMQL API web Service
#'
#' Allow access to web service GMQL as guest or registered user
#' with username and password
#'
#'
#' @param url server address
#'

showQueries <- function(url = "http://genomic.elet.polimi.it/gmql-rest/query")
{
  h <- c('Accept' = 'Application/json', 'X-Auth-Token' = authToken)
  #req <<- GET(url, add_headers(h),verbose(data_in = TRUE,info = TRUE))
  req <- GET(url, add_headers(h))
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
#' Allow access to web service GMQL as guest or registered user
#' with username and password
#'
#'
#'
#' @param queryName query name
#' @param url server address
#' @param queryTxt query text


# TODO: testo preso anche da file?
saveQuery <- function(url = "http://genomic.elet.polimi.it/gmql-rest/query/",queryName,queryTxt)
{
  URL <- paste0(url,queryName,"/save")
  h <- c('Accept' = 'Application/json', 'X-Auth-Token' = authToken)
  #req <<- POST(url, add_headers(h),verbose(data_in = TRUE,info = TRUE),body = queryTxt)
  req <- POST(URL, add_headers(h),body = queryTxt)
  content <- httr::content(req)

  if(req$status_code==200)
    print(content) # print Saved
  else
    stop(content$error)
}
