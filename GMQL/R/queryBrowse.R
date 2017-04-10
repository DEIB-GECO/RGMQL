#' GMQL API web Service
#'
#' Allow access to web service GMQL as guest or registered user
#' with username and password
#'
#'
#'
#'

myQueries <- function()
{
  url <- "http://genomic.elet.polimi.it/gmql-rest/query"
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
#' @param nameQuery name of query
#' @param queryTxt text of query

saveQuery <- function(nameQuery,queryTxt)
{
  url <- paste0("http://genomic.elet.polimi.it/gmql-rest/query/",nameQuery,"/save")
  h <- c('Accept' = 'Application/json', 'X-Auth-Token' = authToken)
  #req <<- POST(url, add_headers(h),verbose(data_in = TRUE,info = TRUE),body = queryTxt)
  req <- POST(url, add_headers(h),body = queryTxt)
  content <- httr::content(req)

  if(req$status_code==200)
    print(content) # print Saved
  else
    stop(content$error)
}
