#' GMQL API web Service
#'
#'
#'
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
#'
#'
#'


# TODO: testo preso anche da file?
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
