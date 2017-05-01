
#' GMQL API web Service
#'
#' Allow access to web service GMQL as guest or registered user
#' with username and password
#'
#'
#'
#'

allDatasets <- function(url = "http://genomic.elet.polimi.it/gmql-rest/datasets")
{
  h <- c('X-Auth-Token' = authToken)
  #req <- GET(url, add_headers(h),verbose(data_in = TRUE,info = TRUE))
  req <- GET(url, add_headers(h))
  content <- httr::content(req,"parsed") #JSON
  if(req$status_code !=200)
    stop(content$error)
  else
  {
   # dataSetList <- Reduce(c, content)
    return(content)

  }
}

#' GMQL API web Service
#'
#' Allow access to web service GMQL as guest or registered user
#' with username and password
#'
#'
#'
#'

samplesFromDataset <- function(url = "http://genomic.elet.polimi.it/gmql-rest/datasets/",datasetName)
{
  URL <- paste0(url,datasetName)
  h <- c('X-Auth-Token' = authToken)
  #req <- GET(url, add_headers(h),verbose(data_in = TRUE,info = TRUE))
  req <- GET(URL, add_headers(h))
  content <- httr::content(req,"parsed")
  if(req$status_code !=200)
    stop(content$error)
  else
  {
    #listSample <- Reduce(c, content)
    return(content)
  }
}


#' GMQL API web Service
#'
#' Allow access to web service GMQL as guest or registered user
#' with username and password
#'
#'
#'
#'

schemaFromDataset <- function(datasetName)
{
  url <- paste0("http://genomic.elet.polimi.it/gmql-rest/datasets/","public.",datasetName,"/schema")
  h <- c('X-Auth-Token' = authToken)
  #req <- GET(url, add_headers(h),verbose(data_in = TRUE,info = TRUE))
  req <- GET(url, add_headers(h))
  content <- httr::content(req,"parsed")
  if(req$status_code !=200)
    stop(content$error)
  else
    return(content)
}

#' GMQL API web Service
#'
#' Allow access to web service GMQL as guest or registered user
#' with username and password
#'
#'
#'
#'

uploadSamples <- function()
{}


#' GMQL API web Service
#'
#' Allow access to web service GMQL as guest or registered user
#' with username and password
#'
#'
#'
#'

downloadDataset <- function(datasetName,path = getwd())
{
  url <- paste0("gmql-rest/dataSets/",datasetName,"/zip")
  h <- c('X-Auth-Token' = authToken)
  #req <- GET(url,add_headers(h))
  req <- GET(url,add_headers(h),verbose(info = TRUE))
  print(req)
  fileZip <- httr::content(req)
  writeBin(fileZip,path)
  print("Download Complete")
}

#' GMQL API web Service
#'
#' Allow access to web service GMQL as guest or registered user
#' with username and password
#'
#'
#'
#'

metadataFromSample <- function()
{

}


#' GMQL API web Service
#'
#' Allow access to web service GMQL as guest or registered user
#' with username and password
#'
#'
#'
#'

regionFromSample <- function()
{

}

