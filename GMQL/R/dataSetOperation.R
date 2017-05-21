
#' GMQL API web Service
#'
#'
#'
#'
#'

showDatasets <- function(url = "http://genomic.elet.polimi.it/gmql-rest/datasets")
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
#'
#'
#'
#'

showSamplesFromDataset <- function(url = "http://genomic.elet.polimi.it/gmql-rest/datasets/",datasetName)
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

showSchemaFromDataset <- function(datasetName)
{
  url <- paste0("http://genomic.elet.polimi.it/gmql-rest/datasets/",datasetName,"/schema")
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
#'
#'
#'
#'

uploadSamples <- function(url = "http://genomic.elet.polimi.it/gmql-rest/datasets",datasetName,schemaName=NULL,...)
{
  URL <- paste0(url,"/",datasetName,"/uploadSample")
  h <- c('X-Auth-Token' = authToken, 'Accept:' = 'application/json')
  #  req <<- POST(url,body = query ,add_headers(h),encode = "json")
  req <<- POST(url,body =  ,add_headers(h))

  files <- as.character(x=...)

 # apply(files, function(x) )

  content <- httr::content(req,"parsed") #JSON

  if(req$status_code !=200)
    stop(content$error)
  else
  {
    print(content$result)
  }
}

#' GMQL API web Service
#'
#'
#'
#'
#'
#'

deleteDataset <- function(url = "http://genomic.elet.polimi.it/gmql-rest/datasets",datasetName)
{
  URL <- paste0(url,"/",datasetName)
  h <- c('X-Auth-Token' = authToken, 'Accept:' = 'application/json')
  #req <- DELETE(url, add_headers(h),verbose(data_in = TRUE,info = TRUE))
  req <- DELETE(URL, add_headers(h))
  content <- httr::content(req,"parsed") #JSON

  if(req$status_code !=200)
    stop(content$error)
  else
  {
    print(content$result)
  }
}

#' GMQL API web Service
#'
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

#'
#'
#'
#'

metadataFromSample <- function(url="http://genomic.elet.polimi.it/gmql-rest/datasets/", datasetName,sampleName)
{
  URL <- paste0(url,datasetName,"/",sampleName,"/metadata")
  h <- c('X-Auth-Token' = authToke, 'Accpet' = 'text/plain')
  #req <- GET(url, add_headers(h),verbose(data_in = TRUE,info = TRUE))
  req <- GET(URL, add_headers(h))
  content <- httr::content(req, 'text',encoding = "UTF-8")

  #trasform text to list
  metadata <- strsplit(content, "\n")
  metadata <- strsplit(unlist(metadata), "\t")
  names(metadata) <- sapply(metadata, `[[`, 1)
  listMeta <- lapply(metadata, `[`, -1)

  if(req$status_code !=200)
    stop(content$error)
  else
    return(listMeta)
}


#' GMQL API web Service
#'
#'
#'
#'
#'



# start.time <- Sys.time()
# end.time <- Sys.time()
#  time.taken <- end.time - start.time

regionFromSample <- function(url="http://genomic.elet.polimi.it/gmql-rest/datasets/", datasetName,sampleName)
{
  URL <- paste0(url,datasetName,"/",sampleName,"/region")
  h <- c('X-Auth-Token' = authToken, 'Accpet' = 'text/plain')
  #req <- GET(url, add_headers(h),verbose(data_in = TRUE,info = TRUE))
  req <- GET(URL, add_headers(h))
  content <- httr::content(req, 'text',encoding = "UTF-8")

  #trasform text to Granges
  region <- strsplit(content, "\n")
  region <- strsplit(unlist(region), "\t")

  if(req$status_code !=200)
    stop(content$error)
  else
    return(listMeta)
}

