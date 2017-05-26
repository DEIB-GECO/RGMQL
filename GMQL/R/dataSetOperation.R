#' GMQL API web Service
#'
#' It show all GMQL dataset stored in repository
#'
#'
#' @param url server address
#' @return list of datasets
#' every dataset in the list is identified by:
#' \itemize{
#' \item{name}
#' \item{owner}
#' }
#' @seealso \code{\link{deleteDataset}}
#'
#' @details
#' If error occured stop and print error
#'
#' @examples
#'
#' \dontrun{
#' url <- <http_server_address>
#' login.GMQL(url = <http_server_address>)
#' list <- showDatasets(url)
#' }

showDatasets <- function(url)
{
  URL <- paste0(url,"/datasets")
  h <- c('X-Auth-Token' = authToken)
  #req <- GET(url, add_headers(h),verbose(data_in = TRUE,info = TRUE))
  req <- GET(URL, add_headers(h))
  content <- httr::content(req,"parsed") #JSON
  if(req$status_code !=200)
    stop(content$error)
  else
  {
    return(content)
  }
}

#' GMQL API web Service
#'
#' It show all sample from a specific GMQL dataset
#'
#'
#' @param url server address
#' @param datasetName name of dataset
#' @return list of samples
#' every sample in the list is identified by:
#' \itemize{
#' \item{id}
#' \item{name}
#' \item{path}
#' }
#' @seealso \code{\link{uploadSamples}}
#'
#' @details
#' If error occured stop and print error
#'
#' @examples
#'
#' \dontrun{
#' login.GMQL(url = <http_server_address>)
#' url <- <http_server_address>)
#' list <- showSamplesFromDataset(url,<dataset_name>)
#' }

showSamplesFromDataset <- function(url,datasetName)
{
  URL <- paste0(url,"/datasets",datasetName)
  h <- c('X-Auth-Token' = authToken)
  #req <- GET(url, add_headers(h),verbose(data_in = TRUE,info = TRUE))
  req <- GET(URL, add_headers(h))
  content <- httr::content(req,"parsed")
  if(req$status_code !=200)
    stop(content$error)
  else
  {
    return(content)
  }
}


#' GMQL API web Service
#'
#' It show the region schema of a specific GMQL dataset
#'
#'
#' @param url server address
#' @param datasetName name of dataset
#' @return list of region schema fields
#' every field in the list is identified by:
#' \itemize{
#' \item{name}
#' \item{fieldType: (e.g STRING, DOUBLE)}
#' }
#'
#' @details
#' If error occured stop and print error
#'
#' @examples
#'
#' \dontrun{
#' login.GMQL(url = <http_server_address>)
#' url <- <http_server_address>)
#' list <- showSchemaFromDataset(url,<dataset_name>)
#' }

showSchemaFromDataset <- function(url,datasetName)
{
  URL <- paste0(url,"/datasets/",datasetName,"/schema")
  h <- c('X-Auth-Token' = authToken)
  #req <- GET(url, add_headers(h),verbose(data_in = TRUE,info = TRUE))
  req <- GET(URL, add_headers(h))
  content <- httr::content(req,"parsed")
  if(req$status_code !=200)
    stop(content$error)
  else
    return(content)
}


#' GMQL API web Service
#'
#'
#' It upload sample files to create a new dataset on repository
#'
#'
#' @param url server address
#' @param datasetName name of new dataset
#' @param folderPath path of samples folder you want to upload
#' @param schemaName name of schema used to parse the samples
#'
#' @details
#' If error occured stop and print error
#'
#' @examples
#'
#' \dontrun{
#' url <- <http_server_address>)
#' login.GMQL(url = <http_server_address>)
#' uploadSamples(url,"prova1",folderPath = <folder_path>)
#' }
#'
uploadSamples <- function(url,datasetName,folderPath,schemaName=NULL)
{
  #file_ext_regex <- paste0("*.",fileExt)
#  files <- list.files(folderPath, pattern = file_ext_regex,full.names = T)
  files <- list.files(folderPath,full.names = T)
  if(length(files)==0)
  {
    stop("no files present")
  }

  count = .counter(0)

  list_files <- lapply(files, function(x) {
    upload_file(x)
  })

  list_files_names <- sapply(list_files, function(x) {
    paste0("file",count())
  })

  names(list_files) <- list_files_names
  URL <- paste0(url,"/datasets/",datasetName,"/uploadSample")
  h <- c('X-Auth-Token' = authToken, 'Accept:' = 'Application/json')
  #  req <<- POST(url,body = query ,add_headers(h),encode = "json")

  if(is.null(schemaName))
  {
    schema_name <- list.files(folderPath, pattern = "*.schema$",full.names = T)
    if(length(schema_name)==0)
    {
      stop("schema must be present")
    }

    list_files <- list(list("schema" = upload_file(schema_name)),list_files)
    list_files <- unlist(list_files,recursive = F)
  }
  else
  {
    if(!is("SCHEMA",schemaName))
      stop("schema not admissable")

    schema_name <- schemaName
    URL <- paste0(url,"/datasets/",datasetName,"/uploadSample?schemaName=",schema_name)
  }

  req <- POST(URL, body = list_files ,add_headers(h))
  content <- httr::content(req)
  if(req$status_code !=200)
    stop(content)
  else
    print("upload Complete")
}

#' GMQL API web Service
#'
#' It delete private dataset from repository
#'
#'
#' @param url server address
#' @param datasetName dataset name to delete
#'
#'
#' @details
#' If error occured stop and print error
#'
#' @seealso \code{\link{downloadDataset}}
#'
#' @examples
#'
#' \dontrun{
#' login.GMQL(url = <http_server_address>)
#' url <- <http_server_address>)
#' deleteDataset(url,<dataset_name>)
#' }

deleteDataset <- function(url,datasetName)
{
  URL <- paste0(url,"/datasets/",datasetName)
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
#' It donwload private dataset from repository
#'
#'
#' @param url server address
#' @param datasetName dataset name to delete
#' @param path local path folder where store dataset
#' by defualt is R working directory
#'
#'
#' @details
#' If error occured stop and print error
#'
#'
#' @examples
#'
#' \dontrun{
#' login.GMQL(url = <http_server_address>)
#' url <- <http_server_address>)
#' downloadDataset(url,<dataset_name>,path = <folder_path>)
#' }

downloadDataset <- function(url,datasetName,path = getwd())
{
  URL <- paste0(url,"/datasets/",datasetName,"/zip")
  h <- c('X-Auth-Token' = authToken)
  #req <- GET(url,add_headers(h))
  req <- GET(URL,add_headers(h),verbose(info = TRUE))
  if(req$status_code !=200)
    stop(content$error)

  #print(content$result)
  fileZip <- httr::content(req)
  writeBin(fileZip,path)
  print("Download Complete")
}

#' GMQL API web Service
#'
#' It retrieve metadata for a specific sample in dataset
#'
#' @param url server address
#' @param datasetName dataset name to delete
#' @param sampleName sample name
#' @return list of metadata
#'

metadataFromSample <- function(url, datasetName,sampleName)
{
  URL <- paste0(url,"/datasets/",datasetName,"/",sampleName,"/metadata")
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
#' It retrieve regions for a specific sample in dataset
#'
#' @param url server address
#' @param datasetName dataset name to delete
#' @param sampleName sample name
#' @return Granges containing regions
#'
#'
regionFromSample <- function(url, datasetName,sampleName)
{
  URL <- paste0(url,"/datasets/",datasetName,"/",sampleName,"/region")
  h <- c('X-Auth-Token' = authToken, 'Accpet' = 'text/plain')
  #req <- GET(url, add_headers(h),verbose(data_in = TRUE,info = TRUE))
  req <- GET(URL, add_headers(h))

  if(req$status_code !=200)
    stop(content$error)
  else
  {
    list <- showSchemaFromDataset(url,datasetName)
    schema_type <- list$schemaType

    content <- httr::content(req, 'text',encoding = "UTF-8")
    temp <- tempfile("temp") #use temporary files
    write.table(content,temp,quote = F,sep = '\t',col.names = F,row.names = F)
    if(schema_type=="gtf")
      samples <- import.gff2(temp)
    else
    {
      vector_field <- sapply(list$fields,function(x){
        name <- x$name
      })
      df <- fread(temp,header = FALSE,sep = "\t")
      setnames(df,vector_field)
      samples <- makeGRangesFromDataFrame(df,keep.extra.columns = T,start.field = "left",end.field = "right")
    }
    unlink(temp)
    return(samples)
  }
}
