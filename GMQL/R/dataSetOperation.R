#' Show Dataset
#'
#' It show all GMQL dataset stored in repository
#'
#' @import httr
#' @param url single string url of server: it must contain the server address and base url;
#' service name will be added automatically
#'
#' @return list of datasets.
#' Every dataset in the list is identified by:
#' \itemize{
#' \item{name: name of dataset}
#' \item{owner: pulbic or name of the user}
#' }
#'
#' @seealso \code{\link{deleteDataset}}
#'
#' @details
#' If error occured a specific error will be printed
#'
#' @examples
#'
#' \dontrun{
#'
#' url <- <http_server_address>
#' login.GMQL(url = <http_server_address>)
#' list <- showDatasets(url)
#' }

showDatasets <- function(url)
{
  URL <- paste0(url,"/datasets")
  h <- c('X-Auth-Token' = authToken)
  req <- httr::GET(URL, httr::add_headers(h))
  content <- httr::content(req,"parsed") #JSON
  if(req$status_code !=200)
    stop(content$error)
  else
  {
    return(content)
  }
}

#' Show dataset samples
#'
#' It show all sample from a specific GMQL dataset
#'
#' @import httr
#'
#' @param url single string url of server: it must contain the server address and base url;
#' service name will be added automatically
#' @param datasetName name of dataset we want to get
#' @return list of samples in dataset.
#' Every sample in the list is identified by:
#' \itemize{
#' \item{id: id of sample}
#' \item{name: name of sample}
#' \item{path: sample repository path}
#' }
#' @seealso \code{\link{uploadSamples}}
#'
#' @details
#' If error occured a specific error will be printed
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
  URL <- paste0(url,"/datasets/",datasetName)
  h <- c('X-Auth-Token' = authToken)
  #req <- GET(url, add_headers(h),verbose(data_in = TRUE,info = TRUE))
  req <- httr::GET(URL, httr::add_headers(h))
  content <- httr::content(req,"parsed")
  if(req$status_code !=200)
    stop(content$error)
  else
  {
    return(content)
  }
}


#' Show dataset schema
#'
#' It shows the region schema of a specific GMQL dataset
#'
#' @import httr
#' @param url single string url of server: it must contain the server address and base url;
#' service name will be added automatically
#' @param datasetName name of dataset we want to get
#' @return list of region schema fields.
#' Every field in the list is identified by:
#' \itemize{
#' \item{name: name of field (e.g. chr, start, end, strand ...)}
#' \item{fieldType: (e.g STRING, DOUBLE ...)}
#' }
#'
#' @details
#' If error occured a specific error will be printed
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
  req <- httr::GET(URL, httr::add_headers(h))
  content <- httr::content(req,"parsed")
  if(req$status_code !=200)
    stop(content$error)
  else
    return(content)
}


#' Upload dataset
#'
#'
#' It uploads folder containing dataset files: a new dataset will be created on repository
#'
#'
#' @param url single string url of server: it must contain the server address and base url;
#' service name will be added automatically
#' @param datasetName name of dataset we want to get
#' @param folderPath local path to the folder containing the samples
#' @param schemaName name of schema used to parse the samples
#' schemaName available are:
#' \itemize{
#' \item{NARROWPEAK}
#' \item{BROADPEAK}
#' \item{VCF}
#' \item{BED}
#' \item{BEDGRAPH}
#' }
#' if schema is NULL it looking for a XML schema file to read
#' @param isGMQL logical value indicating whether is GMQL dataset or not
#'
#' @details
#' If error occured a specific error will be printed
#'
#' @examples
#'
#' \dontrun{
#' url <- <http_server_address>)
#' login.GMQL(url = <http_server_address>)
#' uploadSamples(url,"prova1",folderPath = <folder_path>)
#' }
#'
uploadSamples <- function(url,datasetName,folderPath,schemaName=NULL,isGMQL=T)
{
  if(isGMQL)
    folderPath <- paste0(folderPath,"/files")

  files <- list.files(folderPath,full.names = T)
  if(length(files)==0)
    stop("no files present")

  count = .counter(0)

  list_files <- lapply(files, function(x) {
    file <- httr::upload_file(x)
  })

  list_files_names <- sapply(list_files, function(x) {
    paste0("file",count())
  })

  names(list_files) <- list_files_names
  URL <- paste0(url,"/datasets/",datasetName,"/uploadSample")
  h <- c('X-Auth-Token' = authToken, 'Accept:' = 'Application/json')

  if(is.null(schemaName))
  {
    schema_name <- list.files(folderPath, pattern = "*.schema$",full.names = T)
    if(length(schema_name)==0)
      stop("schema must be present")

    list_files <- list(list("schema" = httr::upload_file(schema_name)),list_files)
    list_files <- unlist(list_files,recursive = F)
  }
  else
  {
    schema_name <- tolower(schemaName)
    if(!identical(schema_name,"narrowpeak") && !identical(schema_name,"vcf") && !identical(schema_name,"broadpeak")
       && !identical(schema_name,"bed")  && !identical(schema_name,"bedgraph"))
      stop("schema not admissable")

    URL <- paste0(url,"/datasets/",datasetName,"/uploadSample?schemaName=",schema_name)
  }

  req <- httr::POST(URL, body = list_files ,httr::add_headers(h))
  content <- httr::content(req)
  if(req$status_code !=200)
    stop(content)
  else
    print("upload Complete")
}

#' Delete dataset
#'
#' It deletes single private dataset from repository specified by name
#'
#' @import httr
#'
#' @param url single string url of server: it must contain the server address and base url;
#' service name will be added automatically
#' @param datasetName name of dataset we want to get
#'
#' @details
#' If error occured a specific error will be printed
#'
#' @seealso \code{\link{downloadDataset}}
#'
#' @examples
#'
#' \dontrun{
#'
#' login.GMQL(url = <http_server_address>)
#' url <- <http_server_address>)
#' deleteDataset(url,<dataset_name>)
#' }

deleteDataset <- function(url,datasetName)
{
  URL <- paste0(url,"/datasets/",datasetName)
  h <- c('X-Auth-Token' = authToken, 'Accept:' = 'application/json')
  #req <- DELETE(url, add_headers(h),verbose(data_in = TRUE,info = TRUE))
  req <- httr::DELETE(URL, httr::add_headers(h))
  content <- httr::content(req,"parsed") #JSON

  if(req$status_code !=200)
    stop(content$error)
  else
  {
    print(content$result)
  }
}

#' Download Dataset
#'
#' It donwloads private dataset as zip file from repository to local path specified
#'
#' @import httr
#'
#' @param url single string url of server: it must contain the server address and base url;
#' service name will be added automatically
#' @param datasetName name of dataset we want to get
#' @param path local path folder where store dataset
#' by defualt is R working directory
#'
#'
#' @details
#' If error occured a specific error will be printed
#'
#'
#' @examples
#'
#' \dontrun{
#'
#' login.GMQL(url = <http_server_address>)
#' url <- <http_server_address>)
#' downloadDataset(url,<dataset_name>,path = <folder_path>)
#' }

downloadDataset <- function(url,datasetName,path = getwd())
{
  URL <- paste0(url,"/datasets/",datasetName,"/zip")
  h <- c('X-Auth-Token' = authToken, 'Accept' = 'application/zip')
  req <- httr::GET(URL,httr::add_headers(h))
  #req <- httr::GET(URL,httr::add_headers(h),verbose(info = TRUE))
  if(req$status_code !=200)
    stop(content$error)

  #print(content$result)
  fileZip <- httr::content(req)
  zip_path = paste0(path,"/",datasetName,".zip")
  writeBin(fileZip,zip_path)
  print("Download Complete")
}

#' Shows metadata list from dataset sample
#'
#' It retrieves metadata for a specific sample in dataset
#'
#' @import httr
#'
#' @param url single string url of server: it must contain the server address and base url;
#' service name will be added automatically
#' @param datasetName name of dataset we want to get
#' @param sampleName sample name we want to get
#'
#' @return list of metadata in the form 'key = value'
#'
#' @details
#' If error occured a specific error will be printed
#'
#'
#' \dontrun{
#'
#' login.GMQL(url = <http_server_address>)
#' url <- <http_server_address>)
#' metadataFromSample(url,"<dataset_name>",<dataset_sample_name>)
#' }
#'
metadataFromSample <- function(url, datasetName,sampleName)
{
  URL <- paste0(url,"/datasets/",datasetName,"/",sampleName,"/metadata")
  h <- c('X-Auth-Token' = authToken, 'Accpet' = 'text/plain')
  #req <- GET(url, add_headers(h),verbose(data_in = TRUE,info = TRUE))
  req <- httr::GET(URL, httr::add_headers(h))
  content <- httr::content(req, 'text',encoding = "UTF-8")

  #trasform text to list
  metadata <- strsplit(content, "\n")
  metadata <- strsplit(unlist(metadata), "\t")
  names(metadata) <- sapply(metadata, `[[`, 1)
  listMeta <- lapply(metadata, `[`, -1)

  if(req$status_code !=200)
    stop(content)
  else
    return(listMeta)
}


#' Shows regions from a dataset sample
#'
#'
#' It retrieves regions for a specific sample (whose name is specified in the paramter "sampleName")
#' in a specific dataset (whose name is specified in the paramter "datasetName")
#'
#' @import httr
#' @import rtracklayer
#' @import data.table
#' @import GenomicRanges
#'
#' @param url single string url of server: it must contain the server address and base url;
#' service name will be added automatically
#' @param datasetName name of dataset we want to get
#' @param sampleName sample name we want to get
#'
#' @return Granges data containing regions of sample
#'
#' @details
#' If error occured a specific error will be printed
#'
#' \dontrun{
#'
#' login.GMQL(url = <http_server_address>)
#' url <- <http_server_address>)
#' regionFromSample(url,"<dataset_name>",<dataset_sample_name>)
#' }
#'
regionFromSample <- function(url, datasetName,sampleName)
{
  URL <- paste0(url,"/datasets/",datasetName,"/",sampleName,"/region")
  h <- c('X-Auth-Token' = authToken, 'Accpet' = 'text/plain')
  req <- httr::GET(URL, httr::add_headers(h))
  content <- httr::content(req, 'parsed',encoding = "UTF-8")

  if(req$status_code !=200)
    stop(content)
  else
  {
    list <- showSchemaFromDataset(url,datasetName)
    schema_type <- list$schemaType

    temp <- tempfile("temp") #use temporary files
    write.table(content,temp,quote = F,sep = '\t',col.names = F,row.names = F)
    if(schema_type=="gtf")
      samples <- rtracklayer::import(temp,format = "gtf")
    else
    {
      vector_field <- sapply(list$fields,function(x){
        name <- x$name
      })
      df <- data.table::fread(temp,header = FALSE,sep = "\t")
      data.table::setnames(df,vector_field)
      samples <- GenomicRanges::makeGRangesFromDataFrame(df,keep.extra.columns = T,start.field = "left",end.field = "right")
    }
    unlink(temp)
    return(samples)
  }
}
