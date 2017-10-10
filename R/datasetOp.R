#' Show Dataset
#'
#' It show all GMQL dataset stored in repository using the proper GMQL web service 
#' available on a remote server
#'
#' @import httr
#' @param url single string url of server: it must contain the server address and base url;
#' service name is added automatically
#'
#' @return list of datasets.
#'
#' Every dataset in the list is identified by:
#' \itemize{
#' \item{name: name of dataset}
#' \item{owner: public or name of the user}
#' }
#'
#' @seealso \code{\link{deleteDataset}}
#'
#' @details
#' If error occures a specific error is printed
#'
#' @examples
#'
#' #### show dataset when logged as guest
#' 
#' PolimiUrl = "http://130.186.13.219/gmql-rest"
#' login.GMQL(url = PolimiUrl)
#' list <- showDatasets(PolimiUrl)
#'
#' @export
#'
showDatasets <- function(url)
{
  URL <- paste0(url,"/datasets")

  h <- c('X-Auth-Token' = authToken)
  req <- httr::GET(URL, httr::add_headers(h))
  content <- httr::content(req,"parsed") #JSON
  if(req$status_code !=200)
    print(content$error)
  else
  {
    return(content)
  }
}

#' Show dataset samples
#'
#' It show all sample from a specific GMQL dataset using the proper 
#' GMQL web service available on a remote server
#'
#' @import httr
#'
#' @param url single string url of server: it must contain the server address and base url;
#' service name is added automatically
#' @param datasetName name of dataset to get
#' if the dataset is a public dataset, we have to add "public." as prefix, as shown in the example below
#' otherwise no prefix is needed
#'
#' @return list of samples in dataset.
#'
#' Every sample in the list is identified by:
#' \itemize{
#' \item{id: id of sample}
#' \item{name: name of sample}
#' \item{path: sample repository path}
#' }
#' @seealso \code{\link{uploadSamples}}
#'
#' @details
#' If error occures a specific error is printed
#'
#' @examples
#'
#' PolimiUrl = "http://130.186.13.219/gmql-rest"
#' login.GMQL(url = PolimiUrl)
#' list <- showSamplesFromDataset(PolimiUrl,"public.GRCh38_ENCODE_BROAD_MAY_2017")
#'
#' @export
#'
showSamplesFromDataset <- function(url,datasetName)
{
  URL <- paste0(url,"/datasets/",datasetName)
  h <- c('X-Auth-Token' = authToken)
  #req <- GET(url, add_headers(h),verbose(data_in = TRUE,info = TRUE))
  req <- httr::GET(URL, httr::add_headers(h))
  content <- httr::content(req,"parsed")
  if(req$status_code !=200)
    print(content$error)
  else
    return(content)

}


#' Show dataset schema
#'
#' It shows the region attribute schema of a specific GMQL dataset using the proper GMQL 
#' web service available on a remote server
#'
#' @import httr
#' @param url single string url of server: it must contain the server address and base url;
#' service name is added automatically
#' @param datasetName name of dataset to get
#' if the dataset is a public dataset, we have to add "public." as prefix, as shown in the example below
#' otherwise no prefix is needed
#'
#' @return list of region schema fields.
#'
#' Every field in the list is identified by:
#' \itemize{
#' \item{name: name of field (e.g. chr, start, end, strand ...)}
#' \item{fieldType: (e.g STRING, DOUBLE ...)}
#' }
#'
#' @details
#' If error occures a specific error is printed
#'
#'
#' @examples
#'
#' ### show schema of public dataset
#' PolimiUrl = "http://130.186.13.219/gmql-rest"
#' login.GMQL(url = PolimiUrl)
#' list <- showSchemaFromDataset(PolimiUrl,"public.GRCh38_ENCODE_BROAD_MAY_2017")
#'
#' @export
#'
showSchemaFromDataset <- function(url,datasetName)
{
  URL <- paste0(url,"/datasets/",datasetName,"/schema")
  h <- c('X-Auth-Token' = authToken)
  #req <- GET(url, add_headers(h),verbose(data_in = TRUE,info = TRUE))
  req <- httr::GET(URL, httr::add_headers(h))
  content <- httr::content(req,"parsed")
  if(req$status_code != 200)
    stop(content$error)
  else
    return(content)
}


#' Upload dataset
#'
#'
#' It uploads a folder (GMQL or not) containing sample files using the proper GMQL 
#' web service available on a remote server: a new dataset is created on repository
#'
#'
#' @param url single string url of server: it must contain the server address and base url;
#' service name is added automatically
#' @param datasetName name of dataset to get
#' @param folderPath single string local path to the folder containing the samples
#' @param schemaName single string name of schema used to parse the samples
#' schemaName available are:
#' \itemize{
#' \item{NARROWPEAK}
#' \item{BROADPEAK}
#' \item{VCF}
#' \item{BED}
#' \item{BEDGRAPH}
#' }
#' if schema is NULL it's looking for a XML schema file to read
#' @param isGMQL single logical value indicating whether is GMQL dataset or not
#'
#' @return None
#'
#' @details
#' If no error occures print "Upload Complete", otherwise a specific error is printed
#'
#' @examples
#'
#' \dontrun{
#' 
#' ### upload of GMQL dataset with no schema selection
#' test_path <- system.file("example","DATA_SET_VAR_GDM",package = "GMQL")
#' PolimiUrl = "http://130.186.13.219/gmql-rest"
#' login.GMQL(url = PolimiUrl)
#' uploadSamples(PolimiUrl,"dataset1",folderPath = test_path)
#' }
#' 
#' @export
#'
uploadSamples <- function(url,datasetName,folderPath,schemaName=NULL,isGMQL=TRUE)
{
  if(isGMQL)
    folderPath <- paste0(folderPath,"/files")

  files <- list.files(folderPath,full.names = TRUE)
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

  schema_name <- tolower(schemaName)
  
  if(is.null(schemaName))
  {
    schema_name <- list.files(folderPath, pattern = "*.schema$",full.names = TRUE)
    if(length(schema_name)==0)
      stop("schema must be present")

    list_files <- list(list("schema" = httr::upload_file(schema_name)),list_files)
    list_files <- unlist(list_files,recursive = FALSE)
    
    URL <- paste0(url,"/datasets/",datasetName,"/uploadSample")
  }
  else
  {
    schema_name <- tolower(schemaName)
    if(identical(schema_name,"customparser"))
    {
      schema_name <- list.files(folderPath, pattern = "*.schema$",full.names = TRUE)
      if(length(schema_name)==0)
        stop("schema must be present")
      
      list_files <- list(list("schema" = httr::upload_file(schema_name)),list_files)
      list_files <- unlist(list_files,recursive = FALSE)
      
      URL <- paste0(url,"/datasets/",datasetName,"/uploadSample")
      
    }
    else
    {
      if(!identical(schema_name,"narrowpeak") && !identical(schema_name,"vcf") && !identical(schema_name,"broadpeak") 
         && !identical(schema_name,"bed")  && !identical(schema_name,"bedgraph"))
        stop("schema not admissable")

      URL <- paste0(url,"/datasets/",datasetName,"/uploadSample?schemaName=",schema_name)
    }
  }

  req <- httr::POST(URL, body = list_files ,httr::add_headers(h))
  content <- httr::content(req)
  if(req$status_code !=200)
    print(content)
  else
    print("upload Complete")
}

#' Delete dataset
#'
#' It deletes single private dataset specified by name from repository using the proper GMQL 
#' web service available on a remote server
#'
#' @import httr
#'
#' @param url single string url of server: it must contain the server address and base url;
#' service name is added automatically
#' @param datasetName single string name of dataset to delete
#'
#' @return None
#'
#'
#' @details
#' If no error occures print "Deleted Dataset", otherwise a specific error is printed
#'
#' @seealso \code{\link{downloadDataset}}
#'
#' @examples
#'
#' \dontrun{
#' 
#' ### this dataset does not exist
#' 
#' PolimiUrl = "http://130.186.13.219/gmql-rest"
#' login.GMQL(url = PolimiUrl)
#' deleteDataset(PolimiUrl,"job_test1_test101_20170604_180908_RESULT_DS")
#' 
#' }
#' 
#' @export
#'
deleteDataset <- function(url,datasetName)
{
  URL <- paste0(url,"/datasets/",datasetName)
  h <- c('X-Auth-Token' = authToken, 'Accept:' = 'application/json')
  #req <- DELETE(url, add_headers(h),verbose(data_in = TRUE,info = TRUE))
  req <- httr::DELETE(URL, httr::add_headers(h))
  content <- httr::content(req,"parsed") #JSON

  if(req$status_code !=200)
    print(content$error)
  else
    print(content$result)
}

#' Download Dataset
#'
#' It donwloads private dataset as zip file from repository to local path specified 
#' using the proper GMQL web service available on a remote server
#'
#' @import httr
#' @importFrom utils unzip
#'
#' @param url single string url of server: it must contain the server address and base url;
#' service name will be added automatically
#' @param datasetName single string name of dataset we want to get
#' @param path single string local path folder where store dataset,
#' by defualt is R working directory
#' @return None
#'
#' @details
#' If error occures a specific error is printed
#'
#'
#' @examples
#'
#' #### download dataset in r working directory
#' #### in this case we try to download public dataset
#' 
#' PolimiUrl = "http://130.186.13.219/gmql-rest"
#' login.GMQL(url = PolimiUrl)
#' downloadDataset(PolimiUrl,"public.HG19_BED_ANNOTATION",path = getwd())
#'
#' @export
#'
downloadDataset <- function(url,datasetName,path = getwd())
{
  URL <- paste0(url,"/datasets/",datasetName,"/zip")
  h <- c('X-Auth-Token' = authToken, 'Accept' = 'application/zip')
  req <- httr::GET(URL,httr::add_headers(h))
  #req <- httr::GET(URL,httr::add_headers(h),verbose(info = TRUE))

  #print(content$result)
  content <- httr::content(req)
  if(req$status_code !=200)
    print(content)
  else
  {
    zip_path <- paste0(path,"/",datasetName,".zip")
    dir_out <-paste0(path,"/")
    writeBin(content,zip_path)
    unzip(zip_path,exdir=dir_out)
    print("Download Complete")
  }
}

#' Download Dataset in GrangesList
#'
#' It donwloads private dataset from repository saving into R environemnt as GrangesList 
#'
#' @import httr
#' @importClassesFrom GenomicRanges GRangesList
#' @importFrom S4Vectors metadata
#' 
#' @param url single string url of server: it must contain the server address and base url;
#' service name is added automatically
#' @param datasetName single string name of dataset we want to get
#'
#' @return GrangesList containing all GMQL sample in dataset
#'
#' @details
#' If error occures a specific error is printed
#'
#'
#' @examples
#'
#' \dontrun{
#' #### create grangeslist from public dataset HG19_BED_ANNOTATION got from repository
#' PolimiUrl = "http://130.186.13.219/gmql-rest"
#' login.GMQL(url = PolimiUrl)
#' downloadDatasetToGrangesList(PolimiUrl,"public.HG19_BED_ANNOTATION")
#' }
#' 
#' @export
#'
downloadDatasetToGrangesList <- function(url,datasetName)
{
  list <- showSamplesFromDataset(url,datasetName)
  samples <- list$samples
  sample_list_name <- sapply(samples, function(x){
    name <- x$name
  })

  sampleList <- lapply(samples, function(x){
    name <- x$name
    range <- regionFromSample(url,datasetName,name)
  })

  names(sampleList) <- sample_list_name
  gRange_list <- GenomicRanges::GRangesList(sampleList)

  meta_list <- lapply(samples, function(x){
    name <- x$name
    meta <- metadataFromSample(url,datasetName,name)
  })
  names(meta_list) <- sample_list_name
  S4Vectors::metadata(gRange_list) <- meta_list

  return(gRange_list)
}




#' Shows metadata list from dataset sample
#'
#' It retrieves metadata for a specific sample in dataset using the proper GMQL web service 
#' available on a remote server
#'
#' @import httr
#'
#' @param url single string url of server: it must contain the server address and base url;
#' service name is added automatically
#' @param datasetName single string name of dataset to get
#' @param sampleName single string sample name to get
#'
#' @return list of metadata in the form 'key = value'
#'
#' @details
#' If error occures a specific error is printed
#'
#' @examples
#'
#' ## download metadata with real test login
#' PolimiUrl = "http://130.186.13.219/gmql-rest"
#' login.GMQL(url = PolimiUrl)
#' metadataFromSample(PolimiUrl,"public.HG19_BED_ANNOTATION","genes")
#'
#' @export
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
#' using the proper GMQL web service available on a remote server
#'
#' @import httr
#' @importFrom rtracklayer import
#' @importFrom data.table fread
#' @importFrom GenomicRanges makeGRangesFromDataFrame
#' @importFrom utils write.table
#'
#' @param url single string url of server: it must contain the server address and base url;
#' service name is added automatically
#' @param datasetName single string name of dataset to get
#' @param sampleName single string sample name to get
#'
#' @return Granges data containing regions of sample
#'
#' @details
#' If error occures a specific error is printed
#'
#' @examples
#'
#' 
#' PolimiUrl = "http://130.186.13.219/gmql-rest"
#' login.GMQL(url = PolimiUrl)
#' regionFromSample(PolimiUrl,"public.HG19_BED_ANNOTATION","genes")
#' 
#' 
#' @export
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
    schema_type <- list$type

    temp <- tempfile("temp") #use temporary files
    write.table(content,temp,quote = FALSE,sep = '\t',col.names = FALSE,row.names = FALSE)
    if(schema_type=="gtf")
      samples <- rtracklayer::import(temp,format = "gtf")
    else
    {
      vector_field <- sapply(list$fields,function(x){
        name <- x$name
      })
      df <- data.table::fread(temp,header = FALSE,sep = "\t")
      a <- df[1,2]
      if(is.na(as.numeric(a)))
        df <- df[-1]
      data.table::setnames(df,vector_field)
      samples <- GenomicRanges::makeGRangesFromDataFrame(df,keep.extra.columns = TRUE,
                                                         start.field = "left",
                                                         end.field = "right",
                                                         strand.field="strand")
      }
    unlink(temp)
    return(samples)
  }
}
