#' Init GMQL Server
#'
#' Initialize and run GMQL server for executing GMQL query
#' It is also perform a login to GMQL REST services suite if needed
#' 
#' @importFrom rJava J
#' 
#' @param output_format single string identifies the output format of sample files.
#' Can be TAB, GTF or COLLECT
#' \itemize{
#' \item{TAB: tab delimited file format}
#' \item{GTF: file format used to hold information about gene structure.
#' It is a tab-delimited text format based on the general feature format}
#' \item{COLLECT: used for storing output in memory}
#' }
#' @param remote_processing logical value specifying the processing mode.
#' True for processing on cluster (remote), false for local processing (local machine)
#' @param url single string url of server: it must contain the server address and base url;
#' service name is added automatically.
#' If null no login is performed, you can always perform it calling the function 
#' \code{\link{login.GMQL}} explicitly
#' @param username single string name used during signup
#' @param password single string password used during signup
#' @return None
#'
#' @examples
#'
#' # initialize GMQL with local processing
#' library(rJava)
#' initGMQL("tab",FALSE)
#' 
#' @export
#'
initGMQL <- function(output_format = "gtf", remote_processing = FALSE, url = NULL, 
                     username = NULL, password = NULL)
{
  out_format <- toupper(output_format)

  if(!identical(out_format,"TAB") && !identical(out_format,"GTF") 
     && !identical(out_format,"COLLECT"))
    stop("output_format must be TAB, GTF or COLLECT")

  .check_logical(remote_processing)
  
  if(!is.null(url) && !exists("authToken",envir = .GlobalEnv))
    login.GMQL(url,username,password)
  
  WrappeR <- J("it/polimi/genomics/r/Wrapper")
  WrappeR$initGMQL(out_format,remote_processing)
}

#' GMQL Function: READ
#'
#' Read a GMQL dataset or any other folder containig some homogenus sample
#' from disk, saving in Scala memory that can be referenced in R
#' Also used to read a repository dataset in case of remote processing.
#' 
#' @importFrom rJava .jnull
#' @importFrom methods is
#' @importFrom rJava J
#' 
#' @param dataset single string folder path for GMQL dataset or datasetname on repository
#' @param parser single string used to parsing dataset files
#' The Parser's available are:
#' \itemize{
#' \item{BedParser}
#' \item{ANNParser}
#' \item{BroadProjParser}
#' \item{BedParser}
#' \item{NarrowPeakParser}
#' \item{RnaSeqParser}
#' \item{CustomParser.}
#' }
#' Default is CustomParser.
#' @param is_local single logical value indicating local or remote dataset
#' @param is_GMQL single logical value indicating if dataset is GMQL dataset or not 
#' 
#' 
#' @return DAGgraph class object. It contains the value associated to the graph used 
#' as input for the subsequent GMQL function
#'
#' @details
#' Normally a GMQL dataset contains an XML schema file that contains
#' name of column header. (e.g chr, start, stop, strand)
#' The CustomParser read this XML schema; if you already know what kind of schema your files are use one of
#' the parser defined without reading any XML schema
#'
#' @examples
#' 
#' ### local with CustomParser
#' initGMQL("gtf")
#' test_path <- system.file("example","DATA_SET_VAR_GTF",package = "RGMQL")
#' r = readDataset(test_path)
#' 
#' \dontrun{
#' 
#' ### local with other Parser
#' initGMQL("gtf")
#' test_path <- system.file("example","DATA_SET_VAR_GTF",package = "RGMQL")
#' r = readDataset(test_path,"ANNParser")
#' }
#' 
#' @export
#'
readDataset <- function(dataset, parser = "CustomParser", is_local=TRUE, 
                        is_GMQL=TRUE)
{
  .check_input(dataset)
  .check_logical(is_local)
  .check_logical(is_GMQL)
  if(is_local)
  {
    if(!dir.exists(dataset))
      stop("folder does not exist")
    
    dataset <- sub("/*[/]$","",dataset)
    
    schema_matrix <- .jnull("java/lang/String")
    url <- .jnull("java/lang/String")
  }
  else
  {
    url <- WrappeR$get_url()
    if(is.null(url))
      stop("You have to log on using login function")
    
    if(!exists("authToken",envir = .GlobalEnv))
      stop("You have to log on using login function")
   
    list <- showSchemaFromDataset(url,dataset)
    schema_names <- sapply(list$fields, function(x){x$name})
    schema_type <- sapply(list$fields, function(x){x$type})
    schema_matrix <- cbind(schema_type,schema_names)
    #schema_type <- list$type
    
    if(is.null(schema_matrix) || length(schema_matrix)==0)
      schema_matrix <- .jnull("java/lang/String")
  }

  parser_name <- .check_parser(parser)
  WrappeR <- J("it/polimi/genomics/r/Wrapper")
  response <- WrappeR$readDataset(dataset,parser_name,is_local,is_GMQL,schema_matrix)
  error <- strtoi(response[1])
  data <- response[2]
  if(error!=0)
    stop(data)
  else
    DAGgraph(data)
}

#' GMQL Function: READ
#'
#' Read a GrangesList saving in scala memory that can be referenced in R
#'
#'
#' @importFrom S4Vectors metadata
#' @importFrom rJava J
#' @importFrom rJava .jarray
#' 
#' @param samples GrangesList
#' 
#'
#' @return DAGgraph class object. It contains the value associated to the graph used 
#' as input for the subsequent GMQL function
#' 
#' @examples
#' "prova prova"
#'
#' @export
#'
read <- function(samples)
{
  if(!is(samples,"GRangesList"))
    stop("only GrangesList")

  meta <- S4Vectors::metadata(samples)
  if(is.null(meta) || length(meta)==0) {
    #repeat meta for each sample in samples list
    len <- length(samples)
    warning("GrangesList has no metadata. we provide two metadata for you")
    index_meta <- rep(1:len,each = len)
    rep_meta <- rep(c("Provider","Polimi", "Application", "R-GMQL"),times=len)
    meta_matrix <- matrix(rep_meta,ncol = 2,byrow = TRUE)
    meta_matrix <- cbind(index_meta,meta_matrix)
  }
  else {
    unlist_meta <- unlist(meta)
    names_meta <- names(unlist_meta)
    group_names <- gsub(".*_([0-9]*)\\..*","\\1", names_meta)
    names(unlist_meta) <- NULL
    meta_matrix <- cbind(group_names,names_meta,unlist_meta)
  }
  df <- data.frame(samples)
  df <- df[-2] #delete group_name
  region_matrix <- as.matrix(sapply(df, as.character))
  region_matrix[is.na(region_matrix)] <- "NA"
  region_matrix <- region_matrix[,setdiff(colnames(region_matrix),"width")]
  col_types <- sapply(df,class)
  col_names <- names(col_types)
  #re order the schema?
  if("phase" %in% col_names) # if GTF, change
  {
    col_names <- plyr::revalue(col_names,c(type = "feature",phase = "frame",seqnames = "seqname"))
    schema_matrix <- cbind(toupper(col_types),col_names)
    schema_matrix<- schema_matrix[setdiff(rownames(schema_matrix),c("group","width")),]
  }
  else
  {
    col_names <- plyr::revalue(col_names,c(start = "left", end = "right", seqnames = "chr"))
    schema_matrix <- cbind(toupper(col_types),col_names)
    schema_matrix<- schema_matrix[setdiff(rownames(schema_matrix),c("group","width")),]
  }
  rownames(schema_matrix) <- NULL
  colnames(schema_matrix) <- NULL
  
  schema_matrix <- .jarray(schema_matrix,dispatch = TRUE)
  meta_matrix <- .jarray(meta_matrix,dispatch = TRUE)
  region_matrix <- .jarray(region_matrix,dispatch = TRUE)
  

  WrappeR <- J("it/polimi/genomics/r/Wrapper")
  response <- WrappeR$read(meta_matrix,region_matrix,schema_matrix)
  DAGgraph(response)
}


.check_parser <- function(parser)
{
  parser <- toupper(parser)
  if(!identical(parser,"BEDPARSER") && !identical(parser,"ANNPARSER") && !identical(parser,"BROADPROJPARSER")
     && !identical(parser,"BASICPARSER") && !identical(parser,"NARROWPEAKPARSER") && !identical(parser,"RNASEQPARSER")
     && !identical(parser,"CUSTOMPARSER"))
    stop("parser not defined")

  parser
}

#' Disable or Enable remote processing
#'
#' It allows to enable or disable remote processing
#' 
#' @details 
#' The invocation of this function allow to change mode of processing.
#' The switch is possible at the beginning, when you didn't run any query at all, or after an execution 
#' (or take) function
#' 
#' @importFrom rJava J
#' 
#' @param is_remote single logical value used in order to set the processing mode.
#' TRUE you will set a remote query processing mode otherwise will be local
#' 
#' @return None
#' 
#' @examples
#' 
#' # initialize with remote processing off
#' initGMQL("tab",remote_processing=FALSE)
#' 
#' # change processing mode to remote
#' remote_processing(TRUE)
#'
#' @export
#'
remote_processing<-function(is_remote)
{
  WrappeR <- J("it/polimi/genomics/r/Wrapper")
  .check_logical(is_remote)
  response <- WrappeR$remote_processing(is_remote)
  print(response)
}


# remote_proc <- WrappeR$is_remote_processing()
# if(remote_proc)
# {
#   if(override)
#   {
#     list <- showDatasets(url)
#     name_dataset <- basename(dataset)
#     if(name_dataset %in% unlist(list$datasets))
#       deleteDataset(url,name_dataset)
#   }
#   else
#   {
#     list <- showDatasets(url)
#     name_dataset <- basename(dataset)
#     if(name_dataset %in% unlist(list$datasets))
#       stop("dataset already exist in repository")
#   }
#   uploadSamples(url,name_dataset,dataset,isGMQL = TRUE)
# }

