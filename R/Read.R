#' Init GMQL Server
#'
#' Initialize and run GMQL server for executing GMQL query
#'
#' @import rscala
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
#'
#' @return None
#'
#' @examples
#'
#' # initialize GMQL with local processing
#' library(rscala)
#' initGMQL("tab",FALSE)
#' 
#' @export
#'
initGMQL <- function(output_format, remote_processing = FALSE)
{
  out_format <- toupper(output_format)

  if(!identical(out_format,"TAB")  && !identical(out_format,"GTF")
     && !identical(out_format,"COLLECT"))
    stop("output_format must be TAB, GTF or COLLECT")

  if(!is.logical(remote_processing) || length(remote_processing) >1)
    stop("remote_processing: invalid input or length > 1")
  
  WrappeR$initGMQL(out_format,remote_processing)
}

#' GMQL Function: READ
#'
#' Read a GMQL dataset or any other folder containig some homogenus sample
#' from disk, saving in Scala memory that can be referenced in R
#' Also used to read a repository dataset in case of remote processing.
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
#' if the remote processing is off you cannot set is_local=FALSE (an error occures)
#' @param url single string url of server: it must contain the server address and base url;
#' service name will be added automatically
#' useful only in remote processing
#' @param override single logical value used in order to determine the overriding of reading
#' dataset into repository, if an other dataset with the same name already exist into repostiory 
#' and override value is FALSE an error occures.
#' useful only in remote processing
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
#' test_path <- system.file("example","DATA_SET_VAR_GTF",package = "GMQL")
#' r = readDataset(test_path)
#' 
#' \dontrun{
#' ### local with other Parser
#' initGMQL("gtf")
#' test_path <- system.file("example","DATA_SET_VAR_GTF",package = "GMQL")
#' r = readDataset(test_path,"ANNParser")
#' }
#' 
#' @export
#'
readDataset <- function(dataset, parser = "CustomParser",is_local=TRUE,url=NULL, override= FALSE)
{
  remote_proc <- WrappeR$is_remote_processing()
  if(!remote_proc && !is_local)
    stop("you cannot use local processing with remote repository")
  
  if(!is.character(dataset) || length(dataset) >1)
    stop("dataset: invalid input or length > 1")
  
  if(!is.logical(override) || length(override) >1)
    stop("override: invalid input or length > 1")
  
  if(!is.logical(is_local) || length(is_local) >1)
    stop("is_local: invalid input or length > 1")

  if(is_local)
  {
    if(!dir.exists(dataset))
      stop("folder does not exist")

    remote_proc <- WrappeR$is_remote_processing()
    if(remote_proc)
    {
      if(override)
      {
        list <- showDatasets(url)
        name_dataset <- basename(dataset)
        if(name_dataset %in% unlist(list$datasets))
          deleteDataset(url,name_dataset)
      }
      else
      {
        list <- showDatasets(url)
        name_dataset <- basename(dataset)
        if(name_dataset %in% unlist(list$datasets))
          stop("dataset already exist in repository")
      }
      uploadSamples(url,name_dataset,dataset,isGMQL = TRUE)
    }
    
    schema_matrix <- scalaNull("Array[Array[String]]")
    schema_type <- scalaNull("String")
  }
  else
  {
    list <- showSchemaFromDataset(url,name_dataset)
    schema_names <- sapply(list$fields, function(x){x$name})
    schema_type <- sapply(list$fields, function(x){x$fieldType})
    schema_matrix <- cbind(schema_type,schema_names)
    schema_type <- list$schemaType
  }

  parser_name <- .check_parser(parser)

  out <- WrappeR$readDataset(dataset,parser_name,is_local,schema_matrix)
  if(grepl("File",out,ignore.case = TRUE) || grepl("No",out,ignore.case = TRUE))
    stop(out)
  else
    DAGgraph(out)
}

#' GMQL Function: READ
#'
#' Read a GrangesList saving in scala memory that can be referenced in R
#'
#' @param samples GrangesList
#'
#' @return DAGgraph class object. It contains the value associated to the graph used 
#' as input for the subsequent GMQL function
#' 
#' @examples
#'
#' \dontrun{
#' 
#' }
#' ""
#'
#' @export
#'
read <- function(samples)
{
  if(!is(samples,"GRangesList"))
    stop("only GrangesList")

  meta <- metadata(samples)
  if(is.null(meta)) {
    warning("GrangesList has no metadata. we provide two metadata for you")
    meta_matrix <- matrix(c("Provider","Polimi", "Application", "R-GMQL"),ncol = 2,byrow = TRUE)
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
  region_matrix<- region_matrix[,setdiff(colnames(region_matrix),"width")]
  col_types <- sapply(df,class)
  col_names <- names(col_types)
  #re order the schema?
  if("phase" %in% col_names) # if GTF
  {
    col_names <- plyr::revalue(col_names,c(type = "feature",phase = "frame",seqnames = "seqname"))
    schema_matrix <- cbind(toupper(col_types),col_names)
    schema_matrix<- schema_matrix[setdiff(rownames(schema_matrix),c("group","width")),]
  }
  else
  {
    col_names <- plyr::revalue(col_names,c(start = "left", end = "right", seqnarmes = "chr"))
    schema_matrix <- cbind(toupper(col_types),col_names)
    schema_matrix<- schema_matrix[setdiff(rownames(schema_matrix),c("group","width")),]
  }
  rownames(schema_matrix) <- NULL
  colnames(schema_matrix) <- NULL
  out <- WrappeR$read(meta_matrix,region_matrix,schema_matrix)
  if(grepl("No",out,ignore.case = TRUE))
    stop(out)
  else
    DAGgraph(out)
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
#' @param is_remote single logical value used in order to set the processing mode.
#' TRUE you will set a remote query processing mode otherwise will be local
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
  if(!is.logical(is_remote) || length(is_remote)>1)
    stop("is_remote: invalid input or length > 1")

  out <- WrappeR$remote_processing(is_remote)
  print(out)
}



