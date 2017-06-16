if(getRversion() >= "2.15.1")
  utils::globalVariables("WrappeR")

if(getRversion() >= "3.1.0")
  utils::suppressForeignCheck("WrappeR")


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
#' can be local (local machine) or remote (cluster).
#'
#'
#' @return no object return
#'
#' @details
#' Instantiate in .GlobalEnv a scala singleton (Wrapper) used to call its scala methods
#'
#' @examples
#'
#' \dontrun{
#'
#' library(rscala)
#' initGMQL("tab",FALSE)
#' }
#' ""
#' @export
#'
#'
initGMQL <- function(output_format, remote_processing = FALSE)
{
  out_format <- toupper(output_format)

  if(!identical(out_format,"TAB")  && !identical(out_format,"GTF")
     && !identical(out_format,"COLLECT"))
    stop("output_format must be TAB, GTF or COLLECT")

  scalaCompiler <- rscala::scala(classpath = './inst/java/GMQL.jar',command.line.options = "-J-Xmx4g")
  assign("WrappeR",scalaCompiler$do('it.polimi.genomics.r.Wrapper'),.GlobalEnv)
  WrappeR$initGMQL(out_format,remote_processing)
}

#' GMQL Function: READ
#'
#' Read a GMQL dataset or any other folder containig some homogenus sample
#' from disk, saving in Scala memory that can be referenced in R
#' Also used to read a repository dataset.
#'
#' @param DatasetFolder single string folder path for GMQL dataset or datasetname on repository
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
#' @param is_local logical value indicating if dataset is a local or remote dataset
#' if the remote processing is off you cannot use a remote repository (error occured)
#' @param url single string url of server: it must contain the server address and base url;
#' service name will be added automatically
#' @param override logical value
#'
#' @return "url-like" string to dataset
#'
#'
#' @details
#' Normally a GMQL dataset contains an XML schema file that contains
#' name of column header. (e.g chr, start, stop, strand)
#' The CustomParser read this XML schema; if you already know what kind of schema your files are use one of
#' the parser defined without reading any XML schema
#'
#' @examples
#'
#' \dontrun{
#'
#' ### local with CustomParser
#' initGMQL("gtf")
#' test_path <- system.file("example","DATA_SET_VAR_GTF",package = "GMQL")
#' r = read(test_path)
#'
#' ### local with other Parser
#' initGMQL("gtf")
#' test_path <- system.file("example","DATA_SET_VAR_GTF",package = "GMQL")
#' r = read(test_path,"ANNParser")
#' }
#' ""
#' @export
#'
readDataset <- function(dataset, parser = "CustomParser",is_local=TRUE,url=NULL, override= FALSE)
{
  remote_proc <- WrappeR$is_remote_processing()
  if(!remote_proc && !is_local)
    stop("you cannot use local processing with remote repository")

  if(!is.character(dataset))
    stop("dataset: invalid input")

  if(length(dataset)!=1)
    stop("dataset: no multiple string")

  if(length(override)>1)
    warning("override not > 1")
  override <- override[1]
  if(is_local)
  {
    remote_proc <- WrappeR$is_remote_processing()
    if(remote_proc)
    {
      if(override)
      {
        list <- showDatasets(url)
        name_dataset <- basename(dataset)
        if(name_dataset %in% unlist(list$datasets))
          deleteDataset(url,name_dataset)

        uploadSamples(url,name_dataset,dataset,isGMQL = TRUE)
      }
      else
      {
        list <- showDatasets(url)
        name_dataset <- basename(dataset)
        if(name_dataset %in% unlist(list$datasets))
          stop("dataset already exist in repository")

        uploadSamples(url,name_dataset,dataset,isGMQL = TRUE)
      }
    }
    if(!dir.exists(dataset))
      stop("folder does not exist")
    schema_matrix <- NULL
    schema_type <- NULL
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
    out
}

#' GMQL Function: READ
#'
#' Read a GrangesList saving in Scala memory that can be referenced in R
#'
#' @param samples GrangesList
#'
#' @return "url-like" string to dataset
#'
#' @examples
#'
#' \dontrun{
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
  col_names <- plyr::revalue(col_names,c(type = "feature",phase = "frame",seqnames = "seqname"))
  schema_matrix <- cbind(toupper(col_types),col_names)
  schema_matrix<- schema_matrix[setdiff(rownames(schema_matrix),c("group","width","seqnames","strand","end","start")),]
  rownames(schema_matrix) <- NULL
  colnames(schema_matrix) <- NULL
  out <- WrappeR$read(meta_matrix,region_matrix,schema_matrix)
  if(grepl("No",out,ignore.case = TRUE))
    stop(out)
  else
    out

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
#' @param is_remote logical value, if TRUE you will set a remote query processing mode otherwise will be local
#'
#' @examples
#' \dontrun{
#'
#' }
#' ""
#'
#' @export
#'
remote_processing<-function(is_remote)
{
  if(!is.logical(is_remote))
    stop("must be logical")

  if(length(is_remote)>1)
    warning("only the first element is taken")

  is_remote <- is_remote[1]

  out <- WrappeR$remote_processing(is_remote)
  if(grepl("cannot",out,ignore.case = TRUE))
    stop(out)
  else
    print(out)
}



