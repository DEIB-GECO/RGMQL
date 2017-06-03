#' Init GMQL Server
#'
#' Initialize and run GMQL server for executing GMQL query
#'
#' @param output_format single string identifies the output format of sample files.
#' Can be TAB, GTF, VCF or COLLECT
#' \itemize{
#' \item{TAB: tab delimited file format}
#' \item{GTF: file format used to hold information about gene structure.
#' It is a tab-delimited text format based on the general feature format}
#' \item{COLLECT: used for storing output in memory}
#' \item{VCF: Text file format (most likely stored in a compressed manner).
#' It contains meta-information lines, a header line, data lines each containing information about a position in the genome.}
#' }
#' @param remote_processing logical
#'
#' @details
#' Instantiate in .GlobalEnv a scala singleton (Wrapper) used to call its scala methods
#'
#' @examples
#' \dontrun{
#'
#' initGMQL("tab",F)
#' }
#'
initGMQL <- function(output_format, remote_processing = F)
{
  out_format <- toupper(output_format)

  if(!identical(out_format,"TAB") && !identical(out_format,"VCF") && !identical(out_format,"GTF")
     && !identical(out_format,"COLLECT"))
    stop("output_format must be TAB, GTF, VCF or COLLECT")

  scalaCompiler <- scala(classpath = './inst/java/GMQL.jar',command.line.options = "-J-Xmx4g")
  WrappeR <<- scalaCompiler$do('it.polimi.genomics.r.Wrapper')
  WrappeR$initGMQL(out_format,remote_processing)
}

#' GMQL Function: READ
#'
#' Read a GMQL dataset or any other folder containig some homogenus sample
#' from disk, saving in Scala memory that can be referenced in R
#'
#'
#' @param DatasetFolder folder path for GMQL dataset or datasetname on repository
#' @param parser string used to parsing dataset files
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
#' @param is_local logical value indicates if dataset is a local or remote dataset
#' if the remote processing is off you cannot use a remote repository (error occured)
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
#' initGMQL("gtf")
#' path = "/<path_to_your_folder>/<your_dataset_name>"
#' r = read(path)
#' r = read(path,"BedParser")
#' r = read(path,"RnaSeqParser")
#' }
#'
read <- function(DatasetFolder, parser = "CustomParser",is_local=T)
{
  remote_proc <- WrappeR$is_remote_processing()
  if(!remote_proc && !is_local)
    stop("you cannot use local processing with remote repository")

  if(!is.character())
    stop("DatasetFolder: must be one string")

  if(length(DatasetFolder)!=1)
    stop("DatasetFolder: no multiple string")

  if(is_local)
    if(!dir.exists(DatasetFolder))
      stop("folder does not exist")

  parser_name <- .check_parser(parser)

  out <- WrappeR$readDataset(DatasetFolder,parser_name,is_local)
  if(grepl("File",out,ignore.case = T) || grepl("No",out,ignore.case = T))
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



#' Disable remote processing
#'
#'
#'
off_remote_processing<- function()
{
  WrappeR$remote_processing(F)
  print("remote proessing off")
}

#' Enable remote processing
#'
#'
#'
on_remote_processing<- function()
{
  WrappeR$remote_processing(T)
  print("remote proessing on")
}



