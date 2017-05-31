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
#'
#' @details
#' Instantiate in .GlobalEnv a scala singleton (Wrapper) used to call its scala methods
#'
#' @examples
#' \dontrun{
#'
#' initGMQL("tab")
#' }
#'
initGMQL <- function(output_format)
{
  out_format <- toupper(output_format)

  if(!identical(out_format,"TAB") && !identical(out_format,"VCF") && !identical(out_format,"GTF")
     && !identical(out_format,"COLLECT"))
    stop("output_format must be TAB, GTF, VCF or COLLECT")

  scalaCompiler <- scala(classpath = './inst/java/GMQL.jar',command.line.options = "-J-Xmx4g")
  WrappeR <<- scalaCompiler$do('it.polimi.genomics.r.Wrapper')
  WrappeR$initGMQL(out_format)
}

#' GMQL Function: READ
#'
#' Read a GMQL dataset or any other folder containig some homogenus sample
#' from disk, saving in Scala memory that can be referenced in R
#'
#'
#' @param DatasetPathFolder folder path for GMQL dataset
#' @param parser \code{\link{PARSER}} objects used to parsing dataset files
#' The PARSER's available are: BedParser, ANNParser, BroadProjParser, BasicParser,
#' NarrowPeakParser, RnaSeqParser, CustomParser.
#' Default is CustomParser.
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
#' r = read(path,BedParser())
#' r = read(path,RnaSeqParser())
#' }
#'
read <- function(DatasetPathFolder, parser = CustomParser(),local = T)
{
  if(local)
  {
    if(is.character(DatasetPathFolder) && !length(DatasetPathFolder)==1)
      stop("DatasetPathFolder: no multiple string")

    if(!dir.exists(DatasetPathFolder))
      stop("folder does not exist")
  }
  if(!is(parser,"PARSER"))
    stop("No correct parser")

  parser_name <- as.character(parser)
  out <- WrappeR$readDataset(DatasetPathFolder,parser_name,local)
  if(grepl("File",out,ignore.case = T) || grepl("No",out,ignore.case = T))
    stop(out)
  else
    out
}

