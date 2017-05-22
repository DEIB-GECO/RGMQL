#' start GMQL Server
#'
#' Set and run GMQL server for executing GMQL query
#'
#' @examples
#' startGMQL()
#'
startGMQL <- function()
{
  #call rscala, create my enviroment?
  #-Xmx4096m or --driver-memory 4g
  scalaCompiler <- scala(classpath = './inst/java/GMQL.jar',command.line.options = "-J-Xmx4g")
  frappeR <<- scalaCompiler$do('it.polimi.genomics.r.Wrapper')
  frappeR$startGMQL()
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
#' startGMQL()
#' path = "/<path_to_your_folder>/<your_dataset_name>"
#' r = read(path)
#' }
#'
read <- function(DatasetPathFolder, parser = CustomParser())
{
  if(!is.character(DatasetPathFolder))
    stop("input must be string")

  if(!dir.exists(DatasetPathFolder))
    stop("folder does not exist")

  if(!is(parser,"PARSER"))
    stop("you need to choose a parser")

  parser_name <- as.character(parser)
  out <- frappeR$readDataset(DatasetPathFolder,parser_name)
  if(grepl("File",out,ignore.case = T) || grepl("No",out,ignore.case = T))
    stop(out)
  else
    out
}


stopGMQL <- function()
{

}
