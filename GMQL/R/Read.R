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
#' Read a GMQL dataset from disk saving in Scala memory that can be referenced in R
#'
#'
#' @param DatasetPathFolder folder path for GMQL dataset
#' @return url-like string "pointer" to dataset
#' @examples
#'
#' startGMQL()
#' path = /.../dataset_name
#' r = read(path)
#'
#'
# read.GMQL <- function(DatasetPathFolder)
read <- function(DatasetPathFolder)
{
  if(!is.character(DatasetPathFolder))
    stop("input must be string")

  if(!dir.exists(DatasetPathFolder))
    stop("folder does not exist")

  out <- frappeR$readDataset(DatasetPathFolder)
  if(grepl("File",out,ignore.case = T))
    stop(out)
  else
    out
}


stopGMQL <- function()
{

}
