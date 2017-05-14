#' start GMQL Server
#'
#' Set and run GMQL server
#'
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
#' Read a GMQL Dataset from disk
#'
#'
#' @param DatasetPathFolder folder path (e.g /Users/../../foldername)
#' @return string "pointer" to dataset
#'
#'
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
