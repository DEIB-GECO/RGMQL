
textQueryEnv <- new.env()
textQueryEnv$a = "asdas"
textQueryEnv$b = "sa"

debug <- function()
{
  scalaCompiler <<- scala(classpath = './inst/java/GMQL.jar',command.line.options = "-J-Xmx4g" )
  frappeR <<- scalaCompiler$.it.polimi.genomics.r.Wrapper
}


#' Run GMQL Server
#'
#' Run GMQL Server
#'
#'
startGMQL <- function()
{
  #call rscala, create my enviroment?
  #delete debug now
  #-Xmx4096m or --driver-memory 4g
  scalaCompiler <<- scala(classpath = './inst/java/GMQL.jar',command.line.options = "-J-Xmx4g" )
  frappeR <<- scalaCompiler$.it.polimi.genomics.r.Wrapper
  frappeR$startGMQL()
}

#' Read Dataset from Disk
#'
#' Allow to read a GMQL Dataset from disk
#'
#' @param DatasetPathFolder folder path (e.g /Users/../../foldername)
#' @return string "pointer" to dataset
#'
read <- function(DatasetPathFolder)
{
  if(!is.character(DatasetPathFolder) && length(DatasetPathFolder)>1)
    stop("input must be string")

  pointer <- frappeR$readDataset(DatasetPathFolder)
  return(pointer)
}

#' Execute GMQL query
#'
#'
#'
#'
#'
execute <- function()
{
  start <- Sys.time()
  exe <- frappeR$execute()
  if(grepl("OK",out,ignore.case = T))
    invisible(exe)
  else
    stop(exe)
  end <- Sys.time()
  diff <- end - start
  print(diff)

}


#' Materialize new dataset
#'
#'
#'
#'
#' @param input_data string pointer taken from GMQL function
#' @param dir_out out folder path (e.g /Users/../../foldername)
#'
materialize <- function(input_data, dir_out = "/Users/simone/Downloads/res")
{
  frappeR$materialize(input_data,dir_out)
}

#' GMQL Operation: SELECT
#'
#'
#'
#'
#' @param predicate string made up by logical oepration: AND,OR,NOT
#' @param region region
#' @param semijoin semijoin
#' @param input_data string pointer taken from GMQL function
#'
#'

select <- function(predicate = NULL, region = NULL,semijoin = NULL,input_data)
{
  predicate <- "(dataType == 'ChipSeq' AND view == 'Peaks' AND setType == 'exp'
  AND antibody_target == 'TEAD4')"
  if(!is.character(predicate))
    stop("prdicate must be a string")

  if(!is.character(region))
    stop("region must be a string")
}

#' GMQL Operation: PROJECT
#'
#'
#'
#'
#' @param predicate string made up by logical oepration: AND,OR,NOT
#' @param region region
#' @param semijoin semijoin
#' @param input_data string pointer taken from GMQL function
#'
#'
project <-function()
{

}


#' GMQL Operation: EXTEND
#'
#'
#'
#'
#' @param metadata a list of MetaAggregatesClass object
#' @param input_data string pointer taken from GMQL function
#'
#'
extend <-function(metadata = NULL, input_data)
{
  if(!is.null(metadata) && !is.list(metadata))
    stop("Aggregates must be a list")

  if(!is.null(metadata) && !all(sapply(metadata, function(x) class(x) == "MetaAggregates")))
    stop("aggregates must be a list of Class MetaAggregates Object")

  if(is.null(metadata))
    aggrMatrix <- NULL
  else
    aggrMatrix <- t(sapply(metadata, function(x) as(x,"character")))

  out <- frappeR$extend(aggrMatrix,input_data)

  if(grepl("missing",out,ignore.case = T))
    stop(out)
  else
    out
}

group <-function()
{

}

merge <- function(groupBy = NULL,input_data)
{
  if(!is.character(groupBy) && !is.null(groupBy))
    stop("groupBy can be only null, single string or an array of string")

  frappeR$merge(groupBy,input_data)
}

order <- function(input_data)
{

}

union <- function(right_input_data,left_input_data)
{
  frappeR$union(right_input_data,left_input_data)
}

difference <- function(joinBy = NULL,left_input_data,right_input_data)
{
  if(!is.character(joinBy) && !is.null(joinBy))
    stop("joinBy can be only null, single string or an array of string")

  frappeR$difference(joinBy,right_input_data,left_input_data)
}

#COVER methods and variant

flat <- function(minAcc,maxAcc,groupBy = NULL,aggregates = NULL, input_data)
{
  doVariant("FLAT",minAcc,maxAcc,groupBy,aggregates,input_data)
}

cover <- function(minAcc,maxAcc,groupBy = NULL,aggregates = NULL, input_data)
{
  doVariant("COVER",minAcc,maxAcc,groupBy,aggregates,input_data)
}

histogram <- function(minAcc,maxAcc,groupBy = NULL,aggregates = NULL, input_data)
{
  doVariant("HISTOGRAM",minAcc,maxAcc,groupBy,aggregates,input_data)
}

summit <- function(minAcc,maxAcc,groupBy = NULL,aggregates = NULL, input_data)
{
  doVariant("SUMMIT",minAcc,maxAcc,groupBy,aggregates,input_data)
}

doVariant <- function(flag,minAcc,maxAcc,groupBy,aggregates,input_data)
{
  if(!is.numeric(minAcc) || !is.numeric(maxAcc))
    stop("minAcc and maxAcc must be numeric")

  if(minAcc < -1 || maxAcc < -1)
    stop("only 0,-1 or any positive value")

  min = as.integer(minAcc)
  max = as.integer(maxAcc)

  if(!is.character(groupBy) && !is.null(groupBy))
    stop("groupBy can be only null, single string or an array of string")

  if(!is.null(aggregates) && !is.list(aggregates))
    stop("Aggregates must be a list")

  if(!is.null(aggregates) && !all(sapply(aggregates, function(x) class(x) == "Aggregates")))
    stop("aggregates must be a list of Class Aggregates Object")

  if(is.null(aggregates))
    aggrMatrix <- NULL
  else
    aggrMatrix <- t(sapply(aggregates, function(x) as(x,"character")))

  out <- switch(flag,
                "COVER" = frappeR$cover(min,max,groupBy,aggrMatrix,input_data),
                "FLAT" = frappeR$flat(min,max,groupBy,aggrMatrix,input_data),
                "SUMMIT" = frappeR$summit(min,max,groupBy,aggrMatrix,input_data),
                "HISTOGRAM" = frappeR$histogram(min,max,groupBy,aggrMatrix,input_data))

  if(grepl("missing",out,ignore.case = T))
    stop(out)
  else
    out
}

map <- function(aggregates = NULL, right_input_data,exp_name = "",
                left_input_data,ref_name ="",count_name = "")
{
  frappeR$map(aggregates,right_input_data,exp_name,left_input_data,ref_name,count_name)
}

join <- function(input_data)
{

}

