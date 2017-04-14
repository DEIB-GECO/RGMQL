#' Run GMQL Server
#'
#' Retrieve a Java instance of SpellCorrector, with the training file
#' specified. Language model is trained before the instance is returned.
#' The spell corrector is adapted from Peter Norvig's demonstration.
#'
#' @param no param
#' @return a Java instance
#' @export
#'

runGMQLJava <- function()
{
  .jinit()
  .jaddClassPath('./inst/java/GMQL.jar')
  myExchange <- .jnew('it.polimi.genomics.r.Wrapper')
}

debug <- function()
{
  scalaCompiler <<- scala(classpath = './inst/java/GMQL.jar',command.line.options = "-J-Xmx4g" )
  frappeR <<- scalaCompiler$do('it.polimi.genomics.r.Wrapper')
}

#call rscala, create my enviroment?
#delete debug now
runGMQL <- function()
{
  #-Xmx4096m or --driver-memory 4g
  scalaCompiler <<- scala(classpath = './inst/java/GMQL.jar',command.line.options = "-J-Xmx4g" )
  frappeR <<- scalaCompiler$do('it.polimi.genomics.r.Wrapper')
  frappeR$runGMQL()
}

readDataset <- function(DatasetPathFolder)
{
  if(!is.character(DatasetPathFolder))
    stop("input must be string")

  pointer <- frappeR$readDataset(DatasetPathFolder)
  return(pointer)
}

materialize <- function(input_data, dir_out = "/Users/simone/Downloads/res")
{
  frappeR$materialize(input_data,dir_out)
}

select <- function(predicate = "(dataType == 'ChipSeq' AND view == 'Peaks'
                   AND setType == 'exp' AND antibody_target == 'TEAD4')", region = NULL,
                   semijoin = NULL,input_data)
{
  if(!is(semijoin,"SemiJoinParam") && !is.null(semijoin))
    stop("semijoin must be a SemiJoinParam")

  list_attribute_semiJoin <- semijoin@semiJoinMeta
  operation <- semijoin@operation_in
  dataset_semijoin <- semijoin@dataset_path_join_IN

  out <- frappeR$select(predicate,region,list_attribute_semiJoin,
                 operation,dataset_semijoin,input_data)

  return(out)
}

project <-function()
{

}

extend <-function(metadata = NULL, input_data = "")
{
  if(!is.null(metadata) && !is.list(metadata))
    stop("Aggregates must be a list")

  if(!is.null(metadata) && !all(sapply(metadata, function(x) class(x) == "AggregatesMeta")))
    stop("aggregates must be a list of Class AggregatesMeta Object")

  if(is.null(metadata))
    aggrMatrix <- NULL
  else
    aggrMatrix <- t(sapply(metadata, function(x) as(x,"character")))

  frappeR$extend(aggrMatrix,input)
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

union <- function(right_input_data, right_prefix = "",left_input_datat,left_prefix = "")
{
  frappeR$union(right_input_data,right_prefix,left_input_datat,left_prefix)
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
  doVariant(coverFlag$FLAT,minAcc,maxAcc,groupBy,aggregates,input_data)
}

cover <- function(minAcc,maxAcc,groupBy = NULL,aggregates = NULL, input_data)
{
  doVariant(coverFlag$COVER,minAcc,maxAcc,groupBy,aggregates,input_data)
}

histogram <- function(minAcc,maxAcc,groupBy = NULL,aggregates = NULL, input_data)
{
  doVariant(coverFlag$HISTOGRAM,minAcc,maxAcc,groupBy,aggregates,input_data)
}

summit <- function(minAcc,maxAcc,groupBy = NULL,aggregates = NULL, input_data)
{
  doVariant(coverFlag$SUMMIT,minAcc,maxAcc,groupBy,aggregates,input_data)
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

  out <- switch(flag,"COVER" = frappeR$cover(min,max,groupBy,aggrMatrix,input_data),
                     "FLAT" = frappeR$flat(min,max,groupBy,aggrMatrix,input_data),
                     "SUMMIT" = frappeR$summit(min,max,groupBy,aggrMatrix,input_data),
                     "HISTOGRAM" = frappeR$histogram(min,max,groupBy,aggrMatrix,input_data))
#wchich value return on error?
}

map <- function(aggregates = NULL, right_input_data,exp_name = "",
                left_input_data,ref_name ="",count_name = "")
{
  frappeR$map(aggregates,right_input_data,exp_name,left_input_data,ref_name,count_name)
}

join <- function(input_data)
{

}


prova <- function(l)
{
  if(all(sapply(l, function(x) class(x) == "Aggregates")))
    print("OK")
  else
    print("NO")
}
