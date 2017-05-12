#' start GMQL Server
#'
#' Set Spark configuration and context.
#' set GMQL spark executor and run GMQL server
#'
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

#' Read Dataset from Disk
#'
#' Allow to read a GMQL Dataset from disk
#'
#' @param DatasetPathFolder folder path (e.g /Users/../../foldername)
#' @return string "pointer" to dataset
#'
read <- function(DatasetPathFolder)
{
  if(!is.character(DatasetPathFolder))
    stop("input must be string")

  if(!dir.exists(DatasetPathFolder))
    stop("folder does not exist")

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
  out <- frappeR$execute()
  if(grepl("OK",out,ignore.case = T))
    invisible("")
  else
    stop(out)
  end <- Sys.time()
  diff <- end - start
  print(diff)

}

#' GMQL Operation: MATERIALIZE
#'
#'The materialize operation saved dataset in the system to make it usable in other GMQL queries.
#'To preserve the content of any dataset generated during a GMQL query,
#'the dataset must be materialized.
#'Any dataset can be materialized, however the operation is time expensive;
#'for best performance materialize the relevant data only.
#'
#'
#' @param input_data string pointer taken from GMQL function
#' @param dir_out out path folder for default is working directory
#' @examples
#' materialize(input_data = c, dir_out = "/.../foldername")
#' materialize(c,"/.../foldername")
#'
materialize <- function(input_data, dir_out)
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

select <- function(predicate = NULL, region = NULL,semijoin = NULL,semi_join_dataset = NULL,input_data)
{
  predicate <- "(dataType == 'ChipSeq' AND view == 'Peaks' AND setType == 'exp'
  AND antibody_target == 'TEAD4')"

  if(!is.character(predicate))
    stop("prdicate must be a string")

  if(!is.character(region))
    stop("region must be a string")

  if(!is.list(semijoin))
    stop("semijoin must be a list")


  if(length(semijoin)!=0 || !is.null(semijoin))
  {
    value <- names(semijoin)
    attributeStrategy <- unlist(semijoin)
  }






  frapper$select()
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
#'It generates new metadata attributes Am as result of aggregate
#'functions g applied to region attributes;
#'aggregate functions are applied sample by sample,
#'resulting tuples are triples with the sample identifier,
#'the attribute name Am, and the computed aggregate value.
#'
#'
#' @param metadata a list of MetaAggregate object
#' @param input_data string pointer returned from all GMQL function
#' @examples
#' r = read(path)
#' e = extend(metadata = list(),input_data = r)
#' e = extena(input_data = r)
#'
extend <-function(metadata = NULL, input_data)
{
  if(!is.null(metadata) && !is.list(metadata))
    stop("metadata must be a list")

 # if(!is.null(metadata) && !all(sapply(metadata, function(x) class(x) == "MetaAggregates")))
  #  stop("aggregates must be a list of Class MetaAggregates Object")




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

#' GMQL Operation: MERGE
#'
#'It builds a dataset consisting of a single sample having as regions
#'all the regions of the input data (without altering their
#'coordinates, even when overlapping) and as metadata the union of all
#'the attribute-values of the input samples.
#'When a groupBy clause is present, the samples are partitioned by groups,
#'each with distinct values of grouping metadata attributes.
#'The merge operation is separately applied to each group,
#'yielding to one sample in the result for each group.
#'
#'
#' @param metadata a list of MetaAggregate object
#' @param input_data string pointer taken from GMQL function
#' @examples
#' r = read(path)
#' m = merge(groupBy = c(),input_data = r)
#' m = merge(input_data = r)
#'
merge <- function(groupBy = NULL,input_data)
{
  if(!is.character(groupBy) && !is.null(groupBy))
    stop("groupBy can be NULL, single string or an array of string")

  frappeR$merge(groupBy,input_data)
}

order <- function(metadata = NULL, mtop = NULL, mtopg = NULL,
                  regions = NULL,rtop = NULL,rtopg = NULL, input_data)
{

  frappeR$order(metadata,mtopg,mtop,regions,rtopg,rtop,input_data)

}

#' GMQL Operation: UNION
#'
#'It is used to integrate possibly heterogeneous samples of two datasets within a single dataset;
#'each sample of both input datasets contributes to one sample of the result
#'with identical metadata and merged region schema.
#'New identifiers are assigned to each sample.
#'Two region attributes are considered identical if they have the same name and type;
#'the merging of two schemas is performed by projecting the schema of the second dataset
#'over the schema of the first one.
#'Fields of the first dataset which are missing in the second one are set to NULL value,
#'for all the regions of the second operator.
#'For what concerns metadata,attributes are prefixed with the strings LEFT or RIGHT
#'so as to trace the dataset to which they refer.
#'
#'
#' @param right_input_data string pointer taken from GMQL function
#' @param left_input_data string pointer taken from GMQL function
#' @examples
#' r = read(path)
#' r2 = read(path2)
#' u = union(r2,r)
#' u = union(right_input_data = r,left_input_data = r2)
#'
union <- function(right_input_data,left_input_data)
{
  frappeR$union(right_input_data,left_input_data)
}

difference <- function(joinBy = NULL,left_input_data,right_input_data)
{
  if(!is.character(joinBy) && !is.null(joinBy))
    stop("joinBy can be NULL, single string or an array of string")

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

map <- function(aggregates = NULL, right_input_data,
                left_input_data)
{
  frappeR$map(aggregates,right_input_data,left_input_data)
}

join <- function(input_data)
{

}

