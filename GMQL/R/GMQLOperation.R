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

#' GMQL Function: EXECUTE
#'
#' execute GMQL query
#'
#'
#' @examples
#' execute()
#'
execute <- function()
{
  out <- frappeR$execute()
  if(grepl("OK",out,ignore.case = T))
    print("Done")
  else
    stop(out)
}

#' GMQL Operation: MATERIALIZE
#'
#'The materialize operation saved dataset in the system to make it usable in other GMQL queries.
#'To preserve the content of any dataset generated during a GMQL query,
#'the dataset must be materialized.
#'Any dataset can be materialized, however the operation is time expensive;
#'for best performance, materialize the relevant data only.
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
  out <- frappeR$materialize(input_data,dir_out)
  if(grepl("No",out,ignore.case = T))
    stop(out)
  else
    NULL
}

#' GMQL Operation: SELECT
#'
#'It keeps in the result all the samples which existentially satisfy the predicate on metadata
#'and then selects those regions of selected samples which satisfy the predicate on regions.
#'A sample is legal also when it contains no regions as result of a selection.
#'Semi-join clauses are used to further select samples;
#'A semi-join clause can be constructed as the conjunction of the simple metadata predicates
#'that refer to the same dataset.
#'Semi-joins are used to connect variables
#'
#'
#' @param predicate string made up by logical oepration: AND,OR,NOT on metadata values
#' @param region_predicate string made up by logical oepration: AND,OR,NOT on region values
#' @param semijoin vector of metadata attribute consider for semijoining
#' @param semi_join_in logical value
#' @param semi_join_dataset string pointer taken from GMQL function needed for semijoining
#' @param input_data string pointer taken from GMQL function
#'
#'
select <- function(predicate = NULL, region_predicate = NULL,semi_join = NULL,
                   semi_join_dataset = NULL,input_data)
{

  if(!is.null(predicate))
    if(!is.character(predicate))
      stop("prdicate must be a string predicate")

  if(!is.null(region_predicate))
    if(!is.character(region_predicate))
      stop("region must be a string predicate")

  if(is.null(semi_join) && is.null(semi_join_dataset)){
    #trick, if we call it like that
  }
  else if(is.null(semi_join) || is.null(semi_join_dataset)){
    warning("You did not set correctly semijoin parameters.\n Select function will be invoked without semijoin expression")
    semi_join=NULL
    semi_join_dataset=NULL
  }
  else
  {
    if(!is.character(semi_join))
      stop("must be a vector of attributes")

    if(!is.character(semi_join_dataset))
      stop("must be a string")
  }

  out <- frappeR$select(predicate,region_predicate,semi_join,semi_join_dataset,input_data)
  if(grepl("No",out,ignore.case = T))
    stop(out)
  else
    out
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
#' @param metadata a list of element key = value, where value is c("aggregate function","value")
#' @param input_data string pointer returned from all GMQL function
#' @examples
#' r = read(path)
#' e = extend(metadata = list(regionsCount = c("COUNT","")),input_data = r)
#' e = extend(metadata = list(sumValue = c("SUM","pvalue")),input_data = r)
#' e = extena(input_data = r)
#'
extend <-function(metadata = NULL, input_data)
{
  if(!is.null(metadata)) {
    if(!is.list(metadata))
      stop("metadata must be a list")

    names <- names(metadata)
    if(is.null(names)) {
      warning("you did not assign a names to a list.\nWe use the same name of values used to perform the aggregates function")
      names <- sapply(metadata, function(x) {
        func <- check.AggregatesFunction(x[1])
        if(func=="COUNT")
          names <- "count"
        else
          names <- x[[2]]
      })
    }
    else {
      if(!all(sapply(names, function(x) {
        if(x=="") F else T})))
        {
        stop("no partial names assignment to list")
      }
    }
    aggregate_matrix <- t(sapply(metadata, function(x){

      if(length(x)>2)
        warning("must be a vector of only two element, we will not consider other parameter")

      func <- check.AggregatesFunction(x[1])
      if(x[2] == "")
        stop("second parameter must be defined")

      if(length(x)<2 && func!="COUNT")
        stop("except COUNT, the other aggregates function must have the value")
      if(func=="COUNT")
        x = c(x[1],"")

      new_value = c(x[1],x[2])
      matrix <- matrix(new_value)
    }))
    m_names <- matrix(names)
    metadata_matrix <- cbind(m_names,aggregate_matrix)
  }
  else
    metadata_matrix <- NULL

  out <- frappeR$extend(metadata_matrix,input_data)

  if(grepl("No",out,ignore.case = T))
    stop(out)
  else
    out
}

#' GMQL Operation: GROUP
#'
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
#' @param metadata a vector of metadata
#' @param input_data string pointer taken from GMQL function
#' @examples
#' r = read(path)
#' m = merge(groupBy = c("antibody_targer","cell_karyotype"),input_data = r)
#' m = merge(input_data = r)
#'
merge <- function(groupBy = NULL,input_data)
{
  if(!is.character(groupBy) && !is.null(groupBy))
    stop("groupBy can be a string or an array of string")

  out <- frappeR$merge(groupBy,input_data)
  if(grepl("No",out,ignore.case = T))
    stop(out)
  else
    out
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
  out <- frappeR$union(right_input_data,left_input_data)
  if(grepl("No",out,ignore.case = T))
    stop(out)
  else
    out
}
#' GMQL Operation: DIFFERENCE
#'
#'This operation produces a sample in the result for each sample of the first operand S1,
#'with identical identifier and metadata. It considers all the regions of the second operand,
#'that we denote as negative regions; for each sample s1 of S1, it includes in the corresponding
#'result sample those regions which do not intersect with any negative region.
#'When the JOINBY clause is present, for each sample s1 of the first dataset S1
#'we consider as negative regions only the regions of the samples s2 of S2 that satisfy the join condition.
#'Syntactically, the clause consists of a list of attribute names,
#'which are homonyms from the schemas of S1 and of S2;
#'the strings LEFT or RIGHT that may be present as prefixes of attribute names
#'as result of binary operators are not considered for detecting homonyms.
#'We formally define a simple equi-join predicate ai == aj ,
#'but the generalization to conjunctions of simple predicates is straightforward.
#'The predicate is true for given samples s1 and s2 iff the two attributes share at least one value,
#'e.g.: p(ai,aj) ⇐⇒ ∃ (ai,vi) ∈ M1,(aj,vj) ∈ M2 : vi = vj The operation:
#'
#' @param joinBy a vector of metadata
#' @param right_input_data string pointer taken from GMQL function
#' @param left_input_data string pointer taken from GMQL function
#' @examples
#'
difference <- function(joinBy = NULL,left_input_data,right_input_data)
{
  if(!is.character(joinBy) && !is.null(joinBy))
    stop("joinBy can be NULL, single string or an array of string")

  out <- frappeR$difference(joinBy,right_input_data,left_input_data)
  if(grepl("No",out,ignore.case = T))
    stop(out)
  else
    out
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

  if(!is.null(metadata)) {
    if(!is.list(metadata))
      stop("metadata must be a list")

    names <- names(metadata)
    if(is.null(names)) {
      warning("you did not assign a names to a list.\nWe use the same name of values used to perform the aggregates function")
      names <- sapply(metadata, function(x) {
        func <- check.AggregatesFunction(x[1])
        if(func=="COUNT")
          names <- "count"
        else
          names <- x[[2]]
      })
    }
    else {
      if(!all(sapply(names, function(x) {
        if(x=="") F else T})))
      {
        stop("no partial names assignment to list")
      }
    }
    aggregate_matrix <- t(sapply(metadata, function(x){

      if(length(x)>2)
        warning("must be a vector of only two element, we will not consider other parameter")

      func <- check.AggregatesFunction(x[1])
      if(x[2] == "")
        stop("second parameter must be defined")

      if(length(x)<2 && func!="COUNT")
        stop("except COUNT, the other aggregates function must have the value")
      if(func=="COUNT")
        x = c(x[1],"")

      new_value = c(x[1],x[2])
      matrix <- matrix(new_value)
    }))
    m_names <- matrix(names)
    metadata_matrix <- cbind(m_names,aggregate_matrix)

  out <- switch(flag,
                "COVER" = frappeR$cover(min,max,groupBy,metadata_matrix,input_data),
                "FLAT" = frappeR$flat(min,max,groupBy,metadata_matrix,input_data),
                "SUMMIT" = frappeR$summit(min,max,groupBy,metadata_matrix,input_data),
                "HISTOGRAM" = frappeR$histogram(min,max,groupBy,metadata_matrix,input_data))

  if(grepl("No",out,ignore.case = T))
    stop(out)
  else
    out
}

map <- function(aggregates = NULL, joinBy = NULL, right_input_data,left_input_data)
{
  frappeR$map(aggregates,right_input_data,left_input_data)
}

join <- function(genometric_predicate = NULL, output=NULL, joinBy = NULL, right_input_data, left_input_data)
{

}

