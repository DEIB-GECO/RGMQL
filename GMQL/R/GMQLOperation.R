#' Retrieve a Java instance of
#'
#' Retrieve a Java instance of SpellCorrector, with the training file
#' specified. Language model is trained before the instance is returned.
#' The spell corrector is adapted from Peter Norvig's demonstration.
#'
#' @param no param
#' @return a Java instance
#' @export
#'

#java works
prova <- function()
{
  .jinit()
  .jaddClassPath('./instr/java/prova.jar')
  aa <- .jfloat(10.4)
  myExchange <- .jnew("prova/prova2/myExchange",aa)
  myExchange2 <- .jnew("prova/prova2/myExchange",564.9999)
  myExchange3 <- .jnew("prova/prova2/myExchange",as.integer(3))
  stringTest <- .jcall(myExchange, "S", "getS")
  floatB <- .jcall(myExchange, "F", "getF")
  DoubleD <- .jcall(myExchange2, "D", "getD")
  integerI <- .jcall(myExchange3, "I", "getI")
  type <- .jcall(myExchange, "S", "getLastType")
  type2 <- .jcall(myExchange2, "S", "getLastType")
  type3 <- .jcall(myExchange3, "S", "getLastType")
  stringArrayTest <- .jcall(myExchange, "[S", "getStringArray")
  print(stringTest)
  stringArrayTest
}

#call rscala, create my enviroment?
#delete debug now
startGMQLServer <- function()
{
  #-Xmx4096m or --driver-memory 4g
  scalaCompiler <<- scala(classpath = './inst/java/GMQL.jar',command.line.options = "-J-Xmx4g" )
  frappeR <<- scalaCompiler$do('it.polimi.genomics.r.Wrapper')

  #TODO: change...now keep for better debugging
  frappeR$SetSparkContext()
  frappeR$runGMQLServer()
}

readDataset <- function(DatasetPathFolder)
{
  if(!is.character(DatasetPathFolder))
    stop("path must be string")

  pointer <- frappeR$readDataset(DatasetPathFolder)
  return(pointer)
}

materialize <- function(input_data, dir_out = "/Users/simone/Downloads/res")
{
  frappeR$materialize(input_data,dir_out)
}

#dataType == 'ChipSeq' AND view == 'Peaks' AND setType == 'exp' AND antibody_target == 'TEAD4'
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

extend <-function(metadata = NULL, input = "")
{
  #TODO: decide what R data structure use for aggregates
  aggrList <- scalaCompiler$do('List')$'apply[Array[String]]'(c("nuovoP","SUM","pvalue"),
                                                              c("aaaa","COUNT"))
  frappeR$extend(aggrList,input)
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

order <- function()
{

}
#  def union(right_data_input_path: String, right_name: String,
# left_data_input_path: String, left_name: String, data_output_path: String)
union <- function(right_data_input_path ="", right_prefix = "",
                  left_data_input_path ="",left_prefix = "", dir_out)
{
  left_data_input_path = "/Users/simone/Downloads/job_filename_guest_new14_20170316_162715_DATA_SET_VAR/files/"
  right_data_input_path = "/Users/simone/Downloads/res/"
  dir_out = "/Users/simone/Downloads/"

  frappeR$union(right_data_input_path,right_prefix,left_data_input_path,left_prefix,dir_out)
  return(dir_out)
}

difference <- function(joinBy = NULL,left_data_input_path = "",right_data_input_path ="", dir_out = "")
{
  if(!is.character(joinBy) && !is.null(joinBy))
    stop("joinBy can be only null, single string or an array of string")

  frappeR$difference(joinBy,right_data_input_path,left_data_input_path,dir_out)

}

#COVER methods and variant

flat <- function(minAcc,maxAcc,groupBy = NULL,aggregates = NULL, input)
{
  variantMethod("flat",minAcc,maxAcc,groupBy,aggregates,input)
}

cover <- function(minAcc,maxAcc,groupBy = NULL,aggregates = NULL, input)
{
  variantMethod("cover",minAcc,maxAcc,groupBy,aggregates,input)
}

histogram <- function(minAcc,maxAcc,groupBy = NULL,aggregates = NULL, input)
{
  variantMethod("histogram",minAcc,maxAcc,groupBy,aggregates,input)
}

summit <- function(minAcc,maxAcc,groupBy = NULL,aggregates = NULL, input)
{
  variantMethod("summit",minAcc,maxAcc,groupBy,aggregates,input)
}

variantMethod <- function(flag,minAcc,maxAcc,groupBy,aggregates,input)
{
  if(!is.numeric(minAcc) || !is.numeric(maxAcc))
    stop("minAcc and maxAcc must be numeric")

  if(minAcc < -1 || maxAcc < -1)
    stop("only 0,-1 or any positive value")

  min = as.integer(minAcc)
  max = as.integer(maxAcc)

  if(!is.character(groupBy) && !is.null(groupBy))
    stop("groupBy can be only null, single string or an array of string")

  #TODO: decide what R data structure use for aggregates
  aggrList <- scalaCompiler$do('List')$'apply[Array[String]]'(c("nuovoP","SUM","pvalue"),
                                                              c("aaaa","COUNT"))

  fl <- check(coverFlag,flag)

  switch(fl,"COVER" = frappeR$cover(min,max,groupBy,aggrList,input),
         "FLAT" = frappeR$flat(min,max,groupBy,aggrList,input),
         "SUMMIT" = frappeR$summit(min,max,groupBy,aggrList,input),
         "HISTOGRAM" = frappeR$histogram(min,max,groupBy,aggrList,input))

}

map <- function()
{

}

join <- function()
{

}

