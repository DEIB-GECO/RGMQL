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
startGMQLServer <- function()
{
  #-Xmx4096m or --driver-memory 4g
  scalaCompiler <- scala(classpath = './inst/java/GMQL.jar',command.line.options = "-J-Xmx4g" )
  frappeR <<- scalaCompiler$do('it.polimi.genomics.r.Wrapper')
  frappeR$SetSparkContext()
  frappeR$runGMQLServer()
}

readDataset <- function(Dataset)
{
  if(!is.character(Dataset))
    stop("path must be string")

  dataset <- paste0(Dataset,"/files")

  frappeR$readDataset(dataset)
  return(Dataset)
}

materialize <- function(input, dir_out = "")
{
  dir_out = "/Users/simone/Downloads/res/"
  frappeR$materialize(input,dir_out)
}

#dataType == 'ChipSeq' AND view == 'Peaks' AND setType == 'exp' AND antibody_target == 'TEAD4'
select <- function(predicate = "(dataType == 'ChipSeq' AND view == 'Peaks'
                   AND setType == 'exp' AND antibody_target == 'TEAD4')", region = NULL,
                   semijoin = NULL,from)
{
  if(!is(semijoin,"SemiJoinParam") && !is.null(semijoin))
    stop("semijoin must be a SemiJoinParam")


  dir_in = "/Users/simone/Downloads/job_filename_guest_new14_20170316_162715_DATA_SET_VAR/files/"
  dir_out = "/Users/simone/Downloads/res/"

  list_attribute_semiJoin <- semijoin@semiJoinMeta
  operation <- semijoin@operation_in
  dataset_semijoin <- semijoin@dataset_path_join_IN

  frappeR$select(predicate,region,list_attribute_semiJoin,
                 operation,dataset_semijoin,from, "dir_out")

  return(dir_out)
}

project <-function()
{

}

extend <-function(metadata = NULL, dir_input = "", dir_output = "")
{
  #TODO: decide what R data structure use for aggregates
  aggrList <- scalaCompiler$do('List')$'apply[Array[String]]'(c("nuovoP","SUM","pvalue"),
                                                              c("aaaa","COUNT"))

  dir_in = "/Users/simone/Downloads/job_filename_guest_new14_20170316_162715_DATA_SET_VAR/files/"
  dir_out = "/Users/simone/Downloads/res/"
  frappeR$extend(aggrList,dir_in,dir_out)
}

group <-function()
{

}

merge <- function(groupBy = NULL)
{
  if(!is.character(groupBy) && !is.null(groupBy))
    stop("groupBy can be only null, single string or an array of string")

  frappeR$merge(groupBy,dir_in,dir_out)
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

cover <- function(flag,minAcc,maxAcc,groupBy = NULL,aggregates = NULL, input)
{
  fl <- check(coverFlag,flag)

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

  frappeR$cover(fl,min,max,groupBy,aggrList,input)
}

map <- function()
{

}

join <- function()
{

}

