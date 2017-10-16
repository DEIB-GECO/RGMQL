#############################
#       OPERATOR            #
#############################

OPERATOR <- function(value)
{
  op_list <- list(
    value = value
  )
  ## Set the name for the class
  class(op_list) <- "OPERATOR"
  return(op_list)
}

check.META_OPERATOR <- function(value)
{
  if(is.character(value) && length(value)>1)
    stop("value: no multiple string")

  if(!is.character(value))
    stop("value: is not a string")
}

META_OPERATOR <- function(value)
{
  op_list <- list(
    value = value
  )
  ## Set the name for the class
  class(op_list) <- "META_OPERATOR"
  return(op_list)
}

print.META_OPERATOR <- function(obj) {
  res <- as.character(obj)
  cat(res)
}

as.character.META_OPERATOR <- function(obj) {
  class <- class(obj)[1]
  val <- obj$value
  c(class,val)
}

take_value.META_OPERATOR <- function(obj){
  class <- class(obj)[1]
  val <- obj$value
  text <- switch(class,
                 "SUM" = paste0("sum_",val),
                 "MIN" = paste0("min_",val),
                 "MAX" = paste0("max_",val),
                 "COUNT" = paste0("count"),
                 "BAG" = paste0("bag_",val),
                 "AVG" = paste0("avg_",val),
                 "STD" = paste0("std_"),
                 "MEDIAN" = paste0("median_",val),
                 "Q1" = paste0("q1_",val),
                 "Q2" = paste0("q2_"),
                 "Q3" = paste0("q3_",val)
  )
  text
}

#' OPERATOR object class constructor
#' 
#' This class constructor is used to create instances of OPERATOR object
#' to be used in GMQL functions that require operator on value,
#' it prepared input parameter to be passed to library function sum,
#' performing all the type conversion needed
#' 
#' @param value single string identifying name of region attribute
#'
#' @return SUM Operator object
#' 
#' @seealso \code{\link{MIN}} \code{\link{MAX}} \code{\link{AVG}} \code{\link{COUNT}}
#' \code{\link{BAG}} \code{\link{STD}} \code{\link{MEDIAN}} \code{\link{Q1}} \code{\link{Q2}}
#' \code{\link{Q3}}
#' 
#' @examples
#' 
#' ### local with CustomParser
#' initGMQL("gtf")
#' test_path <- system.file("example","DATA_SET_VAR_GTF",package = "RGMQL")
#' exp = readDataset(test_path)
#' 
#' ## This statement copies all samples of exp into res dataset, and then calculates new 
#' ## metadata attributes for each of them: sum_score is the sum of score of the sample regions.
#' res = extend(input_data = exp, list(sum_score = SUM("score")))
#' 
#' @export
#'
SUM <- function(value)
{
  check.META_OPERATOR(value)

  list <- list(
    value = value
  )
  ## Set the name for the class
  class(list) <- c("SUM","OPERATOR","META_OPERATOR")
  return(list)
}

#' OPERATOR object class constructor
#'
#' This class constructor is used to create instances of OPERATOR object
#' to be used in GMQL functions that require operator on value, 
#' it prepared input parameter to be passed to library function minimum,
#' performing all the type conversion needed
#' 
#' @param value single string identifying name of region attribute
#'
#' @return MIN Operator object
#' 
#' @seealso \code{\link{SUM}} \code{\link{MAX}} \code{\link{AVG}} \code{\link{COUNT}}
#' \code{\link{BAG}} \code{\link{STD}} \code{\link{MEDIAN}} \code{\link{Q1}} \code{\link{Q2}}
#' \code{\link{Q3}}
#' 
#' @examples
#' 
#' ### local with CustomParser
#' initGMQL("gtf")
#' test_path <- system.file("example","DATA_SET_VAR_GTF",package = "RGMQL")
#' exp = readDataset(test_path)
#' 
#' ## This statement copies all samples of exp into res dataset, and then calculates new 
#' ## metadata attributes for each of them: MinP is the minimum pvalue of the sample regions.
#' res = extend(input_data = exp, list(minP = MIN("pvalue")))
#' 
#' @export
#'
MIN <- function(value)
{
  check.META_OPERATOR(value)

  list <- list(
    value = value
  )
  ## Set the name for the class
  class(list) <- c("MIN","OPERATOR","META_OPERATOR")
  return(list)
}

#' OPERATOR object class constructor
#'
#' This class constructor is used to create instances of OPERATOR object
#' to be used in GMQL functions that require operator on value,
#' it prepared input parameter to be passed to library function maximum,
#' performing all the type conversion needed
#' 
#' @param value single string identifying name of region attribute
#'
#' @return MAX Operator object
#' 
#' @seealso \code{\link{MIN}} \code{\link{SUM}} \code{\link{AVG}} \code{\link{COUNT}}
#' \code{\link{BAG}} \code{\link{STD}} \code{\link{MEDIAN}} \code{\link{Q1}} \code{\link{Q2}}
#' \code{\link{Q3}}
#' 
#' @examples
#' 
#' ### local with CustomParser
#' initGMQL("gtf")
#' test_path <- system.file("example","DATA_SET_VAR_GTF",package = "RGMQL")
#' exp = readDataset(test_path)
#' 
#' ## This statement copies all samples of exp into res dataset, and then calculates new 
#' ## metadata attributes for each of them: max_score is the maximum score of the sample regions.
#' res = extend(input_data = exp, list(max_score = MAX("score")))
#' 
#' 
#' @export
#'
MAX <- function(value)
{
  check.META_OPERATOR(value)

  list <- list(
    value = value
  )
  ## Set the name for the class
  class(list) <- c("MAX","OPERATOR","META_OPERATOR")
  return(list)
}

#' OPERATOR object class constructor
#'
#' This class constructor is used to create instances of OPERATOR object
#' to be used in GMQL functions that require operator on value,
#' it prepared input parameter to be passed to library function arithmetic mean,
#' performing all the type conversion needed
#' 
#' @param value single string identifying name of metadata attribute
#'
#' @return AVG Operator object
#' 
#' @seealso \code{\link{MIN}} \code{\link{MAX}} \code{\link{SUM}} \code{\link{COUNT}}
#' \code{\link{BAG}} \code{\link{STD}} \code{\link{MEDIAN}} \code{\link{Q1}} \code{\link{Q2}}
#' \code{\link{Q3}}
#' 
#' @examples
#' 
#' ### local with CustomParser
#' initGMQL("gtf")
#' test_path <- system.file("example","DATA_SET_VAR_GTF",package = "RGMQL")
#' exp = readDataset(test_path)
#' 
#' ## The following cover operation produces output regions where at least 2 and at most 3 regions of
#' ## exp overlap, having as resulting region attributes the avg signal of the overlapping regions;
#' ## the result has one sample for each input cell.
#' res = cover(input_data = exp,2,3, c("cell"), list(avg_signal = AVG("signal")))
#'
#' @export
#'
AVG <- function(value)
{
  check.META_OPERATOR(value)

  list <- list(
    value = value
  )
  ## Set the name for the class
  class(list) <- c("AVG","OPERATOR","META_OPERATOR")
  return(list)
}

#' OPERATOR object class constructor
#'
#' This class constructor is used to create instances of OPERATOR object
#' to be used in GMQL functions that require operator on value,
#' it prepared input parameter to be passed to library function bag,
#' this function creates comma-separated strings of distinct attribute values
#' performing all the type conversion needed
#' 
#' @param value single string identifying name of region attribute
#'
#' @return BAG Operator object
#' 
#' @seealso \code{\link{MIN}} \code{\link{MAX}} \code{\link{AVG}} \code{\link{COUNT}}
#' \code{\link{SUM}} \code{\link{STD}} \code{\link{MEDIAN}} \code{\link{Q1}} \code{\link{Q2}}
#' \code{\link{Q3}}
#' 
#' @examples
#' 
#' ### local with CustomParser
#' initGMQL("gtf")
#' test_path <- system.file("example","DATA_SET_VAR_GTF",package = "RGMQL")
#' data = readDataset(test_path)
#' 
#' ## copies all samples of DATA into OUT dataset, and then for each of them adds another 
#' ## metadata attribute,  allScores, which is the aggregation comma-separated list of all the 
#' ## distinct values that the attribute  score takes in the sample.
#' out = extend(input_data = data, list(allScore = BAG("score")))
#'
#' @export
#'
BAG <- function(value)
{
  check.META_OPERATOR(value)

  list <- list(
    value = value
  )
  ## Set the name for the class
  class(list) <- c("BAG","OPERATOR","META_OPERATOR")
  return(list)
}

#' OPERATOR object class constructor
#'
#' This class constructor is used to create instances of OPERATOR object
#' to be used in GMQL functions that require operator,
#' it prepared input parameter to be passed to library function count,
#' performing all the type conversion needed
#'
#' @return COUNT Operator object
#' 
#' @seealso \code{\link{MIN}} \code{\link{MAX}} \code{\link{AVG}} \code{\link{SUM}}
#' \code{\link{BAG}} \code{\link{STD}} \code{\link{MEDIAN}} \code{\link{Q1}} \code{\link{Q2}}
#' \code{\link{Q3}}
#' 
#' @examples
#' 
#' ## local with CustomParser
#' initGMQL("gtf")
#' test_path <- system.file("example","DATA_SET_VAR_GTF",package = "RGMQL")
#' exp = readDataset(test_path)
#' 
#' ## counts the regions in each sample and stores their number as value of the new metadata 
#' ## RegionCount attribute of the sample.
#' out = extend(input_data = exp, list(RegionCount = COUNT()))
#'
#' @export
#'
COUNT <- function()
{
  list <- list()
  ## Set the name for the class
  class(list) <- c("COUNT","OPERATOR","META_OPERATOR")
  return(list)
}
as.character.COUNT <- function(obj) {
  class <- class(obj)[1]
  c(class,"")
}
check.COUNT <- function(obj){}

#' OPERATOR object class constructor
#'
#' This class constructor is used to create instances of OPERATOR object
#' to be used in GMQL functions that require operator on value,
#' it prepared input parameter to be passed to library function standard deviation,
#' performing all the type conversion needed
#' 
#' @param value single string identifying name of region attribute
#'
#' @return STD Operator object
#' 
#' @seealso \code{\link{MIN}} \code{\link{MAX}} \code{\link{AVG}} \code{\link{COUNT}}
#' \code{\link{BAG}} \code{\link{SUM}} \code{\link{MEDIAN}} \code{\link{Q1}} \code{\link{Q2}}
#' \code{\link{Q3}}
#' 
#' @examples
#' 
#' ### local with CustomParser
#' initGMQL("gtf")
#' test_path <- system.file("example","DATA_SET_VAR_GTF",package = "RGMQL")
#' exp = readDataset(test_path)
#' 
#' ## This statement copies all samples of exp into res dataset, and then calculates new 
#' ## metadata attributes for each of them: std_score is the standard deviation score of the sample regions.
#' res = extend(input_data = exp, list(std_score = STD("score")))
#'
#' @export
#'
STD <- function(value)
{
  check.META_OPERATOR(value)

  list <- list(
    value = value
  )
  ## Set the name for the class
  class(list) <- c("STD","META_OPERATOR")
  return(list)
}

#' OPERATOR object class constructor
#'
#' This class constructor is used to create instances of OPERATOR object
#' to be used in GMQL functions that require operator on value,
#' it prepared input parameter to be passed to library function median,
#' performing all the type conversion needed
#' 
#' @param value single string identifying name of region attribute
#'
#' @return MEDIAN Operator object
#' 
#' @seealso \code{\link{MIN}} \code{\link{MAX}} \code{\link{AVG}} \code{\link{COUNT}}
#' \code{\link{BAG}} \code{\link{STD}} \code{\link{SUM}} \code{\link{Q1}} \code{\link{Q2}}
#' \code{\link{Q3}}
#' 
#' @examples
#' 
#' ### local with CustomParser
#' initGMQL("gtf")
#' test_path <- system.file("example","DATA_SET_VAR_GTF",package = "RGMQL")
#' exp = readDataset(test_path)
#' 
#' ## This statement copies all samples of exp into res dataset, and then calculates new 
#' ## metadata attributes for each of them: m_score is the median score of the sample regions.
#' res = extend(input_data = exp, list(m_score = MEDIAN("score")))
#'
#' @export
#'
MEDIAN <- function(value)
{
  check.META_OPERATOR(value)

  list <- list(
    value = value
  )
  ## Set the name for the class
  class(list) <- c("MEDIAN","OPERATOR","META_OPERATOR")
  return(list)
}

#' OPERATOR object class constructor
#'
#' This class constructor is used to create instances of OPERATOR object
#' to be used in GMQL functions that require operator on value,
#' it prepared input parameter to be passed to library function first quartile,
#' performing all the type conversion needed
#' 
#' @param value single string identifying name of metadata attribute
#'
#' @return Q1 Operator object
#' 
#' @seealso \code{\link{MIN}} \code{\link{MAX}} \code{\link{AVG}} \code{\link{COUNT}}
#' \code{\link{BAG}} \code{\link{STD}} \code{\link{MEDIAN}} \code{\link{SUM}} \code{\link{Q2}}
#' \code{\link{Q3}}
#' 
#' @examples
#' 
#' ### local with CustomParser
#' initGMQL("gtf")
#' test_path <- system.file("example","DATA_SET_VAR_GTF",package = "RGMQL")
#' exp = readDataset(test_path)
#' 
#' ## This statement copies all samples of exp into res dataset, and then calculates new 
#' ## metadata attributes for each of them: q1_score is the first quartile of score of the sample regions.
#' res = extend(input_data = exp, list(q1_score = Q1("score")))
#'
#'
#' @export
#'
Q1 <- function(value)
{
  check.META_OPERATOR(value)

  list <- list(
    value = value
  )
  ## Set the name for the class
  class(list) <- c("Q1","META_OPERATOR")
  return(list)
}

#' OPERATOR object class constructor
#'
#' This class constructor is used to create instances of OPERATOR object
#' to be used in GMQL functions that require operator on value,
#' it prepared input parameter to be passed to library function second quartile,
#' performing all the type conversion needed
#' 
#' @param value single string identifying name of region attribute
#'
#' @return Q2 Operator object
#' 
#' @seealso \code{\link{MIN}} \code{\link{MAX}} \code{\link{AVG}} \code{\link{COUNT}}
#' \code{\link{BAG}} \code{\link{STD}} \code{\link{MEDIAN}} \code{\link{Q1}} \code{\link{SUM}}
#' \code{\link{Q3}}
#' 
#' @examples
#' 
#' ### local with CustomParser
#' initGMQL("gtf")
#' test_path <- system.file("example","DATA_SET_VAR_GTF",package = "RGMQL")
#' exp = readDataset(test_path)
#' 
#' ## This statement copies all samples of exp into res dataset, and then calculates new 
#' ## metadata attributes for each of them: q2_score is the second quartile of score of the sample regions.
#' res = extend(input_data = exp, list(q2_score = Q2("score")))
#'
#' @export
#'
Q2 <- function(value)
{
  check.META_OPERATOR(value)

  list <- list(
    value = value
  )
  ## Set the name for the class
  class(list) <- c("Q2","META_OPERATOR")
  return(list)
}

#' OPERATOR object class constructor
#'
#' This class constructor is used to create instances of OPERATOR object
#' to be used in GMQL functions that require operator on value,
#' it prepared input parameter to be passed to library function third quartile,
#' performing all the type conversion needed
#' 
#' @param value single string identifying name of region attribute
#'
#' @return Q3 Operator object
#'
#' @seealso \code{\link{MIN}} \code{\link{MAX}} \code{\link{AVG}} \code{\link{COUNT}}
#' \code{\link{BAG}} \code{\link{STD}} \code{\link{MEDIAN}} \code{\link{Q1}} \code{\link{SUM}}
#' \code{\link{SUM}}
#'
#' @examples
#' 
#' ### local with CustomParser
#' initGMQL("gtf")
#' test_path <- system.file("example","DATA_SET_VAR_GTF",package = "RGMQL")
#' exp = readDataset(test_path)
#' 
#' ## This statement copies all samples of exp into res dataset, and then calculates new 
#' ## metadata attributes for each of them: q3_score is the third quartile of score of the sample regions.
#' res = extend(input_data = exp, list(q3_score = Q3("score")))
#' 
#' @export
#'
Q3 <- function(value)
{
  check.META_OPERATOR(value)

  list <- list(
    value = value
  )
  ## Set the name for the class
  class(list) <- c("Q3","META_OPERATOR")
  return(list)
}

