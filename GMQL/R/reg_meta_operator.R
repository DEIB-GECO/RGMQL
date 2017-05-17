META_OPERATOR <- function(value)
{
  op_list <- list(
    value = value
  )
  ## Set the name for the class
  class(op_list) <- "META_OPERATOR"
  return(op_list)
}

check.META_OPERATOR <- function(obj)
{
  if(!is.character(obj$value))
    stop("parameter must be a single string")

  if(length(obj$value)>2 && class(obj)[1] != "COUNT")
    stop("only one parameter")
}

print.META_OPERATOR <- function(obj) {}


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
         "COUNT" = paste0("count_"),
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



SUM <- function(value)
{
  list <- list(
    value = value
  )
  ## Set the name for the class
  class(list) <- c("SUM","META_OPERATOR")
  return(list)
}

MIN <- function(value)
{
  list <- list(
    value = value
  )
  ## Set the name for the class
  class(list) <- c("MIN","META_OPERATOR")
  return(list)
}

MAX <- function(value)
{
  list <- list(
    value = value
  )
  ## Set the name for the class
  class(list) <- c("MAX","META_OPERATOR")
  return(list)
}

AVG <- function(value)
{
  list <- list(
    value = value
  )
  ## Set the name for the class
  class(list) <- c("AVG","META_OPERATOR")
  return(list)
}

BAG <- function(value)
{
  list <- list(
    value = value
  )
  ## Set the name for the class
  class(list) <- c("BAG","META_OPERATOR")
  return(list)
}

COUNT <- function()
{
  list <- list()
  ## Set the name for the class
  class(list) <- c("COUNT","META_OPERATOR")
  return(list)
}
as.character.COUNT <- function(obj) {
  class <- class(obj)[1]
  c(class,"")
}
check.COUNT <- function(obj){}


STD <- function(value)
{
  list <- list(
    value = value
  )
  ## Set the name for the class
  class(list) <- c("STD","META_OPERATOR")
  return(list)
}

MEDIAN <- function(value)
{
  list <- list(
    value = value
  )
  ## Set the name for the class
  class(list) <- c("MEDIAN","META_OPERATOR")
  return(list)
}

Q1 <- function(value)
{
  list <- list(
    value = value
  )
  ## Set the name for the class
  class(list) <- c("Q1","META_OPERATOR")
  return(list)
}

Q2 <- function(value)
{
  list <- list(
    value = value
  )
  ## Set the name for the class
  class(list) <- c("Q2","META_OPERATOR")
  return(list)
}

Q3 <- function(value)
{
  list <- list(
    value = value
  )
  ## Set the name for the class
  class(list) <- c("Q3","META_OPERATOR")
  return(list)
}

