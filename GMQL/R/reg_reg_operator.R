OPERATOR <- function(value)
{
  op_list <- list(
    value = value
  )
  ## Set the name for the class
  class(op_list) <- "OPERATOR"
  return(op_list)
}

check.OPERATOR <- function(obj)
{
  if(!is.character(obj$value))
    stop("parameter must be a single string")

  if(length(obj$value)>2 && class(obj)[1] != "COUNT")
    stop("only one parameter")
}

print.OPERATOR <- function(obj) {}


as.character.OPERATOR <- function(obj) {
  class <- class(obj)[1]
  val <- obj$value
  c(class,val)
}
take_value.OPERATOR <- function(obj){
  class <- class(obj)[1]
  val <- obj$value
  text <- switch(class,
                 "SUM" = paste0("sum_",val),
                 "MIN" = paste0("min_",val),
                 "MAX" = paste0("max_",val),
                 "COUNT" = paste0("count_"),
                 "BAG" = paste0("bag_",val),
                 "AVG" = paste0("avg_",val)
  )
  text
}



SUM <- function(value)
{
  list <- list(
    value = value
  )
  ## Set the name for the class
  class(list) <- c("SUM","OPERATOR")
  return(list)
}

MIN <- function(value)
{
  list <- list(
    value = value
  )
  ## Set the name for the class
  class(list) <- c("MIN","OPERATOR")
  return(list)
}

MAX <- function(value)
{
  list <- list(
    value = value
  )
  ## Set the name for the class
  class(list) <- c("MAX","OPERATOR")
  return(list)
}

AVG <- function(value)
{
  list <- list(
    value = value
  )
  ## Set the name for the class
  class(list) <- c("AVG","OPERATOR")
  return(list)
}

BAG <- function(value)
{
  list <- list(
    value = value
  )
  ## Set the name for the class
  class(list) <- c("BAG","OPERATOR")
  return(list)
}

COUNT <- function()
{
  list <- list()
  ## Set the name for the class
  class(list) <- c("COUNT","OPERATOR")
  return(list)
}
as.character.COUNT <- function(obj) {
  class <- class(obj)[1]
  c(class,"")
}
check.COUNT <- function(obj){}

