ORDER <- function(value)
{
  op_list <- list(
    value = value
  )
  ## Set the name for the class
  class(op_list) <- "ORDER"
  return(op_list)
}

check.ORDER <- function(obj)
{
  if(!is.character(obj$value))
    stop("parameter must be a single string")

  if(length(obj$value)>2 && class(obj)[1] != "COUNT")
    stop("only one parameter")
}

print.ORDER <- function(obj) {}


as.character.ORDER <- function(obj) {
  class <- class(obj)[1]
  val <- obj$value
  c(val,class)
}


DESC <- function(value)
{

  list <- list(
    value = value
  )
  ## Set the name for the class
  class(list) <- c("DESC","OPERATOR")
  return(list)
}

ASC <- function(value)
{
  list <- list(
    value = value
  )
  ## Set the name for the class
  class(list) <- c("ASC","OPERATOR")
  return(list)
}
