BUILDER <- function(value)
{
  op_list <- list(
    value = value
  )
  ## Set the name for the class
  class(op_list) <- "BUILDER"
  return(op_list)
}


print.BUILDER <- function(obj) {}

as.character.BUILDER <- function(obj) {
  class <- class(obj)[1]
  val <- obj$value
  c(val,class)
}


LEFT <- function(value)
{

  list <- list(
    value = value
  )
  ## Set the name for the class
  class(list) <- c("LEFT","BUILDER")
  return(list)
}

RIGHT <- function(value)
{
  list <- list(
    value = value
  )
  ## Set the name for the class
  class(list) <- c("RIGHT","BUILDER")
  return(list)
}

CONTIG <- function(value)
{
  list <- list(
    value = value
  )
  ## Set the name for the class
  class(list) <- c("CONTIG","BUILDER")
  return(list)
}

INTERSECTION <- function(value)
{
  list <- list(
    value = value
  )
  ## Set the name for the class
  class(list) <- c("INTERSECTION","BUILDER")
  return(list)
}
