CONDITION <- function(value)
{
  op_list <- list(
    value = value
  )
  ## Set the name for the class
  class(op_list) <- "CONDITION"
  return(op_list)
}


print.CONDITION <- function(obj) {}

as.character.CONDITION <- function(obj) {
  class <- class(obj)[1]
  val <- obj$value
  c(class,val)
}


DEFAULT <- function(value)
{
  if(!is.character(value) || length(value)>=2)
    stop("parameter must be a one string")

  list <- list(
    value = value
  )
  ## Set the name for the class
  class(list) <- c("DEFAULT","CONDITION")
  return(list)
}

EXACT <- function(value)
{
  if(!is.character(value) || length(value)>=2)
    stop("parameter must be one string")

  list <- list(
    value = value
  )
  ## Set the name for the class
  class(list) <- c("EXACT","CONDITION")
  return(list)
}

FULLNAME <- function(value)
{
  if(!is.character(value) || length(value)>=2)
    stop("parameter must be one string")

  list <- list(
    value = value
  )
  ## Set the name for the class
  class(list) <- c("FULLNAME","CONDITION")
  return(list)
}

