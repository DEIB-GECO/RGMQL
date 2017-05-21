ATOMIC_CONDITION <- function(value)
{
  op_list <- list(
    value = value
  )
  ## Set the name for the class
  class(op_list) <- "ATOMIC_CONDITION"
  return(op_list)
}

print.ATOMIC_CONDITION <- function(obj) {}

as.character.ATOMIC_CONDITION <- function(obj) {
  class <- class(obj)[1]
  val <- obj$value
  c(val,class)
}

DISTLESS <- function(value)
{

  list <- list(
    value = value
  )
  ## Set the name for the class
  class(list) <- c("DISTLESS","ATOMIC_CONDITION")
  return(list)
}

DISTGREAT <- function(value)
{
  list <- list(
    value = value
  )
  ## Set the name for the class
  class(list) <- c("DISTGREAT","ATOMIC_CONDITION")
  return(list)
}

MINDIST <- function(value)
{
  list <- list(
    value = value
  )
  ## Set the name for the class
  class(list) <- c("MINDIST","ATOMIC_CONDITION")
  return(list)
}

UPSTREAM <- function()
{
  list <- list(
    value = value
  )
  ## Set the name for the class
  class(list) <- c("UPSTREAM","ATOMIC_CONDITION")
  return(list)
}

DOWNSTREAM <- function()
{
  list <- list(
    value = value
  )
  ## Set the name for the class
  class(list) <- c("DOWNSTREAM","ATOMIC_CONDITION")
  return(list)
}
