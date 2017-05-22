DISTAL <- function(value)
{
  op_list <- list(
    value = value
  )
  ## Set the name for the class
  class(op_list) <- "DISTAL"
  return(op_list)
}

print.DISTAL <- function(obj) {}

as.character.DISTAL <- function(obj) {
  class <- class(obj)[1]
  val <- obj$value
  c(val,class)
}

DLE <- function(value)
{

  list <- list(
    value = value
  )
  ## Set the name for the class
  class(list) <- c("DLE","DISTAL")
  return(list)
}

DGE <- function(value)
{
  list <- list(
    value = value
  )
  ## Set the name for the class
  class(list) <- c("DGE","DISTAL")
  return(list)
}

MD <- function(value)
{
  list <- list(
    value = value
  )
  ## Set the name for the class
  class(list) <- c("MD","DISTAL")
  return(list)
}

UP <- function()
{
  list <- list()
  ## Set the name for the class
  class(list) <- c("UP","DISTAL")
  return(list)
}
as.character.UP <- function(obj) {
  class <- class(obj)[1]
  c(class,"")
}
DOWN <- function()
{
  list <- list()
  ## Set the name for the class
  class(list) <- c("DOWN","DISTAL")
  return(list)
}
as.character.DOWN <- function(obj) {
  class <- class(obj)[1]
  c(class,"")
}
