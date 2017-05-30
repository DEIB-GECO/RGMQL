#############################
#       DISTAL          #
############################


#' DISTAL object class
#'
#'
#' DISTAL object available are:
#' \itemize{
#' \item{UP}
#' \item{DOWN}
#' \item{DGE}
#' \item{DLE}
#' \item{MD}
#' }
#' @details
#' you never use a parent class DISTAL()
#'
#'
#'


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
  c(class,val)
}

check.DISTAL <- function(value)
{
  if(!is.numeric(value))
    stop("value: is not a numeric")

  if(is.numeric(value) && length(value)>1)
    stop("value: no multiple string")

}

DLE <- function(value)
{
  check.DISTAL(value)

  list <- list(
    value = as.integer(value)
  )
  ## Set the name for the class
  class(list) <- c("DLE","DISTAL")
  return(list)
}

DGE <- function(value)
{
  check.DISTAL(value)

  list <- list(
    value = as.integer(value)
  )
  ## Set the name for the class
  class(list) <- c("DGE","DISTAL")
  return(list)
}

MD <- function(value)
{
  check.DISTAL(value)

  list <- list(
    value = as.integer(value)
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
