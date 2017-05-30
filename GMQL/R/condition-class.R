#############################
#       CONDITION          #
############################


#' CONDITION object class
#'
#'
#' CONDITION object available are:
#' \itemize{
#' \item{DEFAULT}
#' \item{EXACT}
#' \item{FULLNAME}
#' }
#' @details
#' you never use a parent class CONDITION()
#'
#'
#'

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

check.CONDITION <- function(value)
{
  if(is.character(value) && length(value)>1)
    stop("value: no multiple string")

  if(!is.character(value))
    stop("value: is not a string")
}

DEFAULT <- function(value)
{
  check.CONDITION(value)

  list <- list(
    value = value
  )
  ## Set the name for the class
  class(list) <- c("DEFAULT","CONDITION")
  return(list)
}

EXACT <- function(value)
{
  check.CONDITION(value)

  list <- list(
    value = value
  )
  ## Set the name for the class
  class(list) <- c("EXACT","CONDITION")
  return(list)
}

FULLNAME <- function(value)
{
  check.CONDITION(value)

  list <- list(
    value = value
  )
  ## Set the name for the class
  class(list) <- c("FULLNAME","CONDITION")
  return(list)
}

