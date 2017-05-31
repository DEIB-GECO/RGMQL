#############################
#       CONDITION          #
############################


#' CONDITION object class
#'
#'
#' CONDITION object available are:
#' \itemize{
#' \item{DEF: DEFAULT}
#' \item{EXACT: EXACT}
#' \item{FULL: FULLNAME}
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

as.character.CONDITION <- function(obj) {
  class <- class(obj)[1]
  val <- obj$value
  c(class,val)
}

print.CONDITION <- function(obj){
  print(as.character.CONDITION(obj))
}

c.CONDITION <- function(...)
{
  a <- list(...)
}

check.CONDITION <- function(value)
{
  if(is.character(value) && length(value)>1)
    stop("value: no multiple string")

  if(!is.character(value))
    stop("value: is not a string")
}

DEF <- function(value)
{
  check.CONDITION(value)

  list <- list(
    value = value
  )
  ## Set the name for the class
  class(list) <- c("DEF","CONDITION")
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

FULL <- function(value)
{
  check.CONDITION(value)

  list <- list(
    value = value
  )
  ## Set the name for the class
  class(list) <- c("FULL","CONDITION")
  return(list)
}

