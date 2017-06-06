############################
#       ORDER              #
############################


ORDER <- function(value)
{
  op_list <- list(
    value = value
  )
  ## Set the name for the class
  class(op_list) <- "ORDER"
  return(op_list)
}

check.ORDER <- function(value)
{
  if(is.character(value) && length(value)>1)
    stop("value: no multiple string")

  if(!is.character(value))
    stop("value: is not a string")
}

print.ORDER <- function(obj) {
  as.character(obj)
}


c.ORDER <- function(...)
{
  a <- list(...)
}

as.character.ORDER <- function(obj) {
  class <- class(obj)[1]
  val <- obj$value
  c(class,val)
}



#' ORDER object class
#'
#' This class is used to create an ordering object for input to GMQL function
#'
#' ORDER object available are:
#' \itemize{
#' \item{ASC}
#' \item{DESC}
#' }
#'
#' @param value single string identifying name of metadata attribute
#'
#' @return DESC ordering object
#' @export
#'
DESC <- function(value)
{
  check.ORDER(value)

  list <- list(
    value = value
  )
  ## Set the name for the class
  class(list) <- c("DESC","ORDER")
  return(list)
}

#' ORDER object class
#'
#' This class is used to create an ordering object for input to GMQL function
#'
#' ORDER object available are:
#' \itemize{
#' \item{ASC: ascending ordering}
#' \item{DESC: descending ordering}
#' }
#'
#' @param value single string identifying name of metadata attribute
#'
#' @return ASC ordering object
#'
#'
#' @export
#'
ASC <- function(value)
{
  check.ORDER(value)

  list <- list(
    value = value
  )
  ## Set the name for the class
  class(list) <- c("ASC","ORDER")
  return(list)
}
