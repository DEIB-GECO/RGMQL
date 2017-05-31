############################
#       ORDER              #
############################

#' ORDER object class
#'
#'
#' ORDER object available are:
#' \itemize{
#' \item{ASC}
#' \item{DESC}
#' }
#'
#' @details
#' you never use a parent class ORDER()
#'
#'

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
