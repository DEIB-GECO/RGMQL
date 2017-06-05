#############################
#       CONDITION          #
############################


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


#' CONDITION object class
#'
#'
#' CONDITION object available are:
#' \itemize{
#' \item{FULL: Fullname evaluation, two attributes match if they both end with value and,
#' if they have a further prefixes, the two prefix sequence are identical}
#' \item{DEF: Default evaluation, two attributes match if both end with value. }
#' \item{EXACT: Exact evaluation, only attributes exactly as value will match; no further prefixes are allowed. }
#' }
#'
#' @export
#'
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

#' CONDITION object class
#'
#'
#' CONDITION object available are:
#' \itemize{
#' \item{FULL: Fullname evaluation, two attributes match if they both end with value and,
#' if they have a further prefixes, the two prefix sequence are identical}
#' \item{DEF: Default evaluation, two attributes match if both end with value. }
#' \item{EXACT: Exact evaluation, only attributes exactly as value will match; no further prefixes are allowed. }
#' }
#'
#' @export
#'
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

#' CONDITION object class
#'
#'
#' CONDITION object available are:
#' \itemize{
#' \item{FULL: Fullname evaluation, two attributes match if they both end with value and,
#' if they have a further prefixes, the two prefix sequence are identical}
#' \item{DEF: Default evaluation, two attributes match if both end with value. }
#' \item{EXACT: Exact evaluation, only attributes exactly as value will match; no further prefixes are allowed. }
#' }
#'
#' @export
#'
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

