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
  if(!is.character(value))
    stop("value: no valid input")

  if(length(value)>1)
    stop("value: no multiple string")
}


#' CONDITION object class
#'
#' This class is used to create instances of condition object
#' to be used in GMQL functions that require evaluation condition on value
#'
#' CONDITION object available are:
#' \itemize{
#' \item{FULL: Fullname evaluation, two attributes match if they both end with value and,
#' if they have a further prefixes, the two prefix sequence are identical}
#' \item{DEF: Default evaluation, two attributes match if both end with value. }
#' \item{EXACT: Exact evaluation, only attributes exactly as value will match; no further prefixes are allowed. }
#' }
#'
#' @param value single string identifying name of metadata attribute you
#' want to put in evaluation
#'
#' @return DEF condition object
#'
#' @examples
#' \dontrun{
#'
#' #### select with DEF condition
#' #### the full condition is treated as DEF due to coercion
#' s = select(input_data = r, semi_join = c("cell_type","cell"),
#' semi_join_dataset = c)
#'
#' #### select with condition
#' #### the first is FULL and the other ones are DEF
#' s = select(input_data = r, semi_join = c(DEF("attribute_tag"),"cell_type","cell"),
#' semi_join_dataset = c)
#'
#' #### select with condition
#' s = select(input_data = r, semi_join = list("cell_type",DEF("cell")), semi_join_dataset = c)
#'
#' }
#' ""
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
#' This class is used to create instances of condition object
#' to be used in GMQL functions that require evaluation condition on value
#'
#' CONDITION object available are:
#' \itemize{
#' \item{FULL: Fullname evaluation, two attributes match if they both end with value and,
#' if they have a further prefixes, the two prefix sequence are identical}
#' \item{DEF: Default evaluation, two attributes match if both end with value. }
#' \item{EXACT: Exact evaluation, only attributes exactly as value will match; no further prefixes are allowed. }
#' }
#'
#' @param value single string identifying name of metadata attribute you
#' want to put in evaluation
#'
#' @return EXACT condition object
#'
#' @examples
#' \dontrun{
#'
#' #### select with DEF condition
#' #### the full condition is treated as DEF due to coercion
#' s = select(input_data = r, semi_join = c("cell_type","cell",EXACT("attribute_tag")),
#' semi_join_dataset = c)
#'
#' #### select with condition
#' #### the first is FULL and the other ones are DEF
#' s = select(input_data = r, semi_join = c(EXACT("attribute_tag"),"cell_type","cell"),
#' semi_join_dataset = c)
#'
#' #### select with condition
#' s = select(input_data = r, semi_join = list("cell_type",EXACT("cell")), semi_join_dataset = c)
#'
#' }
#' ""
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
#' This class is used to create instances of condition object
#' to be used in GMQL functions that require evaluation condition on value
#'
#' CONDITION object available are:
#' \itemize{
#' \item{FULL: Fullname evaluation, two attributes match if they both end with value and,
#' if they have a further prefixes, the two prefix sequence are identical}
#' \item{DEF: Default evaluation, two attributes match if both end with value. }
#' \item{EXACT: Exact evaluation, only attributes exactly as value will match; no further prefixes are allowed. }
#' }
#'
#' @param value single string identifying name of metadata attribute you
#' want to put in evaluation
#'
#' @return FULL condition object
#'
#' @examples
#' \dontrun{
#'
#' #### select with DEF condition
#' #### the full condition is treated as DEF due to coercion
#' s = select(input_data = r, semi_join = c("cell_type","cell",FULL("attribute_tag")),
#' semi_join_dataset = c)
#'
#' #### select with condition
#' #### the first is FULL and the other ones are DEF
#' s = select(input_data = r, semi_join = c(FULL("attribute_tag"),"cell_type","cell"),
#' semi_join_dataset = c)
#'
#' }
#' ""
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

