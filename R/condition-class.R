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
  if(!is.character(value) || length(value)>1)
    stop("value: no valid input or length > 1")
}


#' CONDITION object class constructor
#'
#' This class constructor is used to create instances of CONDITION object
#' to be used in GMQL functions that require evaluation on value
#' This constructor define a DEF (DEFAULT) evaluation of the input value.
#' DEF evaluation: two attributes match if both end with value. 
#'
#' @param value single string identifying name of metadata and/or region attribute
#' to be evaluated
#'
#' @return DEF condition object
#'
#' @seealso \code{\link{FULL}} \code{\link{EXACT}}
#' 
#' @examples
#' 
#' initGMQL("gtf")
#' test_path <- system.file("example","DATA_SET_VAR_GTF",package = "GMQL")
#' r = readDataset(test_path)
#'
#' #### select with DEF condition
#' s = select(input_data = r, semi_join = c("cell_type","cell"),
#' semi_join_dataset = c)
#' 
#' \dontrun{
#' 
#' #### select with DEF condition declared explicitily
#' s = select(input_data = r, semi_join = c(DEF("attribute_tag"),"cell_type","cell"),
#' semi_join_dataset = c)
#'
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

#' CONDITION object class constructor
#'
#' This class constructor is used to create instances of CONDITION object
#' to be used in GMQL functions that require evaluation on value
#' This constructor define a EXACT evaluation of the input value.
#' EXACT evaluation: only attributes exactly as value will match; 
#' no further prefixes are allowed. 
#'
#' @param value single string identifying name of metadata and/or region attribute
#' to be evaluated
#'
#' @return EXACT condition object
#' 
#' @seealso \code{\link{DEF}} \code{\link{FULL}}
#' 
#' @examples
#' 
#' initGMQL("gtf")
#' test_path <- system.file("example","DATA_SET_VAR_GTF",package = "GMQL")
#' r = readDataset(test_path)
#' 
#' #### select with condition
#' #### the first and the third attribute are DEF the second one is EXACT
#' s = select(input_data = r, semi_join = list("cell_type",EXACT("cell"),"attribute_tag"), semi_join_dataset = r)
#'
#' \dontrun{
#'
#' #### select with DEF condition
#' #### the EXACT condition is treated as DEF due to coercion
#' s = select(input_data = r, semi_join = c("cell_type","cell",EXACT("attribute_tag")),
#' semi_join_dataset = c)
#'
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

#' CONDITION object class constructor
#'
#' This class constructor is used to create instances of CONDITION object
#' to be used in GMQL functions that require evaluation on value
#' This constructor define a FULL (FULLNAME) evaluation of the input value.
#' FULL evaluation: two attributes match if they both end with value and,
#' if they have further prefixes, the two prefix sequences are identical
#'
#' @param value single string identifying name of metadata and/or region attribute
#' to be evaluated
#' 
#' @return FULL condition object
#' 
#' @seealso \code{\link{DEF}} \code{\link{EXACT}}
#' 
#' @examples
#' 
#' initGMQL("gtf")
#' test_path <- system.file("example","DATA_SET_VAR_GTF",package = "GMQL")
#' r = readDataset(test_path)
#' 
#' #### select with condition
#' #### the first and the third attribute are DEF the second one is FULL
#' s = select(input_data = r, semi_join = list("cell_type",FULL("cell"),"attribute_tag"), semi_join_dataset = c)
#'
#' \dontrun{
#'
#' #### select with DEF condition
#' #### the FULL condition is treated as DEF due to coercion
#' s = select(input_data = r, semi_join = c("cell_type","cell",FULL("attribute_tag")),
#' semi_join_dataset = c)
#'
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

