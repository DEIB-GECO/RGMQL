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


#' ORDER object class constructor
#'
#' This class constructor is used to create instances of ORDER object
#' to be used in GMQL functions that require ordering on value.
#' It define a descending order for input value
#' 
#' @param value single string identifying name of metadata and/or region attribute
#'
#' @return DESC ordering object
#' 
#' @examples
#' 
#' ## it orders the samples according to the Region_count metadata attribute and takes the two samples 
#' ## that have the highest count. 
#'
#' initGMQL()
#' test_path <- system.file("example","DATA_SET_VAR_GTF",package = "RGMQL")
#' r = readDataset(test_path)
#' o = order(r,list(DESC("Region_Count")), mtop = 2)
#' 
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

#' ORDER object class constructor
#'
#' This class constructor is used to create instances of ORDER object
#' to be used in GMQL functions that require ordering on value.
#' It define an ascending order for input value
#' 
#' @param value single string identifying name of metadata and/or region attribute
#'
#' @return ASC ordering object
#'
#' @examples
#' 
#' ## it get the first 5 samples on the basis of their region counter, 
#' ## those with the smaller RegionCount and then for each of them, 7 regions on the basis of 
#' ## their score, those with the higher score
#' 
#' initGMQL("gtf")
#' test_path <- system.file("example","DATA_SET_VAR_GTF",package = "RGMQL")
#' r = readDataset(test_path)
#' o = order(r,list(ASC("Region_Count")), mtop = 5,regions_ordering = list(DESC("score")),rtop=7)
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
