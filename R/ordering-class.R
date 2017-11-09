############################
#       ORDER              #
############################


ORDER <- function(value)
{
  op_list <- list(value = value)
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

c.ORDER <- function(...) {
  a <- list(...)
}

as.character.ORDER <- function(obj) {
  class <- class(obj)[1]
  val <- obj$value
  c(class,val)
}


#' ORDER object class constructor
#'
#' This class constructor is used to create instances of ORDER object,
#' to be used in GMQL functions that require ordering on value.
#' It defines a descending order for input value
#' 
#' @param value string identifying name of metadata or region attribute
#'
#' @return DESC ordering object
#' 
#' @examples
#' 
#' ## It orders the samples according to the Region_count metadata attribute 
#' ## and takes the two samples that have the highest count. 
#'
#' init_gmql()
#' test_path <- system.file("example", "DATASET", package = "RGMQL")
#' r = read_dataset(test_path)
#' o = order(r, list(DESC("Region_Count")), mtop = 2)
#' 
#' @export
#'
DESC <- function(value)
{
    check.ORDER(value)
    
    list <- list(value = value)
    ## Set the name for the class
    class(list) <- c("DESC","ORDER")
    return(list)
}

#' ORDER object class constructor
#'
#' This class constructor is used to create instances of ORDER object,
#' to be used in GMQL functions that require ordering on value.
#' It defines a descending order for input value
#' 
#' @param value string identifying name of metadata or region attribute
#'
#' @return ASC ordering object
#'
#' @examples
#' 
#' ## It gets the first 5 samples on the basis of their region count, 
#' ## those with the smaller Region_Count and then for each of them, 
#' ## It gets 7 regions on the basis of their score, those with the 
#' ## higher score
#' 
#' init_gmql()
#' test_path <- system.file("example", "DATASET", package = "RGMQL")
#' r = read_dataset(test_path)
#' o = order(r, list(ASC("Region_Count")), mtop = 5, 
#' regions_ordering = list(DESC("score")), rtop = 7)
#' 
#' @export
#'
ASC <- function(value)
{
    check.ORDER(value)
    
    list <- list(value = value)
    ## Set the name for the class
    class(list) <- c("ASC","ORDER")
    return(list)
}
