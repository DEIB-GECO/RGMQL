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
#' ### local with CustomParser
#' initGMQL("gtf")
#' test_path <- system.file("example","DATA_SET_VAR_GTF",package = "GMQL")
#' r = readDataset(test_path)
#' 
#' ### treatment_type and ID descending
#' o = order(list(DESC(treatment_type),DESC(ID)), mtop = 2, input_data = r)
#' 
#' \dontrun{
#' ### treatment_type ascending and ID descending
#' o = order(list("treatment_type", DESC(ID)), mtop = 2, input_data = r)
#' }
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
#' ### local with CustomParser
#' initGMQL("gtf")
#' test_path <- system.file("example","DATA_SET_VAR_GTF",package = "GMQL")
#' r = readDataset(test_path)
#' 
#' ###regions ordering as ascending
#' o = order(regions_ordering = c("pvalue","length","name"), rtopg = 1, input_data = r)
#'
#' \dontrun{
#' #' ###regions ordering as ascending
#' o = order(regions_ordering = list(ASC("pvalue"),ASC("length"),ASC("name")), rtopg = 1, input_data = r)
#'
#' ### pvalue ascending and length descending
#' o = order(list("pvalue", DESC(length)), mtop = 2, input_data = r)
#' }
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
