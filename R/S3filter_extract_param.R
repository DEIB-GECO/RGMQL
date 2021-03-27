##########################################
#       PARAMETER_FILTER_EXTRACT        #
#########################################

PARAMETER_FILTER_EXTRACT <- function() {
  op_list <- list()
  ## Set the name for the class
  class(op_list) <- "PARAMETER_FILTER_EXTRACT"
  return(op_list)
}

as.character.PARAMETER_FILTER_EXTRACT <- function(obj) {
  class <- class(obj)[1]
}

print.PARAMETER_FILTER_EXTRACT <- function(obj){
  print(as.character.PARAMETER_FILTER_EXTRACT(obj))
}


#' PARAM object class constructor
#'
#' This class constructor is used to create instances of PARAM object
#' to be used in filter and extract function.
#' 
#' It is used to encompasses all the region parameters already present 
#' into the dataset or GRangesList
#' 
#' \itemize{
#' \item{FULL: It consider all the region paramter}
#' }
#' @param except The list of attribute to not consider
#' 
#' @return Param object
#'
#' @name filter-extract
#' @aliases FULL
#' @rdname filter-extract-param-class
#' @export
#'
FULL <- function(except = NULL) {
  value <- list(values = c(except))
  ## Set the name for the class
  class(value) <- c("FULL", "PARAMETER_FILTER_EXTRACT")
  return(value)
}