#' GMQL Operation: PROJECT
#'
#'
#'
#'
#' @param predicate string made up by logical oepration: AND,OR,NOT
#' @param region region
#' @param semijoin semijoin
#' @param input_data string pointer taken from GMQL function
#'
#'
project <-function(metadata = NULL,regions = NULL,input_data)
{
  if(!is.character(metadata) && !is.null(metadata))
    stop("groupBy can be a string or an array of string")

  if(!is.character(regions) && !is.null(regions))
    stop("groupBy can be a string or an array of string")

  out <- frappeR$project(metadata,regions,input_data)
  if(grepl("No",out,ignore.case = T))
    stop(out)
  else
    out
}
