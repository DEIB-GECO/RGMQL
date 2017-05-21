#' GMQL Operation: PROJECT
#'
#' It creates, from an existing dataset, a new dataset with all the samples from input dataset
#' but keeping for each sample in the input dataset only those metadata and/or region attributes
#' expressed in the operator parameter list.
#' Region coordinates and values of the remaining metadata remain equal to those in the input dataset.
#' It allows to:
#' a) Remove existing metadata and/or region attributes from a dataset;
#' b) Create new metadata and/or region attributes in the result
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
