#' GMQL Operation: EXTEND
#'
#' It generates new metadata attributes as result of aggregate functions applied to sample region attributes
#' and adds them to the existing metadata attributes of the sample.
#' Aggregate functions are applied sample by sample.
#'
#'
#' @param input_data "url-like" string returned from GMQL function
#' @param metadata a list of element in the form key = 'function_aggregate'.
#' 'function_aggregate' is an object of class OPERATOR
#' The aggregate functions available are: MIN, MAX, SUM, BAG, AVG, COUNT, STD, MEDIAN, Q1, Q1, Q3.
#' Every operator accepts a string value, execet for COUNT that cannot have a value.
#' Argument of 'function_aggregate' must exist in schema
#' Two style are allowed:
#' \itemize{
#' \item list of key-value pairs: e.g. sum = SUM("pvalue")
#' \item list of values: e.g. SUM("pvalue")
#' }
#' "mixed style" is not allowed
#'
#' @references \url{http://www.bioinformatics.deib.polimi.it/genomic_computing/GMQL/doc/GMQLUserTutorial.pdf}
#'
#' @examples
#'
#' \dontrun{
#'
#' initGMQL("gtf")
#' path = "/<path_to_your_folder>/<your_dataset_name>"
#' r = read(path)
#' e = extend(list(sum = SUM("pvalue"),c = COUNT(), m = AVG("score")),input_data = r)
#' }
extend <-function(input_data, metadata = NULL)
{
  if(!is.null(metadata))
  {
    if(!is.list(metadata))
      stop("metadata must be a list")

    if(!all(sapply(metadata, function(x) is(x,"META_OPERATOR") )))
      stop("All elements should be META_OPERATOR object")

    names <- names(metadata)
    if(is.null(names))
    {
      warning("You did not assign a names to a list.\nWe build names for you")
      names <- sapply(metadata, take_value.META_OPERATOR)
    }
    else
    {
      if("" %in% names)
        stop("No partial names assignment is allowed")
    }
    aggregate_matrix <- t(sapply(metadata, function(x) {

      new_value = as.character(x)
      matrix <- matrix(new_value)

    }))
    m_names <- matrix(names)
    metadata_matrix <- cbind(m_names,aggregate_matrix)
  }
  else
    metadata_matrix <- NULL

  out <- WrappeR$extend(metadata_matrix,input_data)

  if(grepl("No",out,ignore.case = TRUE))
    stop(out)
  else
    out
}

