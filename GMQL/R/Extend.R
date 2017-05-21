#' GMQL Operation: EXTEND
#'
#' It generates new metadata attributes as result of aggregate functions applied to sample region attributes
#' and adds them to the existing metadata attributes of the sample.
#' Aggregate functions are applied sample by sample.
#'
#'
#' @param metadata a list of element in the form key = 'function_aggregate'.
#' 'function_aggregate' is an object of class META_OPERATOR
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
#' @param input_data "url-like" string returned from GMQL function
#'
#' @seealso \url{http://www.bioinformatics.deib.polimi.it/genomic_computing/GMQL/doc/GMQLUserTutorial.pdf}
#' @seealso \code{\link{META_OPERATOR}}
#'
#' @examples
#'
#' \dontrun{
#' startGMQL()
#' path = "/<path_to_your_folder>/<your_dataset_name>"
#' r = read(path)
#' e = extend(sum = SUM("pvalue"),c = COUNT(), m = AVG("score"),input_data = r)
#' }
extend <-function(metadata = NULL, input_data)
{
  if(!is.null(metadata))
  {
    if(!is.list(metadata))
      stop("metadata must be a list")

    if(!all(sapply(metadata, function(x) is(x,"META_OPERATOR") )))
    {
      stop("you must use OPERATOR object for defining aggregates function")
    }

    names <- names(metadata)
    if(is.null(names))
    {
      warning("you did not assign a names to a list.\nWe build names for you")
      names <- sapply(metadata, function(x) {
        take_value.META_OPERATOR(x)
      })
    }
    else {
      if(all(sapply(names, function(x) (x==""))))
      {
        stop("no partial names assignment to list")
      }
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

  out <- frappeR$extend(metadata_matrix,input_data)

  if(grepl("No",out,ignore.case = T))
    stop(out)
  else
    out
}
