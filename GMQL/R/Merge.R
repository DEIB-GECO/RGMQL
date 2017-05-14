#' GMQL Operation: MERGE
#'
#'It builds a dataset consisting of a single sample having as regions
#'all the regions of the input data (without altering their
#'coordinates, even when overlapping) and as metadata the union of all
#'the attribute-values of the input samples.
#'When a groupBy clause is present, the samples are partitioned by groups,
#'each with distinct values of grouping metadata attributes.
#'The merge operation is separately applied to each group,
#'yielding to one sample in the result for each group.
#'
#'
#' @param metadata a vector of metadata
#' @param input_data string pointer taken from GMQL function
#' @examples
#' r = read(path)
#' m = merge(groupBy = c("antibody_targer","cell_karyotype"),input_data = r)
#' m = merge(input_data = r)
#'
merge <- function(groupBy = NULL,input_data)
{
  if(!is.character(groupBy) && !is.null(groupBy))
    stop("groupBy can be a string or an array of string")

  out <- frappeR$merge(groupBy,input_data)
  if(grepl("No",out,ignore.case = T))
    stop(out)
  else
    out
}
