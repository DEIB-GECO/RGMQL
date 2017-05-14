#' GMQL Operation: UNION
#'
#'It is used to integrate possibly heterogeneous samples of two datasets within a single dataset;
#'each sample of both input datasets contributes to one sample of the result
#'with identical metadata and merged region schema.
#'New identifiers are assigned to each sample.
#'Two region attributes are considered identical if they have the same name and type;
#'the merging of two schemas is performed by projecting the schema of the second dataset
#'over the schema of the first one.
#'Fields of the first dataset which are missing in the second one are set to NULL value,
#'for all the regions of the second operator.
#'For what concerns metadata,attributes are prefixed with the strings LEFT or RIGHT
#'so as to trace the dataset to which they refer.
#'
#'
#' @param right_input_data string pointer taken from GMQL function
#' @param left_input_data string pointer taken from GMQL function
#' @examples
#' r = read(path)
#' r2 = read(path2)
#' u = union(r2,r)
#' u = union(right_input_data = r,left_input_data = r2)
#'
union <- function(right_input_data,left_input_data)
{
  out <- frappeR$union(right_input_data,left_input_data)
  if(grepl("No",out,ignore.case = T))
    stop(out)
  else
    out
}
