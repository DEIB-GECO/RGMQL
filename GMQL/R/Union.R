#' GMQL Operation: UNION
#'
#' It is used to integrate heterogeneous samples of two datasets within a single dataset;
#' each sample of both input datasets contributes to one sample of the result
#' with identical metadata and merged region schema.
#' Two region attributes are considered identical if they have the same name and type;
#' the union is performed by projecting the schema of the right dataset
#' over the schema of the left one.
#' Fields of the left dataset which are missing in the right one are set to NULL value,
#' for all the regions of the right operator.
#' For what concerns metadata,attributes are prefixed with the strings LEFT or RIGHT
#' so as to trace the dataset to which they refer.
#'
#'
#' @param right_input_data url-like "string" pointer taken from GMQL function
#' @param left_input_data url-like "string" pointer taken from GMQL function
#' @examples
#'
#' r = read(path)
#' r2 = read(path2)
#' c = cover(2,3,input_data = r)
#' u = union(r2,c)
#'
union <- function(right_input_data,left_input_data)
{
  out <- frappeR$union(right_input_data,left_input_data)
  if(grepl("No",out,ignore.case = T))
    stop(out)
  else
    out
}
