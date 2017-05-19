#' GMQL Operation: UNION
#'
#' It is used to integrate homogeneous or heterogeneous samples of two datasets within a single dataset
#' for each sample of either one of the input datasets, a sample is created in the result as follow
#' its metadata are the same as in the original sample.
#' Its regions are the same (in coordinates and attribute values) as in the original sample.
#' Region attributes which are missing in an input dataset sample (w.r.t. themerged schema) are set to null.
#' its schema is the schema of the first (left) input dataset
#' (more properly, it will be the merging of the schemas of the two input datasets) ;
#' new identifiers are assigned to each output sample;
#' The merging of two schemas is performed by projecting the schema of the right dataset
#' over the schema of the left one
#' two region attributes are considered identical if they have the same name and type.
#' For what concerns metadata, attributes of samples from the left (right) input dataset are prefixed
#' with the strings LEFT (RIGHT), so as to trace the dataset to which they originally belonged.
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
union <- function(left_input_data,right_input_data)
{
  out <- frappeR$union(right_input_data,left_input_data)
  if(grepl("No",out,ignore.case = T))
    stop(out)
  else
    out
}
