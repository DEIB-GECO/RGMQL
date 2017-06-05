#' GMQL Operation: UNION
#'
#' It is used to integrate homogeneous or heterogeneous samples of two datasets within a single dataset
#' for each sample of either input dataset, a result sample is created as follows:
#' \itemize{
#' \item Metadata are the same as in the original sample.
#' \item Resulting schema is obtained by projecting the schema of the right dataset over the schema of the left one
#' (more properly, it will be performed by adding to the schema of the left dataset the region attributes of the right dataset
#' which are not identical to those of the left dataset)
#' \item Regions are the same (in coordinates and attribute values) as in the original sample.
#' Region attributes which are missing in an input dataset sample w.r.t. the merged schema are set to null.
#' }
#' For what concerns metadata, attributes of samples from the left (right) input dataset are prefixed
#' with the strings LEFT (RIGHT), so as to trace the dataset to which they originally belonged.
#'
#' @param left_input_data "url-like" string taken from GMQL function
#' @param right_input_data "url-like" string taken from GMQL function
#'
#' @references \url{http://www.bioinformatics.deib.polimi.it/genomic_computing/GMQL/doc/GMQLUserTutorial.pdf}
#'
#' @examples
#' \dontrun{
#'
#' r = read(path)
#' r2 = read(path2)
#' c = cover(2,3,input_data = r)
#' u = union(r2,c)
#' }
#'
union <- function(left_input_data,right_input_data)
{
  out <- WrappeR$union(right_input_data,left_input_data)
  if(grepl("No",out,ignore.case = TRUE))
    stop(out)
  else
    out
}
