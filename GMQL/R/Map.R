#' GMQL Operation: MAP
#'
#' It computes, for each sample in the right dataset, aggregates over the values of the right regions
#' that intersect with a region in a left sample, for each region of each sample in the left dataset;
#' The number of generated output samples is the Cartesian product of the samples in the two input datasets;
#' each output sample has the same regions as the related input left sample, with their attributes and values,
#' plus the attributes computed as aggregates over right region values.
#' Output sample metadata are the union of the related input sample metadata,
#' whose attribute names are prefixed with "left" or "right" respectively.
#'
#' When the joinby clause is present, only pairs of samples of left_input_data and of right_input_data with
#' metadata M1 and M2 respectively that satisfy the joinby condition are considered.
#'
#' The clause consists of a list of metadata attribute names that must be present with equal values
#' in both M1 and  M2
#'
#' @seealso
#'
#' @param left_input_data "url-like" string taken from GMQL function
#' @param right_input_data "url-like" string taken from GMQL function
#' @examples
#'
#' r = read(path)
#' r2 = read(path2)
#' c = cover(2,3,input_data = r)
#' u = union(r2,c)
#'

map <- function(left_input_data, right_input_data, output, aggregates = NULL, joinBy = NULL)
{

  out<-frappeR$map(aggregates,left_input_data,right_input_data)

  if(grepl("No",out,ignore.case = T))
    stop(out)
  else
    out
}
