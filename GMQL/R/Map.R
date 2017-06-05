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
#'
#' @param left_input_data returned object from any GMQL function
#' @param right_input_data returned object from any GMQL function
#' @param aggregates a list of element in the form key = 'function_aggregate'.
#' 'function_aggregate' is an object of class OPERATOR
#' The aggregate functions available are: MIN, MAX, SUM, BAG, AVG, COUNT, MEDIAN.
#' Every operator accepts a string value, execet for COUNT that cannot have a value.
#' Argument of 'function_aggregate' must exist in schema
#' Two style are allowed:
#' \itemize{
#' \item list of key-value pairs: e.g. sum = SUM("pvalue")
#' \item list of values: e.g. SUM("pvalue")
#' }
#' "mixed style" is not allowed
#' @param joinBy list of CONDITION objects every object contains the name of metadata to be used in semijoin,
#' or simple string concatenation c("cell_type","attribute_tag","size") without declaring condition.
#' In the latter form all metadata are considered having DEF condition
#' The CONDITION's available are:
#' \itemize{
#' \item{FULL: Fullname evaluation, two attributes match if they both end with value and,
#' if they have a further prefixes, the two prefix sequence are identical}
#' \item{DEF: Default evaluation, two attributes match if both end with value. }
#' \item{EXACT: Exact evaluation, only attributes exactly as value will match; no further prefixes are allowed. }
#' }
#' Every condition accepts only one string value. (e.g. DEF("cell_type") )
#'
#'
#' @return "url-like" string
#'
#' @references \url{http://www.bioinformatics.deib.polimi.it/genomic_computing/GMQL/doc/GMQLUserTutorial.pdf}
#'
#' @examples
#' \dontrun{
#'
#' initGMQL("gtf")
#' test_path <- system.file("example","DATA_SET_VAR_GTF",package = "GMQL")
#' test_path2 <- system.file("example","DATA_SET_VAR_GDM",package = "GMQL")
#' r = read(test_path)
#' r2 = read(test_path2)
#' m = map(r,r2)
#' }
#'
#' @export
#'
map <- function(left_input_data, right_input_data, aggregates = NULL, joinBy = NULL)
{
  if(!is.null(aggregates))
    metadata_matrix <- .aggregates(metadata,"OPERATOR")
  else
    metadata_matrix = NULL

  if(!is.null(joinBy))
    join_condition_matrix <- .join_condition(joinBy)
  else
    join_condition_matrix <- NULL

  out<-WrappeR$map(join_condition_matrix,aggregates,left_input_data,right_input_data)

  if(grepl("No",out,ignore.case = TRUE))
    stop(out)
  else
    out
}
