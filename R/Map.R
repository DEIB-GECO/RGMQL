#' GMQL Operation: MAP
#'
#' It computes, for each sample in the right dataset, aggregates over the 
#' values of the right regions that intersect with a region in a left sample, 
#' for each region of each sample in the left dataset;
#' The number of generated output samples is the Cartesian product 
#' of the samples in the two input datasets;
#' each output sample has the same regions as the related input left sample, 
#' with their attributes and values, plus the attributes computed as 
#' aggregates over right region values.
#' Output sample metadata are the union of the related input sample metadata,
#' whose attribute names are prefixed with "left" or "right" respectively.
#'
#' When the joinby clause is present, only pairs of samples of left_input_data 
#' and of right_input_data with metadata M1 and M2 respectively that satisfy 
#' the joinby condition are considered.
#'
#' The clause consists of a list of metadata attribute names that must be
#' present with equal values in both M1 and  M2
#'
#'
#' @param left_input_data returned object from any GMQL function
#' @param right_input_data returned object from any GMQL function
#' @param aggregates list of element in the form \emph{key} = \emph{aggregate}.
#' The \emph{aggregate} is an object of class AGGREGATES
#' The aggregate functions available are: \code{\link{SUM}}, 
#' \code{\link{COUNT}}, \code{\link{MIN}}, \code{\link{MAX}}, 
#' \code{\link{AVG}}, \code{\link{MEDIAN}}, \code{\link{STD}}, 
#' \code{\link{BAG}}, \code{\link{BAGD}}, \code{\link{Q1}}, 
#' \code{\link{Q2}}, \code{\link{Q3}}.
#' Every aggregate accepts a string value, execet for COUNT
#' Argument of 'aggregate' must exist in schema
#' Two style are allowed:
#' \itemize{
#' \item list of key-value pairs: e.g. sum = SUM("pvalue")
#' \item list of values: e.g. SUM("pvalue")
#' }
#' "mixed style" is not allowed
#'
#' @param joinBy list of CONDITION objects where every object contains 
#' the name of metadata to be used in semijoin, or simple string concatenation 
#' of name of metadata, e.g. c("cell_type", "attribute_tag", "size") 
#' without declaring condition.
#' The CONDITION's available are:
#' \itemize{
#' \item{\code{\link{FULL}}: Fullname evaluation, two attributes match 
#' if they both end with value and, if they have a further prefixes,
#' the two prefix sequence are identical}
#' \item{\code{\link{EXACT}}: Exact evaluation, only attributes exactly 
#' as value will match; no further prefixes are allowed. }
#' }
#' Every condition accepts only one string value. (e.g. FULL("cell_type") )
#' In case of single concatenation with no CONDITION, or list with some value 
#' without conditon, the metadata are considered having default 
#' evaluation: the two attributes match if both end with value.
#' 
#' @return DataSet class object. It contains the value to use as input 
#' for the subsequent GMQL function
#' 
#'
#' @examples
#'
#' # It counts the number of regions in each sample from exp that overlap with 
#' # a ref region, and for each ref region it computes the minimum score 
#' # of all the regions in each exp sample that overlap with it. 
#' # The MAP joinby option ensures that only the exp samples referring to 
#' # the same 'cell_tissue' of a ref sample are mapped on such ref sample; 
#' # exp samples with no cell_tissue metadata attribute, or with such metadata 
#' # but with a different value from the one(s) of ref sample(s), 
#' # are disregarded.
#' 
#' init_gmql()
#' test_path <- system.file("example", "DATASET", package = "RGMQL")
#' test_path2 <- system.file("example", "DATASET_GDM", package = "RGMQL")
#' exp = read_dataset(test_path)
#' ref = read_dataset(test_path2)
#' out = map(ref,exp, list(minScore = MIN("score")), 
#' joinBy = c("cell_tissue"))
#' 
#' 
#' @export
#'
map <- function(left_input_data, right_input_data, aggregates = NULL, 
                    joinBy = NULL)
{
    if(!is.null(aggregates))
        metadata_matrix <- .jarray(.aggregates(aggregates,"AGGREGATES"),
                                    dispatch = TRUE)
    else
        metadata_matrix = .jnull("java/lang/String")

    if(!is.null(joinBy))
        join_condition_matrix <- .jarray(.join_condition(joinBy),
                                            dispatch = TRUE)
    else
        join_condition_matrix <- .jnull("java/lang/String")
  
    WrappeR <- J("it/polimi/genomics/r/Wrapper")
    response<-WrappeR$map(join_condition_matrix, metadata_matrix, 
                            left_input_data$value, right_input_data$value)
    error <- strtoi(response[1])
    data <- response[2]
    if(error!=0)
        stop(data)
    else
        DataSet(data)
}
