#' GMQL Operation: MERGE
#'
#' It builds a dataset consisting of a single sample having as many regions
#' as the number of regions of the input data and as many metadata 
#' as the union of the 'attribute-value' tuples of the input samples.
#' A groupby clause can be specified on metadata: the samples are then 
#' partitioned in groups, each with a distinct value of the grouping
#' metadata attributes.
#' The operation is separately applied to each group, yielding one sample 
#' in the result for each group.
#' Samples whose names are not present in the grouping metadata parameter 
#' are disregarded.
#'
#' @importFrom rJava J
#' @importFrom rJava .jnull
#' @importFrom rJava .jarray
#'  
#' @param data GMQLDataset class object 
#' @param groupBy list of CONDITION objects where every object contains 
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
#' @examples
#' 
#' # It creates a dataset called merged which contains one sample for each 
#' # antibody_target value found within the metadata of the exp dataset sample; 
#' # each created sample contains all regions from all 'exp' samples 
#' # with a specific value for their antibody_target metadata attribute.
#' 
#' init_gmql()
#' test_path <- system.file("example","DATASET",package = "RGMQL")
#' exp = read_dataset(test_path)
#' merged = aggregate(exp, groupBy = c("antibody_target"))
#' 
#' @aliases aggregate-method
#' @export
#' 
setMethod("aggregate", "GMQLDataset",
            function(data, groupBy = NULL)
            {
                val = data@value
                gmql_merge(val, groupBy)
            })

gmql_merge <- function(data, groupBy = NULL)
{
    if(!is.null(groupBy))
        join_condition_matrix <- .jarray(.join_condition(groupBy), 
                                            dispatch = TRUE)
    else
        join_condition_matrix <- .jnull("java/lang/String")
    
    WrappeR <- J("it/polimi/genomics/r/Wrapper")
    response <- WrappeR$merge(join_condition_matrix,data)
    error <- strtoi(response[1])
    data <- response[2]
    if(error!=0)
        stop(data)
    else
        GMQLDataset(data)
}

