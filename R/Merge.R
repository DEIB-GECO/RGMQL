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
#' @param ... Additional arguments for use in specific methods.
#' 
#' 
#' This method accept a function to define condition evaluation on metadata.
#' \itemize{
#' \item{\code{\link{FN}}: Fullname evaluation, two attributes match 
#' if they both end with value and, if they have a further prefixes,
#' the two prefix sequence are identical}
#' \item{\code{\link{EX}}: Exact evaluation, only attributes exactly 
#' as value will match; no further prefixes are allowed. }
#' \item{\code{\link{DF}}: Default evaluation, the two attributes match 
#' if both end with value.}
#' }
#' 
#' @return DataSet class object. It contains the value to use as input 
#' for the subsequent GMQL function
#' 
#' @examples
#' 
#' # It creates a dataset called merged which contains one sample for each 
#' # antibody_target value found within the metadata of the exp dataset sample; 
#' # each created sample contains all regions from all 'exp' samples 
#' # with a specific value for their antibody_target and cell metadata 
#' # attributes.
#' 
#' init_gmql()
#' test_path <- system.file("example","DATASET",package = "RGMQL")
#' exp = read_dataset(test_path)
#' merged = aggregate(exp, DF("antibody_target","cell"))
#' 
#' @aliases aggregate-method
#' @export
#' 
setMethod("aggregate", "GMQLDataset",
            function(data, ...)
            {
                ptr_data = data@value
                groupBy = list(...)
                gmql_merge(ptr_data, groupBy)
            })

gmql_merge <- function(input_data, groupBy)
{
    if(!is.null(groupBy) && !length(groupBy) == 0)
    {
        cond <- .join_condition(groupBy)
        join_condition_matrix <- .jarray(cond, dispatch = TRUE)
    }
    else
        join_condition_matrix <- .jnull("java/lang/String")
    
    WrappeR <- J("it/polimi/genomics/r/Wrapper")
    response <- WrappeR$merge(join_condition_matrix, input_data)
    error <- strtoi(response[1])
    data <- response[2]
    if(error!=0)
        stop(data)
    else
        GMQLDataset(data)
}

