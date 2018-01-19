#' Method setdiff
#' 
#' @description Wrapper to GMQL DIFFERENCE operator
#' 
#' @description It produces one sample in the result for each sample of the 
#' left operand, by keeping the same metadata of the left input sample 
#' and only those regions (with their attributes and values) of the left input 
#' sample which do not intersect with any region in any right operand sample.
#' The optional \emph{joinBy} clause is used to extract a subset of pairs
#' from the Cartesian product of the two input datasets \emph{x} and \emph{y} 
#' on which to apply the DIFFERENCE operator:
#' only those samples that have the same value for each specified metadata 
#' attribute are considered when performing the difference.
#'
#' @importFrom rJava J .jnull .jarray
#' @importFrom BiocGenerics setdiff
#' 
#' @param x GMQLDataset class object
#' @param y GMQLDataset class object
#' @param joinBy \code{\link{conds}} function to support methods with 
#' groupBy or JoinBy input parameter
#' 
#' @param is_exact single logical value: TRUE means that the region difference 
#' is executed only on regions in 'x' dataset with exactly the same 
#' coordinates of at least one region present in 'y' dataset; 
#' if is_exact = FALSE, the difference is executed on all regions in 
#' 'x' dataset that overlap (even just one base) with at least one region in 
#' 'y' dataset
#' 
#' 
#' @return GMQLDataset object. It contains the value to use as input 
#' for the subsequent GMQLDataset method
#' 
#' @examples
#' ## This statement initializes and runs the GMQL server for local execution 
#' ## and creation of results on disk. Then, with system.file() it defines 
#' ## the path to the folders "DATASET" and "DATASET_GDM" in the subdirectory 
#' ## "example" of the package "RGMQL" and opens such folders as a GMQL 
#' ## datasets named "data1" and "data2", respectively, using CustomParser
#' 
#' init_gmql()
#' test_path <- system.file("example", "DATASET", package = "RGMQL")
#' test_path2 <- system.file("example", "DATASET_GDM", package = "RGMQL")
#' data1 = read_gmql(test_path)
#' data2 = read_gmql(test_path2)
#' 
#' ## This statement returns all the regions in the first dataset 
#' ## that do not overlap any region in the second dataset.
#' 
#' out = setdiff(data1, data2)
#' 
#' ## This statement extracts for every pair of samples s1 in data1 
#' ## and s2 in data2 having the same value of the metadata 
#' ## attribute 'cell' the regions that appear in s1 but 
#' ## do not overlap any region in s2.
#' ## Metadata of the result are the same as the metadata of s1.
#' 
#' out_t = setdiff(data1, data2, conds("cell"))
#'
#' @name setdiff
#' @aliases setdiff,GMQLDataset,GMQLDataset-method
#' @aliases setdiff-method
#' @export
setMethod("setdiff", c("GMQLDataset","GMQLDataset"),
            function(x, y, joinBy = conds(), is_exact = FALSE)
            {
                ptr_data_x = value(x)
                ptr_data_y = value(y)
                gmql_difference(ptr_data_x, ptr_data_y, is_exact, joinBy)
            })

gmql_difference <- function(left_data, right_data, is_exact, joinBy)
{
    if(!is.null(joinBy))
    {
        cond <- .join_condition(joinBy)
        if(is.null(cond))
            join_matrix <- .jnull("java/lang/String")
        else
            join_matrix <- .jarray(cond, dispatch = TRUE)
    }
    else
        join_matrix <- .jnull("java/lang/String")
    
    WrappeR <- J("it/polimi/genomics/r/Wrapper")
    response <- WrappeR$difference(join_matrix, left_data, right_data, 
                                        is_exact)
    error <- strtoi(response[1])
    val <- response[2]
    if(error)
        stop(val)
    else
        GMQLDataset(val)
}


