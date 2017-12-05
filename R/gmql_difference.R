#' Method setdiff
#' 
#' @description Wrapper to GMQL difference function
#' 
#' @description It produces one sample in the result for each sample of the 
#' left operand, by keeping the same metadata of the left input sample 
#' and only those regions (with their schema and values) of the left input 
#' sample which do not intersect with any region in the right operand sample.
#' The optional \emph{joinby} clause is used to extract a subset of couples
#' from the cartesian product of two dataset \emph{x} and \emph{y} 
#' on which to apply the DIFFERENCE operator:
#' only those samples that have the same value for each attribute
#' are considered when performing the difference.
#'
#' @importFrom rJava J .jnull .jarray
#' @importFrom BiocGenerics setdiff
#' 
#' @param x GMQLDataset class object
#' @param y GMQLDataset class object
#' @param ... Additional arguments for use in specific methods.
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
#' @param is_exact single logical value: TRUE means that the region difference 
#' is executed only on regions in left_input_data with exactly the same 
#' coordinates of at least one region present in right_input_data; 
#' if is_exact = FALSE, the difference is executed on all regions in 
#' left_input_data that overlap with at least one region in right_input_data 
#' (even just one base).
#' 
#' @return GMQLDataset object. It contains the value to use as input 
#' for the subsequent GMQLDataset method
#' 
#'
#' @examples
#'
#' ## This GMQL statement returns all the regions in the first dataset 
#' ## that do not overlap any region in the second dataset.
#' 
#' init_gmql()
#' test_path <- system.file("example", "DATASET", package = "RGMQL")
#' test_path2 <- system.file("example", "DATASET_GDM", package = "RGMQL")
#' r_left = read_dataset(test_path)
#' r_right = read_dataset(test_path2)
#' out = setdiff(r_left, r_right)
#' 
#' \dontrun{
#' ## This GMQL statement extracts for every pair of samples s1 in EXP1 
#' ## and s2 in EXP2 having the same value of the metadata 
#' ## attribute 'antibody_target' the regions that appear in s1 but 
#' ## do not overlap any region in s2; 
#' ## metadata of the result are the same as the metadata of s1.
#' 
#' init_gmql()
#' test_path <- system.file("example", "DATASET", package = "RGMQL")
#' test_path2 <- system.file("example", "DATASET_GDM", package = "RGMQL")
#' exp1 = read_dataset(test_path)
#' exp2 = read_dataset(test_path2)
#' out = setdiff(exp1, exp2, DF("antibody_target"))
#'
#' }
#' @name setdiff
#' @aliases setdiff,GMQLDataset,GMQLDataset-method
#' @aliases setdiff-method
#' @export
setMethod("setdiff", c("GMQLDataset","GMQLDataset"),
            function(x, y, ..., is_exact = FALSE)
            {
                ptr_data_x = x@value
                ptr_data_y = y@value
                joinBy = list(...)
                gmql_difference(ptr_data_x, ptr_data_y, is_exact, joinBy)
            })

gmql_difference <- function(left_data, right_data, is_exact, joinBy)
{
    if(!is.null(joinBy) && !length(joinBy) == 0)
    {
        cond <- .join_condition(joinBy)
        join_condition_matrix <- .jarray(cond, dispatch = TRUE)
    }
    else
        join_condition_matrix <- .jnull("java/lang/String")
    
    WrappeR <- J("it/polimi/genomics/r/Wrapper")
    response <- WrappeR$difference(join_condition_matrix, right_data, 
                                        left_data, is_exact)
    error <- strtoi(response[1])
    data <- response[2]
    if(error!=0)
        stop(data)
    else
        GMQLDataset(data)
}


