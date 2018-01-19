#' Method aggregate
#' 
#' @description Wrapper to GMQL MERGE operator
#' 
#' @description It builds a dataset consisting of a single sample having as 
#' many regions as the number of regions of all the input dataset samples
#' and as many metadata as the union of the 'attribute-value' tuples of the 
#' input samples. If \emph{groupBy} is specified, the samples are then 
#' partitioned in groups, each with a distinct value of the grouping metadata 
#' attributes. The operation is separately applied to each group, yielding 
#' one sample in the result for each group. Samples whose metadata are 
#' not present in the grouping metadata parameter are disregarded.
#'
#' @importFrom rJava J .jarray .jnull
#' @importFrom S4Vectors aggregate
#'
#' @param x GMQLDataset class object
#' @param groupBy \code{\link{condition_evaluation}} function to support 
#' methods with groupBy or JoinBy input paramter
#' 
#' @return GMQLDataset object. It contains the value to use as input 
#' for the subsequent GMQLDataset method
#'
#' @examples
#' 
#' ## This statement initializes and runs the GMQL server for local execution 
#' ## and creation of results on disk. Then, with system.file() it defines 
#' ## the path to the folder "DATASET" in the subdirectory "example"
#' ## of the package "RGMQL" and opens such file as a GMQL dataset named "exp" 
#' ## using CustomParser
#'
#' init_gmql()
#' test_path <- system.file("example", "DATASET", package = "RGMQL")
#' exp = read_gmql(test_path)
#'
#' ## This statement creates a dataset called merged which contains one 
#' ## sample for each antibody_target and cell value found within the metadata 
#' ## of the exp dataset sample; each created sample contains all regions 
#' ## from all 'exp' samples with a specific value for their 
#' ## antibody_target and cell metadata attributes.
#'
#' merged = aggregate(exp, conds(c("antibody_target", "cell")))
#'
#' @name aggregate
#' @rdname aggregate
#' @aliases aggregate,GMQLDataset-method
#' @aliases aggregate-method
#' @export
#' 
setMethod("aggregate", "GMQLDataset",
            function(x, groupBy = conds())
            {
                ptr_data = value(x)
                gmql_merge(ptr_data, groupBy)
            })

gmql_merge <- function(input_data, groupBy)
{
    if(!is.null(groupBy))
    {
        cond <- .join_condition(groupBy)
        if(is.null(cond))
            join_matrix <- .jnull("java/lang/String")
        else
            join_matrix <- .jarray(cond, dispatch = TRUE)
    }
    else
        join_matrix <- .jnull("java/lang/String")
    
    WrappeR <- J("it/polimi/genomics/r/Wrapper")
    response <- WrappeR$merge(join_matrix, input_data)
    error <- strtoi(response[1])
    val <- response[2]
    if(error)
        stop(val)
    else
        GMQLDataset(val)
}

