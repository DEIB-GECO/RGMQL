#' Method extend
#'
#' For each sample in an input dataset, it generates new metadata attributes 
#' as result of aggregate functions applied to sample region attributes 
#' and adds them to the existing metadata attributes of the sample.
#' Aggregate functions are applied sample by sample.
#'
#' @importFrom rJava J .jnull .jarray
#'
#' @param .data GMQLDataset class object 
#' @param ... a series of expressions separated by comma in the form 
#' \emph{key} = \emph{aggregate}. The \emph{aggregate} is an object of 
#' class AGGREGATES. The aggregate functions available are: \code{\link{SUM}}, 
#' \code{\link{COUNT}}, \code{\link{MIN}}, \code{\link{MAX}}, 
#' \code{\link{AVG}}, \code{\link{MEDIAN}}, \code{\link{STD}}, 
#' \code{\link{BAG}}, \code{\link{BAGD}}, \code{\link{Q1}}, 
#' \code{\link{Q2}}, \code{\link{Q3}}.
#' Every aggregate accepts a string value, except for COUNT, which does not 
#' have any value.
#' Argument of 'aggregate function' must exist in schema, i.e. among region 
#' attributes. Two styles are allowed:
#' \itemize{
#' \item list of key-value pairs: e.g. sum = SUM("pvalue")
#' \item list of values: e.g. SUM("pvalue")
#' }
#' "mixed style" is not allowed
#'
#' @return GMQLDataset object. It contains the value to use as input 
#' for the subsequent GMQLDataset method
#' 
#' @examples
#' 
#' ## This statement initializes and runs the GMQL server for local execution 
#' ## and creation of results on disk. Then, with system.file() it defines 
#' ## the path to the folders "DATASET" in the subdirectory "example" 
#' ## of the package "RGMQL" and opens such folder as a GMQL dataset 
#' ## named "data"
#' 
#' init_gmql()
#' test_path <- system.file("example", "DATASET", package = "RGMQL")
#' data <- read_dataset(test_path)
#' 
#' ## This statement counts the regions in each sample and stores their number 
#' ## as value of the new metadata attribute RegionCount of the sample.
#' 
#' e <- extend(data, RegionCount = COUNT())
#' 
#' ## This statement copies all samples of data dataset into 'res' dataset, 
#' ## and then calculates for each of them two new metadata attributes:
#' ##  1. RegionCount is the number of sample regions;
#' ##  2. MinP is the minimum pvalue of the sample regions.
#' ## res sample regions are the same as the ones in data.
#' 
#' res = extend(data, RegionCount = COUNT(), MinP = MIN("pvalue"))
#' 
#' @name extend
#' @rdname extend
#' @aliases extend-method
#' @export
setMethod("extend", "GMQLDataset", function(.data, ...)
            {
                ptr_data = value(.data)
                meta <- list(...)
                gmql_extend(ptr_data, meta)
            })


gmql_extend <-function(input_data, meta)
{
    if(!is.null(meta) && length(meta))
    {
        aggr <- .aggregates(meta, "META_AGGREGATES")
        metadata_matrix <- .jarray(aggr, dispatch = TRUE)
    }
    else
        metadata_matrix <- .jnull("java/lang/String")
    
    WrappeR <- J("it/polimi/genomics/r/Wrapper")
    response <- WrappeR$extend(metadata_matrix, input_data)
    error <- strtoi(response[1])
    val <- response[2]
    if(error!=0)
        stop(val)
    else
        GMQLDataset(val)
}
