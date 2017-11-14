#' GMQL Operation: EXTEND
#'
#' It generates new metadata attributes as result of aggregate functions 
#' applied to sample region attributes and adds them to the existing metadata 
#' attributes of the sample.
#' Aggregate functions are applied sample by sample.
#'
#' @importFrom rJava .jnull
#' @importFrom rJava J
#' @importFrom rJava .jarray
#'
#' @param .data GMQLDataset class object 
#' @param metadata list of element in the form \emph{key} = \emph{aggregate}.
#' The \emph{aggregate} is an object of class AGGREGATES
#' The aggregate functions available are: \code{\link{SUM}}, 
#' \code{\link{COUNT}}, \code{\link{MIN}}, \code{\link{MAX}}, 
#' \code{\link{AVG}}, \code{\link{MEDIAN}}, \code{\link{STD}}, 
#' \code{\link{BAG}}, \code{\link{BAGD}}, \code{\link{Q1}}, 
#' \code{\link{Q2}}, \code{\link{Q3}}.
#' Every aggregate accepts a string value, execet for COUNT, which does not 
#' have any value.
#' Argument of 'aggregate function' must exist in schema, i.e. among region 
#' attributes. Two style are allowed:
#' \itemize{
#' \item list of key-value pairs: e.g. sum = SUM("pvalue")
#' \item list of values: e.g. SUM("pvalue")
#' }
#' "mixed style" is not allowed
#'
#' @return DataSet class object. It contains the value to use as input 
#' for the subsequent GMQL function
#' 
#' @examples
#'
#' ## it counts the regions in each sample and stores their number as value 
#' ## of the new metadata RegionCount attribute of the sample.
#' init_gmql()
#' test_path <- system.file("example", "DATASET", package = "RGMQL")
#' r <- read_dataset(test_path)
#' e <- mutate(input_data = r, list(RegionCount = COUNT()))
#' 
#' \dontrun{
#' 
#' ## it copies all samples of exp dataset into res dataset, 
#' ## and then calculates for each of them two new metadata attributes:
#' ##  1. RegionCount is the number of sample regions;
#' ##  2. MinP is the minimum pvalue of the sample regions.
#' ## res sample regions are the same as the ones in exp.
#' 
#' init_gmql()
#' test_path <- system.file("example", "DATASET", package = "RGMQL")
#' exp = read_dataset(test_path)
#' res = mutate(input_data = exp, list(RegionCount = COUNT(),
#' MinP = MIN("pvalue")))
#' 
#' }
#' 
#' @name mutate
#' @rdname mutate-methods
#' @aliases mutate, mutate-methods
#' @export
setMethod("mutate", "GMQLDataset",
            function(.data, metadata = NULL)
            {
                val_x = .data@value
                gmql_extend(val_x, metadata)
            })


gmql_extend <-function(input_data, metadata = NULL)
{
    if(!is.null(metadata))
        metadata_matrix <- .jarray(.aggregates(metadata,"META_AGGREGATES"),
                                    dispatch = TRUE)
    else
        metadata_matrix <- .jnull("java/lang/String")
    
    WrappeR <- J("it/polimi/genomics/r/Wrapper")
    response <- WrappeR$extend(metadata_matrix,input_data)
    error <- strtoi(response[1])
    data <- response[2]
    if(error!=0)
        stop(data)
    else
        GMQLDataset(data)
}
