#' Method map
#'
#' It computes, for each sample in the right dataset, aggregates over the 
#' values of the right dataset regions that intersect with a region in a left 
#' dataset sample, for each region of each sample in the left dataset.
#' The number of generated output samples is the Cartesian product 
#' of the samples in the two input datasets;
#' each output sample has the same regions as the related input left dataset 
#' sample, with their attributes and values, plus the attributes computed as 
#' aggregates over right region values.
#' Output sample metadata are the union of the related input sample metadata,
#' whose attribute names are prefixed with 'left' or 'right' respectively.
#'
#' When the joinby clause is present, only pairs of samples of x dataset
#' and of y dataset with metadata M1 and M2, respectively, that satisfy 
#' the joinby condition are considered.
#'
#' The clause consists of a list of metadata attribute names that must be
#' present with equal values in both M1 and  M2
#'
#'
#' @param x GMQLDataset class object
#' @param y GMQLDataset class object 
#' 
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
#' @param joinBy \code{\link{conds}} function to support methods with 
#' groupBy or JoinBy input parameter
#' @param count_name string defining the metadata count name; if it is 
#' not specified the name is "count_left_right" 
#' 
#' @return GMQLDataset object. It contains the value to use as input 
#' for the subsequent GMQLDataset method
#' 
#' @examples
#' 
#' ## This statement initializes and runs the GMQL server for local execution 
#' ## and creation of results on disk. Then, with system.file() it defines 
#' ## the path to the folders "DATASET" and "DATASET_GDM" in the subdirectory 
#' ## "example" of the package "RGMQL", and opens such folders as a GMQL 
#' ## dataset named "exp" and "ref", respectively, using CustomParser
#' 
#' init_gmql()
#' test_path <- system.file("example", "DATASET", package = "RGMQL")
#' test_path2 <- system.file("example", "DATASET_GDM", package = "RGMQL")
#' exp = read_gmql(test_path)
#' ref = read_gmql(test_path2)
#' 
#' ## This statement counts the number of regions in each sample from exp 
#' ## dataset that overlap with a ref dataset region, and for each ref region 
#' ## it computes the minimum score of all the regions in each exp sample that 
#' ## overlap with it. The MAP joinBy option ensures that only the exp samples 
#' ## referring to the same 'cell_tissue' of a ref sample are mapped on such 
#' ## ref sample; exp samples with no cell_tissue metadata attribute, or with 
#' ## such metadata attribute, but with a different value from the one(s) 
#' ## of ref sample(s), are disregarded.
#' 
#' out = map(ref, exp, minScore = MIN("score"), joinBy = conds("cell_tissue"))
#' 
#' @name map
#' @rdname map
#' @aliases map-method
#' @export
setMethod("map", "GMQLDataset",
            function(x, y, ..., joinBy = conds(), count_name = "")
            {
                left_data <- value(x)
                right_data <- value(y)
                aggregates = list(...)
                gmql_map(left_data, right_data,aggregates, joinBy, count_name)
            })


gmql_map <- function(left_data, right_data, aggregates, joinBy, count_name)
{
    if(!is.null(aggregates) && length(aggregates))
    {
        aggr <- .aggregates(aggregates, "META_AGGREGATES")
        metadata_matrix <- .jarray(aggr, dispatch = TRUE)
    }
    else
        metadata_matrix <- .jnull("java/lang/String")
    
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
    
    if(!is.null(count_name))
    {
        if(!is.character(count_name))
            stop("count_name: must be string")
        
        if(identical(count_name,""))
            count_name <- .jnull("java/lang/String")
    }
    else
        count_name <- .jnull("java/lang/String")
    
    WrappeR <- J("it/polimi/genomics/r/Wrapper")
    response<-WrappeR$map(join_matrix, metadata_matrix, count_name, left_data, 
                            right_data)
    error <- strtoi(response[1])
    val <- response[2]
    if(error)
        stop(val)
    else
        GMQLDataset(val)
}
