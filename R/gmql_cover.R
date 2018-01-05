#' Method cover
#'
#' It takes as input a dataset containing one or more samples and returns 
#' another dataset (with a single sample, if no \emph{groupby} option is 
#' specified) by “collapsing” the input dataset samples and their regions 
#' according to certain rules specified by the input parameters.
#' The attributes of the output genomic regions are only the region 
#' coordinates, and Jaccard indexes (\emph{JaccardIntersect} and 
#' \emph{JaccardResult}).
#' Jaccard Indexes are standard measures of similarity of the contributing 
#' regions, added as default region attributes.
#' The JaccardIntersect index is calculated as the ratio between the lengths 
#' of the intersection and of the union of the contributing regions; 
#' the JaccardResult index is calculated as the ratio between the lengths 
#' of the result and the union of the contributing regions.
#' If aggregate functions are specified, a new attribute is added for 
#' each aggregate function specified.
#' Output metadata are the union of the input ones.
#' If \emph{groupby} clause is specified, the input samples are partitioned 
#' in groups, each with distinct values of the grouping metadata attributes, 
#' and the \emph{cover} operation is separately applied to each group, 
#' yielding to one sample in the result for each group.
#' Input samples that do not satisfy the \emph{groupby} condition 
#' are disregarded.
#' 
#' @include AllClasses.R
#' @importFrom methods is
#' @importFrom rJava J .jnull .jarray
#' 
#' @param .data GMQLDataset class object
#' @param min_acc minimum number of overlapping regions to be considered 
#' during execution. It is an integer number, declared also as string.
#' minAcc accepts also:
#' \itemize{
#' \item{PARAMETER class object: \code{\link{ALL}}, that represents the number 
#' of samples in the input dataset}
#' \item{an expression built using PARAMETER object: (ALL() + N) / K or
#' ALL() / K, with N and K integer values }
#' }
#' @param max_acc maximum number of overlapping regions to be considered 
#' during execution. It is an integer number, declared also as string.
#' maxAcc accept also:
#' \itemize{
#' \item{PARAMETER class object: \code{\link{ALL}}, that represents the number 
#' of samples in the input dataset}
#' \item{PARAMETER class object: \code{\link{ANY}}}, that acts as a wildcard, 
#' considering any amount of overlapping regions.
#' \item{an expression built using PARAMETER object: (ALL() + N) / K or
#' ALL() / K, with N and K integer values  }
#' }
#' @param groupBy \code{\link{conds}} function to support methods with 
#' groupBy or JoinBy input parameter
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
#' attributes. Two style are allowed:
#' \itemize{
#' \item list of key-value pairs: e.g. sum = SUM("pvalue")
#' \item list of values: e.g. SUM("pvalue")
#' }
#' "mixed style" is not allowed
#'
#' @param variation string identifying the cover GMQL operator variation.
#' The admissible strings are:
#' \itemize{
#' \item{FLAT: It returns the regions that start from the first end and stop 
#' at the last end of the regions which would contribute to each region 
#' of the \emph{cover}.}
#' \item{SUMMIT: It returns regions that start from a position
#' where the number of intersecting regions is not increasing afterwards and
#' stop at a position where either the number of intersecting regions 
#' decreases, or it violates the max accumulation index.}
#' \item{HISTOGRAM: It returns the non-overlapping regions contributing to 
#' the \emph{cover}, each with its accumulation index value, which is assigned 
#' to the \emph{AccIndex} region attribute.}
#' \item{COVER: default value.}
#' }
#' Can be all caps or lowercase
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
#' ## using customParser
#' 
#' init_gmql()
#' test_path <- system.file("example","DATASET",package = "RGMQL")
#' exp = read_GMQL(test_path)
#'   
#' ## the following statement produces an output dataset with a single output 
#' ## sample. The COVER operation considers all areas defined by a minimum 
#' ## of two overlapping regions in the input samples, up to any amount of 
#' ## overlapping regions.
#' 
#' res = cover(exp, 2, ANY())
#'
#' ## The following GMQL statement computes the result grouping the input 
#' ## exp samples by the values of their cell metadata attribute, 
#' ## thus one output res sample is generated for each cell type; 
#' ## output regions are produced where at least 2 and at most 3 regions 
#' ## of grouped exp samples overlap, setting as attributes of the resulting 
#' ## regions the minimum pvalue of the overlapping regions (min_pvalue) 
#' ## and their Jaccard indexes (JaccardIntersect and JaccardResult).
#' 
#' res = cover(exp, 2, 3, groupBy = conds("cell"), min_pValue = MIN("pvalue"))
#' 
#' @name cover
#' @rdname cover
#' @aliases cover,GMQLDataset-method
#' @aliases cover-method
#' @export
setMethod("cover", "GMQLDataset",
            function(.data, min_acc, max_acc, groupBy = conds(), 
                    variation = "cover", ...)
            {
                val <- value(.data)
                s_min <- substitute(min_acc)
                s_min <- .trasform_cover(deparse(s_min))                
                s_max <- substitute(max_acc)
                s_max <- .trasform_cover(deparse(s_max))
                
                q_max <- .check_cover_param(s_max,FALSE)
                q_min <- .check_cover_param(s_min,TRUE)
                
                flag = toupper(variation)
                aggregates = list(...)
                gmql_cover(val, q_min, q_max, groupBy, aggregates, flag)
            })

gmql_cover <- function(input_data, min_acc, max_acc, groupBy,aggregates,flag)
{
    if(!is.null(groupBy))
    {
        if("condition" %in% names(groupBy))
        {
            cond <- .join_condition(groupBy)
            if(is.null(cond))
                join_matrix <- .jnull("java/lang/String")
            else
                join_matrix <- .jarray(cond, dispatch = TRUE)
        }
        else
            stop("use function conds()")
    }
    else
        join_matrix <- .jnull("java/lang/String")

    if(!is.null(aggregates) && length(aggregates))
    {
        aggr <- .aggregates(aggregates,"AGGREGATES")
        metadata_matrix <- .jarray(aggr, dispatch = TRUE)
    }
    else
        metadata_matrix <- .jnull("java/lang/String")
    
    WrappeR <- J("it/polimi/genomics/r/Wrapper")
    response <- switch(flag,
        "COVER" = WrappeR$cover(min_acc, max_acc, join_matrix,
                                    metadata_matrix, input_data),
        "FLAT" = WrappeR$flat(min_acc, max_acc, join_matrix,
                                    metadata_matrix, input_data),
        "SUMMIT" = WrappeR$summit(min_acc,max_acc, join_matrix,
                                    metadata_matrix, input_data),
        "HISTOGRAM" = WrappeR$histogram(min_acc, max_acc, join_matrix, 
                                    metadata_matrix, input_data))
    if(is.null(response))
        stop("no admissible variation: cover, flat, summit, histogram")
    
    error <- strtoi(response[1])
    val <- response[2]
    if(error)
        stop(val)
    else
        GMQLDataset(val)
}

.check_cover_param <- function(param, is_min)
{
    if(length(param) > 1)
        stop("length > 1")

    if(is.character(param))
    {
        if(is_min && identical(param,"ANY"))
            stop("min cannot assume ANY as value")
        
        return(param)
    }
    else
        stop("invalid input data")
    
}

.trasform_cover <- function(predicate)
{
    predicate <- gsub("\\(\\)","",predicate)
}

