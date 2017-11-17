#' GMQL Operation: COVER
#'
#' It takes as input a dataset containing one or more samples and returns 
#' another dataset (with a single sample, if no \emph{groupby} option is 
#' specified) by “collapsing” the input dataset samples and their regions 
#' according to certain rules specified by the input parameters.
#' The attributes of the output genomic regions are only the region 
#' coordinates, and Jaccard indexes (JaccardIntersect and JaccardResult).
#' Jaccard Indexes are standard measures of similarity of the contributing 
#' regions, added as default region attributes.
#' The JaccardIntersect index is calculated as the ratio between the lengths 
#' of the intersection and of the union of the contributing regions; 
#' the JaccardResult index is calculated as the ratio between the lengths 
#' of the result and the union of the contributing regions.
#' If aggregate functions are specified, a new attributes is added for 
#' each aggregate function specified.
#' Output metadata are the union of the input ones.
#' If \emph{groupby} clause is specified, the input samples are partitioned 
#' in groups, each with distinct values of the grouping metadata attributes, 
#' and the \emph{cover} operation is separately applied to each group, 
#' yielding to one sample in the result for each group.
#' Input samples that do not satisfy the \emph{groupby} condition 
#' are disregarded.
#' 
#' @include GMQLDataset-class.R
#' @importFrom methods is
#' @importFrom rJava J
#' @importFrom rJava .jnull
#' @importFrom rJava .jarray
#' 
#' @param data GMQLDataset class object
#' @param min_acc minimum number of overlapping regions to be considered 
#' during execution
#' Is a integer number, declared also as string.
#' minAcc accept also:
#' \itemize{
#' \item{PARAMETER class object: \code{\link{ALL}} that represents the number 
#' of samples in the input dataset}
#' \item{and expression built using PARAMETER object: (ALL() + N) / K or
#' ALL() / K }
#' }
#' @param max_acc maximum number of overlapping regions to be considered 
#' during execution
#' Is a integer number, declared also as string.
#' maxAcc accept also:
#' \itemize{
#' \item{PARAMETER class object: \code{\link{ALL}} that represents the number 
#' of samples in the input dataset}
#' \item{PARAMETER calss object: \code{\link{ANY}}} that acts as a wildcard, 
#' considering any amount of overlapping.
#' \item{and expression built using PARAMETER object: (ALL() + N) / K or
#' ALL() / K }
#' }
#' @param groupBy list of evalation function to define condition 
#' evaluation on metadata:
#' \itemize{
#' \item{\code{\link{FN}}: Fullname evaluation, two attributes match 
#' if they both end with value and, if they have a further prefixes,
#' the two prefix sequence are identical}
#' \item{\code{\link{EX}}: Exact evaluation, only attributes exactly 
#' as value will match; no further prefixes are allowed. }
#' \item{\code{\link{DF}}: Default evaluation, the two attributes match 
#' if both end with value.}
#' }
#' @param ... Additional arguments for use in specific methods.
#' 
#' In this case a series of element in the form \emph{key} = \emph{aggregate}.
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
#' @param variation string identifying the cover GMQL function variation.
#' The admissible string are:
#' \itemize{
#' \item{flat: returns the contiguous region that starts from the first end 
#' and stops at the last end of the regions which would contribute 
#' to each region of the \emph{cover}.}
#' \item{summit: returns regions that start from a position
#' where the number of intersecting regions is not increasing afterwards and
#' stops at a position where either the number of intersecting regions 
#' decreases, or it violates the max accumulation index.}
#' \item{histogram: returns the non-overlapping regions contributing to 
#' the cover, each with its accumulation index value, which is assigned to 
#' the AccIndex region attribute.}
#' \item{cover: default value.}
#' }
#' 
#' @return GMQLDataset class object. It contains the value to use as input 
#' for the subsequent GMQL function
#' 
#' @examples
#' 
#' ## This statement produces an output dataset with a single output sample. 
#' ## The COVER operation considers all areas defined by a minimum 
#' ## of two overlapping regions in the input samples, 
#' ## up to any amount of overlapping regions.
#' 
#' init_gmql()
#' test_path <- system.file("example","DATASET",package = "RGMQL")
#' exp = read_dataset(test_path)
#' res = cover(exp, 2, "ANY")
#'
#' \dontrun{
#' ## This GMQL statement computes the result grouping the input exp samples 
#' ## by the values of their cell metadata attribute, 
#' ## thus one output res sample is generated for each cell type; 
#' ## output regions are produced where at least 2 and at most 3 regions 
#' ## of grouped exp samples overlap, setting as attributes of the resulting 
#' ## regions the minimum pvalue of the overlapping regions (min_pvalue) 
#' ## and their Jaccard indexes (JaccardIntersect and JaccardResult).
#' 
#' test_path <- system.file("example", "DATASET", package = "RGMQL")
#' exp = read_dataset(test_path)
#' res = cover(exp, 2, 3, groupBy = list(DF("cell")), 
#' min_pValue = MIN("pvalue"))
#' }
#' 
#' @aliases cover, cover-method
#' @export
setMethod("cover", "GMQLDataset",
            function(data, min_acc, max_acc, groupBy = NULL, 
                    variation = "cover", ...)
            {
                val <- data@value
                q_max <- .check_cover_param(max_acc,FALSE)
                q_min <- .check_cover_param(min_acc,FALSE)
                flag = toupper(variation)
                aggregates = list(...)
                gmql_cover(val, q_min, q_max, groupBy, aggregates, flag)
            })



gmql_cover <- function(data, min_acc, max_acc, groupBy = NULL, 
                            aggregates = NULL, flag)
{
    
    if(!is.null(groupBy))
        join_condition_matrix <- .jarray(.join_condition(groupBy),
                                            dispatch = TRUE)
    else
        join_condition_matrix <- .jnull("java/lang/String")

    if(!is.null(aggregates) && !length(aggregates) == 0)
        metadata_matrix <- .jarray(.aggregates(aggregates,"AGGREGATES"),
                                    dispatch = TRUE)
    else
        metadata_matrix <- .jnull("java/lang/String")

    WrappeR <- J("it/polimi/genomics/r/Wrapper")
    response <- switch(flag,
        "COVER" = WrappeR$cover(min_acc, max_acc, join_condition_matrix,
                                    metadata_matrix, data),
        "FLAT" = WrappeR$flat(min_acc, max_acc, join_condition_matrix,
                                    metadata_matrix, data),
        "SUMMIT" = WrappeR$summit(min_acc,max_acc, join_condition_matrix,
                                    metadata_matrix, data),
        "HISTOGRAM" = WrappeR$histogram(min_acc, max_acc, 
                        join_condition_matrix, metadata_matrix, data))
    if(is.null(response))
        stop("no admissible variation: cover, flat, summit, histogram")
    
    error <- strtoi(response[1])
    data <- response[2]
    if(error!=0)
        stop(data)
    else
        GMQLDataset(data)
}

.check_cover_param <- function(param, is_min)
{
    if(length(param)>1)
        stop("length > 1")

    if(is.numeric(param))
    {
        if(param<=0)
            stop("No negative value")
        else
            return(as.character(param))
    }
    else if(is.character(param))
    {
        if(is.na(as.numeric(param)))
        {
            if(is_min && identical(param,"ANY"))
                stop("min cannot assume ANY as value")
            
            if(!identical(param,"ANY") && !identical(param,"ALL"))
                stop("invalid input data")
        }
        return(param)
    }
    else
        stop("invalid input data")
    
}

.trasform_cover <- function(predicate=NULL)
{
    predicate <- gsub("\\(\\)","",predicate)
}

