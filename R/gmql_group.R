group_by.GMQLDateset <- function(.data, groupBy_meta = NULL, 
    groupBy_regions = NULL, region_aggregates = NULL, meta_aggregates = NULL)
{
    ptr_data = .data@value
    gmql_group(ptr_data, groupBy_meta, groupBy_regions, region_aggregates, 
                meta_aggregates)
}

#' Method group_by
#' 
#' @description Wrapper to GMQL GROUP operator
#' 
#' @importFrom rJava J .jarray .jnull
#' @importFrom dplyr group_by
#' 
#' @param .data GMQLDataset object
#' @param groupBy_meta it define condition evaluation on metadata.
#' \itemize{
#' \item{\code{\link{FN}}: Fullname evaluation, two attributes match 
#' if they both end with value and, if they have a further prefixes,
#' the two prefix sequence are identical}
#' \item{\code{\link{EX}}: Exact evaluation, only attributes exactly 
#' as value will match; no further prefixes are allowed. }
#' \item{\code{\link{DF}}: Default evaluation, the two attributes match 
#' if both end with value.}
#' }
#' @param groupBy_regions vector of string made up by schema field attribute
#' @param region_aggregates It accept a series of aggregate function on 
#' region attribute. 
#' All the element in the form \emph{key} = \emph{aggregate}.
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
#' @param meta_aggregates It accept a series of aggregate function on 
#' metadata attribute.
#' All the element in the form \emph{key} = \emph{aggregate}.
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
#' \item list of key-value pairs: e.g. sum = SUM("cell")
#' \item list of values: e.g. SUM("cell")
#' }
#' "mixed style" is not allowed
#' 
#' 
#' @return GMQLDataset object. It contains the value to use as input 
#' for the subsequent GMQLDataset method
#'
#' @examples
#' 
#' @name group_by
#' @rdname group_by
#' @aliases group_by,GMQLDataset-method
#' @aliases group_by-method
#' @export
setMethod("group_by","GMQLDataset",group_by.GMQLDateset)

gmql_group <- function(input_data, group_meta, group_reg, region_aggregates, 
                            meta_aggregates)
{
    if(!is.null(group_meta))
    {
        if("condition" %in% names(group_meta))
        {
            cond <- .join_condition(group_meta)
            if(is.null(cond))
                join_matrix <- .jnull("java/lang/String")
            else
                join_matrix <- .jarray(cond, dispatch = TRUE)
        }
        else
            stop("use function condition_evaluation()")
    }
    else
        join_matrix <- .jnull("java/lang/String")
    
    
    if(!is.null(group_reg))
    {
        if(!is.character(group_reg))
            stop("metadata: no valid input")
        
        group_reg <- group_reg[!group_reg %in% ""]
        group_reg <- group_reg[!duplicated(group_reg)]
        
        if(!length(group_reg))
            group_reg <- .jnull("java/lang/String")
        
        group_reg <- .jarray(metadata)
    }
    else
        group_reg <- .jnull("java/lang/String")
    
    if(!is.null(meta_aggregates) && length(meta_aggregates))
    {
        aggr <- .aggregates(meta_aggregates,"AGGREGATES")
        metadata_matrix <- .jarray(aggr, dispatch = TRUE)
    }
    else
        metadata_matrix <- .jnull("java/lang/String")
    
    if(!is.null(region_aggregates) && length(region_aggregates))
    {
        aggr <- .aggregates(region_aggregates,"AGGREGATES")
        region_matrix <- .jarray(aggr, dispatch = TRUE)
    }
    else
        region_matrix <- .jnull("java/lang/String")
    
    WrappeR <- J("it/polimi/genomics/r/Wrapper")
    response <- WrappeR$group(join_matrix, metadata_matrix, group_reg, 
                                region_matrix, input_data)
    error <- strtoi(response[1])
    val <- response[2]
    if(error!=0)
        stop(val)
    else
        GMQLDataset(val)
}
