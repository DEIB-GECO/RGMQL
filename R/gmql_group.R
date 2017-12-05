#' Method group_by
#' 
#' 
#' @importFrom rJava J .jarray .jnull
#' 
#' 
#' @param .data GMQLDataset object
#'
#' @return GMQLDataset object. It contains the value to use as input 
#' for the subsequent GMQLDataset method
#'
#' @examples
#' 
#' 
#' 
#' @aliases group_by group_by-method
#' @export
setMethod("group_by","GMQLDataset",
          function(.data, groupBy_meta = NULL, groupBy_regions = NULL, 
            region_aggregates = NULL, meta_aggregates = NULL)
          {
              ptr_data = .data@value
              gmql_group(ptr_data, groupBy_meta, groupBy_regions, 
                            region_aggregates, meta_aggregates)
          })

gmql_group <- function(input_data, group_meta, group_reg)
{
    
    if(!is.null(group_meta))
    {
        cond <- .join_condition(group_meta)
        join_condition_matrix <- .jarray(cond, dispatch = TRUE)
    }
    else
        join_condition_matrix <- .jnull("java/lang/String")
    
    if(!is.null(group_reg))
    {
        if(!is.character(group_reg))
            stop("metadata: no valid input")
        
        group_reg <- group_reg[!group_reg %in% ""]
        group_reg <- group_reg[!duplicated(group_reg)]
        
        if(length(group_reg)==0)
            group_reg <- .jnull("java/lang/String")
        
        group_reg <- .jarray(metadata)
    }
    else
        group_reg <- .jnull("java/lang/String")
    
    if(!is.null(meta_aggregates) && !length(meta_aggregates) == 0)
    {
        aggr <- .aggregates(meta_aggregates,"AGGREGATES")
        metadata_matrix <- .jarray(aggr, dispatch = TRUE)
    }
    else
        metadata_matrix <- .jnull("java/lang/String")
    
    if(!is.null(region_aggregates) && !length(region_aggregates) == 0)
    {
        aggr <- .aggregates(region_aggregates,"AGGREGATES")
        region_matrix <- .jarray(aggr, dispatch = TRUE)
    }
    else
        region_matrix <- .jnull("java/lang/String")
    
    WrappeR <- J("it/polimi/genomics/r/Wrapper")
    response <- WrappeR$group(join_condition_matrix, metadata_matrix, 
                                groupBy_regions, region_matrix, input_data)
    error <- strtoi(response[1])
    data <- response[2]
    if(error!=0)
        stop(data)
    else
        GMQLDataset(data)
}
