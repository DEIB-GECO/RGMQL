#' GMQL operation: ORDER
#'
#' It is used to order either samples or sample regions or both, according to 
#' a set of metadata and/or region attributes, and/or region coordinates.
#' Order can be specified as ascending / descending for every attribute
#' The number of samples and their regions remain the same 
#' (unless mtop/rtop parameters specified) but a new ordering metadata 
#' and/or region attribute is added.
#' Sorted samples or regions have a new attribute "order", 
#' added to either metadata, or regions, or both of them as specified in input
#' The input mtop = k and rtop = m extracts the first k samples 
#' and m regions respectively, the clause mtopg = k and rtopg = m 
#' performs grouping operation, grouping by identical values 
#' of ordering attributes and then selects the first k samples 
#' or regions of each group
#'
#' @importFrom rJava J
#' @importFrom rJava .jnull
#' @importFrom rJava .jarray
#' 
#' @param input_data returned object from any GMQL function
#' @param metadata_ordering list of ORDER objects where every object 
#' contains the name of metadata.
#' The ORDER's available are: \code{\link{ASC}}, \code{\link{DESC}}
#' Every condition accepts only one string value. (e.g. ASC("cell_type") )
#' @param mtop integer value specifying the first k samples to retrieve.
#' default is 0 that means every sample must be considered
#' @param mtopg integer value specifying the first k samples to retrieve 
#' in each group. 
#' default is 0 that means every sample must be considered
#' @param mtopp integer value specifying the percentage of samples to retrieve.
#' default is 0 that means every sample must be considered
#' @param regions_ordering list of ORDER objects where every object contains 
#' the name of region schema value.
#' The ORDER's available are: ASC, DESC.
#' Every condition accepts only one string value. (e.g. DESC("pvalue") )
#' @param rtop integer value specifying the first k regions in each group.
#' default is 0 that means every sample must be considered
#' @param rtopg integer value specifying the first k regions to retrieve 
#' in each group.
#' default is 0 that means every sample must be considered
#' @param rtopp integer value specifying the percentage of regions to retrieve.
#' default is 0 that means every sample must be considered
#'
#' @return DataSet class object. It contains the value to use as input 
#' for the subsequent GMQL function
#' 
#' @details
#' mtop, mtopg,mtopp, rtop, rtopg and rtopp are normally numbers: 
#' if you specify a vector, only the first element will be used
#' mtop and mtopg and mtopp are mutalbe exclusive, so rtop and rtopg and rtopp
#'
#'
#' @examples
#' 
#' ## It orders the samples according to the Region_count metadata attribute 
#' ## and takes the two samples that have the highest count. 
#'
#' init_gmql()
#' test_path <- system.file("example","DATASET",package = "RGMQL")
#' r = read_dataset(test_path)
#' o = order(r,list(DESC("Region_Count")), mtop = 2)
#'
#' @export
#'
order <- function(input_data, metadata_ordering = NULL, mtop = 0, mtopg = 0, 
                    mtopp = 0, regions_ordering = NULL, rtop = 0, rtopg = 0,
                    rtopp = 0)
{
    if(!is.numeric(mtop) || !is.numeric(mtopg) || !is.numeric(rtop) || 
        !is.numeric(rtopg) || !is.numeric(mtopp)|| !is.numeric(rtopp))
        stop("mtop, rtop, rtopg and mtopg must be integer")
    
    if(length(mtop)>1 || length(mtopg)>1 || length(rtop)>1 || length(rtopg)>1
        || length(mtopp)>1 || length(rtopp)>1)
        warning("only first element: rtop, mtop, mtopg, rtopg, rtopp, mtopp")
    
    # we consider only the first element even if input is a vector of Int
    # we cut the other arguments
    
    mtop = as.integer(mtop[1])
    mtopg = as.integer(mtopg[1])
    mtopp = as.integer(mtopp[1])
    
    rtop = as.integer(rtop[1])
    rtopg = as.integer(rtopg[1])
    rtopp = as.integer(rtopp[1])
    
    if(mtop > 0 && mtopg >0)
    {
        warning("cannot be used together.\nWe set mtopg = 0")
        mtopg = 0L
    }

    if(mtop >0 && mtopp>0)
    {
        warning("cannot be used together.\nWe set mtopp = 0")
        mtopp = 0L
    }
    
    if(mtopg >0 && mtopp>0)
    {
        warning("cannot be used together.\nWe set mtopp = 0")
        mtopp = 0L
    }
    
    if(rtop > 0 && rtopg >0)
    {
        warning("cannot be used together.\nWe set rtopg = 0")
        rtopg = 0L
    }
    
    if(rtop >0 && rtopp>0)
    {
        warning("cannot be used together.\nWe set rtopp = 0")
        rtopp = 0L
    }
    
    if(rtopg >0 && rtopp>0)
    {
        warning("cannot be used together.\nWe set rtopp = 0")
        rtopp = 0L
    }
    
    if(!is.null(metadata_ordering))
        meta_matrix <- .jarray(.ordering_meta(metadata_ordering),
                                    dispatch = TRUE)
    else
        meta_matrix <- .jnull("java/lang/String")
    
    if(!is.null(regions_ordering))
        region_matrix <- .jarray(.ordering_meta(regions_ordering),
                                    dispatch = TRUE)
    else
        region_matrix <- .jnull("java/lang/String")
    
    WrappeR <- J("it/polimi/genomics/r/Wrapper")
    response <- WrappeR$order(meta_matrix, mtopg, mtop, mtopp, region_matrix,
                                rtopg, rtop, rtopp, input_data$value)
    error <- strtoi(response[1])
    data <- response[2]
    if(error!=0)
        stop(data)
    else
        DataSet(data)
}


.ordering_meta <- function(ordering)
{
    if(is.list(ordering))
    {
        order_matrix <- t(sapply(ordering,function(x){
            new_value <- as.character(x)
            if(length(new_value)==1)
                new_value = c("ASC",new_value)
            else if(!identical("ASC",new_value[1]) && 
                    !identical("DESC",new_value[1]))
                stop("no more than one value")
            matrix <- matrix(new_value)
        }))
    }
    else if(is.character(ordering))
    {
        order_matrix <- t(sapply(ordering, function(x) {
            new_value = c("ASC",x)
            matrix <- matrix(new_value)
        }))
    }
    else
        stop("only list or character")
    
}


