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
#' @param x GMQLDataset class object
#' @param decreasing logical value indicating the sorting for both metadata
#' and regions.
#' if you define \emph{metadata_ordering} or \emph{regions_ordering} 
#' defyinig order objects, this value is discarded 
#' @param metadata_ordering list of order objects where every object 
#' contains the name of metadata.
#' The ORDER's available are: \code{\link{ASC}}, \code{\link{DESC}}
#' Every condition accepts only one string value. (e.g. ASC("cell_type") )
#' 
#' @param fetch_opt string indicating the option used to fetch the 
#' first k sample:
#' \itemize{
#' \item{mtop: it fetch the first k sample}
#' \item{mtopp: it fetch the first k sample in each group.}
#' \item{mtopg: it fetch the percentage of sample.}
#' }
#' if NULL, \emph{num_fetch} is not considered 
#' 
#' @param num_fetch integer value identifying the number of region to fetch
#' by default is 0, that's means all sample are fetched
#' s
#' @param regions_ordering list of ORDER objects where every object contains 
#' the name of region schema value.
#' The ORDER's available are: \code{\link{ASC}}, \code{\link{DESC}}.
#' Every condition accepts only one string value. (e.g. DESC("pvalue") )
#' 
#' @param reg_fetch_opt string indicating the option used to fetch the 
#' first k regions:
#' \itemize{
#' \item{rtop: it fetch the first k regions.}
#' \item{rtopp: it fetch the first k regions in each group.}
#' \item{rtopg: it fetch the percentage of regions.}
#' }
#' if NULL, \emph{reg_num_fetch} is not considered 
#' 
#' @param reg_num_fetch integer value identifying the number of region to fetch
#' by default is 0, that's means all regions are fetched
#' 
#' 
#' @return DataSet class object. It contains the value to use as input 
#' for the subsequent GMQL function
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
#' o = sort(r,TRUE, c("Region_Count"),fetch_opt= "mtop",num_fetch = 2)
#' 
#' 
#' \dontrun{
#' 
#' ## the same result is obtained with using GMQL-syntax like:
#' ## in this case decreasing parameter TRUE is not considerd
#' o = sort(r,TRUE, c(DESC("Region_Count")), fetch_opt = "mtop", 
#' num_fetch = 2)
#' 
#' }
#' @name sort
#' @rdname sort-methods
#' @aliases sort, GMQLDataset-methods
#' @export
setMethod("sort", "GMQLDataset",
            function(x, decreasing = FALSE, metadata_ordering = NULL, 
                    regions_ordering = NULL, fetch_opt = NULL, 
                    num_fetch = 0, reg_fetch_opt = NULL, reg_num_fetch = 0)
            {
                gmql_order(x@value, decreasing, metadata_ordering, 
                            regions_ordering, fetch_opt, num_fetch, 
                            reg_fetch_opt, reg_num_fetch)
            })

gmql_order <- function(data, decreasing, metadata_ordering, regions_ordering,
                    fetch_opt, num_fetch, reg_fetch_opt, reg_num_fetch)
{
    if(!is.null(fetch_opt))
        fetch_opt <- .check_option(fetch_opt)
    else
        fetch_opt <- .jnull("java/lang/String")
    
    if(!is.null(num_fetch))
        .check_opt_value(num_fetch)
    else
        num_fetch <- 0
    
    if(!is.null(reg_num_fetch))
        .check_opt_value(reg_num_fetch)
    else
        reg_num_fetch <- 0  
    
    if(!is.null(reg_fetch_opt))
        reg_fetch_opt <- .check_option(reg_fetch_opt)
    else
        reg_fetch_opt <- .jnull("java/lang/String")
    
    if(!is.null(metadata_ordering))
    {
        meta_matrix <- .ordering_meta(metadata_ordering, decreasing)
        meta_matrix <- .jarray(meta_matrix, dispatch = TRUE)
    }
    else
        meta_matrix <- .jnull("java/lang/String")
    
    if(!is.null(regions_ordering))
    {
        region_matrix <- .ordering_meta(regions_ordering, decreasing)
        region_matrix <- .jarray(region_matrix, dispatch = TRUE)
    }
    else
        region_matrix <- .jnull("java/lang/String")
    
    WrappeR <- J("it/polimi/genomics/r/Wrapper")
    response <- WrappeR$order(meta_matrix, fetch_opt, as.integer(num_fetch), 
                        reg_fetch_opt, as.integer(reg_num_fetch), 
                        region_matrix, data)
    error <- strtoi(response[1])
    data <- response[2]
    if(error!=0)
        stop(data)
    else
        GMQLDataset(data)
}


.ordering_meta <- function(ordering, decreasing)
{
    if(is.list(ordering))
    {
        order_matrix <- t(mapply(function(x,dec){
            new_value <- as.character(x)
            if(length(new_value)==1 && dec == FALSE)
                new_value = c("ASC",new_value)
            else if(length(new_value)==1 && dec == TRUE)
                new_value = c("DESC",new_value)
            else if(!identical("ASC",new_value[1]) && 
                    !identical("DESC",new_value[1]))
                stop("no more than one value")
            matrix <- matrix(new_value)
        },ordering, decreasing))
    }
    else if(is.character(ordering))
    {
        order_matrix <- t(mapply(function(x,dec) {
            if( dec == FALSE)
                new_value = c("ASC",x)
            else
                new_value = c("DESC",x)
            matrix <- matrix(new_value)
        }, ordering, decreasing))
    }
    else
        stop("only list or character")
}

.check_option <- function(opt)
{
    opt <- tolower(opt)
    if(!identical("mtop",opt) && !identical("mtopp",opt) && 
                    !identical("mtopg",opt) && !identical("rtop",opt) && 
                    !identical("rtopp",opt) && !identical("rtopg",opt))
        stop("option not admissable")
    opt
}

.check_opt_value <- function(opt_value)
{
    if(!is.numeric(opt_value))
        stop("no valid data")
    
    if(length(opt_value)>1)
        stop("no multiple value")
}
