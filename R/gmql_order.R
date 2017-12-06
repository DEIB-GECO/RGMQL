arrange.GMQLDataset <- function(.data, metadata_ordering = NULL, 
        regions_ordering = NULL, fetch_opt = NULL, num_fetch = 0, 
        reg_fetch_opt = NULL, reg_num_fetch = 0)
{
    ptr_data <- .data@value
    gmql_order(ptr_data, metadata_ordering, regions_ordering, 
                fetch_opt, num_fetch, reg_fetch_opt, reg_num_fetch)
}

#' Method arrange
#' 
#' @description Wrapper to GMQL ORDER operator
#' 
#' @description It is used to order either samples or sample regions or both, 
#' according to a set of metadata and/or region attributes.
#' Order can be specified as ascending / descending for every attribute. 
#' The number of samples and their regions remain the same as well as 
#' their attributes, (unless fetching options are specified) but a new 
#' ordering metadata and/or region attribute is added.
#' Sorted samples or regions have a new attribute "order", 
#' added to either metadata, or regions, or both of them as specified in inputs
#'
#' @importFrom rJava J .jnull .jarray
#' @importFrom dplyr arrange
#' 
#' @param .data GMQLDataset class object
#' @param metadata_ordering list of ordering functions containing name of 
#' metadata attribute.
#' The functions available are: \code{\link{ASC}}, \code{\link{DESC}}
#' 
#' @param fetch_opt string indicating the option used to fetch the 
#' first k samples; it can assume the values:
#' \itemize{
#' \item{mtop: it fetches the first k samples}
#' \item{mtopg: it fetches the percentage of samples.}
#' \item{mtopp: it fetches the first k samples in each group.}
#' 
#' }
#' if NULL, \emph{num_fetch} is not considered 
#' 
#' @param num_fetch integer value identifying the number of samples to fetch;
#' by default it is 0, that means all samples are fetched
#' s
#' @param regions_ordering list of ordering functions containing name of 
#' region attribute.
#' The functions available are: \code{\link{ASC}}, \code{\link{DESC}}.
#' 
#' @param reg_fetch_opt string indicating the option used to fetch the 
#' first k regions; it can assume the values:
#' \itemize{
#' \item{rtop: it fetches the first k regions.}
#' \item{rtopg: it fetches the first k percentage of regions.}
#' \item{rtopp: it fetches the first k regions in each group.}
#' }
#' if NULL, \emph{reg_num_fetch} is not considered 
#' 
#' @param reg_num_fetch integer value identifying the number of region to fetch
#' by default it is 0, that means all regions are fetched
#' 
#' @return GMQLDataset object. It contains the value to use as input 
#' for the subsequent GMQLDataset method
#'
#' @examples
#' 
#' ## This statement initializes and runs the GMQL server for local execution 
#' ## and creation of results on disk. Then, with system.file() it defines 
#' ## the path to the folder "DATASET" in the subdirectory "example"
#' ## of the package "RGMQL" and opens such file as a GMQL dataset named 
#' ## "data" using customParser
#' 
#' init_gmql()
#' test_path <- system.file("example", "DATASET", package = "RGMQL")
#' data = read_dataset(test_path)
#' 
#' ## The following statement orders the samples according to the Region_Count 
#' ## metadata attribute and takes the two samples that have the highest count. 
#'
#' o = arrange(data, list(ASC("Region_Count")), fetch_opt = "mtop", 
#' num_fetch = 2)
#' 
#' @name arrange
#' @rdname arrange
#' @aliases arrange,GMQLDataset-method
#' @aliases arrange-method
#' @export
setMethod("arrange", "GMQLDataset", arrange.GMQLDataset)

gmql_order <- function(data, metadata_ordering, regions_ordering,
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
        meta_matrix <- .ordering_meta(metadata_ordering)
        meta_matrix <- .jarray(meta_matrix, dispatch = TRUE)
    }
    else
        meta_matrix <- .jnull("java/lang/String")
    
    if(!is.null(regions_ordering))
    {
        region_matrix <- .ordering_meta(regions_ordering)
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


.ordering_meta <- function(ordering)
{
    order_matrix <- do.call(rbind, ordering)
    order_matrix
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
