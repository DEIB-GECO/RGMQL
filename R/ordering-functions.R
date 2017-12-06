############################
#       ORDERING          #
###########################


#' Ordering functions
#'
#' These functions is used to create a series of metadata as string
#' that require ordering on value; is used only in arrange method
#' (see example).
#' 
#' \itemize{
#' \item{ASC: It defines an ascending order for input value}
#' \item{DESC: It defines a descending order for input value}
#' }
#' 
#' @param ... series of metatdata as string
#'
#' @return Ordering object
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
#' ## It orders the samples according to the Region_Count metadata attribute 
#' ## and takes the two samples that have the lowest count. 
#'
#' asc = arrange(data, list(ASC("Region_Count")), fetch_opt = "mtop", 
#' num_fetch = 2)
#' 
#' ## It orders the samples according to the pvalue schema attribute 
#' ## and takes the first five samples that have the lowest pvalue and the 
#' ## first seven regions.
#'  
#' desc = arrange(data, list(DESC("pvalue")), fetch_opt = "mtop", 
#' num_fetch = 5, reg_fetch_opt = "rtop", reg_num_fetch = 7)
#' 
#' @name Ordering-functions
#' @aliases DESC
#' @rdname ordering-class
#' @export
#'
DESC <- function(...)
{
    ords <- c(...)
    ords = ords[!ords %in% ""]
    ords = ords[!duplicated(ords)]
    if(length(ords)<=0)
        order_matrix <- .jnull("java/lang/String")
    else
    {
        order_matrix <- t(sapply(ords, function(x) {
            new_value = c("DESC",x)
            matrix <- matrix(new_value)
        }))
    }
    order_matrix
}

#' @name Ordering-functions
#' @aliases ASC
#' @rdname ordering-class
#' @export
#'
ASC <- function(...)
{
    ords <- c(...)
    ords = ords[!ords %in% ""]
    ords = ords[!duplicated(ords)]
    if(length(ords)<=0)
        order_matrix <- .jnull("java/lang/String")
    else
    {
        order_matrix <- t(sapply(ords, function(x) {
            new_value = c("ASC",x)
            matrix <- matrix(new_value)
        }))
    }
    order_matrix
}
