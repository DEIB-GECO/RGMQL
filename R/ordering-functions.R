############################
#       ORDERING          #
###########################


#' Ordering functions
#'
#' These functions is used to create a series of metadata as string
#' that require ordering on value; is used only in arrange method.
#' (see example)
#' 
#' \itemize{
#' \item{ASC: It defines a ascending order for input value}
#' \item{DESC: It defines a descending order for input value}
#' }
#' 
#' @param ... series of metatdata as string
#'
#' @return ordering object
#' 
#' @examples
#' 
#' init_gmql()
#' test_path <- system.file("example", "DATASET", package = "RGMQL")
#' r = read_dataset(test_path)
#' 
#' ## It orders the samples according to the Region_count metadata attribute 
#' ## and takes the two samples that have the highest count. 
#'
#' desc = arrange(r,list(ASC("Region_Count")), fetch_opt = "mtop", 
#' num_fetch = 2)
#'  
#' asc = arrange(r, list(ASC("Region_Count")),list(DESC("score")),
#' fetch_opt = "mtop", num_fetch = 5, reg_fetch_opt = "rtop", 
#' reg_num_fetch = 7)
#' 
#' @name DESC
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

#' @name ASC
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
