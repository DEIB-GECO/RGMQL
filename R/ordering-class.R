############################
#       ORDER              #
############################


ORDER <- function(value)
{
    op_list <- list(value = value)
    ## Set the name for the class
    class(op_list) <- "ORDER"
    return(op_list)
}

check.ORDER <- function(value)
{
    if(is.character(value) && length(value)>1)
        stop("value: no multiple string")
    
    if(!is.character(value))
        stop("value: is not a string")
}

print.ORDER <- function(obj) {
    as.character(as.character.ORDER(obj))
}

as.character.ORDER <- function(obj) {
    class <- class(obj)[1]
    val <- obj$value
    c(class,val)
}


#' ORDER object class constructor
#'
#' This class constructor is used to create instances of ORDER object,
#' to be used in GMQL functions that require ordering on value.
#' 
#' \itemize{
#' \item{ASC: It defines a ascending order for input value}
#' \item{DESC: It defines a descending order for input value}
#' }
#' 
#' @param value string identifying name of metadata or region attribute
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
#' desc = sort(r,TRUE, c("Region_Count"), fetch_opt = "mtop", 
#' num_fetch = 2)
#'  
#' asc = sort(r,TRUE, list(ASC("Region_Count")),list(DESC("score")),
#' fetch_opt = "mtop", num_fetch = 5, reg_fetch_opt = "rtop", 
#' reg_num_fetch = 7)
#' 
#' @name ORDERING
#' @rdname ordering-class
#' @export
#'
DESC <- function(value)
{
    check.ORDER(value)
    list <- list(value = value)
    ## Set the name for the class
    class(list) <- c("DESC","ORDER")
    return(list)
}

#' @name ORDERING
#' @rdname ordering-class
#' @export
#'
ASC <- function(value)
{
    check.ORDER(value)
    list <- list(value = value)
    ## Set the name for the class
    class(list) <- c("ASC","ORDER")
    return(list)
}
