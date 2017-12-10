############################
#       OPERATOR           #
############################


OPERATOR <- function(value)
{
    op_list <- list(value = value)
    ## Set the name for the class
    class(op_list) <- "OPERATOR"
    return(op_list)
}

check.OPERATOR <- function(value)
{
    if(!is.null(value))
    {
        if(is.character(value) && length(value)>1)
            stop("value: no multiple string")
        
        if(!is.character(value))
            stop("value: is not a string")
    }
}

print.OPERATOR <- function(obj) {
    as.character(obj)
}

as.character.OPERATOR <- function(obj) {
    class <- class(obj)[1]
    val <- obj$value
    c(class,val)
}

#' OPERATOR object class constructor
#' 
#' This class constructor is used to create instances of OPERATOR object,
#' to be used in GMQL functions that require operator on value.
#' 
#' \itemize{
#' \item{META: It prepares input parameter to be passed to library function 
#' meta, performing all the type conversions needed}
#' \item{SQRT: It prepares input parameter to be passed to library function 
#' sqrt, performing all the type conversions needed}
#' \item{NIL: It prepares input parameter to be passed to library function 
#' null, performing all the type conversions needed}
#' }
#' 
#' @param value string identifying name of metadata attribute
#' @param type string identifying the type of the attribute value;
#' it must be: INTEGER, DOUBLE or STRING
#'
#' @return Operator object
#' 
#' 
#' @examples
#' ## Thi statement initializes and runs the GMQL server for local execution 
#' ## and creation of results on disk. Then, with system.file() it defines 
#' ## the path to the folders "DATASET" in the subdirectory "example" 
#' ## of the package "RGMQL" and opens such folder as a GMQL dataset 
#' ## named "exp"
#' 
#' init_gmql()
#' test_path <- system.file("example", "DATASET", package = "RGMQL")
#' exp = read_dataset(test_path)
#' 
#' ## This statement allows to select, in all input sample, all those regions 
#' ## for which the region attribute score has a value which is greater 
#' ## than the metadata attribute value "avg_score" in their sample.
#' 
#' data = filter(exp, r_predicate = score > META("avg_score"))
#' 
#' ## This statement defines a new numeric region attribute with "null" value. 
#' ## The syntax for creating a new attribute with null value is 
#' ## attribute_name = NULL(TYPE), where type may be INTEGER, DOUBLE or STRING.
#' 
#' out = select(exp, regions_update = list(signal = NIL("INTEGER"), 
#' pvalue = NIL("DOUBLE")))
#' 
#' ## This statement allows to build an output dataset out such that all 
#' ## the samples from the input dataset exp are conserved, 
#' ## as well as their region attributes (and their values) 
#' ## and their metadata attributes (and their values). 
#' ## The new metadata attribute concSq is added to all output samples 
#' ## with value correspondent to the mathematical squared root 
#' ## of the pre-existing metadata attribute concentration.
#' 
#' out = select(exp, metadata_update = list(concSq = SQRT("concentration")))
#' 
#' @name OPERATOR-Object
#' @aliases META
#' @rdname operator-class
#' @export
#'
META <- function(value, type = NULL)
{
    check.OPERATOR(value)
    if(!is.null(type))
        check.OPERATOR(type)
    
    list <- list(value = value,type = type)
    ## Set the name for the class
    class(list) <- c("META","OPERATOR")
    return(list)
}
print.META <- function(obj) {
    as.character(obj)
}
as.character.META <- function(obj) {
    class <- class(obj)[1]
    val <- obj$value
    type <- obj$type
    c(class,val,type)
}

check.META <- function(type)
{
    check.OPERATOR(value)
    value <- toupper(value)
    if(!identical(value,"DOUBLE") && !identical(value,"INTEGER") &&
            !identical(value,"STRING"))
        stop("only DOUBLE or INTEGER or STRING")
}


#' @name OPERATOR-Object
#' @aliases NIL
#' @rdname operator-class
#' @export
#'
NIL <- function(type)
{
    check.NIL(type)
    
    list <- list(value = type)
    ## Set the name for the class
    class(list) <- c("NIL","OPERATOR")
    return(list)
}

check.NIL <- function(value)
{
    check.OPERATOR(value)
    value <- toupper(value)
    if(!identical(value,"DOUBLE") && !identical(value,"INTEGER"))
        stop("only DOUBLE or INTEGER")
    
}

#' @name OPERATOR-Object
#' @aliases SQRT
#' @rdname operator-class
#' @export
#'
SQRT <- function(value)
{
    check.OPERATOR(value)
    
    list <- list(value = value)
    ## Set the name for the class
    class(list) <- c("SQRT","OPERATOR")
    return(list)
}

