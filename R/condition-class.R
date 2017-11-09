#############################
#       CONDITION          #
############################


CONDITION <- function(value)
{
    op_list <- list(value = value)
    ## Set the name for the class
    class(op_list) <- "CONDITION"
    return(op_list)
}

as.character.CONDITION <- function(obj) {
    class <- class(obj)[1]
    val <- obj$value
    c(class,val)
}

print.CONDITION <- function(obj){
    print(as.character.CONDITION(obj))
}

c.CONDITION <- function(...){
    a <- list(...)
}

check.CONDITION <- function(value)
{
    if(!is.character(value) || length(value)>1)
        stop("value: no valid input or length > 1")
}


#' CONDITION object class constructor
#'
#' This class constructor is used to create instances of CONDITION object,
#' to be used in GMQL functions that require evaluation on value.
#' This constructor defines a EXACT evaluation of the input value.
#' EXACT evaluation: only attributes exactly as value will match; 
#' no further prefixes are allowed. 
#'
#' @param value string identifying name of metadata attribute 
#' to be evaluated
#'
#' @return EXACT condition object
#' 
#' @seealso \code{\link{FULL}}
#' 
#' @examples
#' 
#' init_gmql()
#' test_path <- system.file("example", "DATASET", package = "RGMQL")
#' r = read_dataset(test_path)
#' 
#' ## select with condition
#' ## the first and the third attribute are DEF the second one is EXACT
#' s = select(r, semi_join = list("cell_type", EXACT("cell"), 
#' "attribute_tag"), semi_join_dataset = r)
#'
#' \dontrun{
#'
#' ## select with default condition
#' ## the EXACT condition is treated as default due to coercion
#' s = select(r, semi_join = c("cell_type", "cell", EXACT("attribute_tag")),
#' semi_join_dataset = r)
#'
#' }
#'
#' @export
#'
EXACT <- function(value)
{
    check.CONDITION(value)

    list <- list(value = value)
    ## Set the name for the class
    class(list) <- c("EXACT","CONDITION")
    return(list)
}

#' CONDITION object class constructor
#'
#' This class constructor is used to create instances of CONDITION object
#' to be used in GMQL functions that require evaluation on value.
#' This constructor defines a FULL (FULLNAME) evaluation of the input value.
#' FULL evaluation: two attributes match if they both end with value and,
#' if they have further prefixes, the two prefix sequences are identical
#'
#' @param value string identifying name of metadata and/or 
#' region attribute to be evaluated
#' 
#' @return FULL condition object
#' 
#' @seealso \code{\link{EXACT}}
#' 
#' @examples
#' 
#' init_gmql()
#' test_path <- system.file("example", "DATASET", package = "RGMQL")
#' r = read_dataset(test_path)
#' 
#' ## select with condition
#' ## the first and the third attribute are default the second one is FULL
#' s = select(r, semi_join = list("cell_type", FULL("cell"), "attribute_tag"),
#' semi_join_dataset = r)
#'
#' \dontrun{
#'
#' ## select with default condition
#' ## Tthe FULL condition is treated as default due to coercion
#' s = select(r, semi_join = c("cell_type", "cell", FULL("attribute_tag")),
#' semi_join_dataset = r)
#'
#' }
#'
#' @export
#'
FULL <- function(value)
{
    check.CONDITION(value)

    list <- list(value = value)
    ## Set the name for the class
    class(list) <- c("FULL","CONDITION")
    return(list)
}

