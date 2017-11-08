#############################
#       PARAMETER          #
############################

PARAMETER <- function()
{
    op_list <- list()
    ## Set the name for the class
    class(op_list) <- "PARAMETER"
    return(op_list)
}

as.character.PARAMETER <- function(obj) {
    class <- class(obj)[1]
}

print.PARAMETER <- function(obj){
    print(as.character.PARAMETER(obj))
}


#' PARAM object class constructor
#'
#' This class constructor is used to create instances of PARAM object
#' to be used in GMQL functions
#' It defines the minimum (and/or maximum) to the number of samples 
#' in the input dataset;
#' 
#' @return ALL param object
#'
#' @seealso \code{\link{ANY}}
#' 
#' @examples
#' 
#' ## This statement produces an output dataset with a single output sample. 
#' ## The COVER operation considers all areas defined by a minimum 
#' ## of two overlapping regions in the input samples, 
#' ## up to maximum amount of overlapping regions.
#' 
#' init_gmql()
#' test_path <- system.file("example", "DATASET", package = "RGMQL")
#' exp = read_dataset(test_path)
#' res = cover(input_data = exp, 2, ALL())
#' 
#' @export
#'
ALL <- function()
{
    list <- list()
    ## Set the name for the class
    class(list) <- c("ALL","PARAMETER")
    return(list)
}


#' PARAM object class constructor
#'
#' This class constructor is used to create instances of PARAM object
#' to be used in GMQL functions
#' It defines any amount of overlapping regions to be considered.
#' 
#' @return ANY param object
#'
#' @seealso \code{\link{ALL}}
#' 
#' @examples
#' 
#' ## This statement produces an output dataset with a single output sample. 
#' ## The COVER operation considers all areas defined by a minimum 
#' ## of two overlapping regions in the input samples, 
#' ## up to any amount of overlapping regions.
#' 
#' init_gmql()
#' test_path <- system.file("example", "DATASET", package = "RGMQL")
#' exp = read_dataset(test_path)
#' res = cover(input_data = exp, 2, ANY())
#'
#'
#' @export
#'
ANY <- function()
{
    list <- list()
    ## Set the name for the class
    class(list) <- c("ANY","PARAMETER")
    return(list)
}
