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
#' to be used in GMQL cover method
#' 
#' \itemize{
#' \item{ALL: It represents the number of samples in the input dataset.}
#' \item{ANY: It represents any amount of overlapping regions to be 
#' considered.}
#' }
#' 
#' @return Param object
#'
#' @examples
#' 
#' ## This statement initializes and runs the GMQL server for local execution 
#' ## and creation of results on disk. Then, with system.file() it defines 
#' ## the path to the file "DATASET" in the subdirectory "example"
#' ## of the package "RGMQL" and opens such file as a GMQL dataset named "exp" 
#' ## using customParser
#' 
#' init_gmql()
#' test_path <- system.file("example", "DATASET", package = "RGMQL")
#' exp = read_gmql(test_path)
#' 
#' ## The following statement produces an output dataset with a single 
#' ## output sample. The COVER operation considers all areas defined by 
#' ## a minimum of two overlapping regions in the input samples, 
#' ## up to maximum amount of overlapping regions equal to the number 
#' ## of input samples.
#' 
#' res = cover(exp, 2, ALL())
#' 
#' ## The following statement produces an output dataset with a single 
#' ## output sample. The COVER operation considers all areas defined by 
#' ## a minimum of two overlapping regions in the input samples, 
#' ## up to any amount of overlapping regions.
#' 
#' res = cover(exp, 2, ANY())
#' 
#' ## The following statement produces an output dataset with a single 
#' ## output sample. The COVER operation considers all areas defined by 
#' ## minimum of overlapping regions in the input samples equal to half of 
#' ## the number of input samples, up to any amount of overlapping regions.
#' 
#' res = cover(exp, ALL()/2, ANY())
#' 
#' @name Cover-Param
#' @aliases ALL
#' @rdname cover-param-class
#' @export
#'
ALL <- function()
{
    list <- list()
    ## Set the name for the class
    class(list) <- c("ALL","PARAMETER")
    return(list)
}

#' @name Cover-Param
#' @aliases ANY
#' @rdname cover-param-class
#' @export
#'
ANY <- function()
{
    list <- list()
    ## Set the name for the class
    class(list) <- c("ANY","PARAMETER")
    return(list)
}
