############################
#       ORDERING          #
###########################


#' Ordering functions
#'
#' These functions are used to create a series of metadata as string
#' that require ordering on value; it is used only in arrange method
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
#' ## "data" using CustomParser
#' 
#' init_gmql()
#' test_path <- system.file("example", "DATASET", package = "RGMQL")
#' data = read_gmql(test_path)
#' 
#' ## This statement orders the samples according to the Region_Count metadata 
#' ## attribute and takes the two samples that have the lowest count. 
#'
#' asc = arrange(data, list(ASC("Region_Count")), fetch_opt = "mtop", 
#'     num_fetch = 2)
#' 
#' ## This statement orders the regions of each samples according to their 
#' ## pvalue attribute value and in each sample it takes the first seven
#' ## regions with the highest pvalue
#'  
#' desc = arrange(data, regions_ordering = list(DESC("pvalue")), 
#'     reg_fetch_opt = "rtop", reg_num_fetch = 7)
#' 
#' @name Ordering-Functions
#' @aliases DESC
#' @rdname ordering-class
#' @export
#'
DESC <- function(...) {
  ords <- c(...)
  ords = ords[!ords %in% ""]
  ords = ords[!duplicated(ords)]
  if(!length(ords))
    order_matrix <- .jnull("java/lang/String")
  else {
    order_matrix <- t(vapply(ords, function(x) {
      new_value = c("DESC",x)
      matrix <- matrix(new_value)
    },character(2)))
  }
  order_matrix
}

#' @name Ordering-Functions
#' @aliases ASC
#' @rdname ordering-class
#' @export
#'
ASC <- function(...) {
  ords <- c(...)
  ords = ords[!ords %in% ""]
  ords = ords[!duplicated(ords)]
  if(!length(ords))
    order_matrix <- .jnull("java/lang/String")
  else {
    order_matrix <- t(vapply(ords, function(x) {
      new_value = c("ASC",x)
      matrix <- matrix(new_value)
    },character(2)))
  }
  order_matrix
}
