#' GMQL Operation: EXTEND
#'
#' It generates new metadata attributes as result of aggregate functions applied to sample region attributes
#' and adds them to the existing metadata attributes of the sample.
#' Aggregate functions are applied sample by sample.
#'
#' @param input_data returned object from any GMQL function
#' @param metadata list of element in the form \emph{key} = \emph{function_aggregate}.
#' The \emph{function_aggregate} is an object of class OPERATOR
#' The aggregate functions available are: \code{\link{MIN}}, \code{\link{MAX}},
#' \code{\link{SUM}}, \code{\link{BAG}}, \code{\link{AVG}}, \code{\link{COUNT}},
#' \code{\link{STD}}, \code{\link{MEDIAN}}, \code{\link{Q1}}, \code{\link{Q2}}, \code{\link{Q3}}.
#' Every operator accepts a string value, execet for COUNT that cannot have a value.
#' Argument of 'function_aggregate' must exist in schema
#' Two style are allowed:
#' \itemize{
#' \item list of key-value pairs: e.g. sum = SUM("pvalue")
#' \item list of values: e.g. SUM("pvalue")
#' }
#' "mixed style" is not allowed
#'
#' @return DAGgraph class object. It contains the value associated to the graph used 
#' as input for the subsequent GMQL function
#'
#' @references \url{http://www.bioinformatics.deib.polimi.it/genomic_computing/GMQL/doc/GMQLUserTutorial.pdf}
#' 
#' @examples
#'
#' initGMQL("gtf")
#' test_path <- system.file("example","DATA_SET_VAR_GTF",package = "GMQL")
#' r = readDataset(test_path)
#'
#' ## it counts the regions in each sample and stores their number as value of the new metadata 
#' ## RegionCount attribute of the sample.
#' e = extend(input_data = r, list(RegionCount = COUNT()))
#' \dontrun{
#' 
#' initGMQL("gtf")
#' test_path <- system.file("example","DATA_SET_VAR_GTF",package = "GMQL")
#' exp = readDataset(test_path)
#'
#' ## it copies all samples of exp dataset into res dataset, and then calculates 
#' ## for each of them two new metadata attributes:
#' ##  1. RegionCount is the number of sample regions;
#' ##  2. MinP is the minimum pvalue of the sample regions.
#' ## res sample regions are the same as the ones in exp.
#' 
#' res = extend(input_data = exp, list(RegionCount = COUNT(),MinP = MIN(pvalue)))
#' 
#' }
#' 
#' @export
#'
extend <-function(input_data, metadata = NULL)
{
  if(!is.null(metadata))
    metadata_matrix <- .aggregates(metadata,"META_OPERATOR")
  else
    metadata_matrix <- scalaNull("Array[Array[String]]")

  out <- WrappeR$extend(metadata_matrix,input_data$value)

  if(grepl("No",out,ignore.case = TRUE))
    stop(out)
  else
    DAGgraph(out)
}

