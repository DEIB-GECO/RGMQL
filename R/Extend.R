#' GMQL Operation: EXTEND
#'
#' It generates new metadata attributes as result of aggregate functions applied to sample region attributes
#' and adds them to the existing metadata attributes of the sample.
#' Aggregate functions are applied sample by sample.
#'
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
#' @return "url-like" string
#' for example dataset0/select1/extend4
#'
#' @references \url{http://www.bioinformatics.deib.polimi.it/genomic_computing/GMQL/doc/GMQLUserTutorial.pdf}
#'
#' @examples
#'
#' \dontrun{
#'
#' initGMQL("gtf")
#' test_path <- system.file("example","DATA_SET_VAR_GTF",package = "GMQL")
#' r = read(test_path)
#'
#' ### new metadata sum,c and m are added in metadata sample
#' e = extend(input_data = r, list(sum = SUM("pvalue"),c = COUNT(), m = AVG("score")))
#' }
#' ""
#' @export
#'
extend <-function(input_data, metadata = NULL)
{
  if(!is.null(metadata))
    metadata_matrix <- .aggregates(metadata,"META_OPERATOR")
  else
    metadata_matrix <- NULL

  out <- WrappeR$extend(metadata_matrix,input_data)

  if(grepl("No",out,ignore.case = TRUE))
    stop(out)
  else
    out
}

