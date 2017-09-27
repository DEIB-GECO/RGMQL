#' GMQL Operation: JOIN
#'
#' It takes in input two datasets, respectively known as nchor (left) and experiment (right) and returns
#' a dataset of samples consisting of regions extracted from the operands according to the specified condition
#' (a.k.a genometric_predicate).
#' The number of generated output samples is the Cartesian product of the number of samples
#' in the anchor and in the experiment dataset (if joinBy is not specified).
#' The output metadata are the union of the input metadata, with their attribute names prefixed with
#' left or right respectively.
#'
#'
#' @param left_input_data returned object from any GMQL function
#' @param right_input_data returned object from any GMQL function
#' @param genometric_predicate is a list of lists of DISTAL object by means of logical ANDs
#' @param joinBy list of CONDITION objects, or simple string concatenation 
#' (i.e c("cell_type","attribute_tag","size")).
#' Every object contains the name of metadata to be used in \emph{groupby}.
#' For details of CONDITION objects see:
#' \code{\link{DEF}}, \code{\link{FULL}}, \code{\link{EXACT}}
#' 
#' Every condition accepts only one string value. (e.g. DEF("cell_type") )
#' In case of single concatenation with no CONDITION, all metadata are considering as DEF
#' 
#' @param output single string that declare which region is given in output for each input pair of left dataset
#' right dataset regions satisfying the genometric predicate:
#' \itemize{
#' \item{left: outputs the anchor regions from left_input_data that satisfy the genometric predicate}
#' \item{right: outputs the experiment regions from right_input_data that satisfy the genometric predicate}
#' \item{int (intersection): outputs the overlapping part (intersection) of the left_input_data and right_input_data
#' regions that satisfy the genometric predicate; if the intersection is empty, no output is produced}
#' \item{contig: outputs the concatenation between the left_input_data and right_input_data regions that satisfy
#' the genometric predicate, (i.e. the output regionis defined as having left (right) coordinates
#' equal to the minimum (maximum) of the corresponding coordinate values in the left_input_data and right_input_data
#' regions satisfying the genometric predicate)}
#' }
#'
#' @return DAGgraph class object. It contains the value associated to the graph used 
#' as input for the subsequent GMQL function
#'
#' @references \url{http://www.bioinformatics.deib.polimi.it/genomic_computing/GMQL/doc/GMQLUserTutorial.pdf}
#'
#'
#' @examples
#' \dontrun{
#'
#' }
#'
#' @export
#'
join <- function(right_input_data, left_input_data, genometric_predicate = NULL,
                 joinBy = NULL, output="contig")
{
  if(!is.list(genometric_predicate))
    stop("genometric_predicate must be list of lists")

  if(!all(sapply(genometric_predicate, function(x) is.list(x) )))
    stop("genometric_predicate must be list of lists")

  lapply(genometric_predicate, function(list_pred) {
    if(length(list_pred)>4)
    {
      warning("only 4 element per list, we cut the rest")
      length(list_pred)=4
    }

    if(!all(sapply(list_pred, function(x) {is(x,"DISTAL")} )))
      stop("All elements should be DISTAL object")

  })

  genomatrix <- t(sapply(genometric_predicate, function(list_pred) {
    dist_array <- sapply(list_pred, function(x) {
      new_value = as.character(x)
      array <- c(new_value)
    })
    dist_array = c(dist_array,c("NA","NA"),c("NA","NA"),c("NA","NA"))
    length(dist_array) = 8
    dist_array
  }))

  if(!is.null(joinBy))
    join_condition_matrix <- .join_condition(joinBy)
  else
    join_condition_matrix <- NULL

  ouput <- tolower(output)
  if(!identical(output,"contig") && !identical(output,"left") && !identical(output,"right")
     && !identical(output,"int"))
    stop("output must be contig,left,right or int (intersection)")

  out <- WrappeR$join(genomatrix,joinBy, ouput,right_input_data, left_input_data)
  if(grepl("No",out,ignore.case = TRUE))
    stop(out)
  else
    out
}
