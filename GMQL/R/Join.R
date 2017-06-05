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
#' @param left_input_data "url-like" string taken from GMQL function
#' @param right_input_data "url-like" string taken from GMQL function
#' @param genometric_predicate is a concatenation of distal conditions by means of logical ANDs
#' @param joinBy list of \code{\link{CONDITION}} objects where every object contains the name of metadata to be used in joinBy
#' The CONDITION's available are: EXACT, FULLNAME, DEFAULT.
#' Every condition accepts only one string value. (e.g. DEFAULT("cell_type") )
#' @param output is one of four different values that declare which region is given in
#' output for each input pair of left dataset and right dataset regions satisfying the genometric predicate:
#' \itemize{
#' \item{left}
#' \item{right}
#' \item{intersection}
#' \item{contig}
#' }
#'
#' @references \url{http://www.bioinformatics.deib.polimi.it/genomic_computing/GMQL/doc/GMQLUserTutorial.pdf}
#'
#'
#' @examples
#' \dontrun{
#'
#' initGMQL("gtf")
#' path = "/<path_to_your_folder>/<your_dataset_name>"
#' r = read(path)
#' c = cover(2,3,input_data = r)
#' j = join (list(list(UP(),MD(1)), list(DOWN(),DGE(5000))),right_input_data = r,left_input_data = c)
#'
#' }
#'
#'

join <- function(genometric_predicate = NULL, joinBy = NULL, output="contig",
                 right_input_data, left_input_data)
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
     && !identical(output,"intersection"))
    stop("output must be contig,left,right or intersection")

  out <- WrappeR$join(genomatrix,joinBy, ouput,right_input_data, left_input_data)
  if(grepl("No",out,ignore.case = TRUE))
    stop(out)
  else
    out
}
