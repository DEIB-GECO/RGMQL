#' GMQL Operation: DIFFERENCE
#'
#' It produces one sample in the result for each sample of the left operand,
#' by keeping the same metadata of the left operand sample and only those regions
#' (with their schema and values) of the left operand sample which do not intersect with any region
#' in the right operand sample.
#' The optional joinby clause is used to extract a subset of couples
#' from the cartesian product of two dataset Dleft x Dright
#' on which to apply the DIFFERENCE operator:
#' only those samples that have the same value for each attribute
#' are considered when performing the difference.
#'
#'
#' @param right_input_data "url-like" string taken from GMQL function
#' @param left_input_data "url-like" string taken from GMQL function
#' @param joinBy list of \code{\link{CONDITION}} objects where every object contains the name of metadata to be used in joinBy
#' The CONDITION's available are: EXACT, FULLNAME, DEFAULT.
#' Every condition accepts only one string value. (e.g. DEFAULT("cell_type") )
#' @param is_exact logical
#'
#' @references \url{http://www.bioinformatics.deib.polimi.it/genomic_computing/GMQL/doc/GMQLUserTutorial.pdf}
#'
#' @examples
#' \dontrun{
#'
#' initGMQL("gtf")
#' path = /.../dataset_name
#' r = read(path)
#' c = cover(2,3,input_data = r)
#' d = difference(list(DEF("antibody_target"),FULL("cell_type")),left_input_data = r, right_input_data = c)
#' d = difference(c("cell_type","age","cell_attribute","size"),left_input_data = r, right_input_data = c)
#' }
#'
difference <- function(left_input_data, right_input_data, joinBy = NULL,is_exact = F)
{
  if(!is.null(joinBy))
    join_condition_matrix <- .join_condition(joinBy)
  else
    join_condition_matrix = NULL

  out <- WrappeR$difference(join_condition_matrix,right_input_data,left_input_data,is_exact)
  if(grepl("No",out,ignore.case = T))
    stop(out)
  else
    out
}
