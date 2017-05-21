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
#' @param joinBy list of CONDITION objects where every object contains the name of metadata to be used in joinBy
#' The CONDITION's available are: EXACT, FULLNAME, DEFAULT.
#' Every condition accepts only one string value. (e.g. DEFAULT("cell_type") )
#'
#' @seealso \url{http://www.bioinformatics.deib.polimi.it/genomic_computing/GMQL/doc/GMQLUserTutorial.pdf}
#'
#' @examples
#'
#' startGMQL()
#' path = /.../dataset_name
#' r = read(path)
#' c = cover(2,3,input_data = r)
#' s = select("NOT(Patient_age < 70 AND provider=='Polimi')",input_dat = r)
#' d = difference(left_input_data = r, right_input_data = c)
#' d = difference(list(DEFAULT("antibody_target")),left_input_data = r, right_input_data = c)
#'
difference <- function(left_input_data, right_input_data, joinBy = NULL)
{
  if(!is.null(joinBy))
  {
    if(!is.list(joinBy))
      stop("joinBy have to be a list ")

    if(!all(sapply(semi_join, function(x) is(x,"CONDITION") )))
    {
      stop("you must use CONDITION object for defining attibute in semijoin")
    }

    join_condition_matrix <- t(sapply(joinBy, function(x) {
      new_value = as.character(x)
      matrix <- matrix(new_value)
    }))

  }
  else
    join_condition_matrix = NULL

  out <- frappeR$difference(join_condition_matrix,right_input_data,left_input_data)
  if(grepl("No",out,ignore.case = T))
    stop(out)
  else
    out
}
