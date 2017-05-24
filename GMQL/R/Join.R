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
#'
#'
#' @references \url{http://www.bioinformatics.deib.polimi.it/genomic_computing/GMQL/doc/GMQLUserTutorial.pdf}
#'
#'

join <- function(genometric_predicate = NULL, joinBy = NULL, output=CONTIG(),
                 right_input_data, left_input_data)
{

  if(!is.list(genometric_predicate))
    stop("genometric_predicate must be list of lists")

  if(!all(sapply(genometric_predicate, function(x) is.list(x) )))
  {
    stop("genometric_predicate must be list of lists")
  }

  lapply(genometric_predicate, function(list_pred) {
    if(length(list_pred)>4)
    {
      warning("only 4 element per list, we cut the rest")
      length(list_pred)=4
    }

    if(!all(sapply(list_pred, function(x) {is(x,"DISTAL")} )))
    {
      stop("you must use DISTAL object for defining attibute in genometric_predicate")
    }
  })

  genomatrix <- sapply(genometric_predicate, function(list_pred) {
    t(sapply(list_pred, function(x) {
      new_value = as.character(x)
      matrix <- matrix(new_value)
    }))

  })

  if(!is.null(joinBy))
  {
    if(!is.list(joinBy) )
      stop("joinBy must be a list")

    if(!all(sapply(joinBy, function(x) is(x,"CONDITION") )))
    {
      stop("you must use CONDITION object for defining attibute in semijoin")
    }

    join_condition_matrix <- t(sapply(joinBy, function(x) {
      new_value = as.character(x)
      matrix <- matrix(new_value)
    }))
  }

  if(is.null(output))
    stop("output cannot be null")

  if(!is(output,"BUILDER"))
    stop("output must be CONTIG(), LEFT(), RIGHT() or INTERSECTION()")

  builder <- as.character(output)

  out <- frappeR$join(genomatrix,joinBy, builder,right_input_data, left_input_data)
  if(grepl("No",out,ignore.case = T))
    stop(out)
  else
    out
}
