#' GMQL Operation: SELECT
#'
#'It keeps in the result all the samples which existentially satisfy the predicate on metadata
#'and then selects those regions of selected samples which satisfy the predicate on regions.
#'A sample is legal also when it contains no regions as result of a selection.
#'Semi-join clauses are used to further select samples;
#'A semi-join clause can be constructed as the conjunction of the simple metadata predicates
#'that refer to the same dataset.
#'Semi-joins are used to connect variables
#'
#'
#' @param predicate string made up by logical oepration: AND,OR,NOT on metadata values
#' @param region_predicate string made up by logical oepration: AND,OR,NOT on region values
#' @param semijoin vector of metadata attribute consider for semijoining
#' @param semi_join_in logical value
#' @param semi_join_dataset string pointer taken from GMQL function needed for semijoining
#' @param input_data string pointer taken from GMQL function
#'
#'
select <- function(predicate = NULL, region_predicate = NULL,semi_join = NULL,
                   semi_join_dataset = NULL,input_data)
{

  if(!is.null(predicate))
    if(!is.character(predicate))
      stop("prdicate must be a string predicate")

  if(!is.null(region_predicate))
    if(!is.character(region_predicate))
      stop("region must be a string predicate")

  if(is.null(semi_join) && is.null(semi_join_dataset)){
    #trick, if we call it like that
  }
  else if(is.null(semi_join) || is.null(semi_join_dataset)){
    warning("You did not set correctly semijoin parameters.\n Select function will be invoked without semijoin expression")
    semi_join=NULL
    semi_join_dataset=NULL
  }
  else
  {
    if(!is.character(semi_join))
      stop("must be a vector of attributes")

    if(!is.character(semi_join_dataset))
      stop("must be a string")
  }

  out <- frappeR$select(predicate,region_predicate,semi_join,semi_join_dataset,input_data)
  if(grepl("No",out,ignore.case = T))
    stop(out)
  else
    out
}
