#' GMQL Operation: SELECT
#'
#' It extracts a subset of samples from the input dataset.
#' It returns all the samples satisfying the predicate on metadata.
#' If regions are specified, returns regions satisfying the predicate on regions.
#' If semijoin clauses are specified they are applied, too.
#' When semijoin is defined, it extracts those samples containing all metadata attribute defined in semijoin clause
#' with at least one metadata value in common with semi join dataset
#' If no metadata in common beetween input dataset and semi join dataset, no sample is extracted
#'
#' @param input_data "url-like" string taken from GMQL function
#' @param predicate string predicate made up by logical oepration: AND,OR,NOT on metadata attribute
#' @param region_predicate string predicate made up by logical operation: AND,OR,NOT on schema region values
#' @param semi_join list of \code{\link{CONDITION}} objects every object contains the name of metadata to be used in semijoin,
#' The CONDITION's available are: FULL, DEF, EXACT
#' Every condition accepts only one string value. (e.g. DEF("cell_type") )
#' @param semi_join_dataset "url-like" string taken from GMQL function used in semijoin
#'
#' @references \url{http://www.bioinformatics.deib.polimi.it/genomic_computing/GMQL/doc/GMQLUserTutorial.pdf}
#'
#' @examples
#' \dontrun{
#'
#' initGMQL("tab")
#' path = "/<path_to_your_folder>/<your_dataset_name>"
#' r = read(path)
#' c = cover(2,3,input_data = r)
#' s = select("NOT(Patient_age < 70 AND provider=='Polimi')",input_dat = r)
#' s = select(region_predicate = "NOT(qValue > 0.001)", semi_join = list("cell_type","age",EXACT("cell")),
#' semi_join_dataset = c,input_data = r )
#' s = select("NOT(Patient_age < 70)", semi_join = c("cell_type","age","cell_attribute","size"),
#' semi_join_dataset = c,input_data = r )
#' }
#'
select <- function(input_data, predicate = NULL, region_predicate = NULL, semi_join = NULL,
                   semi_join_dataset = NULL)
{
  if(!is.null(predicate))
    .check_predicate(predicate)

  if(!is.null(region_predicate))
    .check_predicate(region_predicate)

  if(is.null(semi_join) && is.null(semi_join_dataset)) {
    #trick, if we call it like that
  }
  else if(is.null(semi_join) || is.null(semi_join_dataset)) {
    warning("You did not set correctly semijoin parameters.\nAll parameters have to be set.\nSelect function will be invoked without semijoin expression")
    semi_join_dataset <- NULL
    join_condition_matrix <- NULL
  }
  else
  {
    if(!is.character(semi_join_dataset))
      stop("semi_join_dataset: must be string")

    if(is.character(semi_join_dataset) && length(semi_join_dataset)>1)
      stop("semi_join_dataset: no multiple string")

    join_condition_matrix <- .join_condition(semi_join)
  }
  out <- WrappeR$select(predicate,region_predicate,join_condition_matrix,semi_join_dataset,input_data)
  if(grepl("No",out,ignore.case = T) || grepl("expected",out,ignore.case = T))
    stop(out)
  else
    out
}


.join_condition <- function(conditions)
{
  if(is.list(conditions))
  {
    join_condition_matrix <- t(sapply(conditions, function(x) {
      new_value = as.character(x)
      if(length(new_value)==1)
        new_value = c("DEF",new_value)
      else if(!identical("DEF",new_value[1]) && !identical("FULL",new_value[1]) && !identical("EXACT",new_value[1]))
        stop("no more than one value")
      matrix <- matrix(new_value)
    }))
  }
  else if(is.character(conditions))
  {
    join_condition_matrix <- t(sapply(conditions, function(x) {
      new_value = c("DEF",x)
      matrix <- matrix(new_value)
    }))
  }
  else
    stop("only list or character")
}

.check_predicate <- function(predicate_string)
{
  if(!is.character(predicate_string))
    stop("must be a string")

  if(is.character(predicate_string) && length(predicate_string)>1)
    stop("no multiple string")
}

