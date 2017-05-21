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
#'
#' @param predicate string predicate made up by logical oepration: AND,OR,NOT on metadata attribute
#' @param region_predicate string predicate made up by logical oepration: AND,OR,NOT on schema region values
#' @param semijoin list of CONDITION objects where every object contains the name of metadata to be used in semijoin
#' The CONDITION's available are: EXACT, FULLNAME, DEFAULT.
#' Every condition accepts only one string value. (e.g. DEFAULT("cell_type") )
#' @param semi_join_dataset "url-like" string taken from GMQL function used in semijoin
#' @param input_data "url-like" string taken from GMQL function
#'
#' @seealso  \code{\link{EXACT}} \code{\link{FULLNAME}} \code{\link{DEFAULT}}
#' @seealso  \url{http://www.bioinformatics.deib.polimi.it/genomic_computing/GMQL/doc/GMQLUserTutorial.pdf}
#'
#' @examples
#' startGMQL()
#' path = "/<path_to_your_folder>/<your_dataset_name>"
#' r = read(path)
#' c = cover(2,3,input_data = r)
#' s = select("NOT(Patient_age < 70 AND provider=='Polimi')",input_dat = r)
#' s = select("NOT(Patient_age < 70)",region_predicate = "NOT(variant_type == 'SNP' OR pValue < 0.01)",
#' semi_join = list(DEFAULT("cell_type"),FULLNAME("age")),semi_join_dataset = c,input_data = r )
#' s = select("NOT(Patient_age < 70)",region_predicate = "NOT(qValue > 0.001)",
#' semi_join = list(EXACT("cell_type"),EXACT("age")),semi_join_dataset = c,input_data = r )
#'
#'
select <- function(input_data, predicate = NULL, region_predicate = NULL, semi_join = NULL,
                   semi_join_dataset = NULL)
{
  if(!is.null(predicate))
    if(!is.character(predicate))
      stop("predicate must be a single string")

  if(!is.null(region_predicate))
    if(!is.character(region_predicate))
      stop("region_predicate must be a single string ")

  if(is.null(semi_join) && is.null(semi_join_dataset)) {
    #trick, if we call it like that
  }
  else if(is.null(semi_join) || is.null(semi_join_dataset)) {
    warning("You did not set correctly semijoin parameters.\nAll parameters have to be set.\nSelect function will be invoked without semijoin expression")
    semi_join_dataset=NULL
    join_condition_matrix = NULL
  }
  else
  {
    if(!is.list(semi_join))
      stop("semi_join must be a list")

    if(!all(sapply(semi_join, function(x) is(x,"CONDITION") )))
    {
      stop("you must use CONDITION object for defining attibute in semijoin")
    }

    if(!is.character(semi_join_dataset))
      stop("semi_join_dataset must be a single string")

    join_condition_matrix <- t(sapply(semi_join, function(x) {
      new_value = as.character(x)
      matrix <- matrix(new_value)
    }))
  }

  #semi_join = groupBy[!semi_join %in% ""]
  #semi_join = groupBy[!duplicated(semi_join)]

  out <- frappeR$select(predicate,region_predicate,join_condition_matrix,semi_join_dataset,input_data)
  if(grepl("No",out,ignore.case = T) || grepl("expected",out,ignore.case = T))
    stop(out)
  else
    out
}
