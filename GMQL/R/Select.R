#' GMQL Operation: SELECT
#'
#' Extract a subset of samples from the input dataset.
#' It returns all the samples which satisfy the predicate on metadata
#' and / or returns those regions which satisfy the predicate on regions.
#' Also semijoin clause are used to further select samples;
#' When semijoin is defined it extract,
#' based on the existence of certain metadata attributes defined in semijoin clause,
#' those sample that are associated with at least one sample in an semijoin dataset
#'
#' @param predicate string predicate made up by logical oepration: AND,OR,NOT on metadata values
#' @param region_predicate string predicate made up by logical oepration: AND,OR,NOT on schema region values
#' @param semijoin list of CONDITION object using metadata as value. The CONDITION available are
#' EXACT,FULLNAME,DEFAULT. Every condition accept a string value.
#' @param semi_join_dataset url-like "string" pointer taken from GMQL function used in semijoin
#' @param input_data url-like "string" pointer taken from GMQL function
#'
#' @examples
#' startGMQL()
#' path = /.../dataset_name
#' r = read(path)
#' c = cover(2,3,input_data = r)
#' s = select("NOT(Patient_age < 70 AND provider=='Polimi')",input_dat = r)
#' s = select("NOT(Patient_age < 70)",region_predicate = "NOT(variant_type == 'SNP' OR pValue < 0.01)",
#' semi_join = list(DEFAULT("cell_type"),FULLNAME("age")),semi_join_dataset = c,input_data = r )
#' s = select("NOT(Patient_age < 70)",region_predicate = "NOT(qValue > 0.001)",
#' semi_join = list(EXACT("cell_type"),EXACT("age")),semi_join_dataset = c,input_data = r )
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
      stop("must be a string")

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
