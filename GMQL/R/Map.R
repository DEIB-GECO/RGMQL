#' GMQL Operation: MAP
#'
#' It computes, for each sample in the right dataset, aggregates over the values of the right regions
#' that intersect with a region in a left sample, for each region of each sample in the left dataset;
#' The number of generated output samples is the Cartesian product of the samples in the two input datasets;
#' each output sample has the same regions as the related input left sample, with their attributes and values,
#' plus the attributes computed as aggregates over right region values.
#' Output sample metadata are the union of the related input sample metadata,
#' whose attribute names are prefixed with "left" or "right" respectively.
#'
#' When the joinby clause is present, only pairs of samples of left_input_data and of right_input_data with
#' metadata M1 and M2 respectively that satisfy the joinby condition are considered.
#'
#' The clause consists of a list of metadata attribute names that must be present with equal values
#' in both M1 and  M2
#'
#'
#' @param left_input_data "url-like" string taken from GMQL function
#' @param right_input_data "url-like" string taken from GMQL function
#'
#' @references \url{http://www.bioinformatics.deib.polimi.it/genomic_computing/GMQL/doc/GMQLUserTutorial.pdf}
#'
#'
#'
map <- function(left_input_data, right_input_data, aggregates = NULL, joinBy = NULL)
{
  if(!is.null(aggregates))
  {
    if(!is.list(aggregates))
      stop("aggregates must be a list")

    if(!all(sapply(aggregates, function(x) is(x,"OPERATOR") )))
      stop("you must use OPERATOR object for defining aggregates function")

    names <- names(aggregates)
    if(is.null(names))
    {
      warning("you did not assign a names to a list.\nWe build names for you")
      names <- sapply(aggregates, take_value.META_OPERATOR)
    }
    else
    {
      if("" %in% names)
        stop("no partial names assignment to list")
    }
    aggregate_matrix <- t(sapply(aggregates, function(x) {

      new_value = as.character(x)
      matrix <- matrix(new_value)

    }))
    m_names <- matrix(names)
    metadata_matrix <- cbind(m_names,aggregate_matrix)
  }
  else
    metadata_matrix = NULL

  if(!is.null(joinBy))
  {
    if(!is.list(joinBy))
      stop("joinBy have to be a list ")

    if(!all(sapply(semi_join, function(x) is(x,"CONDITION") )))
      stop("you must use CONDITION object for defining attibute in semijoin")

    join_condition_matrix <- t(sapply(joinBy, function(x) {

      new_value = as.character(x)
      matrix <- matrix(new_value)

    }))
  }
  else
    join_condition_matrix = NULL

  out<-frappeR$map(join_condition_matrix,aggregates,left_input_data,right_input_data)

  if(grepl("No",out,ignore.case = T))
    stop(out)
  else
    out
}
