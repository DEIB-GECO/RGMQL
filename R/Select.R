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
#' @param input_data returned object from any GMQL function
#' @param predicate single string predicate made up by logical oepration: AND,OR,NOT on metadata attribute
#' @param region_predicate single string predicate made up by logical operation: AND,OR,NOT on schema region values
#' @param semi_join list of CONDITION objects where every object contains the name of metadata to be used in semijoin,
#' or simple string concatenation of name of metadata (e.g c("cell_type","attribute_tag","size") ) without declaring condition.
#' In the latter form all metadata are considered having DEF condition
#' The CONDITION's available are:
#' \itemize{
#' \item{\code{\link{FULL}}: Fullname evaluation, two attributes match if they both end with value and,
#' if they have a further prefixes, the two prefix sequence are identical}
#' \item{\code{\link{DEF}}: Default evaluation, two attributes match if both end with value. }
#' \item{\code{\link{EXACT}}: Exact evaluation, only attributes exactly as value will match; no further prefixes are allowed. }
#' }
#' Every condition accepts only one string value. (e.g. FULL("cell_type") )
#'
#' @param semi_join_dataset returned object from any GMQL function used in semijoin
#'
#' @return "url-like" string
#'
#' @references \url{http://www.bioinformatics.deib.polimi.it/genomic_computing/GMQL/doc/GMQLUserTutorial.pdf}
#'
#' @examples
#' \dontrun{
#'
#' initGMQL("gtf")
#' test_path <- system.file("example","DATA_SET_VAR_GTF",package = "GMQL")
#' test_path2 <- system.file("example","DATA_SET_VAR_GDM",package = "GMQL")
#' r = read(test_path)
#' r2 = read(test_path2)
#'
#' #### select with condition
#' s = select(input_data = r, semi_join = list("cell_type",EXACT("cell")), semi_join_dataset = r2)
#'
#' #### select with DEF condition
#' s = select(input_data = r, semi_join = list("cell_type","cell"), semi_join_dataset = r2)
#'
#' #### select with DEF condition
#' #### the full condition is treated as DEF due to coercion
#' s = select(input_data = r, semi_join = c("cell_type","cell",FULL("attribute_tag")),
#' semi_join_dataset = r2)
#'
#' #### select with condition
#' #### the first is FULL and the other ones are DEF
#' s = select(input_data = r, semi_join = c(FULL("attribute_tag"),"cell_type","cell"),
#' semi_join_dataset = r2)
#'
#' #### select with predicate metadata
#' s = select(input_data = r, "NOT(biosample_organism=='Homo sapiens' AND assembly=='hg19')")
#'
#' #### select with predicate on regions
#' s = select(input_data = r, region_predicate = " score > 0.5 AND NOT(variant_type == 'SNP')")
#' }
#' ""
#' @export
#'
select <- function(input_data, predicate = NULL, region_predicate = NULL, semi_join = NULL,
                   semi_join_dataset = NULL)
{
  if(!is.null(predicate))
    .check_predicate(predicate)

  if(!is.null(region_predicate))
    .check_predicate(region_predicate)

  if(is.null(semi_join) && is.null(semi_join_dataset)) {
    join_condition_matrix <- NULL
  }
  else if(is.null(semi_join) || is.null(semi_join_dataset)) {
    warning("You did not set correctly semijoin parameters.\nAll parameters have to be set.\nSelect function will be invoked without semijoin expression")
    semi_join_dataset <- NULL
    join_condition_matrix <- NULL
  }
  else
  {
    if(!is.character(semi_join_dataset))
      stop("semi_join_dataset: no valid input")

    if(length(semi_join_dataset)>1)
      stop("semi_join_dataset: no multiple values")

    join_condition_matrix <- .join_condition(semi_join)
  }
  out <- WrappeR$select(predicate,region_predicate,join_condition_matrix,semi_join_dataset,input_data)
  if(grepl("No",out,ignore.case = TRUE) || grepl("expected",out,ignore.case = TRUE))
    stop(out)
  else
    out
}


