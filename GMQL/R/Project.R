#' GMQL Operation: PROJECT
#'
#' It creates, from an existing dataset, a new dataset with all the samples from input dataset
#' but keeping for each sample in the input dataset only those metadata and/or region attributes
#' expressed in the operator parameter list.
#' Region coordinates and values of the remaining metadata remain equal to those in the input dataset.
#' It allows to:
#' \itemize{
#' \item{Remove existing metadata and/or region attributes from a dataset}
#' \item{Create new metadata and/or region attributes in the result}
#' }
#'
#'
#' @param input_data string pointer taken from GMQL function
#' @param metadata vector of string made up by metadata attribute
#' @param region vector of string made up by schema field attribute
#' @param all_but logical value
#' @param regions_update single string predicate
#'
#' @return "url-like" string
#'
#'
#' @references \url{http://www.bioinformatics.deib.polimi.it/genomic_computing/GMQL/doc/GMQLUserTutorial.pdf}
#'
#'
#'
#' @examples
#' \dontrun{
#'
#' initGMQL("gtf")
#' test_path <- system.file("example","DATA_SET_VAR_GTF",package = "GMQL")
#' r = read(test_path)
#' p = project(input_data = r)
#' }
#' .
#' @export
#'
#'
project <-function(input_data, metadata = NULL, regions = NULL, regions_update = NULL,all_but = FALSE)
{
  if(!is.null(metadata))
  {
    if(!is.character(metadata))
      stop("metadata can be a string or an array of string")

    metadata = metadata[!metadata %in% ""]
    metadata = metadata[!duplicated(metadata)]

    if(length(metadata)==0)
      metadata=NULL
  }

  if(!is.null(regions))
  {
    if(!is.character(regions))
      stop("regions can be a string or an array of string")

    regions = regions[!regions %in% ""]
    regions = regions[!duplicated(regions)]

    if(length(regions)==0)
      regions=NULL
  }
  .check_predicate(regions_update)

  if(length(all_but)>1)
    warning("all_but: only the first element is taken")

  all_but <- all_but[1]

  out <- WrappeR$project(metadata,regions,regions_update,all_but,input_data)
  if(grepl("No",out,ignore.case = TRUE))
    stop(out)
  else
    out
}
