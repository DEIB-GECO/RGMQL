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
#' @param predicate string made up by logical oepration: AND,OR,NOT
#' @param metadata metadata
#' @param region region
#' @param metadata_update metadata_update
#' @param regions_update regions_update
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
#'
#' @export
#'
#'
project <-function(input_data, metadata = NULL, regions = NULL, metadata_update = NULL, regions_update = NULL)
{
 # if(!is.character(metadata) && !is.null(metadata))
 #   stop("groupBy can be a string or an array of string")

 # if(!is.character(regions) && !is.null(regions))
  #  stop("groupBy can be a string or an array of string")

 # out <- WrappeR$project(metadata,regions,input_data)
#  if(grepl("No",out,ignore.case = TRUE))
 #   stop(out)
 # else
  #  out
}
