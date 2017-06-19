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
#' @param all_but logical value indicating which schema filed attribute you want to exclude.
#' If FALSE only the regions you choose is kept in the output of the project operation,
#' if TRUE the schema region are all except ones include in region parameter.
#' if regions is not defined all_but is not considerd.
#' @param regions_update single string predicate
#' @param metadata_update single string predicate
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
#'
#' ### preserving all region attributes and creating a new region attribute called length
#' p = project(input_data = r,regions_update="length AS right - left")
#'
#' ### preserving all region attributes apart from  score, and creating a new region attribute called new_score
#' p = project(input_data = r, regions = "score" regions_update="length AS right - left", all_but=TRUE)
#'
#' ### output dataset that contains the same samples as the input dataset. Each output sample only contains,
#' ### as region attributes, the four basic coordinates (chr, left, right, strand)
#' ### and the specified region attributes and as metadata attributes only the specified ones
#' p = project(input_data = r, regions = c("variant_classification", "variant_type"),
#' metadata = c("manually_curated","tissue_status", "tumor_ta") )
#' }
#' ""
#' @export
#'
#'
project <-function(input_data, metadata = NULL,metadata_update=NULL,
                   regions = NULL, regions_update = NULL,all_but = FALSE)
{
  if(!is.null(metadata))
  {
    if(!is.character(metadata))
      stop("metadata: only character")

    metadata = metadata[!metadata %in% ""]
    metadata = metadata[!duplicated(metadata)]

    if(length(metadata)==0)
      metadata=NULL
  }

  if(!is.null(regions))
  {
    if(!is.character(regions))
      stop("regions: only character")

    regions = regions[!regions %in% ""]
    regions = regions[!duplicated(regions)]

    if(length(regions)==0)
      regions=NULL

  }

  if(!is.null(regions_update))
    .check_predicate(regions_update)
  
  if(!is.null(metadata_update))
    .check_predicate(metadata_update)
  
  if(length(all_but)>1)
    warning("all_but: only the first element is taken")

  all_but <- all_but[1]

  out <- WrappeR$project(metadata,regions,regions_update,metadata_update,all_but,input_data)
  if(grepl("No",out,ignore.case = TRUE))
    stop(out)
  else
    out
}
