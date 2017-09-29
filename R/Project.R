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
#' @param input_data string pointer taken from GMQL function
#' @param metadata vector of string made up by metadata attribute
#' @param region vector of string made up by schema field attribute
#' @param all_but_reg logical value indicating which schema filed attribute you want to exclude.
#' If FALSE only the regions you choose is kept in the output of the project operation,
#' if TRUE the schema region are all except ones include in region parameter.
#' if regions is not defined \emph{all_but_reg} is not considerd.
#' @param all_but_meta logical value indicating which metadata you want to exclude.
#' If FALSE only the metadata you choose is kept in the output of the project operation,
#' if TRUE the metadata are all except ones include in region parameter.
#' if metadata is not defined \emph{all_but_meta} is not considerd.
#' @param regions_update single string predicate made up by operation on schema field attribute
#' @param metadata_update single string predicate made up by operation on metadata attribute
#'
#' @return DAGgraph class object. It contains the value associated to the graph used 
#' as input for the subsequent GMQL function#'
#'
#' @references \url{http://www.bioinformatics.deib.polimi.it/genomic_computing/GMQL/doc/GMQLUserTutorial.pdf}
#'
#'
#' @examples
#' 
#' @export
#'
#'
project <-function(input_data, metadata = NULL,metadata_update=NULL,all_but_meta = FALSE,
                   regions = NULL, regions_update = NULL,all_but_reg=FALSE)
{
  if(!is.null(metadata))
  {
    if(!is.character(metadata))
      stop("metadata: no valid input")

    metadata <- metadata[!metadata %in% ""]
    metadata <- metadata[!duplicated(metadata)]

    if(length(metadata)==0)
      metadata <- scalaNull("Array[String]")
  }
  else
    metadata <- scalaNull("Array[String]")

  if(!is.null(regions))
  {
    if(!is.character(regions))
      stop("regions: no valid input")

    regions = regions[!regions %in% ""]
    regions = regions[!duplicated(regions)]

    if(length(regions)==0)
      regions <- scalaNull("Array[String]")
  }
  else
    regions <- scalaNull("Array[String]")

  if(!is.null(regions_update))
    .check_predicate(regions_update)
  else
    regions_update <- scalaNull("String")
  
  if(!is.null(metadata_update))
    .check_predicate(metadata_update)
  else
    metadata_update <- scalaNull("String")
  
  if(length(all_but_meta)>1)
    warning("all_but_meta: no multiple values")
  
  if(length(all_but_reg)>1)
    warning("all_but_reg: no multiple values")
  all_but_reg <- all_but_reg[1]
  all_but_meta <- all_but_meta[1]
  
  out <- WrappeR$project(metadata,metadata_update,all_but_meta,
                         regions,regions_update,all_but_reg,input_data$value)
  if(grepl("No",out,ignore.case = TRUE))
    stop(out)
  else
    DAGgraph(out)
}
