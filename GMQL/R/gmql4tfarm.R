#'
#'
#'
#' @param metadata string vector of metadata
#' @param metadata_prefix string vector of prefix associated to metadata
#' @param regions string vector of regions
#'
TFARMatrix <- function(metadata = NULL,metadata_prefix = NULL, regions = NULL,GMQL_dataset_path)
{
  datasetName <- paste0(GMQL_dataset_path,"/files")

  if(!dir.exists(datasetName))
    stop("Directory does not exists")

  if(length(list.files(datasetName))==0)
    stop("no samples present in this dataset")

  regions <- list.files(datasetName, pattern = "*.gtf$",full.names = T)






}
