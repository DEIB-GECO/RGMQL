#' Create GRangesList from GMQL Dataset
#'
#' It create a GrangesList from GMQL samples in dataset
#' It reads only sample files in GTF format
#'
#' @importFrom rtracklayer import
#' @importClassesFrom GenomicRanges GRangesList
#' @importFrom S4Vectors metadata
#' @import xml2
#'
#'
#' @param datasetName single string GMQL dataset folder path
#'
#' @return GrangesList containing all GMQL samples in dataset
#'
#' @seealso \code{\link{exportGMQL.gdm}} \code{\link{exportGMQL.gtf}} \code{\link{importGMQL.gdm} }
#'
#'
#' @examples
#'
#' test_path <- system.file("example","DATA_SET_VAR_GTF",package = "RGMQL")
#' grl = importGMQL.gtf(test_path)
#'
#'
#' @export
#'
importGMQL.gtf <- function(datasetName)
{
  datasetName <- paste0(datasetName,"/files")

  if(!dir.exists(datasetName))
    stop("Directory does not exists")

  if(length(list.files(datasetName))==0)
    stop("no samples present in this dataset")

  regions <- list.files(datasetName, pattern = "*.gtf$",full.names = TRUE)
  if(length(regions) != 0)
  {
    name_samples <- lapply(regions, function(x){gsub("*.gtf", "", basename(x))})
    sampleList <- lapply(regions, function(x){rtracklayer::import(con = x, format = "gtf")} )
    names(sampleList) <- name_samples
    gRange_list <- GenomicRanges::GRangesList(sampleList)
  }
  else
    stop("No GTF files present")

  meta <- list.files(datasetName, pattern = "*.gtf.meta$",full.names = TRUE)
  if(length(meta) != 0)
  {
    meta_list <- lapply(meta, .add_metadata)
    names(meta_list) <- name_samples
  }
  else
    stop("No meta GTF files present")

  S4Vectors::metadata(gRange_list) <- meta_list
  return(gRange_list)
}


#' Create GrangesList from dataset GMQL in GDM (delimited / tabulated) format file
#'
#'
#' It create a GrangesList from GMQL samples in dataset
#' It reads only sample files in GDM format
#'
#' @importClassesFrom GenomicRanges GRangesList
#' @importFrom S4Vectors metadata
#' @importFrom utils read.delim
#' @import xml2
#'
#' @param datasetName single string GMQL dataset folder path
#'
#' @return GrangesList containing all GMQL samples in dataset
#'
#' @seealso \code{\link{exportGMQL.gdm}} \code{\link{exportGMQL.gtf}} \code{\link{importGMQL.gtf} }
#'
#'
#' @examples
#'
#' test_path <- system.file("example","DATA_SET_VAR_GDM",package = "RGMQL")
#' grl = importGMQL.gdm(test_path)
#'
#' @export
#'
importGMQL.gdm <- function(datasetName)
{
  datasetName <- paste0(datasetName,"/files")

  if(!dir.exists(datasetName))
    stop("Directory does not exists")

  if(length(list.files(datasetName))==0)
    stop("no samples present in this dataset")

  regions <- list.files(datasetName, pattern = "*.gdm$",full.names = TRUE)
  if(length(regions) != 0)
  {
    name_samples <- lapply(regions, function(x){gsub("*.gdm", "", basename(x))})

    vector_field <- .schema_header(datasetName)

    names(vector_field)=NULL
    sampleList <- lapply(regions,function(x){
      df <- read.delim(x,col.names = vector_field,header = FALSE)
      g <- GenomicRanges::makeGRangesFromDataFrame(df,keep.extra.columns = TRUE,start.field = "left",end.field = "right")
    })
    names(sampleList) <- name_samples
    gRange_list <- GenomicRanges::GRangesList(sampleList)
  }
  else
    stop("No GDM files present")

  meta <- list.files(datasetName, pattern = "*.gdm.meta$",full.names = TRUE)
  if(length(meta) != 0)
  {
    meta_list <- lapply(meta, .add_metadata)
    names(meta_list) <- name_samples
  }
  else
    stop("No meta GDM files present")

  S4Vectors::metadata(gRange_list) <- meta_list
  return(gRange_list)
}



