#' Create GRangesList from GMQL dataset
#'
#' It creates a GRangesList from GMQL samples in dataset. 
#' It reads sample files in GTF or GDM/tab-delimited format.
#'
#' @importFrom rtracklayer import
#' @importClassesFrom GenomicRanges GRangesList
#' @importFrom S4Vectors metadata
#' @importFrom utils read.delim
#' @import xml2
#'
#' @param dataset_path string with GMQL dataset folder path
#' @param is_gtf logical value indicating if dataset samples are in GTF format;
#' if TRUE and dataset does not contain GTF samples, an error occurs 
#' 
#' @return GRangesList containing all GMQL samples in dataset
#'
#' @seealso \code{\link{export_gmql}}
#'
#' @examples
#' 
#' ## This statement defines the path to the subdirectory "example" of the 
#' ## package "RGMQL" and imports as GRangesList the contained GMQL dataset
#' 
#' test_path <- system.file("example", "DATASET", package = "RGMQL")
#' grl = import_gmql(test_path, TRUE)
#'
#'
#' @export
#'
import_gmql <- function(dataset_path, is_gtf) {
  if(is_gtf)
    .importGMQL.gtf(dataset_path)
  else
    .importGMQL.gdm(dataset_path)
}

.importGMQL.gtf <- function(datasetName) {
  datasetName <- sub("/*[/]$","",datasetName)
  if(basename(datasetName) !="files")
    datasetName <- file.path(datasetName,"files")
  
  if(!dir.exists(datasetName))
    stop("Directory does not exists")
  
  if(!length(list.files(datasetName)))
    stop("no samples present in this dataset")
  
  regions <- list.files(datasetName, pattern = "*.gtf$",full.names = TRUE)
  if(length(regions)) {
    name_samples <- lapply(regions, function(x){
      gsub("*.gtf", "", basename(x))})
    sampleList <- lapply(regions, function(x){
      rtracklayer::import(con = x, format = "gtf")} )
    names(sampleList) <- name_samples
    gRange_list <- GenomicRanges::GRangesList(sampleList)
    
  } else
    stop("No GTF files present")
  
  meta <- list.files(datasetName, pattern = "*.gtf.meta$",full.names = TRUE)
  if(length(meta)) {
    meta_list <- lapply(meta, .add_metadata)
    names(meta_list) <- name_samples
    
  } else
    stop("No meta GTF files present")
  
  S4Vectors::metadata(gRange_list) <- meta_list
  return(gRange_list)
}

.importGMQL.gdm <- function(datasetName) {
  datasetName <- sub("/*[/]$","",datasetName)
  if(basename(datasetName) !="files")
    datasetName <- file.path(datasetName,"files")
  
  if(!dir.exists(datasetName))
    stop("Directory does not exists")
  
  if(!length(list.files(datasetName)))
    stop("no samples present in this dataset")
  
  regions <- list.files(datasetName, pattern = "*.gdm$",full.names = TRUE)
  if(length(regions)) {
    name_samples <- lapply(regions, function(x){
      gsub("*.gdm", "",basename(x))})
    vector_field <- .schema_header(datasetName)
    type_and_coord <- .schema_type_coordinate(datasetName)
    names(vector_field) <- NULL
    if(type_and_coord$coordinate_system %in% c("1-based")) {
      sampleList <- lapply(regions,function(x){
        df <- read.delim(x,col.names = vector_field,header = FALSE)
        g <- GenomicRanges::makeGRangesFromDataFrame(
          df,
          keep.extra.columns = TRUE,
          start.field = "left",
          end.field = "right")
      })
    } else {
      sampleList <- lapply(regions,function(x){
        df <- read.delim(x,col.names = vector_field,header = FALSE)
        df$left = df$left +1
        g <- GenomicRanges::makeGRangesFromDataFrame(
          df,
          keep.extra.columns = TRUE,
          start.field = "left",
          end.field = "right")
      })
    }
    
    names(sampleList) <- name_samples
    gRange_list <- GenomicRanges::GRangesList(sampleList)
    
  } else
    stop("No GDM files present")
  
  meta <- list.files(datasetName, pattern = "*.gdm.meta$",full.names = TRUE)
  if(length(meta)) {
    meta_list <- lapply(meta, .add_metadata)
    names(meta_list) <- name_samples
    
  } else
    stop("No meta GDM files present")
  
  S4Vectors::metadata(gRange_list) <- meta_list
  return(gRange_list)
}
