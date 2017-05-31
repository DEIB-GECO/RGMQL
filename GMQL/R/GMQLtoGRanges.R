#' Create GRangesList from GMQL Dataset
#'
#' It create a GrangesList from GMQL samples in dataset
#' It reads only sample files in GTF format
#'
#' @import rtracklayer
#' @import GenomicRanges
#' @import S4Vectors
#' @import xml2
#'
#'
#' @param datasetName GMQL dataset folder path
#'
#' @seealso \code{\link{exportGMQL.gdm}} \code{\link{exportGMQL.gtf}} \code{\link{importGMQL.gdm} }
#'
#'
#' @examples
#'
#' \dontrun{
#' path = "<path_folder_input>"
#' grl = importGMQL.gtf(path)
#' }
#'
importGMQL.gtf <- function(datasetName)
{
  datasetName <- paste0(datasetName,"/files")

  if(!dir.exists(datasetName))
    stop("Directory does not exists")

  if(length(list.files(datasetName))==0)
    stop("no samples present in this dataset")

  regions <- list.files(datasetName, pattern = "*.gtf$",full.names = T)
  if(length(regions) != 0)
  {
    name_samples <- lapply(regions, function(x){gsub("*.gtf", "", basename(x))})
    sampleList <- lapply(regions, function(x){rtracklayer::import(con = x, format = "gtf")} )
    names(sampleList) <- name_samples
    gRange_list <- GenomicRanges::GRangesList(sampleList)
  }
  else
    stop("No GTF files present")

  meta <- list.files(datasetName, pattern = "*.gtf.meta$",full.names = T)
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
#' @import GenomicRanges
#' @import S4Vectors
#' @import xml2
#'
#' @param datasetName GMQL dataset folder path
#'
#'
#' @seealso \code{\link{exportGMQL.gdm}} \code{\link{exportGMQL.gtf}} \code{\link{importGMQL.gdm} }
#'
#'
#' @examples
#'
#' \dontrun{
#' path = "<path_folder_input>"
#' grl = importGMQL.gdm(path)
#' }
#'
importGMQL.gdm <- function(datasetName)
{
  datasetName <- paste0(datasetName,"/files")

  if(!dir.exists(datasetName))
    stop("Directory does not exists")

  if(length(list.files(datasetName))==0)
    stop("no samples present in this dataset")

  regions <- list.files(datasetName, pattern = "*.gdm$",full.names = T)
  if(length(regions) != 0)
  {
    name_samples <- lapply(regions, function(x){gsub("*.gdm", "", basename(x))})
    schema <- list.files(datasetName, pattern = "*.schema$",full.names = T)
    if(length(schema)==0)
      stop("schema not present")

    xml_schema <- xml2::read_xml(schema)
    list_field <- xml2::as_list(xml_schema)
    vector_field <- unlist(list_field)
    names(vector_field)=NULL
    sampleList <- lapply(regions,function(x){
      df <- read.delim(x,col.names = vector_field,header = FALSE)
      g <- GenomicRanges::makeGRangesFromDataFrame(df,keep.extra.columns = T,start.field = "left",end.field = "right")
    })
    names(sampleList) <- name_samples
    gRange_list <- GenomicRanges::GRangesList(sampleList)
  }
  else
    stop("No GDM files present")

  meta <- list.files(datasetName, pattern = "*.gdm.meta$",full.names = T)
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


#move to internals
.add_metadata <- function(files)
{
  x <- scan(files, what="", sep="\n")
  y <- strsplit(x, "\t")
  names(y) <- sapply(y, `[[`, 1)
  listMeta <- lapply(y, `[`, -1)
}


