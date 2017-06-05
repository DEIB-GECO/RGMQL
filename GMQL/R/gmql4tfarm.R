#' GMQL to TFARM matrix
#'
#' create matrix for TFARM package
#'
#' @import xml2
#' @importFrom  dplyr bind_cols
#' @importFrom data.table fread
#' @importFrom rtracklayer import
#'
#' @param GMQL_dataset_path  aaa
#' @param metadata aaaaaa   aaa
#' @param metadata_prefix a a a a a a a
#' @param regions a a a a a a
#'
#' @return TFARM matrix
#'
#' @examples
#'
#'
#' test_path <- system.file("example","DATA_SET_VAR_GTF",package = "GMQL")
#' TFARMatrix(test_path,regions = c("pvalue","peak"))
#'
#'
#' @export
#'
TFARMatrix <- function(GMQL_dataset_path, metadata = NULL,metadata_prefix = NULL, regions = NULL)
{
  datasetName <- paste0(GMQL_dataset_path,"/files")

  if(!dir.exists(datasetName))
    stop("Directory does not exists")

  gdm_meta_files <- list.files(datasetName, pattern = "*.gdm.meta$",full.names = TRUE)
  gtf_meta_files <- list.files(datasetName, pattern = "*.gtf.meta$",full.names = TRUE)

  if(length(gdm_meta_files)==0 && length(gtf_meta_files)==0)
    stop("no samples present or no files format supported")

  if(length(gdm_meta_files)>=1 && length(gtf_meta_files)>=1)
    stop("GMQL dataset cannot be mixed dataset: no GTF and GDM together")

  vector_field <- .schema_header(datasetName)

  if(length(gdm_meta_files)>0)
  {
    samples_file <- .check_metadata(metadata,metadata_prefix,gdm_meta_files)
    samples_to_read <- unlist(samples_file)
    if(length(samples_to_read)>0)
      samples_to_read <- gsub(".meta$", "", samples_to_read)
    else
      samples_to_read <- gsub(".meta$", "", gdm_meta_files)

    granges <- .parse_gdm(vector_field,samples_to_read,regions)
  }
  else
  {
    samples_file <- .check_metadata(metadata,metadata_prefix,gtf_meta_files)
    samples_to_read <- unlist(samples_file)
    if(length(samples_to_read)>0)
      samples_to_read <- gsub(".meta$", "", samples_to_read)
    else
      samples_to_read <- gsub(".meta$", "", gtf_meta_files)

    granges <- .parse_gtf(vector_field,samples_to_read,regions)
  }
}

.check_metadata <- function(metadata,metadata_prefix,meta_files)
{
  vec_meta <- paste0(metadata_prefix,metadata)
  meta_list <- lapply(meta_files, function(x){
    list <- .add_metadata(x)
    vec_names <- names(list)
    a <- sapply(vec_meta, function(y) {
      grep(y,vec_names) })

    ## we would like that manage more index from grep
    found <- as.logical(length(unlist(a)))
    #if found retrieve samples that has at least one choosen metadata
    if(found){x}
  })
}

.schema_header <- function(datasetName)
{
  schema_name <- list.files(datasetName, pattern = "*.schema$",full.names = TRUE)
  if(length(schema_name)==0)
    stop("schema not present")

  xml_schema <- xml2::read_xml(schema_name)
  list_field <- xml2::as_list(xml_schema)
  vector_field <- unlist(list_field)
}


.parse_gtf <- function(vector_field,gtf_region_files,regions)
{
  g1 <- rtracklayer::import(con = gtf_region_files[1], format = "gtf")
  elementMetadata(g1) <- NULL
  DF_list <- lapply(gtf_region_files, function(x){
    g_x <- rtracklayer::import(con = x, format = "gtf")
    meta <- elementMetadata(g_x)[regions]
    data.frame(meta)
    })
  DF_only_regions <- dplyr::bind_cols(DF_list)
  elementMetadata(g1) <- DF_only_regions
  g1
}

.parse_gdm <- function(vector_field,gdm_region_files,regions)
{
  #read first sample cause chromosome regions are the same for all samples
  df <- data.table::fread(gdm_region_files[1],col.names = vector_field,header = FALSE,sep = '\t')
  col_names <- names(df)
  df <-  df[c("chr","left","right","strand")]

  df_list <- lapply(gdm_region_files,function(x,regions,vector_field){

    region_frame <- data.table::fread(x,col.names = vector_field,header = FALSE,sep = '\t')
    col_names <- names(region_frame)
    col_names <- col_names[col_names %in% regions] #delete column not choosen by input
    r <- region_frame[col_names]
  },regions,vector_field)


  df_only_regions <- dplyr::bind_cols(df_list)
  complete_df <- dplyr::bind_cols(df,df_only_regions)
  g <- GenomicRanges::makeGRangesFromDataFrame(complete_df,keep.extra.columns = TRUE,
                                               start.field = "left",end.field = "right")
}


