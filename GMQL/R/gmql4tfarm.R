#' GMQL to TFARM matrix
#'
#'
#' @import xml2
#'
#'
#' @details
#'
#' @examples
#'
#' \dontrun{
#' }
#'
TFARMatrix <- function(GMQL_dataset_path, metadata = NULL,metadata_prefix = NULL, regions = NULL)
{
  datasetName <- paste0(GMQL_dataset_path,"/files")

  if(!dir.exists(datasetName))
    stop("Directory does not exists")

  gdm_region_files <- list.files(datasetName, pattern = "*.gdm$",full.names = T)
  gtf_region_files <- list.files(datasetName, pattern = "*.gtf$",full.names = T)

  if(length(gtf_region_files)==0 && length(gdm_region_files)==0)
    stop("no samples present or no files format supported")

  if(length(gtf_region_files)>=1 && length(gdm_region_files)>=1)
    stop("GMQL dataset cannot be mixed dataset: no GTF and GDM together")

  vector_field <- .schema_header(datasetName)






  if(length(gdm_region_files)>0)
  {
    gdm_meta_files <- list.files(datasetName, pattern = "*.gdm.meta$",full.names = T)
    .parse_gdm(gdm_meta_files,vector_field,gdm_region_files)
  }
  else
  {
    gtf_meta_files <- list.files(datasetName, pattern = "*.gtf.meta$",full.names = T)
    .parse_gtf(gtf_meta_files,vector_field,gtf_region_files)
  }


}


prova <- function(m,name)
{
  all_meta <- m
  list <- lapply(all_meta,function(x,name){
    if(name %in% names(x))
    {
      print(x$"name")
    }
  },name)
}


.check_metadata <- function(metadata,metadata_prefix)
{

}


.schema_header <- function(datasetName)
{
  schema <- paste0(datasetName,"/test.schema")
  xml_schema <- xml2::read_xml(schema)
  list_field <- xml2::as_list(xml_schema)
  vector_field <- unlist(list_field)
}

.get_names_list <- function(list)
{
  name_list <- lapply(list,function(x){
    names(x)
  })
}

.parse_gtf <- function(gdm_meta_files,vector_field,gdm_region_files)
{

}

.parse_gdm <- function(gdm_meta_files,vector_field,gdm_region_files)
{
  meta_list <- lapply(gdm_meta_files,function(meta_file){
    file_name <- basename(meta_file)
    list <- .add_metadata(meta_file)
  })

  names_list <- .get_names_list(meta_list)

  df_list <- lapply(gdm_region_files,function(x,regions,vector_field){

    region_frame <- read.delim(x,col.names = vector_field,header = FALSE,sep = '\t')
    col_names <- names(region_frame)
    col_names <- col_names[col_names %in% regions] #delete column not choosen by input
    r <- region_frame[col_names]

  },regions,vector_field)

  df <- rbind.fill(df_list)
  df %>% distinct
}


