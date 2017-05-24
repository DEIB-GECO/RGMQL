
TFARMatrix <- function(GMQL_dataset_path, metadata = NULL,metadata_prefix = NULL, regions = NULL)
{
  datasetName <- paste0(GMQL_dataset_path,"/files")

  if(!dir.exists(datasetName))
    stop("Directory does not exists")

  if(length(list.files(datasetName))==0)
    stop("no samples present in this dataset")

  region_file <- list.files(datasetName, pattern = "*.gdm$",full.names = T)

  schema <- paste0(datasetName,"/test.schema")
  xml_schema <- read_xml(schema)
  list_field <- as_list(xml_schema)
  vector_field <- unlist(list_field)

  df_list <- lapply(region_file,function(x,regions,vector_field){

    region_frame <- read.delim(x,col.names = vector_field,header = FALSE,sep = '\t')
    col_names <- names(region_frame)
    col_names <- col_names[col_names %in% regions] #delete column not choosen by input
    r <- region_frame[col_names]

  },regions,vector_field)

  df <- rbind.fill(df_list)
  df %>% distinct
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
