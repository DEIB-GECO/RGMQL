
#'
#'
#'create GrangesList from sample in GTF format file
#'
#'
GRangesListFromGMQL.gtf <- function(datasetName = "DATA_SET_VAR_GTF/files")
{
  datasetName <- paste0("/Users/simone/Downloads/",datasetName)

  if(!dir.exists(datasetName))
    stop("Directory does not exists")

  if(length(list.files(datasetName))==0)
    stop("no samples present in this dataset")

  regions <- list.files(datasetName, pattern = "*.gtf$",full.names = T)
  name_samples <- lapply(regions, function(x){gsub("*.gtf", "", basename(x))})
  if(length(regions) != 0)
  {
    sampleList <- lapply(regions, import.gff2)
    names(sampleList) <- name_samples
    gRange_list <- GRangesList(sampleList)
  }
  else
    stop("No GTF files present")

  meta <- list.files(datasetName, pattern = "*.gtf.meta$",full.names = T)
  if(length(meta) != 0)
  {
    meta_list <- lapply(meta, add_metadata)
    names(meta_list) <- name_samples
  }
  else
    stop("No meta GTF files present")

  metadata(gRange_list) <- meta_list
  return(gRange_list)
}

#'
#'
#'create GrangesList from sample in GDM (delimited / tabulated) format file
#'
#'
GRangesListFromGMQL.gdm <- function(datasetName = "DATA_SET_VAR_GDM/files")
{
  datasetName <- paste0("/Users/simone/Downloads/",datasetName)

  if(!dir.exists(datasetName))
    stop("Directory does not exists")

  if(length(list.files(datasetName))==0)
    stop("no samples present in this dataset")

  regions <- list.files(datasetName, pattern = "*.gdm$",full.names = T)
  name_samples <- lapply(regions, function(x){gsub("*.gdm", "", basename(x))})
  if(length(regions) != 0)
  {
    schema <- paste0(datasetName,"/test.schema")
    xml_schema <- read_xml(schema)
    list_field <- as_list(xml_schema)
    vector_field <- unlist(list_field)
    names(vector_field)=NULL
    sampleList <- lapply(regions,function(x){
      df <- read.delim(x,col.names = vector_field,header = FALSE)
      g <- makeGRangesFromDataFrame(df,keep.extra.columns = T,start.field = "left",end.field = "right")
    })
    names(sampleList) <- name_samples
    gRange_list <- GRangesList(sampleList)
  }
  else
    stop("No GDM files present")

  meta <- list.files(datasetName, pattern = "*.gdm.meta$",full.names = T)
  if(length(meta) != 0)
  {
    meta_list <- lapply(meta, add_metadata)
    names(meta_list) <- name_samples
  }
  else
    stop("No meta GDM files present")

  metadata(gRange_list) <- meta_list
  return(gRange_list)
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

add_metadata <- function(files)
{
  x <- scan(files, what="", sep="\n")
  y <- strsplit(x, "\t")
  names(y) <- sapply(y, `[[`, 1)
  listMeta <- lapply(y, `[`, -1)
}


