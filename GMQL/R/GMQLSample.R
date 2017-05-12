
#assume that all files only one type of files and check the first file extension
readSamples <- function(datasetName = "DATA_SET_VAR_GTF/files")
{
  datasetName <- paste0("/Users/simone/Downloads/",datasetName)

  if(!dir.exists(datasetName))
    stop("Directory does not exists")

  if(length(list.files(datasetName))==0)
    stop("no samples present in this dataset")

  start <- Sys.time()

  regions <- list.files(datasetName, pattern = "*.gtf$",full.names = T)
  name_samples <- lapply(regions, function(x){gsub("*.gtf", "", basename(x))})
  if(length(regions) != 0)
  {
    sampleList <- lapply(regions, import.gff2)
    names(sampleList) <- name_samples
    gRange_list <- GRangesList(sampleList)
  }

  meta <- list.files(datasetName, pattern = "*.gtf.meta$",full.names = T)
  if(length(meta) != 0)
  {
    meta_list <- lapply(meta, add_metadata)
    names(meta_list) <- name_samples
  }

  metadata(gRange_list) <- meta_list
  end <- Sys.time()
  diff <- end - start
  print(diff)
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

readGDM <- function()
{
  start <- Sys.time()
  path <- "/Users/simone/Downloads/DATA_SET_VAR/files/S_00000.gdm"
  df <- read.delim(path,col.names = c("a","b","c","d","e","f","g","h","j","k","l","m"))
  g <- makeGRangesFromDataFrame(df)
  end <- Sys.time()
  diff <- end - start
  print(diff)
  return(g)
}







