#assume that all files only one type of files and check the first file extension
readSample <- function(datasetName = "job_filename_guest_new14_20170316_162715_DATA_SET_VAR/files")
{
  path <- paste0("/Users/simone/Downloads/",datasetName)
  files <- list.files(path, pattern = "\\.gdm")
  if(length(files) != 0)
    readGDM(files)
  else
    readGTF(files)
}

#TODO: implement data structure ofr samples
readGDM <- function(files)
{
  sampleList <- list()
  for(i in seq_along(files))
  {
    path_file <- paste0(path,"/",files[[i]])
    ext <- tools::file_ext(path_file)
    if(ext == "meta")
    {
      x <- scan(path_file, what="", sep="\n")
      y <- strsplit(x, "\t")
      names(y) <- sapply(y, `[[`, 1)
      listMeta <- lapply(y, `[`, -1)
    }
    else{
      regionDf <- read.delim(path_file)
      nameSample <- files[[i]]
    }

    c(sampleList, list())
  }
}

readGTF <- function(files)
{

}

