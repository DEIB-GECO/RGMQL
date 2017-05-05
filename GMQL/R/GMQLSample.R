#assume that all files only one type of files and check the first file extension
readSample <- function(datasetName = "DATA_SET_VAR/files")
{
  path <- paste0("/Users/simone/Downloads/",datasetName)
  files <- list.files(path, pattern = "\\.gdm")
  if(length(files) != 0)
    readGDM(files)
  else
    readGTF(files)
}

#TODO: implement data structure of samples
readGDM2 <- function(files)
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

readGDM <- function()
{
  start <- Sys.time()
  path = "/Users/simone/Downloads/DATA_SET_VAR/files/S_00000.gdm"
  df = read.delim(path)
  print(class(df))
  end <- Sys.time()
  diff <- end - start
  print(diff)
}

readGTF2 <- function()
{
  start <- Sys.time()
  path = "/Users/simone/Downloads/RESULT_DS/files/S_00000.gtf"
  gr = readGFFAsGRanges(path)
  print(class(gr))
  end <- Sys.time()
  diff <- end - start
  print(diff)
}

readGTF <- function()
{
  start <- Sys.time()
  path = "/Users/simone/Downloads/RESULT_DS/files/S_00000.gtf"
  gr = import.gff(path)
  print(class(gr))
  end <- Sys.time()
  diff <- end - start
  print(diff)
}

