

#S3 class CoverFlag used as Enum
coverFlag <- list(COVER = "COVER",
                  FLAT = "FLAT",
                  SUMMIT = "SUMMIT",
                  HISTOGRAM = "HISTOGRAM")
class(coverFlag) <- "coverFlag"
check <- function(obj, ...)
  UseMethod("check")

check.coverFlag <- function(obj, text)
{
  if(is.character(text))
  {
    idx <- toupper(text)
    if(idx %in% names(coverFlag))
      return(obj[[idx]])
    else
      stop("coverflag name not prensent in list only: COVER,FLAT,SUMMIT,HISTOGRAM")
  }
  else
    stop("the second parameter must be a string ")
}


#S3 class fileOutput used as Enum
fileOutput <- list(GTF = "GTF",
                  TAB = "TAB")
class(fileOutput) <- "fileOutput"
check <- function(obj, ...)
  UseMethod("check")

check.fileOutput <- function(obj, text)
{
  if(is.character(text))
  {
    idx <- toupper(text)
    if(idx %in% names(coverFlag))
      return(obj[[idx]])
    else
      stop("coverflag name not prensent in list only: TAB,GTF")
  }
  else
    stop("the second parameter must be a string ")
}

#S4 class Samples in Dataset




#S4 class SemiJoinParam

SemiJoinParam <- setClass(
  # Set the name for the class
  "SemiJoinParam",

  # Define the slots
  slots = c(
    semiJoinMeta = "character",
    operation_in  = "logical",
    dataset_path_join_IN = "character"
  ),

  # Set the default values for the slots. (optional)
  prototype=list(
    semiJoinMeta = "",
    operation_in  = FALSE,
    dataset_path_join_IN = ""
  ),

  # Make a function that can test to see if the data is consistent.
  # This is not called if you have an initialize function defined!
  validity=function(object)
  {
    return(TRUE)
  }
)

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



prova <- function()
{
  x <<- data.frame(a = 1:10, b = sample(x = ".",size = 10,replace = T), c = 2:11, d = sample(x = c(".",";"),size = 10,replace = T))
  l <<- which(sapply(x, function(x) any(x == ".")))

  df <<- data.frame(col1 = c(letters[1:4],"a"),col2 = 1:5,col3 = letters[10:14])
  which(sapply(df, function(x) any(x == "a")))


}
