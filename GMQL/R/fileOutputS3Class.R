
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
    if(idx %in% names(fileOutput))
      return(obj[[idx]])
    else
      stop("output not prensent in list only: TAB,GTF")
  }
  else
    stop("the second parameter must be a string ")
}

