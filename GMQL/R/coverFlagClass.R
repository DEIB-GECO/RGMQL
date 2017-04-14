
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
