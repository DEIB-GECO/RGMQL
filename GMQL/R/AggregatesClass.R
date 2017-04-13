#S3 class Agregates used as Enum
AggregatesFunction <- list(
  SUM = "SUM",
  MIN = "MIN",
  MAX = "MAX",
  AVG = "AVG",
  BAG = "BAG",
  COUNT = "COUNT"
)
class(AggregatesFunction) <- "AggregatesFunction"
check <- function(obj, ...)
  UseMethod("check")

check.AggregatesFunction <- function(obj, aggr)
{
  if (is.character(aggr))
  {
    idx <- toupper(aggr)
    if (idx %in% names(AggregatesFunction))
      return(obj[[idx]])
    else
      stop("aggregates function not prensent in list only: SUM,MIN,MAX,AVG,BAG,COUNT")
  }
  else
    stop("the second parameter must be a string ")
}

#S4 class of aggregates List idnetify the triple of aggregates

Aggregates <- setClass("Aggregates",
  representation(
    outputName = "character",
    aggrFunction  = "character",
    value = "character"
  )
)


#Constructor

Aggr<- function(outName,aggrFunction,value)
{
  func <- check(AggregatesFunction,aggrFunction)
  new("Aggregates",outputName = outName, aggrFunction = func,value = value)
}
