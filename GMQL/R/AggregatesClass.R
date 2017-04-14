#S3 class Aggregates used as Enum
AggregatesFunction <- list(
  SUM = "SUM",
  MIN = "MIN",
  MAX = "MAX",
  AVG = "AVG",
  BAG = "BAG",
  COUNT = "COUNT")
class(AggregatesFunction) <- "AggregatesFunction"

#S4 class of aggregates List idnetify the triple of aggregates

Aggregates <- setClass("Aggregates",
  representation(
    outputName = "character",
    aggrFunction  = "character",
    value ="character"
  ),
  setMethod("show",
            "Aggregates",
            function(object)
            {
              cat('output:', object@outputName, "\n")
              cat('aggr. function:', object@aggrFunction, "\n")
              if(object@value == "")
                cat("value:", NA, "\n")
              else
                cat("value:", object@value, "\n")
            }
  ),
  setAs("Aggregates", "character",
            function(from)
            {
              vec <- c(from@outputName, from@aggrFunction, from@value)
            }
  )
)


#Constructor

Aggr<- function(outName,aggrFunction,value = NULL)
{
  if (!is.character(aggrFunction))
    stop("the parameter must be a string ")

  aggr <- toupper(aggrFunction)
  if (aggr %in% names(AggregatesFunction))
    func <- AggregatesFunction[[aggr]]
  else
    stop("aggregates function not prensent in list only: SUM,MIN,MAX,AVG,BAG,COUNT")

  if((is.null(value) || value=="") && func != AggregatesFunction$COUNT)
    stop("value cannot be null or empty:\n Only with COUNT can be null or empty")

  if(func == AggregatesFunction$COUNT)
    value = ""

  new("Aggregates",outputName = outName, aggrFunction = func,value = value)
}
