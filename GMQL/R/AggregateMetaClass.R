#S3 class Aggregates used as Enum
AggregatesMetaFunction <- list(
  SUM = "SUM",
  MIN = "MIN",
  MAX = "MAX",
  AVG = "AVG",
  BAG = "BAG",
  STD = "STD",
  MEDIAN = "MEDIAN",
  Q2 = "Q2",
  Q1 = "Q1",
  Q3 = "Q3",
  COUNT = "COUNT")
class(AggregatesMetaFunction) <- "AggregatesMetaFunction"

#S4 class of aggregates List idnetify the triple of aggregates

setOldClass("AggregatesMeta")

setClass("AggregatesMeta",
                       representation(
                         outputName = "character",
                         aggrFunction  = "character",
                         value ="character"
                       ),
                       setMethod("show",
                                 "AggregatesMeta",
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
                       setAs("AggregatesMeta", "character",
                             function(from)
                             {
                               vec <- c(from@outputName, from@aggrFunction, from@value)
                             }
                       )
)


#Constructor

AggrMeta<- function(outName,aggrFunction,value = NULL)
{
  if (!is.character(aggrFunction))
    stop("the parameter must be a string ")

  aggr <- toupper(aggrFunction)
  if (aggr %in% names(AggregatesFunction))
    func <- AggregatesFunction[[aggr]]
  else
    stop("aggregates function not prensent in list only: SUM,MIN,MAX,AVG,BAG,COUNT,STD,MEDIAN,Q1,Q2,Q3")

  if((is.null(value) || value=="") && func !=AggregatesMetaFunction$COUNT)
    stop("value cannot be null or empty:\n Only with COUNT can be null or empty")

  if(func == AggregatesMetaFunction$SUM || func == AggregatesMetaFunction$MIN ||
     func == AggregatesMetaFunction$MAX || func == AggregatesMetaFunction$AVG ||
     func == AggregatesMetaFunction$STD || func == AggregatesMetaFunction$MEDIAN
     && !is.numeric(value))
  {
    stop("value must be numeric type with that function")
  }

  val <- as.character(value)

  if(func == AggregatesMetaFunction$COUNT)
    value = ""

  new("AggregatesMeta",outputName = outName, aggrFunction = func,value = val)
}
