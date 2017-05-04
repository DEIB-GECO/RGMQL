#S3 class Aggregates used as Enum
MetaAggregatesFunction <- list(
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
class(MetaAggregatesFunction) <- "MetaAggregatesFunction"

#S4 class of aggregates List idnetify the triple of aggregates

setOldClass("MetaAggregates")

setClass("MetaAggregates",
                       representation(
                         outputName = "character",
                         aggrFunction  = "character",
                         value ="character"
                       ),
                       setMethod("show",
                                 "MetaAggregates",
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
                       setAs("MetaAggregates", "character",
                             function(from)
                             {
                               vec <- c(from@outputName, from@aggrFunction, from@value)
                             }
                       )
)


#Constructor

MetaAggr<- function(outName,aggrFunction,value = NULL)
{
  if (!is.character(aggrFunction))
    stop("the parameter must be a string ")

  aggr <- toupper(aggrFunction)
  if (aggr %in% names(MetaAggregatesFunction))
    func <- MetaAggregatesFunction[[aggr]]
  else
    stop("aggregates function not prensent in list only: SUM,MIN,MAX,AVG,BAG,COUNT,STD,MEDIAN,Q1,Q2,Q3")

  if((is.null(value) || value=="") && func !=MetaAggregatesFunction$COUNT)
    stop("value cannot be null or empty:\n Only with COUNT can be null or empty")

  if(func == MetaAggregatesFunction$COUNT)
    value = ""

  new("MetaAggregates",outputName = outName, aggrFunction = func,value = value)
}
