
setOldClass("SemiJoinParam")

setClass("SemiJoinParam",
         representation(
           attributes = "character",
           include  = "logical",
           dataset ="character"
         ),
         setMethod("show",
                   "SemiJoinParam",
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
         setAs("SemiJoinParam", "character",
               function(from)
               {
                 vec <- c(from@outputName, from@aggrFunction, from@value)
               }
         )
)


#Constructor

SemiJoin<- function(attributes,include,dataset)
{
  if (!is.character(attributes))
    stop("the parameter must be a string ")

  if((is.null(value) || value=="") && func != AggregatesFunction$COUNT)
    stop("value cannot be null or empty:\n Only with COUNT can be null or empty")

  if(func == AggregatesFunction$COUNT)
    value = ""

  new("SemiJoinParam",attributes = attributes, include = include,dataset = dataset)
}
