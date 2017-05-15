#S3 class Aggregates used as Enum
AggregatesFunction <- list(
  SUM = "SUM",
  MIN = "MIN",
  MAX = "MAX",
  AVG = "AVG",
  BAG = "BAG",
  COUNT = "COUNT")
class(AggregatesFunction) <- "AggregatesFunction"

check <- function(object) {
  UseMethod("check")
}

check.AggregatesFunction <- function(object)
{
  aggr <- toupper(object)
  if (aggr %in% names(AggregatesFunction))
    func <- AggregatesFunction[[aggr]]
  else
    stop("aggregates function not prensent in list, only: SUM,MIN,MAX,AVG,BAG,COUNT")
}


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

check.MetaAggregatesFunction <- function(object)
{
  aggr <- toupper(object)
  if (aggr %in% names(MetaAggregatesFunction))
    func <- MetaAggregatesFunction[[aggr]]
  else
    stop("aggregates function not prensent in list, only: SUM,MIN,MAX,AVG,BAG,COUNT,STD,MEDIAN,Q1,Q2,Q3")
}

#S3 class Ordering used as Enum
Ordering <- list(
  ASC = "ASC",
  DESC = "DESC")
class(Ordering) <- "Ordering"

check <- function(object) {
  UseMethod("check")
}

check.Ordering <- function(object)
{
  ord <- toupper(object)
  if (ord %in% names(Ordering))
    func <- Ordering[[ord]]
  else
    stop("Order ASC or DESC")
}

