#############################
#       OPERATOR            #
#############################

OPERATOR <- function(value)
{
  op_list <- list(
    value = value
  )
  ## Set the name for the class
  class(op_list) <- "OPERATOR"
  return(op_list)
}

check.META_OPERATOR <- function(value)
{
  if(is.character(value) && length(value)>1)
    stop("value: no multiple string")

  if(!is.character(value))
    stop("value: is not a string")
}


META_OPERATOR <- function(value)
{
  op_list <- list(
    value = value
  )
  ## Set the name for the class
  class(op_list) <- "META_OPERATOR"
  return(op_list)
}

print.META_OPERATOR <- function(obj) {
  res <- as.character(obj)
  cat(res)
}

as.character.META_OPERATOR <- function(obj) {
  class <- class(obj)[1]
  val <- obj$value
  c(class,val)
}

take_value.META_OPERATOR <- function(obj){
  class <- class(obj)[1]
  val <- obj$value
  text <- switch(class,
                 "SUM" = paste0("sum_",val),
                 "MIN" = paste0("min_",val),
                 "MAX" = paste0("max_",val),
                 "COUNT" = paste0("count"),
                 "BAG" = paste0("bag_",val),
                 "AVG" = paste0("avg_",val),
                 "STD" = paste0("std_"),
                 "MEDIAN" = paste0("median_",val),
                 "Q1" = paste0("q1_",val),
                 "Q2" = paste0("q2_"),
                 "Q3" = paste0("q3_",val)
  )
  text
}

#' OPERATOR object class
#'
#' This class is used to create instances of operator object
#' to be used in GMQL functions that require operator on value
#'
#' OPERATOR object available are:
#' \itemize{
#' \item{SUM: prepared input parameter to be passed to library function sum,
#' performing all the type conversion needed}
#' \item{MIN: prepared input parameter to be passed to library function minimum,
#' performing all the type conversion needed}
#' \item{MAX: prepared input parameter to be passed to library function maximum,
#' performing all the type conversion needed}
#' \item{COUNT: prepared input parameter to be passed to library function count,
#' performing all the type conversion needed}
#' \item{BAG: prepared input parameter to be passed to library function bag,
#' this function creates comma-separated strings of distinct attribute values,
#' performing all the type conversion needed}
#' \item{AVG: prepared input parameter to be passed to library function arithmetic mean,
#' performing all the type conversion needed}
#' \item{STD: prepared input parameter to be passed to library function deviation standard,
#' performing all the type conversion needed}
#' \item{MEDIAN: prepared input parameter to be passed to library function median,
#' performing all the type conversion needed}
#' \item{Q1: prepared input parameter to be passed to library function first quartile,
#' performing all the type conversion needed}
#' \item{Q2: prepared input parameter to be passed to library function second quartile,
#' performing all the type conversion needed}
#' \item{Q3: prepared input parameter to be passed to library function third quartile,
#' performing all the type conversion needed}
#' }
#' @param value single string identifying name of metadata attribute
#'
#' @return no returned value
#'
#' @examples
#' \dontrun{
#'
#' }
#'
#' @export
#'
SUM <- function(value)
{
  check.META_OPERATOR(value)

  list <- list(
    value = value
  )
  ## Set the name for the class
  class(list) <- c("SUM","OPERATOR","META_OPERATOR")
  return(list)
}

#' OPERATOR object class
#'
#' This class is used to create instances of operator object
#' to be used in GMQL functions that require operator on value
#'
#' OPERATOR object available are:
#' \itemize{
#' \item{SUM: prepared input parameter to be passed to library function sum,
#' performing all the type conversion needed}
#' \item{MIN: prepared input parameter to be passed to library function minimum,
#' performing all the type conversion needed}
#' \item{MAX: prepared input parameter to be passed to library function maximum,
#' performing all the type conversion needed}
#' \item{COUNT: prepared input parameter to be passed to library function count,
#' performing all the type conversion needed}
#' \item{BAG: prepared input parameter to be passed to library function bag,
#' this function creates comma-separated strings of distinct attribute values,
#' performing all the type conversion needed}
#' \item{AVG: prepared input parameter to be passed to library function arithmetic mean,
#' performing all the type conversion needed}
#' \item{STD: prepared input parameter to be passed to library function deviation standard,
#' performing all the type conversion needed}
#' \item{MEDIAN: prepared input parameter to be passed to library function median,
#' performing all the type conversion needed}
#' \item{Q1: prepared input parameter to be passed to library function first quartile,
#' performing all the type conversion needed}
#' \item{Q2: prepared input parameter to be passed to library function second quartile,
#' performing all the type conversion needed}
#' \item{Q3: prepared input parameter to be passed to library function third quartile,
#' performing all the type conversion needed}
#' }
#' @param value single string identifying name of metadata attribute
#'
#' @return no returned value
#'
#' @examples
#' \dontrun{
#'
#' }
#'
#' @export
#'
MIN <- function(value)
{
  check.META_OPERATOR(value)

  list <- list(
    value = value
  )
  ## Set the name for the class
  class(list) <- c("MIN","OPERATOR","META_OPERATOR")
  return(list)
}

#' OPERATOR object class
#'
#' This class is used to create instances of operator object
#' to be used in GMQL functions that require operator on value
#'
#' OPERATOR object available are:
#' \itemize{
#' \item{SUM: prepared input parameter to be passed to library function sum,
#' performing all the type conversion needed}
#' \item{MIN: prepared input parameter to be passed to library function minimum,
#' performing all the type conversion needed}
#' \item{MAX: prepared input parameter to be passed to library function maximum,
#' performing all the type conversion needed}
#' \item{COUNT: prepared input parameter to be passed to library function count,
#' performing all the type conversion needed}
#' \item{BAG: prepared input parameter to be passed to library function bag,
#' this function creates comma-separated strings of distinct attribute values,
#' performing all the type conversion needed}
#' \item{AVG: prepared input parameter to be passed to library function arithmetic mean,
#' performing all the type conversion needed}
#' \item{STD: prepared input parameter to be passed to library function deviation standard,
#' performing all the type conversion needed}
#' \item{MEDIAN: prepared input parameter to be passed to library function median,
#' performing all the type conversion needed}
#' \item{Q1: prepared input parameter to be passed to library function first quartile,
#' performing all the type conversion needed}
#' \item{Q2: prepared input parameter to be passed to library function second quartile,
#' performing all the type conversion needed}
#' \item{Q3: prepared input parameter to be passed to library function third quartile,
#' performing all the type conversion needed}
#' }
#' @param value single string identifying name of metadata attribute
#'
#' @return no returned value
#'
#' @examples
#' \dontrun{
#'
#' }
#'
#' @export
#'
MAX <- function(value)
{
  check.META_OPERATOR(value)

  list <- list(
    value = value
  )
  ## Set the name for the class
  class(list) <- c("MAX","OPERATOR","META_OPERATOR")
  return(list)
}

#' OPERATOR object class
#'
#' This class is used to create instances of operator object
#' to be used in GMQL functions that require operator on value
#'
#' OPERATOR object available are:
#' \itemize{
#' \item{SUM: prepared input parameter to be passed to library function sum,
#' performing all the type conversion needed}
#' \item{MIN: prepared input parameter to be passed to library function minimum,
#' performing all the type conversion needed}
#' \item{MAX: prepared input parameter to be passed to library function maximum,
#' performing all the type conversion needed}
#' \item{COUNT: prepared input parameter to be passed to library function count,
#' performing all the type conversion needed}
#' \item{BAG: prepared input parameter to be passed to library function bag,
#' this function creates comma-separated strings of distinct attribute values,
#' performing all the type conversion needed}
#' \item{AVG: prepared input parameter to be passed to library function arithmetic mean,
#' performing all the type conversion needed}
#' \item{STD: prepared input parameter to be passed to library function deviation standard,
#' performing all the type conversion needed}
#' \item{MEDIAN: prepared input parameter to be passed to library function median,
#' performing all the type conversion needed}
#' \item{Q1: prepared input parameter to be passed to library function first quartile,
#' performing all the type conversion needed}
#' \item{Q2: prepared input parameter to be passed to library function second quartile,
#' performing all the type conversion needed}
#' \item{Q3: prepared input parameter to be passed to library function third quartile,
#' performing all the type conversion needed}
#' }
#' @param value single string identifying name of metadata attribute
#'
#' @return no returned value
#'
#' @examples
#' \dontrun{
#'
#' }
#'
#' @export
#' @export
#'
AVG <- function(value)
{
  check.META_OPERATOR(value)

  list <- list(
    value = value
  )
  ## Set the name for the class
  class(list) <- c("AVG","OPERATOR","META_OPERATOR")
  return(list)
}

#' OPERATOR object class
#'
#' This class is used to create instances of operator object
#' to be used in GMQL functions that require operator on value
#'
#' OPERATOR object available are:
#' \itemize{
#' \item{SUM: prepared input parameter to be passed to library function sum,
#' performing all the type conversion needed}
#' \item{MIN: prepared input parameter to be passed to library function minimum,
#' performing all the type conversion needed}
#' \item{MAX: prepared input parameter to be passed to library function maximum,
#' performing all the type conversion needed}
#' \item{COUNT: prepared input parameter to be passed to library function count,
#' performing all the type conversion needed}
#' \item{BAG: prepared input parameter to be passed to library function bag,
#' this function creates comma-separated strings of distinct attribute values,
#' performing all the type conversion needed}
#' \item{AVG: prepared input parameter to be passed to library function arithmetic mean,
#' performing all the type conversion needed}
#' \item{STD: prepared input parameter to be passed to library function deviation standard,
#' performing all the type conversion needed}
#' \item{MEDIAN: prepared input parameter to be passed to library function median,
#' performing all the type conversion needed}
#' \item{Q1: prepared input parameter to be passed to library function first quartile,
#' performing all the type conversion needed}
#' \item{Q2: prepared input parameter to be passed to library function second quartile,
#' performing all the type conversion needed}
#' \item{Q3: prepared input parameter to be passed to library function third quartile,
#' performing all the type conversion needed}
#' }
#' @param value single string identifying name of metadata attribute
#'
#' @return no returned value
#'
#' @examples
#' \dontrun{
#'
#' }
#'
#' @export
#'
BAG <- function(value)
{
  check.META_OPERATOR(value)

  list <- list(
    value = value
  )
  ## Set the name for the class
  class(list) <- c("BAG","OPERATOR","META_OPERATOR")
  return(list)
}

#' OPERATOR object class
#'
#' This class is used to create instances of operator object
#' to be used in GMQL functions that require operator on value
#'
#' OPERATOR object available are:
#' \itemize{
#' \item{SUM: prepared input parameter to be passed to library function sum,
#' performing all the type conversion needed}
#' \item{MIN: prepared input parameter to be passed to library function minimum,
#' performing all the type conversion needed}
#' \item{MAX: prepared input parameter to be passed to library function maximum,
#' performing all the type conversion needed}
#' \item{COUNT: prepared input parameter to be passed to library function count,
#' performing all the type conversion needed}
#' \item{BAG: prepared input parameter to be passed to library function bag,
#' this function creates comma-separated strings of distinct attribute values,
#' performing all the type conversion needed}
#' \item{AVG: prepared input parameter to be passed to library function arithmetic mean,
#' performing all the type conversion needed}
#' \item{STD: prepared input parameter to be passed to library function deviation standard,
#' performing all the type conversion needed}
#' \item{MEDIAN: prepared input parameter to be passed to library function median,
#' performing all the type conversion needed}
#' \item{Q1: prepared input parameter to be passed to library function first quartile,
#' performing all the type conversion needed}
#' \item{Q2: prepared input parameter to be passed to library function second quartile,
#' performing all the type conversion needed}
#' \item{Q3: prepared input parameter to be passed to library function third quartile,
#' performing all the type conversion needed}
#' }
#' @param value single string identifying name of metadata attribute
#'
#' @return no returned value
#'
#' @examples
#' \dontrun{
#'
#' }
#'
#' @export
#'
COUNT <- function()
{
  list <- list()
  ## Set the name for the class
  class(list) <- c("COUNT","OPERATOR","META_OPERATOR")
  return(list)
}
as.character.COUNT <- function(obj) {
  class <- class(obj)[1]
  c(class,"")
}
check.COUNT <- function(obj){}

#' OPERATOR object class
#'
#' This class is used to create instances of operator object
#' to be used in GMQL functions that require operator on value
#'
#' OPERATOR object available are:
#' \itemize{
#' \item{SUM: prepared input parameter to be passed to library function sum,
#' performing all the type conversion needed}
#' \item{MIN: prepared input parameter to be passed to library function minimum,
#' performing all the type conversion needed}
#' \item{MAX: prepared input parameter to be passed to library function maximum,
#' performing all the type conversion needed}
#' \item{COUNT: prepared input parameter to be passed to library function count,
#' performing all the type conversion needed}
#' \item{BAG: prepared input parameter to be passed to library function bag,
#' this function creates comma-separated strings of distinct attribute values,
#' performing all the type conversion needed}
#' \item{AVG: prepared input parameter to be passed to library function arithmetic mean,
#' performing all the type conversion needed}
#' \item{STD: prepared input parameter to be passed to library function deviation standard,
#' performing all the type conversion needed}
#' \item{MEDIAN: prepared input parameter to be passed to library function median,
#' performing all the type conversion needed}
#' \item{Q1: prepared input parameter to be passed to library function first quartile,
#' performing all the type conversion needed}
#' \item{Q2: prepared input parameter to be passed to library function second quartile,
#' performing all the type conversion needed}
#' \item{Q3: prepared input parameter to be passed to library function third quartile,
#' performing all the type conversion needed}
#' }
#' @param value single string identifying name of metadata attribute
#'
#' @return no returned value
#'
#' @examples
#' \dontrun{
#'
#' }
#'
#' @export
#'
STD <- function(value)
{
  check.META_OPERATOR(value)

  list <- list(
    value = value
  )
  ## Set the name for the class
  class(list) <- c("STD","META_OPERATOR")
  return(list)
}

#' OPERATOR object class
#'
#' This class is used to create instances of operator object
#' to be used in GMQL functions that require operator on value
#'
#' OPERATOR object available are:
#' \itemize{
#' \item{SUM: prepared input parameter to be passed to library function sum,
#' performing all the type conversion needed}
#' \item{MIN: prepared input parameter to be passed to library function minimum,
#' performing all the type conversion needed}
#' \item{MAX: prepared input parameter to be passed to library function maximum,
#' performing all the type conversion needed}
#' \item{COUNT: prepared input parameter to be passed to library function count,
#' performing all the type conversion needed}
#' \item{BAG: prepared input parameter to be passed to library function bag,
#' this function creates comma-separated strings of distinct attribute values,
#' performing all the type conversion needed}
#' \item{AVG: prepared input parameter to be passed to library function arithmetic mean,
#' performing all the type conversion needed}
#' \item{STD: prepared input parameter to be passed to library function deviation standard,
#' performing all the type conversion needed}
#' \item{MEDIAN: prepared input parameter to be passed to library function median,
#' performing all the type conversion needed}
#' \item{Q1: prepared input parameter to be passed to library function first quartile,
#' performing all the type conversion needed}
#' \item{Q2: prepared input parameter to be passed to library function second quartile,
#' performing all the type conversion needed}
#' \item{Q3: prepared input parameter to be passed to library function third quartile,
#' performing all the type conversion needed}
#' }
#' @param value single string identifying name of metadata attribute
#'
#' @return no returned value
#'
#' @examples
#' \dontrun{
#'
#' }
#'
#' @export
#'export
#'
MEDIAN <- function(value)
{
  check.META_OPERATOR(value)

  list <- list(
    value = value
  )
  ## Set the name for the class
  class(list) <- c("MEDIAN","OPERATOR","META_OPERATOR")
  return(list)
}

#' OPERATOR object class
#'
#' This class is used to create instances of operator object
#' to be used in GMQL functions that require operator on value
#'
#' OPERATOR object available are:
#' \itemize{
#' \item{SUM: prepared input parameter to be passed to library function sum,
#' performing all the type conversion needed}
#' \item{MIN: prepared input parameter to be passed to library function minimum,
#' performing all the type conversion needed}
#' \item{MAX: prepared input parameter to be passed to library function maximum,
#' performing all the type conversion needed}
#' \item{COUNT: prepared input parameter to be passed to library function count,
#' performing all the type conversion needed}
#' \item{BAG: prepared input parameter to be passed to library function bag,
#' this function creates comma-separated strings of distinct attribute values,
#' performing all the type conversion needed}
#' \item{AVG: prepared input parameter to be passed to library function arithmetic mean,
#' performing all the type conversion needed}
#' \item{STD: prepared input parameter to be passed to library function deviation standard,
#' performing all the type conversion needed}
#' \item{MEDIAN: prepared input parameter to be passed to library function median,
#' performing all the type conversion needed}
#' \item{Q1: prepared input parameter to be passed to library function first quartile,
#' performing all the type conversion needed}
#' \item{Q2: prepared input parameter to be passed to library function second quartile,
#' performing all the type conversion needed}
#' \item{Q3: prepared input parameter to be passed to library function third quartile,
#' performing all the type conversion needed}
#' }
#' @param value single string identifying name of metadata attribute
#'
#' @return no returned value
#'
#' @examples
#' \dontrun{
#'
#' }
#'
#' @export
#'
Q1 <- function(value)
{
  check.META_OPERATOR(value)

  list <- list(
    value = value
  )
  ## Set the name for the class
  class(list) <- c("Q1","META_OPERATOR")
  return(list)
}

#' OPERATOR object class
#'
#' This class is used to create instances of operator object
#' to be used in GMQL functions that require operator on value
#'
#' OPERATOR object available are:
#' \itemize{
#' \item{SUM: prepared input parameter to be passed to library function sum,
#' performing all the type conversion needed}
#' \item{MIN: prepared input parameter to be passed to library function minimum,
#' performing all the type conversion needed}
#' \item{MAX: prepared input parameter to be passed to library function maximum,
#' performing all the type conversion needed}
#' \item{COUNT: prepared input parameter to be passed to library function count,
#' performing all the type conversion needed}
#' \item{BAG: prepared input parameter to be passed to library function bag,
#' this function creates comma-separated strings of distinct attribute values,
#' performing all the type conversion needed}
#' \item{AVG: prepared input parameter to be passed to library function arithmetic mean,
#' performing all the type conversion needed}
#' \item{STD: prepared input parameter to be passed to library function deviation standard,
#' performing all the type conversion needed}
#' \item{MEDIAN: prepared input parameter to be passed to library function median,
#' performing all the type conversion needed}
#' \item{Q1: prepared input parameter to be passed to library function first quartile,
#' performing all the type conversion needed}
#' \item{Q2: prepared input parameter to be passed to library function second quartile,
#' performing all the type conversion needed}
#' \item{Q3: prepared input parameter to be passed to library function third quartile,
#' performing all the type conversion needed}
#' }
#' @param value single string identifying name of metadata attribute
#'
#' @return no returned value
#'
#' @examples
#' \dontrun{
#'
#' }
#'
#' @export
#'
Q2 <- function(value)
{
  check.META_OPERATOR(value)

  list <- list(
    value = value
  )
  ## Set the name for the class
  class(list) <- c("Q2","META_OPERATOR")
  return(list)
}

#' OPERATOR object class
#'
#' This class is used to create instances of operator object
#' to be used in GMQL functions that require operator on value
#'
#' OPERATOR object available are:
#' \itemize{
#' \item{SUM: prepared input parameter to be passed to library function sum,
#' performing all the type conversion needed}
#' \item{MIN: prepared input parameter to be passed to library function minimum,
#' performing all the type conversion needed}
#' \item{MAX: prepared input parameter to be passed to library function maximum,
#' performing all the type conversion needed}
#' \item{COUNT: prepared input parameter to be passed to library function count,
#' performing all the type conversion needed}
#' \item{BAG: prepared input parameter to be passed to library function bag,
#' this function creates comma-separated strings of distinct attribute values,
#' performing all the type conversion needed}
#' \item{AVG: prepared input parameter to be passed to library function arithmetic mean,
#' performing all the type conversion needed}
#' \item{STD: prepared input parameter to be passed to library function deviation standard,
#' performing all the type conversion needed}
#' \item{MEDIAN: prepared input parameter to be passed to library function median,
#' performing all the type conversion needed}
#' \item{Q1: prepared input parameter to be passed to library function first quartile,
#' performing all the type conversion needed}
#' \item{Q2: prepared input parameter to be passed to library function second quartile,
#' performing all the type conversion needed}
#' \item{Q3: prepared input parameter to be passed to library function third quartile,
#' performing all the type conversion needed}
#' }
#' @param value single string identifying name of metadata attribute
#'
#' @return no returned value
#'
#' @examples
#' \dontrun{
#'
#' }
#' ""
#' @export
#'
Q3 <- function(value)
{
  check.META_OPERATOR(value)

  list <- list(
    value = value
  )
  ## Set the name for the class
  class(list) <- c("Q3","META_OPERATOR")
  return(list)
}

