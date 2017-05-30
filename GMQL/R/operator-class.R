#############################
#       OPERATOR            #
#############################

#' OPERATOR object class
#'
#'
#' OPERATOR object available are:
#' \itemize{
#' \item{SUM}
#' \item{MIN}
#' \item{MAX}
#' \item{COUNT}
#' \item{BAG}
#' \item{AVG}
#' }
#'
#' @details
#' you never use a parent class OPERATOR()
#'
#'
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

#' META_OPERATOR object class
#'
#'
#' META_OPERATOR object available are:
#' \itemize{
#' \item{SUM}
#' \item{MIN}
#' \item{MAX}
#' \item{COUNT}
#' \item{BAG}
#' \item{AVG}
#' \item{STD}
#' \item{MEDIAN}
#' \item{Q1}
#' \item{Q2}
#' \item{Q3}
#' }
#' @details
#' you never use a parent class META_OPERATOR()
#'
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
                 "COUNT" = paste0("count_"),
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

MEDIAN <- function(value)
{
  check.META_OPERATOR(value)

  list <- list(
    value = value
  )
  ## Set the name for the class
  class(list) <- c("MEDIAN","META_OPERATOR")
  return(list)
}

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

