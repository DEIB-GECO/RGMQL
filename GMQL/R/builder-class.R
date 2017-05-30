#############################
#       BUILDER            #
############################


#' BUILDER object class
#'
#'
#' BUILDER object available are:
#' \itemize{
#' \item{LEFT}
#' \item{RIGHT}
#' \item{CONTIG}
#' \item{INTERSECTION}
#' }
#'
#' @details
#' you never use a parent class BUILDER()
#'
BUILDER <- function()
{
  ## Set the name for the class
  class(op_list) <- "BUILDER"
  return(op_list)
}

print.BUILDER <- function(obj) {
  as.character(obj)
}

as.character.BUILDER <- function(obj) {
  class <- class(obj)[1]
}

LEFT <- function()
{
  list <- list()
  ## Set the name for the class
  class(list) <- c("LEFT","BUILDER")
  return(list)
}

RIGHT <- function()
{
  list <- list()
  ## Set the name for the class
  class(list) <- c("RIGHT","BUILDER")
  return(list)
}

CONTIG <- function()
{
  list <- list()
  ## Set the name for the class
  class(list) <- c("CONTIG","BUILDER")
  return(list)
}

INTERSECTION <- function()
{
  list <- list()
  ## Set the name for the class
  class(list) <- c("INTERSECTION","BUILDER")
  return(list)
}
