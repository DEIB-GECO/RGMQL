########################
#     PARSER          #
#######################


#' PARSER object class
#'
#'
#' PARSER object available are:
#' \itemize{
#' \item{BedParser}
#' \item{ANNParser}
#' \item{BroadProjParser}
#' \item{NarrowPeakParser}
#' \item{BasicParser}
#' \item{RnaSeqParser}
#' \item{CustomParser}
#' }
#'
#' @details
#' you never use a parent class PARSER()
#'
PARSER <- function()
{
  ## Set the name for the class
  class(op_list) <- "PARSER"
  return(op_list)
}

print.PARSER<- function(obj) {
  class <- class(obj)[1]
}

as.character.PARSER <- function(obj) {
  class <- class(obj)[1]
}

BedParser <- function()
{
  ## Set the name for the class
  class(list) <- c("BedParser","PARSER")
  return(list)
}

ANNParser <- function()
{
  ## Set the name for the class
  class(list) <- c("ANNParser","PARSER")
  return(list)
}

BroadProjParser <- function()
{
  ## Set the name for the class
  class(list) <- c("BroadProjParser","PARSER")
  return(list)
}

BasicParser <- function()
{
  ## Set the name for the class
  class(list) <- c("BasicParser","PARSER")
  return(list)
}

NarrowPeakParser <- function()
{
  ## Set the name for the class
  class(list) <- c("NarrowPeakParser","PARSER")
  return(list)
}

RnaSeqParser <- function()
{
  ## Set the name for the class
  class(list) <- c("RnaSeqParser","PARSER")
  return(list)
}

CustomParser <- function()
{
  ## Set the name for the class
  class(list) <- c("CustomParser","PARSER")
  return(list)
}
