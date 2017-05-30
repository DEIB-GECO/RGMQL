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


########################
#     SCHEMA          #
#######################


#' SCHEMA object class
#'
#'
#' SCHEMA object available are:
#' \itemize{
#' \item{NARROWPEAK}
#' \item{BROADPEAK}
#' \item{VCF}
#' \item{BED}
#' \item{BEDGRAPH}
#' }
#'
#' @details
#' you never use a parent class SCHEMA()
#'
SCHEMA <- function()
{
  ## Set the name for the class
  class(op_list) <- "SCHEMA"
  return(op_list)
}

print.SCHEMA<- function(obj) {
  as.character(obj)
}

as.character.SCHEMA <- function(obj) {
  class <- class(obj)[1]
}

BED <- function()
{
  ## Set the name for the class
  class(op_list) <- c("BED","SCHEMA")
  return(op_list)
}

NARROWPEAK <- function()
{
  ## Set the name for the class
  class(op_list) <- c("NARROWPEAK","SCHEMA")
  return(op_list)
}

BEDGRAPH <- function()
{
  ## Set the name for the class
  class(op_list) <- c("BEDGRAPH","SCHEMA")
  return(op_list)
}

VCF <- function()
{
  ## Set the name for the class
  class(op_list) <- c("VCF","SCHEMA")
  return(op_list)
}

BROADPEAK <- function()
{
  ## Set the name for the class
  class(op_list) <- c("BROADPEAK","SCHEMA")
  return(op_list)
}



