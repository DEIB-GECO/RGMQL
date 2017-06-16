#############################
#       DISTAL          #
############################


DISTAL <- function(value)
{
  op_list <- list(
    value = value
  )
  ## Set the name for the class
  class(op_list) <- "DISTAL"
  return(op_list)
}

print.DISTAL <- function(obj) {}

as.character.DISTAL <- function(obj) {
  class <- class(obj)[1]
  val <- obj$value
  c(class,val)
}

check.DISTAL <- function(value)
{
  if(!is.numeric(value))
    stop("value: is not a numeric")

  if(is.numeric(value) && length(value)>1)
    stop("value: no multiple string")

}

#' DISTAL object class
#'
#' This class is used to create instances of distal object
#' to be used in GMQL functions \link{\code{JOIN}} in genometric predicate parameter
#' that require distal condition on value
#'
#' DISTAL object available are:
#' \itemize{
#' \item{UP: called the upstream clause, which refers to the upstream  directions of the genome}
#' \item{DOWN: called the downstream clause, which refers to the downstream directions of the genome}
#' \item{DGE: denotes the  greater-equal distance clause, which selects all the regions of the experiment such
#' that their distance from the anchor region is greater than, or equal to, 'value' bases.}
#' \item{DLE: denotes the  less-equal distance clause, which selects all the regions of the experiment such
#' that their distance from the anchor region is less than, or equal to, 'value' bases.
#' There are two special less-equal distances clauses: DLE(-1) searches for regions of the experiment which
#' overlap with the anchor region (regardless the extent of the overlap),
#' while DLE(0) searched for experiment regions adjacent to, or overlapping, the anchor region}
#' \item{MD: denotes the minimum distance clause, which selects the first 'value' regions of an experiment
#' sample at minimal distance from an anchor region of an anchor dataset sample.
#' In case of ties (i.e., regions at the same distance from the anchor region),
#' all tied experiment regions are kept in the result, even if they would exceed the limit of value;}
#' }
#'
#' @param value single string identifying number of regions put on distal condition
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
DLE <- function(value)
{
  check.DISTAL(value)

  list <- list(
    value = as.integer(value)
  )
  ## Set the name for the class
  class(list) <- c("DLE","DISTAL")
  return(list)
}


#' DISTAL object class
#'
#' This class is used to create instances of distal object
#' to be used in GMQL functions \link{\code{JOIN}} in genometric predicate parameter
#' that require distal condition on value
#'
#' DISTAL object available are:
#' \itemize{
#' \item{UP: called the upstream clause, which refers to the upstream  directions of the genome}
#' \item{DOWN: called the downstream clause, which refers to the downstream directions of the genome}
#' \item{DGE: denotes the  greater-equal distance clause, which selects all the regions of the experiment such
#' that their distance from the anchor region is greater than, or equal to, 'value' bases.}
#' \item{DLE: denotes the  less-equal distance clause, which selects all the regions of the experiment such
#' that their distance from the anchor region is less than, or equal to, 'value' bases.
#' There are two special less-equal distances clauses: DLE(-1) searches for regions of the experiment which
#' overlap with the anchor region (regardless the extent of the overlap),
#' while DLE(0) searched for experiment regions adjacent to, or overlapping, the anchor region}
#' \item{MD: denotes the minimum distance clause, which selects the first 'value' regions of an experiment
#' sample at minimal distance from an anchor region of an anchor dataset sample.
#' In case of ties (i.e., regions at the same distance from the anchor region),
#' all tied experiment regions are kept in the result, even if they would exceed the limit of value;}
#' }
#'
#' @param value single string identifying number of regions put on distal condition
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
DGE <- function(value)
{
  check.DISTAL(value)

  list <- list(
    value = as.integer(value)
  )
  ## Set the name for the class
  class(list) <- c("DGE","DISTAL")
  return(list)
}


#' DISTAL object class
#'
#' This class is used to create instances of distal object
#' to be used in GMQL functions \link{\code{JOIN}} in genometric predicate parameter
#' that require distal condition on value
#'
#' DISTAL object available are:
#' \itemize{
#' \item{UP: called the upstream clause, which refers to the upstream  directions of the genome}
#' \item{DOWN: called the downstream clause, which refers to the downstream directions of the genome}
#' \item{DGE: denotes the  greater-equal distance clause, which selects all the regions of the experiment such
#' that their distance from the anchor region is greater than, or equal to, 'value' bases.}
#' \item{DLE: denotes the  less-equal distance clause, which selects all the regions of the experiment such
#' that their distance from the anchor region is less than, or equal to, 'value' bases.
#' There are two special less-equal distances clauses: DLE(-1) searches for regions of the experiment which
#' overlap with the anchor region (regardless the extent of the overlap),
#' while DLE(0) searched for experiment regions adjacent to, or overlapping, the anchor region}
#' \item{MD: denotes the minimum distance clause, which selects the first 'value' regions of an experiment
#' sample at minimal distance from an anchor region of an anchor dataset sample.
#' In case of ties (i.e., regions at the same distance from the anchor region),
#' all tied experiment regions are kept in the result, even if they would exceed the limit of value;}
#' }
#'
#' @param value single string identifying number of regions put on distal condition
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
MD <- function(value)
{
  check.DISTAL(value)

  list <- list(
    value = as.integer(value)
  )
  ## Set the name for the class
  class(list) <- c("MD","DISTAL")
  return(list)
}


#' DISTAL object class
#'
#' This class is used to create instances of distal object
#' to be used in GMQL functions \link{\code{JOIN}} in genometric predicate parameter
#' that require distal condition on value
#'
#' DISTAL object available are:
#' \itemize{
#' \item{UP: called the upstream clause, which refers to the upstream  directions of the genome}
#' \item{DOWN: called the downstream clause, which refers to the downstream directions of the genome}
#' \item{DGE: denotes the  greater-equal distance clause, which selects all the regions of the experiment such
#' that their distance from the anchor region is greater than, or equal to, 'value' bases.}
#' \item{DLE: denotes the  less-equal distance clause, which selects all the regions of the experiment such
#' that their distance from the anchor region is less than, or equal to, 'value' bases.
#' There are two special less-equal distances clauses: DLE(-1) searches for regions of the experiment which
#' overlap with the anchor region (regardless the extent of the overlap),
#' while DLE(0) searched for experiment regions adjacent to, or overlapping, the anchor region}
#' \item{MD: denotes the minimum distance clause, which selects the first 'value' regions of an experiment
#' sample at minimal distance from an anchor region of an anchor dataset sample.
#' In case of ties (i.e., regions at the same distance from the anchor region),
#' all tied experiment regions are kept in the result, even if they would exceed the limit of value;}
#' }
#'
#' @param value single string identifying number of regions put on distal condition
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
UP <- function()
{
  list <- list()
  ## Set the name for the class
  class(list) <- c("UP","DISTAL")
  return(list)
}
as.character.UP <- function(obj) {
  class <- class(obj)[1]
  c(class,"")
}

#' DISTAL object class
#'
#' This class is used to create instances of distal object
#' to be used in GMQL functions \link{\code{JOIN}} in genometric predicate parameter
#' that require distal condition on value
#'
#' DISTAL object available are:
#' \itemize{
#' \item{UP: called the upstream clause, which refers to the upstream  directions of the genome}
#' \item{DOWN: called the downstream clause, which refers to the downstream directions of the genome}
#' \item{DGE: denotes the  greater-equal distance clause, which selects all the regions of the experiment such
#' that their distance from the anchor region is greater than, or equal to, 'value' bases.}
#' \item{DLE: denotes the  less-equal distance clause, which selects all the regions of the experiment such
#' that their distance from the anchor region is less than, or equal to, 'value' bases.
#' There are two special less-equal distances clauses: DLE(-1) searches for regions of the experiment which
#' overlap with the anchor region (regardless the extent of the overlap),
#' while DLE(0) searched for experiment regions adjacent to, or overlapping, the anchor region}
#' \item{MD: denotes the minimum distance clause, which selects the first 'value' regions of an experiment
#' sample at minimal distance from an anchor region of an anchor dataset sample.
#' In case of ties (i.e., regions at the same distance from the anchor region),
#' all tied experiment regions are kept in the result, even if they would exceed the limit of value;}
#' }
#'
#' @param value single string identifying number of regions put on distal condition
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
DOWN <- function()
{
  list <- list()
  ## Set the name for the class
  class(list) <- c("DOWN","DISTAL")
  return(list)
}


as.character.DOWN <- function(obj) {
  class <- class(obj)[1]
  c(class,"")
}
