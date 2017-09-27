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

print.DISTAL <- function(obj) {
  print(as.character.DISTAL(obj))
}

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

#' DISTAL object class constructor
#'
#' This class constructor is used to create instances of DISTAL object
#' to be used in GMQL functions \code{\link{join}} in genometric predicate parameter
#' that require distal condition on value
#' In this case DLE: denotes the  less-equal distance clause, which selects all the regions of the experiment such
#' that their distance from the anchor region is less than, or equal to, 'value' bases.
#' There are two special less-equal distances clauses: DLE(-1) searches for regions of the experiment which
#' overlap with the anchor region (regardless the extent of the overlap),
#' while DLE(0) searched for experiment regions adjacent to, or overlapping, the anchor region}
#' 
#' @param value single string identifying distance between genomic regions in base pairs, 
#'
#' @return None
#' 
#' @seealso \code{\link{DGE}} \code{\link{MD}}  \code{\link{DOWN}} \code{\link{UP}}
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


#' DISTAL object class constructor
#'
#' This class constructor is used to create instances of DISTAL object
#' to be used in GMQL functions \code{\link{join}} in genometric predicate parameter
#' that require distal condition on value
#' In this case DGE: denotes the greater-equal distance clause, which selects all the regions of the experiment such
#' that their distance from the anchor region is greater than, or equal to, 'value' bases.
#' 
#' @param value single string identifying distance between genomic regions in base pairs, 
#'
#' @return None
#' @seealso \code{\link{DLE}} \code{\link{MD}}  \code{\link{DOWN}} \code{\link{UP}}
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


#' DISTAL object class constructor
#'
#' This class constructor is used to create instances of DISTAL object
#' to be used in GMQL functions \code{\link{join}} in genometric predicate parameter
#' that require distal condition on value
#' In this case MD: denotes the minimum distance clause, which selects the 'value' regions of the experiment 
#' at minimial distance from the anchor region.
#' When there are ties (i.e., regions at the same distance from the anchor region), 
#' regions of the experiment are kept in the result even if they exceed the 'value' limit.
#' 
#' @param value single string identifying number of regions at minimum distance beetwen expertiment
#' and anchor region  
#'
#' @return None
#' @seealso \code{\link{DLE}} \code{\link{DGE}}  \code{\link{DOWN}} \code{\link{UP}}
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


#' DISTAL object class constructor
#'
#' This class constructor is used to create instances of DISTAL object
#' to be used in GMQL functions \code{\link{join}} in genometric predicate parameter
#' that require distal condition on value
#' In this case UP: denotes the upstream direction of the genome.
#' They are interpreted as predicates that must hold on the regions of the experiment; 
#' UP is true when region of experiment is in the upstream genome of the anchor region.
#' When this clause is not present, distal conditions apply to both the directions of the genome.
#' 
#'
#' @return None
#' 
#' @seealso \code{\link{DLE}} \code{\link{DGE}}  \code{\link{DOWN}} \code{\link{MD}}
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

#' DISTAL object class constructor
#'
#' This class constructor is used to create instances of DISTAL object
#' to be used in GMQL functions \code{\link{join}} in genometric predicate parameter
#' that require distal condition on value
#' In this case DOWN: denotes the downstream direction of the genome.
#' They are interpreted as predicates that must hold on the regions of the experiment; 
#' DOWN is true when region of experiment is in the downstream genome of the anchor region.
#' When this clause is not present, distal conditions apply to both the directions of the genome.
#' 
#' 
#' @return None
#' 
#' @seealso \code{\link{DLE}} \code{\link{DGE}}  \code{\link{UP}} \code{\link{MD}}
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
