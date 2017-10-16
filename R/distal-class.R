##########################
#       DISTAL          #
#########################


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
#' to be used in GMQL functions that use genometric predicate parameter
#' requiring distal condition on value
#' In this case DLE: denotes the less distance clause, which selects all the regions of the experiment such
#' that their distance from the anchor region is less than, or equal to, 'value' bases.
#' 
#' @param value single string identifying distance between genomic regions in base pairs, 
#'
#' @return None
#' 
#' @seealso \code{\link{DGE}} \code{\link{DLE}} \code{\link{MD}}  \code{\link{DOWN}} \code{\link{UP}} \code{\link{DG}}
#' 
#' @examples
#' 
#' ### Given a dataset HM and one called TSS with a sample including Transcription Start Site annotations,
#' ## it searches for those regions of hm that are at a minimal distance from a transcription start site (TSS) 
#' ## and takes the first/closest one for each TSS, 
#' ## provided that such distance is lesser than 1200 bases and joined TSS and HM samples are obtained 
#' ## from the same provider (joinby clause).
#' 
#' initGMQL("gtf")
#' test_path <- system.file("example","DATA_SET_VAR_GTF",package = "RGMQL")
#' test_path2 <- system.file("example","DATA_SET_VAR_GDM",package = "RGMQL")
#' TSS = readDataset(test_path)
#' HM = readDataset(test_path2)
#' join_data = join(TSS,HM,genometric_predicate=list(list(MD(1),DL(1200))),c("provider"),region_output="RIGHT")
#'
#' 
#' @export
#'
DL <- function(value)
{
  check.DISTAL(value)
  
  list <- list(
    value = as.integer(value)
  )
  ## Set the name for the class
  class(list) <- c("DL","DISTAL")
  return(list)
}
#' DISTAL object class constructor
#'
#' This class constructor is used to create instances of DISTAL object
#' to be used in GMQL functions that use genometric predicate parameter
#' requiring distal condition on value
#' In this case DLE: denotes the great distance clause, which selects all the regions of the experiment such
#' that their distance from the anchor region is less than, or equal to, 'value' bases.
#' @param value single string identifying distance between genomic regions in base pairs, 
#'
#' @return None
#' 
#' @seealso \code{\link{DGE}} \code{\link{DLE}} \code{\link{MD}}  \code{\link{DOWN}} \code{\link{UP}} \code{\link{DL}}
#' 
#' @examples
#' 
#' ### Given a dataset HM and one called TSS with a sample including Transcription Start Site annotations,
#' ## it searches for those regions of hm that are at a minimal distance from a transcription start site (TSS) 
#' ## and takes the first/closest one for each TSS, 
#' ## provided that such distance is greater than 12 bases and joined TSS and HM samples are obtained 
#' ## from the same provider (joinby clause).
#' 
#' initGMQL("gtf")
#' test_path <- system.file("example","DATA_SET_VAR_GTF",package = "RGMQL")
#' test_path2 <- system.file("example","DATA_SET_VAR_GDM",package = "RGMQL")
#' TSS = readDataset(test_path)
#' HM = readDataset(test_path2)
#' join_data = join(TSS,HM,genometric_predicate=list(list(MD(1),DG(12))),c("provider"),region_output="RIGHT")
#'
#' 
#' @export
#'
DG <- function(value)
{
  check.DISTAL(value)
  
  list <- list(
    value = as.integer(value)
  )
  ## Set the name for the class
  class(list) <- c("DG","DISTAL")
  return(list)
}

#' DISTAL object class constructor
#'
#' This class constructor is used to create instances of DISTAL object
#' to be used in GMQL functions that use genometric predicate parameter
#' requiring distal condition on value
#' In this case DLE: denotes the  less-equal distance clause, which selects all the regions of the experiment such
#' that their distance from the anchor region is less than, or equal to, 'value' bases.
#' There are two special less-equal distances clauses: DLE(-1) searches for regions of the experiment which
#' overlap with the anchor region (regardless the extent of the overlap),
#' while DLE(0) searched for experiment regions adjacent to, or overlapping, the anchor region
#' 
#' @param value single string identifying distance between genomic regions in base pairs, 
#'
#' @return None
#' 
#' @seealso \code{\link{DGE}} \code{\link{DL}} \code{\link{MD}}  \code{\link{DOWN}} \code{\link{UP}} \code{\link{DG}}
#' 
#' @examples
#' 
#' ### Given a dataset HM and one called TSS with a sample including Transcription Start Site annotations,
#' ## it searches for those regions of hm that are at a minimal distance from a transcription start site (TSS) 
#' ## and takes the first/closest one for each TSS, 
#' ## provided that such distance is lesser than 120K bases and joined TSS and HM samples are obtained 
#' ## from the same provider (joinby clause).
#' 
#' initGMQL("gtf")
#' test_path <- system.file("example","DATA_SET_VAR_GTF",package = "RGMQL")
#' test_path2 <- system.file("example","DATA_SET_VAR_GDM",package = "RGMQL")
#' TSS = readDataset(test_path)
#' HM = readDataset(test_path2)
#' join_data = join(TSS,HM,genometric_predicate=list(list(MD(1),DLE(120000))),c("provider"),region_output="RIGHT")
#'
#' 
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
#' to be used in GMQL functions that use genometric predicate parameter
#' requiring distal condition on value
#' In this case DGE: denotes the greater-equal distance clause, which selects all the regions of the experiment such
#' that their distance from the anchor region is greater than, or equal to, 'value' bases.
#' 
#' @param value single string identifying distance between genomic regions in base pairs, 
#'
#' @return None
#' 
#' @seealso \code{\link{DLE}} \code{\link{DL}} \code{\link{MD}}  \code{\link{DOWN}} \code{\link{UP}} \code{\link{DG}}
#' 
#' @examples
#' 
#' ## Given a dataset 'hm' and one called 'tss' with a sample including Transcription Start Site annotations,
#' ## it searches for those regions of hm that are at a minimal distance from a transcription start site (TSS) 
#' ## and takes the first/closest one for each TSS, 
#' ## provided that such distance is greater than 120K bases and joined 'tss' and 'hm' samples are obtained 
#' ## from the same provider (joinby clause).
#' 
#' initGMQL("gtf")
#' test_path <- system.file("example","DATA_SET_VAR_GTF",package = "RGMQL")
#' test_path2 <- system.file("example","DATA_SET_VAR_GDM",package = "RGMQL")
#' TSS = readDataset(test_path)
#' HM = readDataset(test_path2)
#' join_data = join(TSS,HM,genometric_predicate=list(list(MD(1),DGE(120000))),c("provider"),region_output="RIGHT")
#'
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
#' to be used in GMQL functions that use genometric predicate parameter
#' requiring distal condition on value
#' In this case MD: denotes the minimum distance clause, which selects the 'value' regions of the experiment 
#' at minimial distance from the anchor region.
#' When there are ties (i.e., regions at the same distance from the anchor region), 
#' regions of the experiment are kept in the result even if they exceed the 'value' limit.
#' 
#' @param value single string identifying number of regions at minimum distance beetwen expertiment
#' and anchor region  
#'
#' @return None
#' @seealso \code{\link{DLE}} \code{\link{DGE}} \code{\link{DL}}  \code{\link{DOWN}} \code{\link{UP}} \code{\link{DG}}
#' 
#' @examples
#' 
#' 
#' ### Given a dataset 'hm' and one called 'tss' with a sample including Transcription Start Site annotations,
#' ## it searches for those regions of hm that are at a minimal distance from a transcription start site (TSS) 
#' ## and takes the first/closest one for each TSS, 
#' ## provided that such distance is greater than 120K bases and joined 'tss' and 'hm' samples are obtained 
#' ## from the same provider (joinby clause).
#' 
#' initGMQL("gtf")
#' test_path <- system.file("example","DATA_SET_VAR_GTF",package = "RGMQL")
#' test_path2 <- system.file("example","DATA_SET_VAR_GDM",package = "RGMQL")
#' TSS = readDataset(test_path)
#' HM = readDataset(test_path2)
#' join_data = join(TSS,HM,genometric_predicate=list(list(MD(1),DGE(120000))),c("provider"),region_output="RIGHT")
#'
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
#' to be used in GMQL functions that use genometric predicate parameter
#' requiring distal condition on value
#' In this case UP: denotes the upstream direction of the genome.
#' They are interpreted as predicates that must hold on the regions of the experiment; 
#' UP is true when region of experiment is in the upstream genome of the anchor region.
#' When this clause is not present, distal conditions apply to both the directions of the genome.
#' 
#'
#' @return None
#' 
#' @seealso \code{\link{DLE}} \code{\link{DGE}} \code{\link{DL}}\code{\link{DOWN}} \code{\link{MD}} \code{\link{DG}}
#' 
#' @examples
#' 
#'
#' ### Given a dataset 'hm' and one called 'tss' with a sample including Transcription Start Site annotations,
#' ## it searches for those regions of hm that are at a minimal distance from a transcription start site (TSS) 
#' ## and takes the first/closest one for each TSS, 
#' ## provided that such distance is greater than 120K bases and joined 'tss' and 'hm' samples are obtained 
#' ## from the same provider (joinby clause).
#' 
#' initGMQL("gtf")
#' test_path <- system.file("example","DATA_SET_VAR_GTF",package = "RGMQL")
#' test_path2 <- system.file("example","DATA_SET_VAR_GDM",package = "RGMQL")
#' TSS = readDataset(test_path)
#' HM = readDataset(test_path2)
#' join_data = join(TSS,HM,genometric_predicate=list(list(MD(1),DGE(120000),UP())),c("provider"),region_output="RIGHT")
#' 
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
#' @seealso \code{\link{DLE}} \code{\link{DGE}} \code{\link{DL}} \code{\link{UP}} \code{\link{MD}} \code{\link{DG}}
#' 
#' @examples
#' 
#' 
#' ### Given a dataset 'hm' and one called 'tss' with a sample including Transcription Start Site annotations,
#' ## it searches for those regions of hm that are at a minimal distance from a transcription start site (TSS) 
#' ## and takes the first/closest one for each TSS, 
#' ## provided that such distance is greater than 12K bases and joined 'tss' and 'hm' samples are obtained 
#' ## from the same provider (joinby clause).
#' 
#' initGMQL("gtf")
#' test_path <- system.file("example","DATA_SET_VAR_GTF",package = "RGMQL")
#' test_path2 <- system.file("example","DATA_SET_VAR_GDM",package = "RGMQL")
#' TSS = readDataset(test_path)
#' HM = readDataset(test_path2)
#' join_data = join(TSS,HM,genometric_predicate=list(list(MD(1),DGE(12000),DOWN())),c("provider"),region_output="RIGHT")
#' 
#' 
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

