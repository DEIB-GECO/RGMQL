##########################
#       DISTAL          #
#########################

DISTAL <- function(value)
{
    op_list <- list(value = value)
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
#' 
#' \itemize{
#' \item{DL: It denotes the less distance clause, 
#' which selects all the regions of the experiment such that their distance 
#' from the anchor region is less than 'value' bases.}
#' \item{DLE: It denotes the less distance clause, 
#' which selects all the regions of the experiment such that their distance 
#' from the anchor region is less than, or equal to, 'value' bases.}
#' \item{DG: it denotes the less distance clause, 
#' which selects all the regions of the experiment such that their distance 
#' from the anchor region is greater than 'value' bases. }
#' \item{DGE: It denotes the less distance clause, which selects all the 
#' regions of the experiment such that their distance from the anchor region 
#' is greater than, or equal to, 'value' bases.}
#' \item{MD: It denotes the minimum distance clause, which selects 
#' the 'value' regions of the experiment at minimial distance from the 
#' anchor region.}
#' \item{UP: It denotes the upstream direction of the genome.
#' They are interpreted as predicates that must hold on the regions 
#' of the experiment.
#' UP is true when region of experiment is in the upstream genome 
#' of the anchor region.
#' When this clause is not present, distal conditions apply to both 
#' the directions of the genome.}
#' \item{DOWN: It denotes the downstream direction of the genome.
#' They are interpreted as predicates that must hold on the regions of 
#' the experiment.
#' DOWN is true when region of experiment is in the downstream genome of 
#' the anchor region.
#' When this clause is not present, distal conditions apply to both the 
#' directions of the genome. }
#' }
#' 
#' @param value string identifying distance between genomic regions 
#' in base pairs, 
#'
#' @return distal object
#' 
#' @examples
#' 
#' init_gmql()
#' test_path <- system.file("example","DATASET",package = "RGMQL")
#' test_path2 <- system.file("example","DATASET_GDM",package = "RGMQL")
#' TSS = read_dataset(test_path)
#' HM = read_dataset(test_path2)
#' 
#' ## Given a dataset HM and one called TSS with a sample including 
#' # Transcription Start Site annotations, it searches for those regions of hm 
#' # that are at a minimal distance from a transcription start site (TSS) 
#' # and takes the first/closest one for each TSS, 
#' # provided that such distance is lesser than 1200 bases and joined TSS 
#' # and HM samples are obtained from the same provider (joinby clause).
#' 
#' join_data = join(TSS, HM, 
#' genometric_predicate = list(list(MD(1), DL(1200))), DF("provider"), 
#' region_output = "RIGHT")
#'
#' #' # Given a dataset 'hm' and one called 'tss' with a sample including 
#' # Transcription Start Site annotations, it searches for those regions of hm 
#' # that are at a minimal distance from a transcription start site (TSS) 
#' # and takes the first/closest one for each TSS, provided that such distance 
#' # is greater than 12K bases and joined 'tss' and 'hm' samples are obtained 
#' # from the same provider (joinby clause).
#' 
#' join_data = join(TSS, HM, 
#' genometric_predicate = list(list(MD(1), DGE(12000), DOWN())), 
#' DF("provider"), region_output = "RIGHT")
#'
#' @name DISTAL
#' @rdname distal-class
#' @export
#' 
DL <- function(value)
{
    check.DISTAL(value)
    list <- list(value = as.integer(value))
    ## Set the name for the class
    class(list) <- c("DL","DISTAL")
    return(list)
}

#' @name DISTAL
#' @rdname distal-class
#' @export
#' 
DG <- function(value)
{
    check.DISTAL(value)
    list <- list(value = as.integer(value))
    ## Set the name for the class
    class(list) <- c("DG","DISTAL")
    return(list)
}

#' @name DISTAL
#' @rdname distal-class
#' @export
#' 
DLE <- function(value)
{
    check.DISTAL(value)
    list <- list(value = as.integer(value))
    ## Set the name for the class
    class(list) <- c("DLE","DISTAL")
    return(list)
}

#' @name DISTAL
#' @rdname distal-class
#' @export
#' 
DGE <- function(value)
{
    check.DISTAL(value)
    list <- list(value = as.integer(value))
    ## Set the name for the class
    class(list) <- c("DGE","DISTAL")
    return(list)
}

#' @name DISTAL
#' @rdname distal-class
#' @export
#' 
MD <- function(value)
{
    check.DISTAL(value)
    list <- list(value = as.integer(value))
    ## Set the name for the class
    class(list) <- c("MD","DISTAL")
    return(list)
}


#' @name DISTAL
#' @rdname distal-class
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


#' @name DISTAL
#' @rdname distal-class
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

