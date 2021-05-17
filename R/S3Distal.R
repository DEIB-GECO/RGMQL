##########################
#       DISTAL          #
#########################

DISTAL <- function(value) {
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

check.DISTAL <- function(value) {
    if(!is.numeric(value))
        stop("value: is not a numeric")
    
    if(is.numeric(value) && length(value)>1)
        stop("value: no multiple string")
}
#' DISTAL object class constructor
#'
#' This class constructor is used to create instances of DISTAL object
#' to be used in GMQL JOIN operations (RGMQL merge functions) that use 
#' genometric predicate parameter requiring distal condition on value
#' 
#' \itemize{
#' \item{DL: It denotes the less distance clause, 
#' which selects all the regions of a joined experiment dataset sample such 
#' that their distance from the anchor region of the joined reference dataset 
#' sample is less than 'value' bases.}
#' \item{DLE: It denotes the less equal distance clause, 
#' which selects all the regions of a joined experiment dataset sample such 
#' that their distance from the anchor region of the joined reference dataset 
#' sample is less than, or equal to, 'value' bases.}
#' \item{DG: It denotes the great distance clause, 
#' which selects all the regions of a joined experiment dataset sample such 
#' that their distance from the anchor region of the joined reference dataset 
#' sample is greater than 'value' bases. }
#' \item{DGE: It denotes the great equal distance clause, 
#' which selects all the regions of a joined experiment dataset sample such 
#' that their distance from the anchor region of the joined reference dataset 
#' sample is greater than, or equal to, 'value' bases.}
#' \item{MD: It denotes the minimum distance clause, which selects 
#' the first 'value' regions of the joined experiment at minimial distance 
#' from the anchor region of the joined reference dataset sample.}
#' \item{UP: It denotes the upstream direction of the genome.
#' It makes predicates to be hold on the upstream of the regions of the joined 
#' reference dataset sample.
#' UP is true when region of the joined experiment dataset sample is in the
#' upstream genome of the anchor region of the joined reference dataset sample.
#' When this clause is not present, distal conditions apply to both 
#' directions of the genome.}
#' \item{DOWN:  It denotes the downstream direction of the genome.
#' It makes predicates to be hold on the downstream of the regions of the 
#' joined reference dataset sample.
#' DOWN is true when region of the joined experiment dataset sample is in the
#' downstream genome of the anchor region of the joined reference dataset 
#' sample. When this clause is not present, distal conditions apply to both 
#' directions of the genome.}
#' }
#' 
#' @param value string identifying distance between genomic regions 
#' in base pair
#'
#' @return Distal object
#' 
#' @examples
#' ## This statement initializes and runs the GMQL server for local execution 
#' ## and creation of results on disk. Then, with system.file() it defines 
#' ## the path to the folders "DATASET" and "DATASET_GDM" in the subdirectory 
#' ## "example" of the package "RGMQL", and opens such folders as a GMQL 
#' ## datasets named "TSS" and "HM", respectively, using CustomParser
#' 
#' init_gmql()
#' test_path <- system.file("example", "DATASET", package = "RGMQL")
#' test_path2 <- system.file("example", "DATASET_GDM", package = "RGMQL")
#' TSS = read_gmql(test_path)
#' HM = read_gmql(test_path2)
#' 
#' ## Given a dataset HM and one called TSS with a sample including 
#' ## Transcription Start Site annotations, this statement  searches for those 
#' ## regions of HM that are at a minimal distance from a transcription 
#' ## start site (TSS) and takes the first/closest one for each TSS, provided 
#' ## that such distance is lesser than 1200 bases and joined TSS and HM 
#' ## samples are obtained from the same provider (joinby clause).
#' 
#' join_data = merge(TSS, HM, 
#'     genometric_predicate = list(MD(1), DL(1200)), conds("provider"), 
#'     region_output = "RIGHT")
#'
#' ## Given a dataset HM and one called TSS with a sample including 
#' ## Transcription Start Site annotations, this statement searches for those 
#' ## regions of HM that are downstream and at a minimal distance from a 
#' ## transcription start site (TSS) and takes the first/closest one for each 
#' ## TSS, provided that such distance is greater than 12K bases and joined 
#' ## TSS and HM samples are obtained from the same provider (joinby clause).
#' 
#' join_data = merge(TSS, HM, 
#'     genometric_predicate = list(MD(1), DGE(12000), DOWN()), 
#'     conds("provider"), region_output = "RIGHT")
#'
#' @name DISTAL-Object
#' @aliases DL
#' @rdname distal-class
#' @export
#' 
DL <- function(value) {
    check.DISTAL(value)
    list <- list(value = as.integer(value))
    ## Set the name for the class
    class(list) <- c("DL","DISTAL")
    return(list)
}

#' @name DG
#' @aliases DG
#' @rdname distal-class
#' @export
#' 
DG <- function(value) {
    check.DISTAL(value)
    list <- list(value = as.integer(value))
    ## Set the name for the class
    class(list) <- c("DG","DISTAL")
    return(list)
}

#' @name DISTAL-Object
#' @aliases DLE
#' @rdname distal-class
#' @export
#' 
DLE <- function(value) {
    check.DISTAL(value)
    list <- list(value = as.integer(value))
    ## Set the name for the class
    class(list) <- c("DLE","DISTAL")
    return(list)
}

#' @name DISTAL-Object
#' @aliases DGE
#' @rdname distal-class
#' @export
#' 
DGE <- function(value) {
    check.DISTAL(value)
    list <- list(value = as.integer(value))
    ## Set the name for the class
    class(list) <- c("DGE","DISTAL")
    return(list)
}

#' @name DISTAL-Object
#' @aliases MD
#' @rdname distal-class
#' @export
#' 
MD <- function(value) {
    check.DISTAL(value)
    list <- list(value = as.integer(value))
    ## Set the name for the class
    class(list) <- c("MD","DISTAL")
    return(list)
}


#' @name DISTAL-Object
#' @aliases UP
#' @rdname distal-class
#' @export
#' 
UP <- function() {
    list <- list()
    ## Set the name for the class
    class(list) <- c("UP","DISTAL")
    return(list)
}
as.character.UP <- function(obj) {
    class <- class(obj)[1]
    c(class,"")
}


#' @name DISTAL-Object
#' @aliases DOWN
#' @rdname distal-class
#' @export
#' 
DOWN <- function() {
    list <- list()
    ## Set the name for the class
    class(list) <- c("DOWN","DISTAL")
    return(list)
}
as.character.DOWN <- function(obj) {
    class <- class(obj)[1]
    c(class,"")
}

