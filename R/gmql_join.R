#' Method merge
#'
#' @description Wrapper to GMQL JOIN operator
#' 
#' @description It takes in input two datasets, respectively known as anchor 
#' (left) and experiment (right) and returns a dataset of samples consisting 
#' of regions extracted from the operands according to the specified condition
#' (a.k.a \emph{genometric_predicate}).
#' The number of generated output samples is the Cartesian product 
#' of the number of samples in the anchor and in the experiment dataset 
#' (if \emph{joinBy} is not specified).
#' The output metadata are the union of the input metadata, 
#' with their attribute names prefixed with left or right dataset name, 
#' respectively.
#'
#' @importFrom rJava J .jnull .jarray
#' @importFrom S4Vectors merge
#' 
#' @param x GMQLDataset class object
#' @param y GMQLDataset class object
#' 
#' @param genometric_predicate it is a list of DISTAL objects
#' For details of DISTAL objects see:
#' \code{\link{DLE}}, \code{\link{DGE}}, \code{\link{DL}}, \code{\link{DG}},
#' \code{\link{MD}}, \code{\link{UP}}, \code{\link{DOWN}}
#' 
#' @param joinBy \code{\link{condition_evaluation}} function to support 
#' methods with groupBy or JoinBy input paramter
#' @param reg_attr vector of string made up by schema field attribute
#' @param region_output single string that declares which region is given in 
#' output for each input pair of left dataset and right dataset regions 
#' satisfying the genometric predicate:
#' \itemize{
#' \item{LEFT: It outputs the anchor regions from 'x' that satisfy the 
#' genometric predicate}
#' \item{RIGHT: It outputs the experiment regions from 'y' that satisfy the 
#' genometric predicate}
#' \item{INT (intersection): It outputs the overlapping part (intersection) 
#' of the 'x' and 'y' regions that satisfy the genometric predicate; if the 
#' intersection is empty, no output is produced}
#' \item{CAT: It outputs the concatenation between the 'x' and 'y' regions 
#' that satisfy the genometric predicate, (i.e. the output regionis defined as 
#' having left (right) coordinates equal to the minimum (maximum) of the 
#' corresponding coordinate values in the 'x' and 'y' regions satisfying 
#' the genometric predicate)}
#' \item{LEFT_DIST: It outputs the duplicate elimination of "x" output 
#' regions with the same values, regardless the "y" paired region and its 
#' values. In this case, the output regions attributes and their values are 
#' all those of "x", and the output metadata are equal to the "x" metadata, 
#' without additional prefixes}
#' \item{RIGHT_DIST: It outputs the duplicate elimination of "y" output 
#' regions with the same values, regardless the "x" paired region and its 
#' values. In this case, the output regions attributes and their values are 
#' all those of "y", and the output metadata are equal to the "y" metadata, 
#' without additional prefixes}
#' \item{BOTH: It outputs the same regions as LEFT, but it adds in the output 
#' region attributes the coordinates of the "y" dataset region that, 
#' together with the output "x" dataset region, satisfies the equi predicate 
#' and the genometric predicate}
#' }
#'
#' @return GMQLDataset object. It contains the value to use as input 
#' for the subsequent GMQLDataset method
#' 
#' @examples
#' 
#' ## Thi statement initializes and runs the GMQL server for local execution 
#' ## and creation of results on disk. Then, with system.file() it defines 
#' ## the path to the folders "DATASET" and "DATASET_GDM" in the subdirectory 
#' ## "example" of the package "RGMQL" and opens such folder as a GMQL 
#' ## dataset named "exp" and "ref" respectively using customParser
#' 
#' init_gmql()
#' test_path <- system.file("example", "DATASET", package = "RGMQL")
#' test_path2 <- system.file("example", "DATASET_GDM", package = "RGMQL")
#' TSS = read_GMQL(test_path)
#' HM = read_GMQL(test_path2)
#' 
#' ## Given a dataset 'HM' and one called 'TSS' with a sample including 
#' ## Transcription Start Site annotations, it searches for those regions of HM 
#' ## that are at a minimal distance from a transcription start site (TSS) 
#' ## and takes the first/closest one for each TSS, provided that such distance 
#' ## is lesser than 120K bases and joined 'tss' and 'hm' samples are obtained 
#' ## from the same provider (joinby clause).
#' 
#' join_data = merge(TSS, HM, genometric_predicate = list(MD(1), DLE(120000)), 
#' conds("provider"), region_output = "RIGHT")
#' 
#' 
#' @name merge
#' @aliases merge,GMQLDataset,GMQLDataset-method
#' @aliases merge-method
#' @export
setMethod("merge", c("GMQLDataset","GMQLDataset"),
                function(x, y, genometric_predicate = NULL, 
                    region_output = "CAT", joinBy = conds(), reg_attr = c(""))
                {
                    ptr_data_x <- value(x)
                    ptr_data_y <- value(y)
                    gmql_join(ptr_data_x, ptr_data_y, 
                        genometric_predicate, joinBy, region_output, reg_attr)
                })


gmql_join <- function(left_data, right_data, genometric_predicate, joinBy, 
                        region_output, reg_attributes)
{
    if(!is.null(genometric_predicate))
    {
        if(length(genometric_predicate) > 4)
            stop("genometric_predicate: only 4 DISTAL condition")
        
        if(!is.list(genometric_predicate))
            stop("genometric_predicate must be a list")
    
        if(!all(vapply(genometric_predicate, function(x) {is(x,"DISTAL")},
                        logical(1))))
            stop("All elements should be DISTAL object")
        
        genomatrix <- t(vapply(genometric_predicate, function(x) {
                new_value = as.character(x)
                array <- c(new_value)
        },character(2)))
        
        genomatrix <- .jarray(genomatrix, dispatch = TRUE)
    }
    else
        genomatrix <- .jnull("java/lang/String")
    
    if(!is.null(joinBy))
    {
        cond <- .join_condition(joinBy)
        if(is.null(cond))
            join_matrix <- .jnull("java/lang/String")
        else
            join_matrix <- .jarray(cond, dispatch = TRUE)
    }
    else
        join_matrix <- .jnull("java/lang/String")
    
    if(!is.null(reg_attributes))
    {
        if(!is.character(reg_attributes))
            stop("metadata: no valid input")
        
        reg_attributes <- reg_attributes[!reg_attributes %in% ""]
        reg_attributes <- reg_attributes[!duplicated(reg_attributes)]
        
        if(!length(reg_attributes))
            reg_attributes <- .jnull("java/lang/String")
        else
            reg_attributes <- .jarray(reg_attributes, dispatch = TRUE)
    }
    else
        reg_attributes <- .jnull("java/lang/String")
    
    ouput <- toupper(region_output)
    if(!ouput %in% c("CAT", "LEFT", "RIGHT", "INT", "BOTH", "RIGHT_DIST", 
                        "LEFT_DIST"))
        stop("region_output must be cat, left, right, right_dist, left_dist 
                or int (intersection)")
    
    WrappeR <- J("it/polimi/genomics/r/Wrapper")
    response <- WrappeR$join(genomatrix, join_matrix, ouput,reg_attributes,
                                left_data, right_data)
    error <- strtoi(response[1])
    val <- response[2]
    if(error)
        stop(val)
    else
        GMQLDataset(val)
}
