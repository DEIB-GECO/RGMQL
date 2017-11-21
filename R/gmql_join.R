#' Method merge
#' 
#' Wrapper to GMQL join function
#'
#' It takes in input two datasets, respectively known as nchor (left) 
#' and experiment (right) and returns a dataset of samples consisting of 
#' regions extracted from the operands according to the specified condition
#' (a.k.a \emph{genometric_predicate}).
#' The number of generated output samples is the Cartesian product 
#' of the number of samples in the anchor and in the experiment dataset 
#' (if \emph{by} is not specified).
#' The output metadata are the union of the input metadata, 
#' with their attribute names prefixed with left or right respectively.
#'
#' @importFrom rJava .jnull
#' @importFrom rJava J
#' @importFrom rJava .jarray
#' 
#' @param x GMQLDataset class object
#' @param y GMQLDataset class object
#' 
#' @param genometric_predicate is a list of lists of DISTAL object
#' For details of DISTAL objects see:
#' \code{\link{DLE}}, \code{\link{DGE}}, \code{\link{DL}}, \code{\link{DG}},
#' \code{\link{MD}}, \code{\link{UP}}, \code{\link{DOWN}}
#' 
#' @param ... Additional arguments for use in specific methods.
#' 
#' This method accept a function to define condition evaluation on metadata.
#' \itemize{
#' \item{\code{\link{FN}}: Fullname evaluation, two attributes match 
#' if they both end with value and, if they have a further prefixes,
#' the two prefix sequence are identical}
#' \item{\code{\link{EX}}: Exact evaluation, only attributes exactly 
#' as value will match; no further prefixes are allowed. }
#' \item{\code{\link{DF}}: Default evaluation, the two attributes match 
#' if both end with value.}
#' }
#' 
#' @param region_output single string that declare which region is given in 
#' output for each input pair of left dataset right dataset regions 
#' satisfying the genometric predicate:
#' \itemize{
#' \item{left: outputs the anchor regions from left_input_data that satisfy 
#' the genometric predicate}
#' \item{right: outputs the experiment regions from right_input_data that 
#' satisfy the genometric predicate}
#' \item{int (intersection): outputs the overlapping part (intersection) 
#' of the left_input_data and right_input_data regions that satisfy 
#' the genometric predicate; if the intersection is empty, 
#' no output is produced}
#' \item{contig: outputs the concatenation between the left_input_data and 
#' right_input_data regions that satisfy the genometric predicate, 
#' (i.e. the output regionis defined as having left (right) coordinates
#' equal to the minimum (maximum) of the corresponding coordinate values 
#' in the left_input_data and right_input_data regions satisfying 
#' the genometric predicate)}
#' }
#'
#' @return GMQLDataset class object. It contains the value to use as input 
#' for the subsequent GMQL function
#' 
#' @examples
#' 
#' # Given a dataset 'hm' and one called 'tss' with a sample including 
#' # Transcription Start Site annotations, it searches for those regions of hm 
#' # that are at a minimal distance from a transcription start site (TSS) 
#' # and takes the first/closest one for each TSS, provided that such distance 
#' # is lesser than 120K bases and joined 'tss' and 'hm' samples are obtained 
#' # from the same provider (joinby clause).
#' 
#' init_gmql()
#' test_path <- system.file("example", "DATASET", package = "RGMQL")
#' test_path2 <- system.file("example", "DATASET_GDM", package = "RGMQL")
#' TSS = read_dataset(test_path)
#' HM = read_dataset(test_path2)
#' join_data = merge(TSS, HM, 
#' genometric_predicate = list(list(MD(1), DLE(120000))), DF("provider"), 
#' region_output = "RIGHT")
#' 
#' 
#' @name merge
#' @aliases merge,GMQLDataset,GMQLDataset-method
#' @aliases merge-method
#' @export
setMethod("merge", c("GMQLDataset","GMQLDataset"),
                function(x, y, genometric_predicate = NULL, 
                    region_output = "contig", ...)
                {
                    ptr_data_x <- x@value
                    ptr_data_y <- y@value
                    joinBy = list(...)
                    gmql_join(ptr_data_x, ptr_data_y, genometric_predicate, 
                                joinBy, region_output="contig")
                })


gmql_join <- function(right_data, left_data, genometric_predicate, joinBy, 
                            region_output="contig")
{
    if(!is.null(genometric_predicate))
    {
        if(!is.list(genometric_predicate))
            stop("genometric_predicate must be list of lists")
    
        if(!all(sapply(genometric_predicate, function(x) is.list(x) )))
            stop("genometric_predicate must be list of lists")
    
        lapply(genometric_predicate, function(list_pred) {
            if(length(list_pred)>4)
            {
                warning("only 4 element per list, we cut the rest")
                length(list_pred)=4
            }
            
            if(!all(sapply(list_pred, function(x) {is(x,"DISTAL")} )))
                stop("All elements should be DISTAL object")
            })
    
        genomatrix <- t(sapply(genometric_predicate, function(list_pred) {
            dist_array <- sapply(list_pred, function(x) {
                new_value = as.character(x)
                array <- c(new_value)
            })
            dist_array = c(dist_array,c("NA","NA"),c("NA","NA"),c("NA","NA"))
            length(dist_array) = 8
            dist_array
        }))
    
        genomatrix <- .jarray(genomatrix, dispatch = TRUE)
    }
    else
        genomatrix <- .jnull("java/lang/String")
    
    if(!is.null(joinBy))
        join_condition_matrix <- .jarray(.join_condition(joinBy), 
                                            dispatch = TRUE)
    else
        join_condition_matrix <- .jnull("java/lang/String")
    
    ouput <- toupper(region_output)
    if(!identical(ouput,"CONTIG") && !identical(ouput,"LEFT") && 
        !identical(ouput,"RIGHT") && !identical(ouput,"INT"))
        stop("region_output must be contig,left,right or int (intersection)")
    
    WrappeR <- J("it/polimi/genomics/r/Wrapper")
    response <- WrappeR$join(genomatrix,join_condition_matrix, 
                                ouput,right_data, left_data)
    error <- strtoi(response[1])
    data <- response[2]
    if(error!=0)
        stop(data)
    else
        GMQLDataset(data)
}
