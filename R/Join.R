#' GMQL Operation: JOIN
#'
#' It takes in input two datasets, respectively known as nchor (left) and experiment (right) and returns
#' a dataset of samples consisting of regions extracted from the operands according to the specified condition
#' (a.k.a genometric_predicate).
#' The number of generated output samples is the Cartesian product of the number of samples
#' in the anchor and in the experiment dataset (if joinBy is not specified).
#' The output metadata are the union of the input metadata, with their attribute names prefixed with
#' left or right respectively.
#'
#'
#' @param left_input_data returned object from any GMQL function
#' @param right_input_data returned object from any GMQL function
#' @param genometric_predicate is a list of lists of DISTAL object by means of logical ANDs
#' For details of DISTAL objects see:
#' \code{\link{DLE}}, \code{\link{DGE}}, \code{\link{MD}}, \code{\link{UP}}, \code{\link{DOWN}}
#' 
#' @param joinBy list of CONDITION objects, or simple string concatenation 
#' (i.e c("cell_type","attribute_tag","size")).
#' Every object contains the name of metadata to be used in \emph{groupby}.
#' For details of CONDITION objects see:
#' \code{\link{DEF}}, \code{\link{FULL}}, \code{\link{EXACT}}
#' 
#' Every condition accepts only one string value. (e.g. DEF("cell_type") )
#' In case of single concatenation with no CONDITION, all metadata are considering as DEF
#' 
#' @param region_output single string that declare which region is given in output for each input pair of left dataset
#' right dataset regions satisfying the genometric predicate:
#' \itemize{
#' \item{left: outputs the anchor regions from left_input_data that satisfy the genometric predicate}
#' \item{right: outputs the experiment regions from right_input_data that satisfy the genometric predicate}
#' \item{int (intersection): outputs the overlapping part (intersection) of the left_input_data and right_input_data
#' regions that satisfy the genometric predicate; if the intersection is empty, no output is produced}
#' \item{contig: outputs the concatenation between the left_input_data and right_input_data regions that satisfy
#' the genometric predicate, (i.e. the output regionis defined as having left (right) coordinates
#' equal to the minimum (maximum) of the corresponding coordinate values in the left_input_data and right_input_data
#' regions satisfying the genometric predicate)}
#' }
#'
#' @return DAGgraph class object. It contains the value associated to the graph used 
#' as input for the subsequent GMQL function
#'
#' @references \url{http://www.bioinformatics.deib.polimi.it/genomic_computing/GMQL/doc/GMQLUserTutorial.pdf}
#'
#'
#' @examples
#' 
#' ## Given a dataset 'hm' and one called 'tss' with a sample including Transcription Start Site annotations,
#' ## it searches for those regions of hm that are at a minimal distance from a transcription start site (TSS) 
#' ## and takes the first/closest one for each TSS, 
#' ## provided that such distance is lesser than 120K bases and joined 'tss' and 'hm' samples are obtained 
#' ## from the same provider (joinby clause).
#' 
#' initGMQL("gtf")
#' test_path <- system.file("example","DATA_SET_VAR_GTF",package = "GMQL")
#' test_path2 <- system.file("example","DATA_SET_VAR_GDM",package = "GMQL")
#' TSS = readDataset(test_path)
#' HM = readDataset(test_path2)
#' join_data = join(TSS,HM,genometric_predicate=list(list(MD(1),DLE(120000))),c("provider"),region_output="RIGHT")
#'
#' @export
#'
join <- function(right_input_data, left_input_data, genometric_predicate = NULL,
                 joinBy = NULL, region_output="contig")
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
  }
  else
    genomatrix <- scalaNull("Array[Array[String]]")
      
  if(!is.null(joinBy))
    join_condition_matrix <- .join_condition(joinBy)
  else
    join_condition_matrix <- scalaNull("Array[Array[String]]")
  
  ouput <- toupper(region_output)
  if(!identical(ouput,"CONTIG") && !identical(ouput,"LEFT") && !identical(ouput,"RIGHT")
     && !identical(ouput,"INT"))
    stop("region_output must be contig,left,right or int (intersection)")
  
  out <- WrappeR$join(genomatrix,join_condition_matrix, ouput,right_input_data$value, left_input_data$value)
  if(grepl("No",out,ignore.case = TRUE))
    stop(out)
  else
    DAGgraph(out)
}
