#' GMQL Operation: DIFFERENCE
#'
#' It produces one sample in the result for each sample of the left operand,
#' by keeping the same metadata of the left input sample and only those regions
#' (with their schema and values) of the left input sample which do not intersect with any region
#' in the right operand sample.
#' The optional \emph{joinby} clause is used to extract a subset of couples
#' from the cartesian product of two dataset \emph{left_input_data} x \emph{right_input_data}
#' on which to apply the DIFFERENCE operator:
#' only those samples that have the same value for each attribute
#' are considered when performing the difference.
#'
#'
#' @param right_input_data returned object from any GMQL function
#' @param left_input_data returned object from any GMQL function
#' @param joinBy list of CONDITION objects, or simple string concatenation 
#' (i.e c("cell_type","attribute_tag","size")).
#' Every object contains the name of metadata to be used in \emph{groupby}.
#' For details of CONDITION objects see:
#' \code{\link{DEF}}, \code{\link{FULL}}, \code{\link{EXACT}}
#' 
#' Every condition accepts only one string value (e.g. DEF("cell_type") )
#' In case of single concatenation with no CONDITION, all metadata are considering as DEF
#' 
#' @param is_exact single logical value: TRUE means that the region difference is executed only 
#' on regions in left_input_data with exactly the same coordinates of at least one region present 
#' in right_input_data; if is_exact = FALSE, the difference is executed on all regions in 
#' left_input_data that overlap with at least one region in right_input_data (even just one base).
#'
#' @return DAGgraph class object. It contains the value associated to the graph used 
#' as input for the subsequent GMQL function
#'
#' @references \url{http://www.bioinformatics.deib.polimi.it/genomic_computing/GMQL/doc/GMQLUserTutorial.pdf}
#'
#' @examples
#'
#' ## This GMQL statement returns all the regions in the first dataset that do not 
#' ## overlap any region in the second dataset.
#' 
#' initGMQL("gtf")
#' test_path <- system.file("example","DATA_SET_VAR_GTF",package = "GMQL")
#' test_path2 <- system.file("example","DATA_SET_VAR_GDM",package = "GMQL")
#' r_left = readDataset(test_path)
#' r_right = readDataset(test_path2)
#' out = difference(r_left,r_right)
#' 
#' \dontrun{
#' ## This GMQL statement extracts for every pair of samples s1 in EXP1 and s2 in EXP2
#' ## having the same value of the metadata attribute 'antibody_target'
#' ## the regions that appear in s1 but do not overlap any region in s2; 
#' ## metadata of the result are the same as the metadata of s1.
#' 
#' initGMQL("gtf")
#' test_path <- system.file("example","DATA_SET_VAR_GTF",package = "GMQL")
#' test_path2 <- system.file("example","DATA_SET_VAR_GDM",package = "GMQL")
#' exp1 = readDataset(test_path)
#' exp2 = readDataset(test_path2)
#' out = difference(exp1,exp2, c("antibody_target"))
#'
#' }
#'
#' @export
#'
difference <- function(left_input_data, right_input_data, joinBy = NULL,is_exact = FALSE)
{
  if(!is.null(joinBy))
    join_condition_matrix <- .join_condition(joinBy)
  else
    join_condition_matrix <- scalaNull("Array[Array[String]]")
  
  out <- WrappeR$difference(join_condition_matrix,right_input_data$value,left_input_data$value,is_exact)
  
  if(grepl("No",out,ignore.case = TRUE))
    stop(out)
  else
    DAGgraph(out)
}

