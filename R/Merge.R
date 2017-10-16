#' GMQL Operation: MERGE
#'
#' It builds a dataset consisting of a single sample having as many regions
#' as the numebr of regions of the input data and as many metadata as the union of
#' the 'attribute-value' tuples of the input samples.
#' A groupby clause can be specified on metadata: the samples are then partitioned in groups,
#' each with a distinct value of the grouping metadata attributes.
#' The operation is separately applied to each group, yielding one sample in the result for each group.
#' Samples whose names are not present in the grouping metadata parameter are disregarded.
#'
#' @importFrom rJava J
#' @importFrom rJava .jnull
#' @importFrom rJava .jarray
#'  
#' @param input_data returned object from any GMQL function
#' @param groupBy list of CONDITION objects, or simple string concatenation 
#' (i.e c("cell_type","attribute_tag","size")).
#' Every object contains the name of metadata to be used in \emph{groupBy}.
#' For details of CONDITION objects see:
#' \code{\link{DEF}}, \code{\link{FULL}}, \code{\link{EXACT}}
#' 
#' Every condition accepts only one string value (e.g. DEF("cell_type") )
#' In case of single concatenation with no CONDITION, all metadata are considering as DEF
#' 
#' @return DAGgraph class object. It contains the value associated to the graph used 
#' as input for the subsequent GMQL function
#'
#' @references \url{http://www.bioinformatics.deib.polimi.it/genomic_computing/GMQL/doc/GMQLUserTutorial.pdf}
#'
#' @examples
#' 
#' ## it creates a dataset called merged which contains one sample for each antibody_target value 
#' ## found within the metadata of the exp dataset sample; 
#' ## each created sample contains all regions from all 'exp' samples with a specific value for their 
#' ## antibody_target metadata attribute.
#' 
#' initGMQL("gtf")
#' test_path <- system.file("example","DATA_SET_VAR_GTF",package = "RGMQL")
#' exp = readDataset(test_path)
#' merged = merge(input_data = exp, groupBy = c("antibody_target"))
#' 
#' @export
#'
merge <- function(input_data, groupBy = NULL)
{
  if(!is.null(groupBy))
    join_condition_matrix <- .jarray(.join_condition(groupBy),dispatch = TRUE)
  else
    join_condition_matrix <- .jnull("java/lang/String")
  
  WrappeR <- J("it/polimi/genomics/r/Wrapper")
  response <- WrappeR$merge(join_condition_matrix,input_data$value)
  error <- strtoi(response[1])
  data <- response[2]
  if(error!=0)
    stop(data)
  else
    DAGgraph(data)
}

