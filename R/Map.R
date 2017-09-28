#' GMQL Operation: MAP
#'
#' It computes, for each sample in the right dataset, aggregates over the values of the right regions
#' that intersect with a region in a left sample, for each region of each sample in the left dataset;
#' The number of generated output samples is the Cartesian product of the samples in the two input datasets;
#' each output sample has the same regions as the related input left sample, with their attributes and values,
#' plus the attributes computed as aggregates over right region values.
#' Output sample metadata are the union of the related input sample metadata,
#' whose attribute names are prefixed with "left" or "right" respectively.
#'
#' When the joinby clause is present, only pairs of samples of left_input_data and of right_input_data with
#' metadata M1 and M2 respectively that satisfy the joinby condition are considered.
#'
#' The clause consists of a list of metadata attribute names that must be present with equal values
#' in both M1 and  M2
#'
#'
#' @param left_input_data returned object from any GMQL function
#' @param right_input_data returned object from any GMQL function
#' @param aggregates list of element in the form \emph{key} = \emph{function_aggregate}.
#' The \emph{function_aggregate} is an object of class OPERATOR
#' The aggregate functions available are: \code{\link{MIN}}, \code{\link{MAX}},
#' \code{\link{SUM}}, \code{\link{BAG}}, \code{\link{AVG}}, \code{\link{COUNT}},
#' \code{\link{STD}}, \code{\link{MEDIAN}}, \code{\link{Q1}}, \code{\link{Q2}}, \code{\link{Q3}}.
#' Every operator accepts a string value, execet for COUNT that cannot have a value.
#' Argument of 'function_aggregate' must exist in schema
#' Two style are allowed:
#' \itemize{
#' \item list of key-value pairs: e.g. sum = SUM("pvalue")
#' \item list of values: e.g. SUM("pvalue")
#' }
#' "mixed style" is not allowed
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
#' @return DAGgraph class object. It contains the value associated to the graph used 
#' as input for the subsequent GMQL function
#'
#' @references \url{http://www.bioinformatics.deib.polimi.it/genomic_computing/GMQL/doc/GMQLUserTutorial.pdf}
#'
#' @examples
#'
#' ### it counts the number of regions in each sample from exp that overlap with a ref region, 
#' and for each ref region it computes the minimum score of all the regions in each exp sample 
#' that overlap with it. 
#' The MAP joinby option ensures that only the exp samples referring to the same 'cell_tissue' 
#' of a ref sample are mapped on such ref sample; 
#' exp samples with no cell_tissue metadata attribute, or with such metadata 
#' but with a different value from the one(s) of ref sample(s), are disregarded.
#' 
#' initGMQL("gtf")
#' test_path <- system.file("example","DATA_SET_VAR_GTF",package = "GMQL")
#' test_path2 <- system.file("example","DATA_SET_VAR_GDM",package = "GMQL")
#' exp = read(test_path)
#' ref = read(test_path2)
#' out = map(ref,exp, list(minScore = MIN("score")), joinBy = c("cell_tissue") )
#' 
#' 
#' @export
#'
map <- function(left_input_data, right_input_data, aggregates = NULL, joinBy = NULL)
{
  if(!is.null(aggregates))
    metadata_matrix <- .aggregates(metadata,"OPERATOR")
  else
    metadata_matrix = scalaNull("Array[Array[String]]")

  if(!is.null(joinBy))
    join_condition_matrix <- .join_condition(joinBy)
  else
    join_condition_matrix <- scalaNull("Array[Array[String]]")

  out<-WrappeR$map(join_condition_matrix,aggregates,left_input_data$value,right_input_data$value)

  if(grepl("No",out,ignore.case = TRUE))
    stop(out)
  else
    DAGgraph(out)
}
