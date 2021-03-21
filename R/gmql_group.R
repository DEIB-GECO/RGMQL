group_by.GMQLDateset <- function(
  .data, 
  groupBy_meta = conds(), 
  groupBy_regions = c(""), 
  region_aggregates = NULL, 
  meta_aggregates = NULL
) {
  ptr_data = value(.data)
  gmql_group(
    ptr_data, 
    groupBy_meta, 
    groupBy_regions, 
    region_aggregates, 
    meta_aggregates
  )
}

#' Method group_by
#' 
#' @description Wrapper to GMQL GROUP operator
#' @description It performs the grouping of samples and/or sample regions of 
#' the input dataset based on one specified metadata and/or region attribute. 
#' If the metadata attribute is multi-value, i.e., it assumes multiple values 
#' for sample (e.g., both <disease, cancer> and <disease, diabetes>), 
#' the grouping identifies different groups of samples for each attribute value 
#' combination (e.g., group1 for samples that feature the combination 
#' <disease, cancer>, group2 for samples that feature the combination 
#' <disease, diabetes>, and group3 for samples that feature both combinations 
#' <disease, cancer> and <disease, diabetes>). For each obtained group, it is 
#' possible to request the evaluation of aggregate functions on metadata 
#' attributes; these functions consider the metadata contained in all samples 
#' of the group. The regions, their attributes and their values in output are 
#' the same as the ones in input for each sample, and the total number of 
#' samples does not change. All metadata in the input samples are conserved 
#' with their values in the output samples, with the addition of the "_group" 
#' attribute, whose value is the identifier of the group to which the specific 
#' sample is assigned; other metadata attributes can be added as aggregate 
#' functions computed on specified metadata. When used on region attributes, 
#' group_by can group regions of each sample individually, based on their 
#' coordinates (chr, start, stop, strand) and possibly also on other 
#' specified grouping region attributes (when these are present in the schema 
#' of the input dataset). In each sample, regions found in the same group 
#' (i.e., regions with same coordinates and grouping attribute values), 
#' are combined into a single region; this allows to merge regions that are 
#' duplicated inside the same sample (based on the values of their coordinates 
#' and of other possible specified region attributes). For each grouped 
#' region, it is possible to request the evaluation of aggregate functions 
#' on other region attributes (i.e., which are not coordinates, or grouping 
#' region attributes). This use is independent on the possible grouping 
#' realised based on metadata. The generated output schema only contains the 
#' original region attributes on which the grouping has been based, and 
#' additionally the attributes in case calculated as aggregated functions.
#' If the group_by is applied only on regions, the output metadata and their 
#' values are equal to the ones in input. Both when applied on metadata and 
#' on regions, the group_by operation returns a number of output samples 
#' equal to the number of input ones. Note that the two possible uses of 
#' group_by, on metadata and on regions, are perfectly orthogonal, 
#' therefore they can be used in combination or independently.
#' 
#' 
#' @importFrom rJava J .jarray .jnull
#' @importFrom dplyr group_by
#' 
#' @param .data GMQLDataset object
#' @param groupBy_meta \code{\link{conds}} function to support methods with 
#' groupBy or JoinBy input parameter
#' 
#' @param groupBy_regions vector of strings made up by region attribute names
#' @param meta_aggregates It accepts a list of aggregate functions on 
#' metadata attribute.
#' All the elements in the form \emph{key} = \emph{aggregate}.
#' The \emph{aggregate} is an object of class AGGREGATES.
#' The aggregate functions available are: \code{\link{SUM}}, 
#' \code{\link{COUNTSAMP}}, \code{\link{MIN}}, \code{\link{MAX}}, 
#' \code{\link{AVG}}, \code{\link{MEDIAN}}, \code{\link{STD}}, 
#' \code{\link{BAG}}, \code{\link{BAGD}}, \code{\link{Q1}}, 
#' \code{\link{Q2}}, \code{\link{Q3}}.
#' Every aggregate accepts a string value, except for COUNTSAMP, which does 
#' not have any value.
#' Argument of 'aggregate function' must exist in schema, i.e. among region 
#' attributes. Two styles are allowed:
#' \itemize{
#' \item list of key-value pairs: e.g. sum = SUM("cell")
#' \item list of values: e.g. SUM("cell")
#' }
#' "mixed style" is not allowed
#' 
#' @param region_aggregates It accepts a list of aggregate functions on 
#' region attribute. 
#' All the elements in the form \emph{key} = \emph{aggregate}.
#' The \emph{aggregate} is an object of class AGGREGATES.
#' The aggregate functions available are: \code{\link{SUM}}, 
#' \code{\link{COUNT}}, \code{\link{MIN}}, \code{\link{MAX}}, 
#' \code{\link{AVG}}, \code{\link{MEDIAN}}, \code{\link{STD}}, 
#' \code{\link{BAG}}, \code{\link{BAGD}}, \code{\link{Q1}}, 
#' \code{\link{Q2}}, \code{\link{Q3}}.
#' Every aggregate accepts a string value, except for COUNT, which does 
#' not have any value.
#' Argument of 'aggregate function' must exist in schema, i.e. among region 
#' attributes. Two styles are allowed:
#' \itemize{
#' \item list of key-value pairs: e.g. sum = SUM("pvalue")
#' \item list of values: e.g. SUM("pvalue")
#' }
#' "mixed style" is not allowed
#' 
#' @return GMQLDataset object. It contains the value to use as input 
#' for the subsequent GMQLDataset method
#'
#' @examples
#' 
#' ## This statement initializes and runs the GMQL server for local execution 
#' ## and creation of results on disk. Then, with system.file() it defines 
#' ## the path to the folder "DATASET" in the subdirectory "example"
#' ## of the package "RGMQL" and opens such file as a GMQL dataset named "exp" 
#' ## using CustomParser
#'
#' init_gmql()
#' test_path <- system.file("example", "DATASET", package = "RGMQL")
#' exp = read_gmql(test_path)
#' 
#' ## This GMQL statement groups samples of the input 'exp' dataset according 
#' ## to their value of the metadata attribute 'tumor_type' and computes the 
#' ## maximum value that the metadata attribute 'size' takes inside the samples 
#' ## belonging to each group. The samples in the output GROUPS_T dataset 
#' ## have a new _group metadata attribute which indicates which group they 
#' ## belong to, based on the grouping on the metadata attribute tumor_type. 
#' ## In addition, they present the new metadata aggregate attribute 'MaxSize'. 
#' ## Note that the samples without metadata attribute 'tumor_type' are 
#' ## assigned to a single group with _group value equal 0
#' 
#' GROUPS_T = group_by(exp, conds("tumor_type"), 
#'     meta_aggregates = list(max_size = MAX("size")))
#' 
#' ## This GMQL statement takes as input dataset the same input dataset as 
#' ## the previous example. Yet, it calculates new _group values based on the 
#' ## grouping attribute 'cell', and adds the metadata aggregate attribute 
#' ## 'n_samp', which counts the number of samples belonging to the respective 
#' ## group. It has the following output GROUPS_C dataset samples 
#' ## (note that now no sample has metadata attribute _group with value 
#' ## equal 0 since all input samples include the metadata attribute cell, 
#' ## with different values, on which the new grouping is based)
#' 
#' GROUPS_C = group_by(exp, conds("cell"),
#'     meta_aggregates = list(n_samp = COUNTSAMP()))
#' 
#' ## This GMQL statement groups the regions of each 'exp' dataset sample by 
#' ## region coordinates chr, left, right, strand  (these are implicitly 
#' ## considered) and the additional region attribute score (which is 
#' ## explicitly specified), and keeps only one region for each group. 
#' ## In the output GROUPS dataset schema, the new region attributes 
#' ## avg_pvalue and max_qvalue are added, respectively computed as the 
#' ## average of the values taken by the pvalue and the maximum of the values 
#' ## taken by the qvalue region attributes in the regions grouped together, 
#' ## and the computed value is assigned to each region of each output sample. 
#' ## Note that the region attributes which are not coordinates or score are 
#' ## discarded.
#' 
#' GROUPS = group_by(exp, groupBy_regions = "score", 
#'     region_aggregates = list(avg_pvalue = AVG("pvalue"), 
#'     max_qvalue = MAX("qvalue")))
#' 
#' @name group_by
#' @rdname group_by
#' @aliases group_by,GMQLDataset-method
#' @aliases group_by-method
#' @export
setMethod("group_by","GMQLDataset",group_by.GMQLDateset)

gmql_group <- function(
  input_data, 
  group_meta,
  group_reg, 
  region_aggregates, 
  meta_aggregates
) {
  if(!is.null(group_meta)) {
    if("condition" %in% names(group_meta)) {
      cond <- .join_condition(group_meta)
      if(is.null(cond))
        join_matrix <- .jnull("java/lang/String")
      else
        join_matrix <- .jarray(cond, dispatch = TRUE)
      
    } else
      stop("use function conds()")
    
  } else
    join_matrix <- .jnull("java/lang/String")
  
  if(!is.null(group_reg)) {
    if(!is.character(group_reg))
      stop("metadata: no valid input")
    
    group_reg <- group_reg[!group_reg %in% ""]
    group_reg <- group_reg[!duplicated(group_reg)]
    
    if(!length(group_reg))
      group_reg <- .jnull("java/lang/String")
    else
      group_reg <- .jarray(group_reg,dispatch = TRUE)
    
  } else
    group_reg <- .jnull("java/lang/String")
  
  if(!is.null(meta_aggregates) && length(meta_aggregates)) {
    aggr <- .aggregates(meta_aggregates,"AGGREGATES")
    metadata_matrix <- .jarray(aggr, dispatch = TRUE)
    
  } else
    metadata_matrix <- .jnull("java/lang/String")
  
  if(!is.null(region_aggregates) && length(region_aggregates)) {
    aggr <- .aggregates(region_aggregates,"AGGREGATES")
    region_matrix <- .jarray(aggr, dispatch = TRUE)
    
  } else
    region_matrix <- .jnull("java/lang/String")
  
  WrappeR <- J("it/polimi/genomics/r/Wrapper")
  response <- WrappeR$group(
    join_matrix, 
    metadata_matrix,
    group_reg, 
    region_matrix, 
    input_data
  )
  error <- strtoi(response[1])
  val <- response[2]
  if(error)
    stop(val)
  else
    GMQLDataset(val)
}
