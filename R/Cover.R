#' GMQL Operation: COVER
#'
#' it takes as input a dataset and returns another dataset (with a single sample, if no \emph{groupby} option is specified)
#' by “collapsing” the input dataset samples and their regions according to certain rules specified by the input parameters.
#' The attributes of the output genomic regions are only the region coordinates, and Jaccard indexes (JaccardIntersect and JaccardResult).
#' Jaccard Indexes are standard measures of similarity of the contributing regions, added as default region attributes.
#' The JaccardIntersect index is calculated as the ratio between the lengths of the intersection
#' and of the union of the contributing regions; the JaccardResult index is calculated as the ratio
#' between the lengths of region and the union of the contributing regions.
#' If aggregate functions are specified, new attributes are added.
#' Output metadata are the union of the input ones.
#' If \emph{groupby} clause is specified, the input samples are partitioned in groups,
#' each with distinct values of the grouping metadata attributes, and the COVER operation is separately
#' applied to each group, yielding to one sample in the result for each group.
#' Input samples that do not satisfy the \emph{groupby} condition are disregarded.
#'
#' @importFrom methods is
#' 
#' @param input_data returned object from any GMQL function
#' @param minAcc minimum number of overlapping regions to be considered during executio.n
#' Is a single integer number, declared also as string.
#' minAcc accept ALL and string like (ALL+N)/K as special keyword 
#' ALL sets the minimum to the number of samples in the input dataset
#' @param maxAcc maximum number of overlapping regions to be considered during execution.
#' Is a single integer number, declared also as string.
#' maxAcc accept ALL, ANY and string like (ALL+N)/K as special keyword 
#' ALL sets the maximum to the number of samples in the input dataset
#' ANY acts as a wildcard, consider all areas defined to any amount of overlapping 
#' @param groupBy list of CONDITION objects, or simple string concatenation 
#' (i.e c("cell_type","attribute_tag","size")).
#' Every object contains the name of metadata to be used in \emph{groupby}.
#' For details of CONDITION objects see:
#' \code{\link{DEF}}, \code{\link{FULL}}, \code{\link{EXACT}}
#' 
#' Every condition accepts only one string value. (e.g. DEF("cell_type") )
#' In case of single concatenation with no CONDITION, all metadata are considering as DEF
#' 
#' @param aggregates list of element in the form \emph{key} = \emph{function_aggregate}.
#' The \emph{function_aggregate} is an object of class OPERATOR
#' The aggregate functions available are: \code{\link{MIN}}, \code{\link{MAX}},
#' \code{\link{SUM}}, \code{\link{BAG}}, \code{\link{AVG}}, \code{\link{COUNT}},
#' \code{\link{STD}}, \code{\link{MEDIAN}}, \code{\link{Q1}}, \code{\link{Q2}}, 
#' \code{\link{Q3}}.
#' Every operator accepts a string value, execet for COUNT that cannot have a value.
#' Argument of 'function_aggregate' must exist in schema
#' Two style are allowed:
#' \itemize{
#' \item list of key-value pairs: e.g. sum = SUM("pvalue")
#' \item list of values: e.g. SUM("pvalue")
#' }
#' "mixed style" is not allowed
#'
#' @return DAGgraph class object. It contains the value associated to the graph used 
#' as input for the subsequent GMQL function
#' 
#' @references \url{http://www.bioinformatics.deib.polimi.it/genomic_computing/GMQL/doc/GMQLUserTutorial.pdf}
#'
#' @seealso  \code{\link{summit}} \code{\link{flat}} \code{\link{histogram}}
#'
#' @examples
#' 
#' ### This GMQL statement produces an output dataset with a single output sample. 
#' The COVER operation considers all areas defined by a minimum of two overlapping regions 
#' in the input samples, up to any amount of overlapping regions.
#' 
#' initGMQL("gtf")
#' test_path <- system.file("example","DATA_SET_VAR_GTF",package = "GMQL")
#' exp = read(test_path)
#' res = cover(input_data = exp,2,"ANY")
#'
#' \dontrun{
#' ### This GMQL statement computes the result grouping the input exp samples by the values of 
#' their cell metadata attribute, 
#' thus one output res sample is generated for each cell type; 
#' output regions are produced where at least 2 and at most 3 regions of grouped exp samples 
#' overlap, setting as attributes of the resulting regions the minimum pValue of the overlapping regions 
#' (min_pvalue) and their Jaccard indexes (JaccardIntersect and JaccardResult).
#' 
#' test_path <- system.file("example","DATA_SET_VAR_GTF",package = "GMQL")
#' exp = read(test_path)
#' res = cover(input_data = exp,2,3, c("cell"), list(min_pValue = MIN(pValue)))
#' }
#' @export
#'
cover <- function(input_data, minAcc, maxAcc, groupBy = NULL, aggregates = NULL)
{
  .doVariant("COVER",minAcc,maxAcc,groupBy,aggregates,input_data)
}

#' GMQL Operation: HISTOGRAM
#'
#' returns the non-overlapping regions contributing to the cover,
#' each with its accumulation index value, which is assigned to the AccIndex region attribute.
#'
#' @importFrom methods is
#'
#' @param input_data returned object from any GMQL function
#' @param minAcc minimum number of overlapping regions to be considered during execution
#' normally is a single integer number, declared also as string.
#' minAcc accept ALL and string like (ALL+N)/K as special keyword 
#' ALL sets the minimum to the number of samples in the input dataset
#' @param maxAcc maximum number of overlapping regions to be considered during execution
#' normally is a single integer number, declared also as string.
#' maxAcc accept ALL, ANY and string like (ALL+N)/K as special keyword 
#' ALL sets the maximum to the number of samples in the input dataset
#' ANY acts as a wildcard, consider all areas defined to any amount of overlapping 
#' @param groupBy list of CONDITION objects, or simple string concatenation 
#' (i.e c("cell_type","attribute_tag","size")).
#' Every object contains the name of metadata to be used in \emph{groupby}.
#' For details of CONDITION objects see:
#' \code{\link{DEF}}, \code{\link{FULL}}, \code{\link{EXACT}}
#' 
#' Every condition accepts only one string value. (e.g. DEF("cell_type") )
#' In case of single concatenation with no CONDITION, all metadata are considering as DEF
#' 
#' @param aggregates list of element in the form \emph{key} = \emph{function_aggregate}.
#' The \emph{function_aggregate} is an object of class OPERATOR
#' The aggregate functions available are: \code{\link{MIN}}, \code{\link{MAX}},
#' \code{\link{SUM}}, \code{\link{BAG}}, \code{\link{AVG}}, \code{\link{COUNT}},
#' \code{\link{STD}}, \code{\link{MEDIAN}}, \code{\link{Q1}}, \code{\link{Q2}}, 
#' \code{\link{Q3}}.
#' Every operator accepts a string value, execet for COUNT that cannot have a value.
#' Argument of 'function_aggregate' must exist in schema
#' Two style are allowed:
#' \itemize{
#' \item list of key-value pairs: e.g. sum = SUM("pvalue")
#' \item list of values: e.g. SUM("pvalue")
#' }
#' "mixed style" is not allowed
#'
#' @return DAGgraph class object. It contains the value associated to the graph used 
#' as input for the subsequent GMQL function
#' 
#' @references \url{http://www.bioinformatics.deib.polimi.it/genomic_computing/GMQL/doc/GMQLUserTutorial.pdf}
#' @seealso \code{\link{flat}} \code{\link{cover}} \code{\link{summit}}
#'
#' @examples
#'
#' ### This GMQL statement computes the result grouping the input \emph{exp} samples 
#' by the values of their \emph{cell} metadata attribute, 
#' thus one output \emph{res} sample is generated for each cell type. 
#' Output regions are produced by dividing results from COVER in contiguous subregions 
#' according to the varying accumulation values (from 2 to 4 in this case): 
#' one region for each accumulation value;
#'
#' initGMQL("gtf")
#' test_path <- system.file("example","DATA_SET_VAR_GTF",package = "GMQL")
#' exp = read(test_path)
#' res = histogram(exp, 2,4,groupBy = c("cell")) exp 
#' 
#' @export
#'
histogram <- function(input_data, minAcc, maxAcc, groupBy = NULL, aggregates = NULL)
{
  .doVariant("HISTOGRAM",minAcc,maxAcc,groupBy,aggregates,input_data)
}

#' GMQL Operation: SUMMIT
#'
#' returns regions that start from a position
#' where the number of intersecting regions is not increasing afterwards and stops
#' at a position where either the number of intersecting regions decreases,
#' or it violates the max accumulation index).
#'
#' @importFrom methods is
#'
#' @param input_data returned object from any GMQL function
#' @param minAcc minimum number of overlapping regions to be considered during execution
#' normally is a single integer number, declared also as string.
#' minAcc accept ALL and string like (ALL+N)/K as special keyword 
#' ALL sets the minimum to the number of samples in the input dataset
#' @param maxAcc maximum number of overlapping regions to be considered during execution
#' normally is a single integer number, declared also as string.
#' maxAcc accept ALL, ANY and string like (ALL+N)/K as special keyword 
#' ALL sets the maximum to the number of samples in the input dataset
#' ANY acts as a wildcard, consider all areas defined to any amount of overlapping 
#' @param groupBy list of CONDITION objects, or simple string concatenation 
#' (i.e c("cell_type","attribute_tag","size")).
#' Every object contains the name of metadata to be used in \emph{groupby}.
#' For details of CONDITION objects see:
#' \code{\link{DEF}}, \code{\link{FULL}}, \code{\link{EXACT}}
#' 
#' Every condition accepts only one string value. (e.g. DEF("cell_type") )
#' In case of single concatenation with no CONDITION, all metadata are considering as DEF
#' 
#' @param aggregates list of element in the form \emph{key} = \emph{function_aggregate}.
#' The \emph{function_aggregate} is an object of class OPERATOR
#' The aggregate functions available are: \code{\link{MIN}}, \code{\link{MAX}},
#' \code{\link{SUM}}, \code{\link{BAG}}, \code{\link{AVG}}, \code{\link{COUNT}},
#' \code{\link{STD}}, \code{\link{MEDIAN}}, \code{\link{Q1}}, \code{\link{Q2}}, 
#' \code{\link{Q3}}.
#' Every operator accepts a string value, execet for COUNT that cannot have a value.
#' Argument of 'function_aggregate' must exist in schema
#' Two style are allowed:
#' \itemize{
#' \item list of key-value pairs: e.g. sum = SUM("pvalue")
#' \item list of values: e.g. SUM("pvalue")
#' }
#' "mixed style" is not allowed
#'
#' @return DAGgraph class object. It contains the value associated to the graph used 
#' as input for the subsequent GMQL function
#' 
#' @references \url{http://www.bioinformatics.deib.polimi.it/genomic_computing/GMQL/doc/GMQLUserTutorial.pdf}
#' @seealso \code{\link{flat}} \code{\link{cover}} \code{\link{histogram}}
#'
#' @examples
#'
#' ### This GMQL statement computes the result grouping the input \emph{exp} samples by the values 
#' of their \emph{cell} metadata attribute, thus one output \emph{res} sample is generated 
#' for each cell type.
#' Output regions are produced by extracting the highest accumulation overlapping 
#' (sub)regions according to the methodologies described above;
#'
#'
#' initGMQL("gtf")
#' test_path <- system.file("example","DATA_SET_VAR_GTF",package = "GMQL")
#' exp = read(test_path)
#' res = summit(input_data = exp,2,4, c("cell"))
#' 
#' @export
#'
summit <- function(input_data, minAcc, maxAcc, groupBy = NULL, aggregates = NULL)
{
  .doVariant("SUMMIT",minAcc,maxAcc,groupBy,aggregates,input_data)
}

#' GMQL Operation: FLAT
#'
#' returns the contiguous region that starts from the first end and stops at
#' the last end of the regions which would contribute to each region of the COVER
#'
#' @importFrom methods is
#'
#' @param input_data returned object from any GMQL function
#' @param minAcc minimum number of overlapping regions to be considered during execution
#' normally is a single integer number, declared also as string.
#' minAcc accept ALL and string like (ALL+N)/K as special keyword 
#' ALL sets the minimum to the number of samples in the input dataset
#' @param maxAcc maximum number of overlapping regions to be considered during execution
#' normally is a single integer number, declared also as string.
#' maxAcc accept ALL, ANY and string like (ALL+N)/K as special keyword 
#' ALL sets the maximum to the number of samples in the input dataset
#' ANY acts as a wildcard, consider all areas defined to any amount of overlapping 
#' @param groupBy list of CONDITION objects, or simple string concatenation 
#' (i.e c("cell_type","attribute_tag","size")).
#' Every object contains the name of metadata to be used in \emph{groupBy}.
#' For details of CONDITION objects see:
#' \code{\link{DEF}}, \code{\link{FULL}}, \code{\link{EXACT}}
#' 
#' Every condition accepts only one string value. (e.g. DEF("cell_type") )
#' In case of single concatenation with no CONDITION, all metadata are considering as DEF
#' 
#' @param aggregates list of element in the form \emph{key} = \emph{function_aggregate}.
#' The \emph{function_aggregate} is an object of class OPERATOR
#' The aggregate functions available are: \code{\link{MIN}}, \code{\link{MAX}},
#' \code{\link{SUM}}, \code{\link{BAG}}, \code{\link{AVG}}, \code{\link{COUNT}},
#' \code{\link{STD}}, \code{\link{MEDIAN}}, \code{\link{Q1}}, \code{\link{Q2}}, 
#' \code{\link{Q3}}.
#' Every operator accepts a string value, execet for COUNT that cannot have a value.
#' Argument of 'function_aggregate' must exist in schema
#' Two style are allowed:
#' \itemize{
#' \item list of key-value pairs: e.g. sum = SUM("pvalue")
#' \item list of values: e.g. SUM("pvalue")
#' }
#' "mixed style" is not allowed
#'
#' @return DAGgraph class object. It contains the value associated to the graph used 
#' as input for the subsequent GMQL function
#' 
#' @references \url{http://www.bioinformatics.deib.polimi.it/genomic_computing/GMQL/doc/GMQLUserTutorial.pdf}
#' @seealso \code{\link{summit}} \code{\link{cover}} \code{\link{histogram}}
#'
#' @examples
#' 
#' ### This GMQL statement computes the result grouping the input \emph{exp} samples by 
#' the values of their \emph{cell} metadata attribute, thus one output \emph{res} sample 
#' is generated for each cell type. 
#' Output regions are produced by concatenating all regions which would have been used 
#' to construct a COVER(2,4) statement on the same dataset; 
#' 
#' initGMQL("gtf")
#' test_path <- system.file("example","DATA_SET_VAR_GTF",package = "GMQL")
#' exp = read(test_path)
#' res = flat(input_data = exp,2,4, c("cell"))
#'
#' @export
#'
flat <- function(input_data, minAcc, maxAcc, groupBy = NULL, aggregates = NULL)
{
  .doVariant("FLAT",minAcc,maxAcc,groupBy,aggregates,input_data)
}

.doVariant <- function(flag,minAcc,maxAcc,groupBy,aggregates,input_data)
{
  min <- .check_cover_param(minAcc,TRUE)
  max <- .check_cover_param(maxAcc,FALSE)

  if(!is.null(groupBy))
    join_condition_matrix <- .join_condition(groupBy)
  else
    join_condition_matrix <- scalaNull("Array[Array[String]]")
  
  if(!is.null(aggregates))
    metadata_matrix <- .aggregates(aggregates,"OPERATOR")
  else
    metadata_matrix <- scalaNull("Array[Array[String]]")


  out <- switch(flag,
                "COVER" = WrappeR$cover(min,max,join_condition_matrix,metadata_matrix,input_data$value),
                "FLAT" = WrappeR$flat(min,max,join_condition_matrix,metadata_matrix,input_data$value),
                "SUMMIT" = WrappeR$summit(min,max,join_condition_matrix,metadata_matrix,input_data$value),
                "HISTOGRAM" = WrappeR$histogram(min,max,join_condition_matrix,metadata_matrix,input_data$value))

  if(grepl("No",out,ignore.case = TRUE))
    stop(out)
  else
    DAGgraph(out)
}

.check_cover_param <- function(param,is_min)
{
  if(length(param)>1)
    stop("length > 1")

  if(is.numeric(param))
  {
    if(param<=0)
      stop("No negative value")
    else
      return(as.integer(param))
  }
  else if(is.character(param))
  {
    if(is_min && identical(param,"ANY"))
      stop("min cannot assume ANY as value")
    return(param)
  }
  else
    stop("invalid input data")
}


