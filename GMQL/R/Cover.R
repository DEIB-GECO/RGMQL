#' GMQL Operation: COVER
#'
#' it takes as input a dataset and returns another dataset (with a single sample, if no \emph{groupby} option is specified)
#' by “collapsing” the input samples and their regions according to certain rules specified by the input parameters.
#' The attributes of the output regions are only the region coordinates, and Jaccard indexes ( JaccardIntersect and JaccardResult).
#' Jaccard Indexes are standard measures of similarity of the contributing regions, added as default region attributes.
#' The JaccardIntersect index is calculated as the ratio between the lengths of the intersection
#' and of the union of the contributing regions; the JaccardResult index is calculated as the ratio
#' between the lengths of the result and of the union of the contributing regions.
#' If aggregate functions are specified, new attributes with aggregate values over schema region values;
#' Output metadata are the union of the input ones.
#' If \emph{groupby} clause is specified, the input samples are partitioned in groups,
#' each with distinct values of the grouping metadata attributes, and the COVER operation is separately
#' applied to each group, yielding to one sample in the result for each group.
#' Input samples that do not satisfy the \emph{groupby} condition are disregarded.
#'
#' @importFrom methods is
#'
#' @param input_data returned object from any GMQL function
#' @param minAcc minimum number of overlapping regions to be considered during execution
#' @param maxAcc maximum number of overlapping regions to be considered during execution
#' @param groupBy list of CONDITION objects every object contains the name of metadata to be used in semijoin,
#' or simple string concatenation c("cell_type","attribute_tag","size") without declaring condition.
#' In the latter form all metadata are considered having DEF condition
#' The CONDITION's available are:
#' \itemize{
#' \item{FULL: Fullname evaluation, two attributes match if they both end with value and,
#' if they have a further prefixes, the two prefix sequence are identical}
#' \item{DEF: Default evaluation, two attributes match if both end with value. }
#' \item{EXACT: Exact evaluation, only attributes exactly as value will match; no further prefixes are allowed. }
#' }
#' Every condition accepts only one string value. (e.g. DEF("cell_type") )
#'
#' @param aggregates a list of element in the form key = 'function_aggregate'.
#' 'function_aggregate' is an object of inherited class of class OPERATOR
#' The aggregate functions available are: MIN, MAX, SUM, BAG, AVG, COUNT,MEDIAN
#' Every operator accepts a string value, execet for COUNT that cannot have a value.
#' Argument of 'function_aggregate' must exist in schema
#' Two style are allowed:
#'
#' @return "url-like" string
#'
#' @references \url{http://www.bioinformatics.deib.polimi.it/genomic_computing/GMQL/doc/GMQLUserTutorial.pdf}
#'
#' @seealso  \code{\link{summit}} \code{\link{flat}} \code{\link{histogram}}
#'
#' @examples
#'
#' \dontrun{
#'
#' library(rscala)
#'
#' initGMQL("gtf")
#' test_path <- system.file("example","DATA_SET_VAR_GTF",package = "GMQL")
#' r = read(test_path)
#' c = cover(input_data = r,2,3)
#' }
#' .
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
#' Normally must be > 0, we admit value 0 and -1 as special value:
#' \itemize{
#' \item {-1: means ALL and sets the minimum to the number of samples in the input dataset}
#' }
#' @param maxAcc maximum number of overlapping regions to be considered during execution
#' \itemize{
#' \item {0: means ANY and acts as a wildcard and can be used only as maxAcc value}
#' \item {-1: means ALL and sets the maximum to the number of samples in the input dataset}
#' }
#' @param groupBy a vector of strings specifying grouping criteria
#' @param aggregates a list of element in the form key = 'function_aggregate'.
#' 'function_aggregate' is an object of class OPERATOR
#' The aggregate functions available are: MIN, MAX, SUM, BAG, AVG, COUNT.
#' Every operator accepts a string value, execet for COUNT that cannot have a value.
#' Argument of 'function_aggregate' must exist in schema
#' Two style are allowed:
#' @return "url-like" string
#'
#' @references \url{http://www.bioinformatics.deib.polimi.it/genomic_computing/GMQL/doc/GMQLUserTutorial.pdf}
#' @seealso \code{\link{flat}} \code{\link{cover}} \code{\link{summit}}
#'
#' @examples
#'
#' \dontrun{
#'
#' library(rscala)
#'
#' initGMQL("gtf")
#' test_path <- system.file("example","DATA_SET_VAR_GTF",package = "GMQL")
#' r = read(test_path)
#' c = histogram(input_data = r,2,3)
#' }
#' .
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
#' Normally must be > 0, we admit value 0 and -1 as special value:
#' \itemize{
#' \item {-1: means ALL and sets the minimum to the number of samples in the input dataset}
#' }
#' @param maxAcc maximum number of overlapping regions to be considered during execution
#' \itemize{
#' \item {0: means ANY and acts as a wildcard and can be used only as maxAcc value}
#' \item {-1: means ALL and sets the maximum to the number of samples in the input dataset}
#' }
#' @param groupBy a vector of strings specifying grouping criteria
#' @param aggregates a list of element in the form key = 'function_aggregate'.
#' 'function_aggregate' is an object of class OPERATOR
#' The aggregate functions available are: MIN, MAX, SUM, BAG, AVG, COUNT.
#' Every operator accepts a string value, execet for COUNT that cannot have a value.
#' Argument of 'function_aggregate' must exist in schema
#' Two style are allowed
#'
#' @return "url-like" string
#'
#' @references \url{http://www.bioinformatics.deib.polimi.it/genomic_computing/GMQL/doc/GMQLUserTutorial.pdf}
#' @seealso \code{\link{flat}} \code{\link{cover}} \code{\link{histogram}}
#'
#' @examples
#'
#' \dontrun{
#'
#' library(rscala)
#'
#' initGMQL("gtf")
#' test_path <- system.file("example","DATA_SET_VAR_GTF",package = "GMQL")
#' r = read(test_path)
#' c = summit(input_data = r,2,3)
#' }
#' .
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
#' @param minAcc minimum number of overlapping regions to be considered during execution.
#' Normally it must be > 0, we admit value 0 and -1 as special value:
#' \itemize{
#' \item {-1: means ALL and sets the minimum to the number of samples in the input dataset}
#' }
#' @param maxAcc maximum number of overlapping regions to be considered during execution
#' \itemize{
#' \item {0: means ANY and acts as a wildcard and can be used only as maxAcc value}
#' \item {-1: means ALL and sets the maximum to the number of samples in the input dataset}
#' }
#' @param groupBy a vector of strings specifying grouping criteria
#' @param aggregates a list of element in the form key = 'function_aggregate'.
#' 'function_aggregate' is an object of class OPERATOR
#' The aggregate functions available are: MIN, MAX, SUM, BAG, AVG, COUNT.
#' Every operator accepts a string value, execet for COUNT that cannot have a value.
#' Argument of 'function_aggregate' must exist in schema.
#' Two styles are allowed:
#'
#' @return "url-like" string
#'
#' @references \url{http://www.bioinformatics.deib.polimi.it/genomic_computing/GMQL/doc/GMQLUserTutorial.pdf}
#' @seealso \code{\link{summit}} \code{\link{cover}} \code{\link{histogram}}
#'
#' @examples
#'
#' \dontrun{
#'
#' library(rscala)
#'
#' initGMQL("gtf")
#' test_path <- system.file("example","DATA_SET_VAR_GTF",package = "GMQL")
#' r = read(test_path)
#' c = flat(input_data = r,2,3)
#' }
#' .
#' @export
#'
#' @export
#'
flat <- function(input_data, minAcc, maxAcc, groupBy = NULL, aggregates = NULL)
{
  .doVariant("FLAT",minAcc,maxAcc,groupBy,aggregates,input_data)
}

#move in internals
.doVariant <- function(flag,minAcc,maxAcc,groupBy,aggregates,input_data)
{
  min <- .check_cover_param(minAcc,TRUE)
  max <- .check_cover_param(maxAcc,FALSE)

  if(!is.null(groupBy))
  {
    if(!is.character(groupBy))
      stop("groupBy can be only null, single string or an array of string")

    groupBy = groupBy[!groupBy %in% ""]
    groupBy = groupBy[!duplicated(groupBy)]

    if(length(groupBy)<=0)
      groupBy=NULL
  }

  if(!is.null(aggregates))
    metadata_matrix <- .aggregates(aggregates,"OPERATOR")
  else
    metadata_matrix = NULL

  out <- switch(flag,
                "COVER" = WrappeR$cover(min,max,groupBy,metadata_matrix,input_data),
                "FLAT" = WrappeR$flat(min,max,groupBy,metadata_matrix,input_data),
                "SUMMIT" = WrappeR$summit(min,max,groupBy,metadata_matrix,input_data),
                "HISTOGRAM" = WrappeR$histogram(min,max,groupBy,metadata_matrix,input_data))

  if(grepl("No",out,ignore.case = TRUE))
    stop(out)
  else
    out
}

.check_cover_param <- function(param,is_min)
{
  if(length(param)>1)
    warning("only the first element is taken")

  param <- param[1]

  if(is.numeric(param))
  {
    if(param<=0)
      stop("only positive value")
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


