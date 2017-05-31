#' GMQL Operation: COVER
#'
#' it takes as input a dataset and returns another dataset (with a single sample, if no groupby option is specified)
#' by “collapsing” the input samples and their regions according to certain rules specified by the input parameters.
#' The attributes of the output regions are the same as input_dataset:
#' if aggregate functions are specified, new attributes with aggregate values over schema region values
#' Output metadata are the union of the input ones, plus the metadata attributes JaccardIntersect and JaccardResult,
#' representing global Jaccard Indexes for the considered dataset,
#' computed as the correspondent region Jaccard Indexes but on the whole sample regions.
#' If groupby clause is specified, the input samples are partitioned in groups,
#' each with distinct values of the grouping metadata attributes, and the COVER operation is separately
#' applied to each group, yielding to one sample in the result for each group.
#' Input samples that do not satisfy the groupby condition are disregarded.
#'
#'
#' @param input_data "url-like" string taken from GMQL function
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
#' 'function_aggregate' is an object of class \code{\link{OPERATOR}}
#' The aggregate functions available are: MIN, MAX, SUM, BAG, AVG, COUNT,MEDIAN
#' Every operator accepts a string value, execet for COUNT that cannot have a value.
#' Argument of 'function_aggregate' must exist in schema
#' Two style are allowed:
#'
#' @references \url{http://www.bioinformatics.deib.polimi.it/genomic_computing/GMQL/doc/GMQLUserTutorial.pdf}
#'
#' @seealso  \code{\link{summit}} \code{\link{flat}} \code{\link{histogram}}
#'
#' @examples
#'
#' \dontrun{
#' initGMQL("gtf")
#' path = "/<path_to_your_folder>/<your_dataset_name>"
#' r = read(path)
#' r2 = read(path2)
#' c = cover(2,3,input_data = r)
#' }
cover <- function(input_data, minAcc, maxAcc, groupBy = NULL, aggregates = NULL)
{
  .doVariant("COVER",minAcc,maxAcc,groupBy,aggregates,input_data)
}

#' GMQL Operation: HISTOGRAM
#'
#' returns the non-overlapping regions contributing to the cover,
#' each with its accumulation index value, which is assigned to the AccIndex region attribute.
#'
#' @param input_data "url-like" string taken from GMQL function
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
#' 'function_aggregate' is an object of class \code{\link{OPERATOR}}
#' The aggregate functions available are: MIN, MAX, SUM, BAG, AVG, COUNT.
#' Every operator accepts a string value, execet for COUNT that cannot have a value.
#' Argument of 'function_aggregate' must exist in schema
#' Two style are allowed:
#'
#' @references \url{http://www.bioinformatics.deib.polimi.it/genomic_computing/GMQL/doc/GMQLUserTutorial.pdf}
#' @seealso \code{\link{flat}} \code{\link{cover}} \code{\link{summit}}
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
#' @param input_data "url-like" string taken from GMQL function
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
#' 'function_aggregate' is an object of class \code{\link{OPERATOR}}
#' The aggregate functions available are: MIN, MAX, SUM, BAG, AVG, COUNT.
#' Every operator accepts a string value, execet for COUNT that cannot have a value.
#' Argument of 'function_aggregate' must exist in schema
#' Two style are allowed:
#'
#' @references \url{http://www.bioinformatics.deib.polimi.it/genomic_computing/GMQL/doc/GMQLUserTutorial.pdf}
#' @seealso \code{\link{flat}} \code{\link{cover}} \code{\link{histogram}}
#'
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
#' @param input_data "url-like" string taken from GMQL function
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
#' 'function_aggregate' is an object of class \code{\link{OPERATOR}}
#' The aggregate functions available are: MIN, MAX, SUM, BAG, AVG, COUNT.
#' Every operator accepts a string value, execet for COUNT that cannot have a value.
#' Argument of 'function_aggregate' must exist in schema
#' Two style are allowed:
#'
#' @references \url{http://www.bioinformatics.deib.polimi.it/genomic_computing/GMQL/doc/GMQLUserTutorial.pdf}
#' @seealso \code{\link{summit}} \code{\link{cover}} \code{\link{histogram}}
#'
#'
#'
flat <- function(input_data, minAcc, maxAcc, groupBy = NULL, aggregates = NULL)
{
  .doVariant("FLAT",minAcc,maxAcc,groupBy,aggregates,input_data)
}

#move in internals
.doVariant <- function(flag,minAcc,maxAcc,groupBy,aggregates,input_data)
{
  if(!is.numeric(minAcc) || !is.numeric(maxAcc))
    stop("minAcc and maxAcc must be numeric")

  if(minAcc < -1 || minAcc == 0)
    stop("only -1 or any positive value")

  if(maxAcc < -1)
    stop("only 0,-1 or any positive value")

  min = as.integer(minAcc)
  max = as.integer(maxAcc)

  min = min[1]
  max = max[1]

  if(!is.character(groupBy) && !is.null(groupBy))
    stop("groupBy can be only null, single string or an array of string")

  groupBy = groupBy[!groupBy %in% ""]
  groupBy = groupBy[!duplicated(groupBy)]

  if(!is.null(aggregates))
  {
    if(!is.list(aggregates))
      stop("aggregates must be a list")

    if(!all(sapply(aggregates, function(x) is(x,"OPERATOR") )))
    {
      stop("you must use OPERATOR object for defining aggregates function")
    }

    names <- names(aggregates)
    if(is.null(names)){
      warning("you did not assign a names to a list.\nWe build names for you")
      names <- sapply(aggregates, function(x) {
        take_value.OPERATOR(x)
      })
    }
    else {
      if(all(sapply(names, function(x) (x=="")))) {
        stop("no partial names assignment to list")
      }
    }
    aggregate_matrix <- t(sapply(aggregates, function(x) {

      new_value = as.character(x)
      matrix <- matrix(new_value)
    }))
    m_names <- matrix(names)
    metadata_matrix <- cbind(m_names,aggregate_matrix)
  }
  else
    metadata_matrix = NULL

  out <- switch(flag,
                "COVER" = frappeR$cover(min,max,groupBy,metadata_matrix,input_data),
                "FLAT" = frappeR$flat(min,max,groupBy,metadata_matrix,input_data),
                "SUMMIT" = frappeR$summit(min,max,groupBy,metadata_matrix,input_data),
                "HISTOGRAM" = frappeR$histogram(min,max,groupBy,metadata_matrix,input_data))

  if(grepl("No",out,ignore.case = T))
    stop(out)
  else
    out
}
