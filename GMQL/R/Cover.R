
#' GMQL Operation: COVER
#'
#' responds to the need of computing properties that reflect region’s intersections,
#' the operation with no grouping produces a single output sample,
#' and all the metadata attributes of the contributing input samples in result dataset
#' are assigned to the resulting single sample in input dataset
#' Regions of the result sample are built from the regions of samples in input dataset
#' according to the following condition:
#'
#' Each resulting region r in input dataset is the contiguous intersection of at least minAcc
#' and at most maxAcc contributing regions in the samples of result dataset;
#' minAcc and maxAcc are called accumulation indexes.
#'
#' Resulting regions may have new attributes, calculated by means of aggregate expressions over
#' the attributes of the contributing regions.
#' Jaccard Indexes are standard measures of similarity of the contributing regions,
#' added as default region attributes.
#' with grouping define instead, the samples are partitioned by groups,
#' each with distinct values of grouping metadata attributes (i.e., homonym attributes in the operand schemas)
#' and the cover operation is separately applied to each group,
#' yielding to one sample in the result for each group
#'
cover <- function(minAcc,maxAcc,groupBy = NULL,aggregates = NULL, input_data)
{
  .doVariant("COVER",minAcc,maxAcc,groupBy,aggregates,input_data)
}

#' GMQL Operation: HISTOGRAM
#'
#' returns the non-overlapping regions contributing to the cover,
#' each with its accumulation index value, which is assigned to the AccIndex region attribute.
#'
histogram <- function(minAcc,maxAcc,groupBy = NULL,aggregates = NULL, input_data)
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
summit <- function(minAcc,maxAcc,groupBy = NULL,aggregates = NULL, input_data)
{
  .doVariant("SUMMIT",minAcc,maxAcc,groupBy,aggregates,input_data)
}

#' GMQL Operation: FLAT
#'
#' returns the contiguous region that starts from the first end and stops at
#' the last end of the regions which would contribute to each region of the COVER
#'
flat <- function(minAcc,maxAcc,groupBy = NULL,aggregates = NULL, input_data)
{
  .doVariant("FLAT",minAcc,maxAcc,groupBy,aggregates,input_data)
}

#move in internals
.doVariant <- function(flag,minAcc,maxAcc,groupBy,aggregates,input_data)
{
  if(!is.numeric(minAcc) || !is.numeric(maxAcc))
    stop("minAcc and maxAcc must be numeric")

  if(minAcc < -1 || maxAcc < -1)
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
