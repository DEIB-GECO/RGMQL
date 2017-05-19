#' GMQL Operation: MERGE
#'
#' It builds a dataset consisting of a single sample having as regions
#' all the regions of the input data and as metadata the union of all
#' the attribute-values of the input samples.
#' A groupby clause can be specified on metadata:the samples are then partitioned in groups,
#' each with a distinct value of the grouping metadata attributes, and the operation is separately
#' applied to each group, yielding to one sample in the result for each group.
#' Samples without the grouping metadata attributes are disregarded.
#'
#'
#' @param metadata a vector of metadata as string
#' @param input_data url-like string "pointer" taken from GMQL function
#' @examples
#'
#' startGMQL()
#' r = read(path)
#' s = select(input_data = r)
#' m = merge(groupBy = c("antibody_targer","cell_karyotype"),input_data = r)
#' m = merge(c("antibody_targer","cell_karyotype"),s)
#' m = merge(input_data = s)
#'
merge <- function(groupBy = NULL,input_data)
{
  if(!is.character(groupBy) && !is.null(groupBy))
    stop("groupBy can be a string or an array of string")

  groupBy = groupBy[!groupBy %in% ""]
  groupBy = groupBy[!duplicated(groupBy)]
  if(length(groupBy)==0)
    groupBy=NULL

  out <- frappeR$merge(groupBy,input_data)
  if(grepl("No",out,ignore.case = T))
    stop(out)
  else
    out
}
