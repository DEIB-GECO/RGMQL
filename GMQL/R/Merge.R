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
#'
#' @param input_data "url-like" string taken from GMQL function
#' @param groupBy a vector of strings specifying grouping criteria
#'
#' @references \url{http://www.bioinformatics.deib.polimi.it/genomic_computing/GMQL/doc/GMQLUserTutorial.pdf}
#'
#' @examples
#'
#' \dontrun{
#' startGMQL()
#' path = "/<path_to_your_folder>/<your_dataset_name>"
#' r = read(path)
#' s = select(input_data = r)
#' m = merge(groupBy = c("antibody_targer","cell_karyotype"),input_data = r)
#' m1 = merge(s,"antibody_targer")
#' m2 = merge(input_data = s)
#' m3 = merge(s)
#' }
merge <- function(input_data, groupBy = NULL)
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
