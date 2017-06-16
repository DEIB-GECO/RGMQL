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
#' @param input_data returned object from any GMQL function
#' @param groupBy a vector of strings specifying grouping criteria
#'
#' @return "url-like" string
#'
#' @references \url{http://www.bioinformatics.deib.polimi.it/genomic_computing/GMQL/doc/GMQLUserTutorial.pdf}
#'
#' @examples
#'
#' \dontrun{
#'
#' initGMQL("gtf")
#' test_path <- system.file("example","DATA_SET_VAR_GTF",package = "GMQL")
#' r = read(test_path)
#' m = merge(groupBy = c("antibody_targer","cell_karyotype"),input_data = r)
#' }
#'
#' ""
#' @export
#'
merge <- function(input_data, groupBy = NULL)
{
  if(!is.null(groupBy))
  {
    if(!is.character(groupBy))
      stop("groupBy: only character")

    groupBy = groupBy[!groupBy %in% ""]
    groupBy = groupBy[!duplicated(groupBy)]

    if(length(groupBy)==0)
      groupBy=NULL
  }

  out <- WrappeR$merge(groupBy,input_data)
  if(grepl("No",out,ignore.case = TRUE))
    stop(out)
  else
    out
}
