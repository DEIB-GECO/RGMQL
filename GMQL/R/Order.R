#' GMQL operation: ORDER
#'
#' It is used to order either samples or sample regions or both,
#' according to a set of metadata and/or region attributes, and/or region coordinates.
#' Order can be specified as ascending / descending for every attribute
#' The number of samples and their regions remain the same (unless mtop/rtop parameters specified)
#' but a new ordering metadata and/or region attribute is added.
#' Sorted samples or regions have a new attribute "order", added to either metadata, or regions,
#' or both of them as specified in input
#' The input mtop = k and rtop = m extracts the first k samples and m regions respectively,
#' the clause mtopg = k and rtopg = m performs grouping operation,
#' grouping by identical values of ordering attributes
#' and then selects the first k samples or regions of each group
#'
#'
#' @param input_data "url-like" string taken from GMQL function
#' @param metadata_ordering list of \code{\link{ORDER}} objects where every object contains the name of metadata
#' The ORDER's available are: ASC, DESC
#' Every condition accepts only one string value. (e.g. ASC("cell_type") )
#' @param mtop integer value specifying the first k samples.
#' default is 0 that means every sample must be considered
#' @param mtopg integer value specifying the first j samples in each group.
#' default is 0 that means every sample must be considered
#' @param regions_ordering list of \code{\link{ORDER}} objects where every object contains the name of region schema value
#' The ORDER's available are: ASC, DESC.
#' Every condition accepts only one string value. (e.g. DESC("pvalue") )
#' @param rtop integer value specifying the first m samples in each group.
#' default is 0 that means every sample must be considered
#' @param rtopg integer value specifying the first i samples in each group.
#' default is 0 that means every sample must be considered
#'
#'
#' @return "url-like" string
#'
#' @details
#' mtop, mtopg, rtop and rtopg are normally numbers: if you specify a vector,
#' only the first element will be used
#' mtop and mtopg are mutalbe exclusive, so rtop and rtopg
#' if you specified both, only mtop (rtop) is taken
#'
#'
#' @references \url{http://www.bioinformatics.deib.polimi.it/genomic_computing/GMQL/doc/GMQLUserTutorial.pdf}
#'
#' @examples
#' \dontrun{
#'
#' initGMQL("gtf")
#' test_path <- system.file("example","DATA_SET_VAR_GTF",package = "GMQL")
#' r = read(test_path)
#' s = select()
#' o = order(DESC(Region_Count), mtop = 2, input_data = s)
#' }
#'
#' @export
#'
order <- function(input_data, metadata_ordering = NULL, mtop = 0, mtopg = 0,
                  regions_ordering = NULL,rtop = 0,rtopg = 0)
{
  if(!is.numeric(mtop) || !is.numeric(mtopg) || !is.numeric(rtop) || !is.numeric(rtopg))
    stop("mtop, rtop, rtopg and mtopg must be integer")

  # we consider only the first element even if input is a vector of Int
  # we cut the other arguments

  mtop = as.integer(mtop[1])
  mtog = as.integer(mtopg[1])

  rtop = as.integer(rtop[1])
  rtopg = as.integer(rtopg[1])

  if(mtop > 0 && mtopg >0)
  {
    warning("cannot be used together.\nWe set mtopg = 0")
    mtopg = 0
  }

  if(rtop > 0 && rtopg >0)
  {
    warning("cannot be used together.\nWe set rtopg = 0")
    rtopg = 0
  }

  if(!is.null(metadata_ordering))
    meta_matrix <- .ordering_meta(metadata_ordering)

  if(!is.null(regions_ordering))
    region_matrix <- .ordering_meta(regions_ordering)

  out <- WrappeR$order(meta_matrix,mtopg,mtop,region_matrix,rtopg,rtop,input_data)
  if(grepl("No",out,ignore.case = TRUE))
    stop(out)
  else
    out
}



.ordering_meta <- function(ordering)
{
  if(is.list(ordering))
  {
    order_matrix <- t(sapply(ordering,function(x){
      new_value <- as.character(x)
      if(length(new_value)==1)
        new_value = c("ASC",new_value)
      else if(!identical("ASC",new_value[1]) && !identical("DESC",new_value[1]))
        stop("no more than one value")
      matrix <- matrix(new_value)
    }))
  }
  else if(is.character(ordering))
  {
    order_matrix <- t(sapply(ordering, function(x) {
      new_value = c("ASC",x)
      matrix <- matrix(new_value)
    }))
  }
  else
    stop("only list or character")
}



