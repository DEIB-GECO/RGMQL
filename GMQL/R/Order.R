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
#' @param metadata_order list of ORDER objects where every object contains the name of metadata
#' The ORDER's available are: ASC, DESC.
#' Every condition accepts only one string value. (e.g. ASC("cell_type") )
#' @param mtop integer value specifying the first k samples.
#' default is 0 that means every sample must be considered
#' @param mtopg integer value specifying the first j samples in each group.
#' default is 0 that means every sample must be considered
#' @param regions_order list of ORDER objects where every object contains the name of region schema value
#' The ORDER's available are: ASC, DESC.
#' Every condition accepts only one string value. (e.g. DESC("pvalue") )
#' @param rtop integer value specifying the first m samples in each group.
#' default is 0 that means every sample must be considered
#' @param rtopg integer value specifying the first i samples in each group.
#' default is 0 that means every sample must be considered
#' @param input_data "url-like" string taken from GMQL function
#'
#' @details
#' mtop, mtopg, rtop and rtopg are normally numbers: if you specify a vector,
#' only the first element will be used
#' mtop and mtopg are mutalbe exclusive, so rtop and rtopg
#' if you specified both, only mtop (rtop) is taken
#'
#' @seealso  \code{\link{DESC}} \code{\link{ASC}}
#' @seealso \url{http://www.bioinformatics.deib.polimi.it/genomic_computing/GMQL/doc/GMQLUserTutorial.pdf}
#'
#' @examples
#'
#' startGMQL()
#' path = /.../dataset_name
#' r = read(path)
#' c = cover(2,3,input_data = r)
#' s = select("NOT(Patient_age < 70 AND provider=='Polimi')",input_dat = r)
#' s = select("NOT(Patient_age < 70)",region_predicate = "NOT(qValue > 0.001)",
#' semi_join = list(EXACT("cell_type"),EXACT("age")),semi_join_dataset = c,input_data = r )
#'
#' o = order(DESC(Region_Count), mtop = 2, input_data = s)
#' o = order(list(DESC(Region_Count)),regions_order = list(DESC(MutationCount),ASC(pvalue)),
#' mtop = 5,rtopg = 1, input_data = c)
#'
#'
order <- function(input_data, metadata_order = NULL, mtop = 0, mtopg = 0,
                  regions_order = NULL,rtop = 0,rtopg = 0)
{
  if(!is.numeric(mtop) || !is.numeric(mtopg) || !is.numeric(rtop) || !is.numeric(rtopg))
    stop("mtop, rtop, rtopg and mtopg must be numeric")

  if(!is.null(metadata_order) && !is.list(metadata_order))
    stop("metadata_order must be a list")

  if(!is.null(regions_order) && !is.list(regions_order))
    stop("regions_order must be a list")

  if(all(sapply(metadata_order, function(x) is(x,"ORDER") )))
  {
    stop("you must use ORDER object for defining ordering")
  }

  # we consider only the first element even if input is a vector of Int
  # we cut the other arguments

  mtop = mtop[1]
  mtog = mtopg[1]

  rtop = rtop[1]
  rtopg = rtopg[1]

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

  order_matrix <- sapply(metadata_order,function(x){
    order <- as.character(x)
    matrix <- t(matrix(order))
  })

  order_matrix <- sapply(regions_order,function(x){
    order <- as.character(x)
    matrix <- matrix(order)
  })
  o_m_names <- matrix(names_reg)
  order_region_matrix <- cbind(o_m_names,order_matrix)

  out <- frappeR$order(metadata,mtopg,mtop,order_region_matrix,rtopg,rtop,input_data)
  if(grepl("No",out,ignore.case = T))
    stop(out)
  else
    out
}

