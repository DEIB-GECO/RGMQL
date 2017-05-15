#'GMQL operation: ORDER
#'
#'
#'It orders either samples, or regions, or both of them; order is ascending as default,
#'and can be turned to descending by an explicit indication.
#'Sorted samples or regions have a new attribute Order, added to either metadata, or regions,
#'or both of them; the value of Order reflects the result of the sorting.
#'Identifiers of the samples of the operand S1 are assigned to the result S2.
#'The clause TOP <k> extracts the first k samples or regions,  the clause TOPG <k> implicitly
#'considers the grouping by identical values of the first n − 1 ordering attributes
#'and then selects the first k samples or regions of each group
#'
#'
#'@param metadata_order
#'@param mtop 0 is default meand that we not use it
#'@param mtopg 0 is default meand that we not use it
#'@param regions_order
#'@param rtop 0 is default meand that we not use it
#'@param rtopg 0 is default meand that we not use it
#'@param input_data
#'
order <- function(metadata_order = NULL, mtop = 0, mtopg = 0,
                  regions_order = NULL,rtop = 0,rtopg = 0, input_data)
{
  if(!is.numeric(mtop) || !is.numeric(mtopg) || !is.numeric(rtop) || !is.numeric(rtopg))
    stop("mtop, rtop, rtopg and mtopg must be numeric")

  if(!is.null(metadata_order) && !is.list(metadata_order))
    stop("metadata must be a list")

  if(!is.null(regions_order) && !is.list(regions_order))
    stop("metadata must be a list")

  if(mtop > 0 && mtopg >0)
  {
    warning("cannot be used together")
    mtopg = 0
  }

  if(rtop > 0 && rtopg >0)
  {
    warning("cannot be used together")
    rtopg = 0
  }

  names_meta <- names(metadata_order)
  if(is.null(names_meta))
    stop("you did not assign a names to a list")
  else {
    if(!all(sapply(names, function(x) {
      if(x=="") F else T})))
    {
      stop("no partial key assignment to list")
    }
  }
  order_matrix <- sapply(metadata_order,create_order_matrix)
  o_m_names <- matrix(names_meta)
  order_metadata_matrix <- cbind(o_m_names,order_matrix)

  names_reg <- names(regions_order)
  if(is.null(names_reg))
    stop("you did not assign a names to a list")
  else {
    if(!all(sapply(names, function(x) {
      if(x=="") F else T})))
    {
      stop("no partial key assignment to list")
    }
  }
  order_matrix <- sapply(regions_order,create_order_matrix)
  o_m_names <- matrix(names_reg)
  order_region_matrix <- cbind(o_m_names,order_matrix)

  out <- frappeR$order(metadata,mtopg,mtop,order_region_matrix,rtopg,rtop,input_data)
  if(grepl("No",out,ignore.case = T))
    stop(out)
  else
    out
}


create_order_matrix <- function(x)
{
  if(length(x)>1)
    warning("we will not consider other parameter")

  order <- check.Ordering(x)
  matrix <- matrix(order)
}
