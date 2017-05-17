#'GMQL operation: ORDER
#'
#'
#'It orders either samples, or regions, or both of them; order is ascending and or descending.
#'Sorted samples or regions have a new attribute Order, added to either metadata, or regions,
#'or both of them; the value of Order reflects the result of the sorting.
#'The input mtop = k extracts the first k samples or regions, the clause mtopg = k implicitly
#'considers the grouping by identical values of the first n − 1 ordering attributes
#'and then selects the first k samples or regions of each group
#'
#'
#'@param metadata_order aaa
#'@param mtop 0 is default meand that we not use it
#'@param mtopg 0 is default meand that we not use it
#'@param regions_order aaa
#'@param rtop 0 is default meand that we not use it
#'@param rtopg 0 is default meand that we not use it
#'@param input_data aaaa
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

