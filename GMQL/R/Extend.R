#' GMQL Operation: EXTEND
#'
#' It generates new metadata attributes as result of aggregate functions applied to sample region attributes
#' and adds them to the existing metadata of the sample
#' aggregate functions are applied sample by sample.
#'
#'
#' @param metadata a list of element key = value. value is an object of class OPERATOR.
#' The functions aggregate available for extend function are: MIN,MAX,SUM,BAG,AVG,COUNT.
#' Every operator accept a string value. only COUNT cannot have a value
#' The key of list is mandatory; if all missed we create that based on function you choose
#'
#'
#' @param input_data url-like "string" pointer returned from GMQL function
#' @examples
#'
#' startGMQL()
#' path = /.../dataset_name
#' r = read(path)
#' e = extend(somma = SUM("pvalue"),c = COUNT(), m = AVG("score"),input_data = r)
#'
extend <-function(metadata = NULL, input_data)
{
  if(!is.null(metadata))
  {
    if(!is.list(metadata))
      stop("metadata must be a list")

    if(!all(sapply(metadata, function(x) is(x,"META_OPERATOR") )))
    {
      stop("you must use OPERATOR object for defining aggregates function")
    }

    names <- names(metadata)
    if(is.null(names))
    {
      warning("you did not assign a names to a list.\nWe build names for you")
      names <- sapply(metadata, function(x) {
        take_value.META_OPERATOR(x)
      })
    }
    else {
      if(all(sapply(names, function(x) (x==""))))
      {
        stop("no partial names assignment to list")
      }
    }
    aggregate_matrix <- t(sapply(metadata, function(x) {

      new_value = as.character(x)
      matrix <- matrix(new_value)

    }))
    m_names <- matrix(names)
    metadata_matrix <- cbind(m_names,aggregate_matrix)
  }
  else
    metadata_matrix <- NULL

  out <- frappeR$extend(metadata_matrix,input_data)

  if(grepl("No",out,ignore.case = T))
    stop(out)
  else
    out
}
