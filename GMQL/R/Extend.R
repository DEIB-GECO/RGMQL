#' GMQL Operation: EXTEND
#'
#'It generates new metadata attributes Am as result of aggregate
#'functions g applied to region attributes;
#'aggregate functions are applied sample by sample,
#'resulting tuples are triples with the sample identifier,
#'the attribute name Am, and the computed aggregate value.
#'
#'
#' @param metadata a list of element key = value, where value is c("aggregate function","value")
#' @param input_data string pointer returned from all GMQL function
#' @examples
#' r = read(path)
#' e = extend(metadata = list(regionsCount = c("COUNT","")),input_data = r)
#' e = extend(metadata = list(sumValue = c("SUM","pvalue")),input_data = r)
#' e = extena(input_data = r)
#'
extend <-function(metadata = NULL, input_data)
{
  if(!is.null(metadata)) {
    if(!is.list(metadata))
      stop("metadata must be a list")

    names <- names(metadata)
    if(is.null(names)) {
      warning("you did not assign a names to a list.\nWe use the same name of values used to perform the aggregates function")
      names <- sapply(metadata, function(x) {
        func <- check.AggregatesFunction(x[1])
        if(func=="COUNT")
          names <- "count"
        else
          names <- x[[2]]
      })
    }
    else {
      if(!all(sapply(names, function(x) {
        if(x=="") F else T})))
      {
        stop("no partial names assignment to list")
      }
    }
    aggregate_matrix <- t(sapply(metadata, function(x){

      if(length(x)>2)
        warning("must be a vector of only two element, we will not consider other parameter")

      func <- check.AggregatesFunction(x[1])
      if(x[2] == "")
        stop("second parameter must be defined")

      if(length(x)<2 && func!="COUNT")
        stop("except COUNT, the other aggregates function must have the value")
      if(func=="COUNT")
        x = c(x[1],"")

      new_value = c(x[1],x[2])
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
