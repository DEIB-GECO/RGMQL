#COVER methods and variant

flat <- function(minAcc,maxAcc,groupBy = NULL,aggregates = NULL, input_data)
{
  doVariant("FLAT",minAcc,maxAcc,groupBy,aggregates,input_data)
}

cover <- function(minAcc,maxAcc,groupBy = NULL,aggregates = NULL, input_data)
{
  doVariant("COVER",minAcc,maxAcc,groupBy,aggregates,input_data)
}

histogram <- function(minAcc,maxAcc,groupBy = NULL,aggregates = NULL, input_data)
{
  doVariant("HISTOGRAM",minAcc,maxAcc,groupBy,aggregates,input_data)
}

summit <- function(minAcc,maxAcc,groupBy = NULL,aggregates = NULL, input_data)
{
  doVariant("SUMMIT",minAcc,maxAcc,groupBy,aggregates,input_data)
}

doVariant <- function(flag,minAcc,maxAcc,groupBy,aggregates,input_data)
{
  if(!is.numeric(minAcc) || !is.numeric(maxAcc))
    stop("minAcc and maxAcc must be numeric")

  if(minAcc < -1 || maxAcc < -1)
    stop("only 0,-1 or any positive value")

  min = as.integer(minAcc)
  max = as.integer(maxAcc)

  if(!is.character(groupBy) && !is.null(groupBy))
    stop("groupBy can be only null, single string or an array of string")

  if(!is.null(metadata))
  {
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
