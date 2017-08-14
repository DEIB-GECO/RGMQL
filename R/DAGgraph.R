DAGgraph <- function(value)
{
  op_list <- list(
    value = value
  )
  ## Set the name for the class
  class(op_list) <- "DAGgraph"
  return(op_list)
}