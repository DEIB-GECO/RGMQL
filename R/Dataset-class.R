DataSet <- function(value)
{
    op_list <- list(value = value)
    ## Set the name for the class
    class(op_list) <- "DataSet"
    return(op_list)
}