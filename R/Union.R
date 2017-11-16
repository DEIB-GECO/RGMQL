#' GMQL Operation: UNION
#'
#' It is used to integrate homogeneous or heterogeneous samples of two datasets 
#' within a single dataset; for each sample of either input dataset, 
#' a result sample is created as follows:
#' \itemize{
#' \item {Metadata are the same as in the original sample.}
#' \item {Resulting schema is obtained by projecting the schema 
#' of the right dataset over the schema of the left one
#' (more properly, it will be performed by adding to the schema of the 
#' left dataset the region attributes of the right dataset which are not 
#' identical to those of the left dataset)}
#' \item {Regions are the same (in coordinates and attribute values) 
#' as in the original sample.
#' Region attributes which are missing in an input dataset sample 
#' w.r.t. the merged schema are set to null.}
#' }
#' For what concerns metadata, attributes of samples from the left (right) 
#' input dataset are prefixed with the strings LEFT (RIGHT), so as to trace 
#' the dataset to which they originally belonged.
#' 
#' @importFrom rJava J
#' 
#' @param x GMQLDataset class object
#' @param y GMQLDataset class object 
#'
#' @return GMQLDataset class object. It contains the value to use as input 
#' for the subsequent GMQL function
#'
#' @examples
#' 
#' ## It creates a dataset called full which contains all samples from the 
#' ## datasets data1 and data2 whose schema is defined by merging the two 
#' ## dataset schemas.
#' ## (union of all the attributes present in the two input datasets).
#' 
#' init_gmql()
#' test_path <- system.file("example", "DATASET", package = "RGMQL")
#' test_path2 <- system.file("example", "DATASET_GDM", package = "RGMQL")
#' data1 <- read_dataset(test_path)
#' data2 <- read_dataset(test_path2)
#' 
#' res <- union(data1, data2)
#' 
#' @rdname union-GMQLDataset-method
#' @aliases union, union-method, 
#' @export 
#' 
setMethod("union", c("GMQLDataset","GMQLDataset"),
            function(x, y)
            {
                val_x = x@value
                val_y = y@value
                gmql_union(val_x, val_y)
            })

gmql_union <- function(left_data, right_data)
{
    WrappeR <- J("it/polimi/genomics/r/Wrapper")
    response <- WrappeR$union(left_data,right_data)
    error <- strtoi(response[1])
    data <- response[2]
    if(error!=0)
        stop(data)
    else
        GMQLDataset(data)
}
