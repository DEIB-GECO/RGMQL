#' Method union
#' 
#' @description Wrapper to GMQL UNION operator
#' 
#' @description It is used to integrate samples of two datasets homogeneous or 
#' heterogeneous within a single dataset; for each sample of either input 
#' dataset, a result sample is created as follows:
#' \itemize{
#' \item {Metadata are the same as in the original sample.}
#' \item {Resulting schema is the schema of the left input dataset. }
#' \item {Regions are the same (in coordinates and attribute values) 
#' as in the original sample, if it is from the left input dataset; 
#' if it is from the right input dataset, its regions are the same in 
#' coordinates, but only region attributes identical (in name and type) to 
#' those of the left  input dataset are retained, with the same values.
#' Region attributes which are missing in an input dataset sample 
#' w.r.t. the merged schema are set to null.}
#' }
#' 
#' @importFrom rJava J
#' @importFrom BiocGenerics union
#' 
#' @param x GMQLDataset object
#' @param y GMQLDataset object 
#'
#' @return GMQLDataset object. It contains the value to use as input 
#' for the subsequent GMQLDataset method
#'
#' @examples
#' 
#' ## Thi statement initializes and runs the GMQL server for local execution 
#' ## and creation of results on disk. Then, with system.file() it defines 
#' ## the path to the folders "DATASET" and "DATASET_GDM" in the subdirectory 
#' ## "example" of the package "RGMQL" and opens such folder as a GMQL 
#' ## dataset named "data1" and "data2" respectively using customParser
#' 
#' init_gmql()
#' test_path <- system.file("example", "DATASET", package = "RGMQL")
#' test_path2 <- system.file("example", "DATASET_GDM", package = "RGMQL")
#' data1 <- read_gmql(test_path)
#' data2 <- read_gmql(test_path2)
#' 
#' ## This statement creates a dataset called 'full' which contains all samples 
#' ## from the datasets 'data1' and 'data2'
#' 
#' res <- union(data1, data2)
#' 
#' 
#' @name union
#' @aliases union,GMQLDataset,GMQLDataset-method
#' @aliases union-method
#' @export
setMethod("union", c("GMQLDataset","GMQLDataset"),
            function(x, y)
            {
                ptr_data_x = value(x)
                ptr_data_y = value(y)
                gmql_union(ptr_data_x, ptr_data_y)
            })

gmql_union <- function(left_data, right_data)
{
    WrappeR <- J("it/polimi/genomics/r/Wrapper")
    response <- WrappeR$union(left_data, right_data)
    error <- strtoi(response[1])
    val <- response[2]
    if(error)
        stop(val)
    else
        GMQLDataset(val)
}
