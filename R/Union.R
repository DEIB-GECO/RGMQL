#' GMQL Operation: UNION
#'
#' It is used to integrate homogeneous or heterogeneous samples of two datasets within a single dataset
#' for each sample of either input dataset, a result sample is created as follows:
#' \itemize{
#' \item Metadata are the same as in the original sample.
#' \item Resulting schema is obtained by projecting the schema of the right dataset over the schema of the left one
#' (more properly, it will be performed by adding to the schema of the left dataset the region attributes of the right dataset
#' which are not identical to those of the left dataset)
#' \item Regions are the same (in coordinates and attribute values) as in the original sample.
#' Region attributes which are missing in an input dataset sample w.r.t. the merged schema are set to null.
#' }
#' For what concerns metadata, attributes of samples from the left (right) input dataset are prefixed
#' with the strings LEFT (RIGHT), so as to trace the dataset to which they originally belonged.
#'
#' @param left_input_data returned object from any GMQL function
#' @param right_input_data returned object from any GMQL function
#'
#' @return DAGgraph class object. It contains the value associated to the graph used 
#' as input for the subsequent GMQL function
#'
#'
#' @references \url{http://www.bioinformatics.deib.polimi.it/genomic_computing/GMQL/doc/GMQLUserTutorial.pdf}
#'
#' @examples
#' ## it creates a dataset called full which contains all samples from the datasets 
#' ## data1 and data2 whose schema is defined by merging data1 and data2 dataset schemas 
#' ## (union of all the attributes present in the two input datasets).
#' 
#' initGMQL("gtf")
#' test_path <- system.file("example","DATA_SET_VAR_GTF",package = "GMQL")
#' test_path2 <- system.file("example","DATA_SET_VAR_GDM",package = "GMQL")
#' data1 = readDataset(test_path)
#' data2 = readDataset(test_path2)
#' full = union(data1,data2)
#' 
#'
#' @export
#'
union <- function(left_input_data,right_input_data)
{
  response <- WrappeR$union(right_input_data$value,left_input_data$value)
  error <- strtoi(response[1])
  data <- response[2]
  if(error!=0)
    stop(data)
  else
    DAGgraph(data)
}
