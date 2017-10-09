#' GMQL Function: EXECUTE
#'
#' execute GMQL query.
#' The function works only after invoking at least one materialize
#'
#' @details 
#' 
#' After invoking execution function, all varialbe associated to DAG will be removed
#' from scala enviroment, although the associated R variable will remain stored in R environment
#' 
#' @return None
#'
#' @examples
#'
#' initGMQL("gtf")
#' test_path <- system.file("example","DATA_SET_VAR_GTF",package = "GMQL")
#' r = readDataset(test_path)
#' s = select(input_data = r)
#' m = merge(groupBy = c("antibody_targer","cell_karyotype"),input_data = s)
#' materialize(input_data = m, dir_out = test_path)
#' 
#' \dontrun{
#' execute()
#' }
#' @export
#'
execute <- function()
{
  remote_proc <- WrappeR$is_remote_processing()
  remote_array <- WrappeR$datasetQueueRemote()
  if(remote_proc)
  {
    local_array <- WrappeR$datasetQueueLocal()
    sapply(local_array,function(x)
      {
      #name_dataset <- basename(x)
      #uploadSamples(url,name_dataset,x,isGMQL = TRUE)
    })
  }
  else
  {
    remote_array <- WrappeR$datasetQueueLocal()
    sapply(remote_array,function(x)
    {
      #name_dataset <- basename(x)
      #downloadDataset(url,name_dataset,path = getwd())
    })
  }
  out <- WrappeR$execute()
  if(grepl("OK",out,ignore.case = TRUE))
    print("Executed")
  else
    stop(out)
}

#' GMQL Operation: MATERIALIZE
#'
#' It saves the contents of a dataset that contains samples metadata and samples regions.
#' It is normally used to persist the contents of any dataset generated during a GMQL query.
#' Any dataset can be materialized, but the operation can be very time-consuming.
#' For best performance, materialize the relevant data only.
#'
#'
#' @param input_data returned object from any GMQL function
#' @param dir_out destination folder path.
#' by default is current working directory of the R process
#'
#' @return None
#'
#' @references \url{http://www.bioinformatics.deib.polimi.it/genomic_computing/GMQL/doc/GMQLUserTutorial.pdf}
#'
#' @examples
#'
#' initGMQL("gtf")
#' test_path <- system.file("example","DATA_SET_VAR_GTF",package = "GMQL")
#' r = readDataset(test_path)
#' s = select(input_data = r)
#' m = merge(groupBy = c("antibody_targer","cell_karyotype"),input_data = s)
#' materialize(input_data = m, dir_out = test_path)
#' 
#' @export
#'
materialize <- function(input_data, dir_out = getwd())
{
  out <- WrappeR$materialize(input_data$value,dir_out)
  if(grepl("No",out,ignore.case = TRUE))
    stop(out)
  else
  {
    invisible(NULL)
  }
}

#' GMQL Operation: TAKE
#'
#' It saves the contents of a dataset that contains samples metadata and samples regions.
#' It is normally used to store in memoery the contents of any dataset generated during a GMQL query.
#' the operation can be very time-consuming.
#' If you have invoked any materialization before take function, all those dataset will be materialized 
#' as folder (like if execution was invoked)
#'
#' @import GenomicRanges
#' @importFrom stats setNames
#' 
#' @param input_data returned object from any GMQL function
#' @param rows number of rows for each sample regions that you want to retrieve and stored in memory
#' by default is 0 that means take all rows for each sample
#'
#' @return GrangesList with associated metadata
#'
#' @examples
#'
#' initGMQL("gtf")
#' test_path <- system.file("example","DATA_SET_VAR_GTF",package = "GMQL")
#' r = readDataset(test_path)
#' s = select(input_data = r)
#' m = merge(groupBy = c("antibody_targer","cell_karyotype"),input_data = s)
#' g <- take(input_data = m, rows = 45)
#' 
#' @export
#'
take <- function(input_data, rows=0L)
{
  rows <- as.integer(rows[1])
  if(rows<0)
    stop("rows cannot be negative")

  out <- WrappeR$take(input_data$value,rows)
  if(grepl("No",out,ignore.case = TRUE))
    stop(out)

  reg <- WrappeR$get_reg()
  meta <- WrappeR$get_meta()
  schema <- WrappeR$get_schema()

  reg_data_frame <- as.data.frame(reg)
  list <- split(reg_data_frame, reg_data_frame[1])
  names <- c("seqname","start","end","strand",schema)
  
  sampleList <- lapply(list, function(x){
    x <- x[-1]
    names(x) <- names
    g <- GenomicRanges::makeGRangesFromDataFrame(x,keep.extra.columns = TRUE,
                                                 start.field = "start",end.field = "end")
  })
  gRange_list <- GRangesList(sampleList)
  
  meta_list <- .metadata_from_frame_to_list(meta)
  
  S4Vectors::metadata(gRange_list) <- meta_list
  return(gRange_list)
}

.metadata_from_frame_to_list <- function(metadata_frame)
{
  meta_frame <- as.data.frame(metadata_frame)
  list <- split(meta_frame, meta_frame[1])
  name_value_list <- lapply(list, function(x){
    x <- x[-1]
  })
  meta_list <- lapply(name_value_list, function(x){
    stats::setNames(as.list(as.character(x[[2]])), x[[1]])
  })
}



