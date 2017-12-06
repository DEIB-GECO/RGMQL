#' GMQL Function: EXECUTE
#'
#' Execute GMQL query.
#' The function works only after invoking at least one collect
#' 
#' @importFrom rJava J
#' 
#' @return None
#'
#' @examples
#'
#' init_gmql()
#' test_path <- system.file("example","DATASET",package = "RGMQL")
#' rd = read_dataset(test_path)
#' filtered = filter(rd)
#' aggr = aggregate(filtered, list(DF("antibody_targer","cell_karyotype")))
#' collect(aggr, dir_out = test_path)
#' 
#' \dontrun{
#' execute()
#' }
#' @export
#'
execute <- function()
{
    WrappeR <- J("it/polimi/genomics/r/Wrapper")
    remote_proc <- WrappeR$is_remote_processing()
    if(!remote_proc)
        .download_or_upload()
    
    response <- WrappeR$execute()
    error <- strtoi(response[1])
    data <- response[2]
    if(error!=0)
        stop(data)
    else
    {
        if(remote_proc)
        {
            url <- WrappeR$get_url()
            .download_or_upload()
            res <- serialize_query(url,FALSE,data)
        }
    }
}

.download_or_upload <- function()
{
    WrappeR <- J("it/polimi/genomics/r/Wrapper")
    data <- .jevalArray(WrappeR$get_dataset_list(),simplify = TRUE)
    data_list <- apply(data, 1, as.list)
    url <- WrappeR$get_url()
    remote <- WrappeR$is_remote_processing()
    if(remote)
    {
        sapply(data_list,function(x){
            if(!is.null(x[[1]]) && !is.na(x[[1]]))
                upload_dataset(url,x[[2]],x[[1]],x[[3]],FALSE)})
    }
    else
    {
        sapply(data_list,function(x){
            if(!is.null(x[[2]]) && !is.na(x[[2]]))
                download_dataset(url,x[[2]],x[[1]])})
    }
}

collect.GMQLDataset <- function(x, dir_out = getwd(), name = "ds1")
{
    ptr_data <- x@value
    gmql_materialize(ptr_data, dir_out, name)
}


#' Method collect
#'
#' @description Wrapper to GMQL MATERIALIZE operator
#' 
#' @description It saves the content of a dataset that contains samples 
#' metadata and regions. It is normally used to persist the content of any 
#' dataset generated during a GMQL query.
#' Any dataset can be materialized, but the operation can be time-consuming.
#' For best performance, materialize the relevant data only.
#'
#' @importFrom rJava J
#' @importFrom dplyr collect
#' 
#' @param x GMQLDataset class object
#' @param dir_out destination folder path.
#' by default it is the current working directory of the R process
#' @param name name of the result dataset. by default it is the string "ds1"
#' 
#' @details 
#' 
#' An error occures if the directory already exist at the destination
#' folder path
#' 
#' @return None
#'
#' @examples
#' 
#' ## This statement initializes and runs the GMQL server for local execution 
#' ## and creation of results on disk. Then, with system.file() it defines 
#' ## the path to the folder "DATASET" in the subdirectory "example"
#' ## of the package "RGMQL" and opens such file as a GMQL dataset named 
#' ## "data" using customParser
#'
#' init_gmql()
#' test_path <- system.file("example", "DATASET", package = "RGMQL")
#' data = read_dataset(test_path)
#' 
#' ## The following statement materialize the dataset, previoulsy read, at 
#' ## th specific destination path into local folder "ds1" opportunely created
#' 
#' collect(data, dir_out = test_path)
#' 
#' @name collect
#' @rdname collect
#' @aliases collect,GMQLDataset-method
#' @aliases collect-method
#' @export
setMethod("collect", "GMQLDataset",collect.GMQLDataset)

gmql_materialize <- function(input_data, dir_out, name)
{
    WrappeR <- J("it/polimi/genomics/r/Wrapper")
    remote_proc <- WrappeR$is_remote_processing()
    if(!remote_proc)
    {
        dir_out <- sub("/*[/]$","",dir_out)
        res_dir_out <- paste0(dir_out,"/",name)
        if(!dir.exists(res_dir_out))
            dir.create(res_dir_out)
    }
    else
        res_dir_out <- dir_out
    
    response <- WrappeR$materialize(input_data, res_dir_out)
    error <- strtoi(response[1])
    data <- response[2]
    if(error!=0)
        stop(data)
    else
        invisible(NULL)
}


#' Method take
#'
#' It saves the contents of a dataset that contains samples metadata 
#' and samples regions as GrangesList.
#' It is normally used to store in memory the contents of any dataset 
#' generated during a GMQL query. The operation can be very time-consuming.
#' If you have invoked any materialization before take function, 
#' all those dataset will be materialized as folder.
#'
#' @import GenomicRanges
#' @importFrom stats setNames
#' @importFrom rJava J
#' @importFrom rJava .jevalArray
#' 
#' @param data returned object from any GMQL function
#' @param rows number of rows for each sample regions that you want to 
#' retrieve and stored in memory.
#' by default is 0 that means take all rows for each sample
#' 
#' @param ... Additional arguments for use in specific methods
#' 
#' @return GrangesList with associated metadata
#'
#' @examples
#'
#' init_gmql()
#' test_path <- system.file("example", "DATASET", package = "RGMQL")
#' rd = read_dataset(test_path)
#' aggr = aggregate(rd, list(DF("antibody_target", "cell_karyotype")))
#' taken <- take(aggr, rows = 45)
#' 
#' @name take
#' @rdname take
#' @aliases take-method
#' @export
setMethod("take", "GMQLDataset",
            function(data, rows = 0L)
            {
                ptr_data <- data@value
                gmql_take(ptr_data, rows)
            })

gmql_take <- function(input_data, rows = 0L)
{
    rows <- as.integer(rows[1])
    if(rows<0)
        stop("rows cannot be negative")
    
    WrappeR <- J("it/polimi/genomics/r/Wrapper")
    response <- WrappeR$take(input_data, rows)
    error <- strtoi(response[1])
    data <- response[2]
    if(error!=0)
        stop(data)
    
    reg <- .jevalArray(WrappeR$get_reg(),simplify = TRUE)
    if(is.null(reg))
        stop("no regions defined")
    meta <- .jevalArray(WrappeR$get_meta(),simplify = TRUE)
    if(is.null(meta))
        stop("no metadata defined")
    schema <- .jevalArray(WrappeR$get_schema(),simplify = TRUE)
    if(is.null(schema))
        stop("no schema defined")
    
    reg_data_frame <- as.data.frame(reg)
    list <- split(reg_data_frame, reg_data_frame[1])
    names <- c("seqname","start","end","strand",schema)
    
    sampleList <- lapply(list, function(x){
        x <- x[-1]
        names(x) <- names
        g <- GenomicRanges::makeGRangesFromDataFrame(x,
                                    keep.extra.columns = TRUE,
                                    start.field = "start",
                                    end.field = "end")
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
    name_value_list <- lapply(list, function(x){x <- x[-1]})
    meta_list <- lapply(name_value_list, function(x){
        stats::setNames(as.list(as.character(x[[2]])), x[[1]])
    })
}



