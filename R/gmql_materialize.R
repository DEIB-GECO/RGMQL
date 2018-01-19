#' GMQL Function: EXECUTE
#'
#' It executes GMQL query.
#' The function works only after invoking at least one collect
#' 
#' @importFrom rJava J
#' 
#' @return None
#'
#' @examples
#' ## This statement initializes and runs the GMQL server for local execution 
#' ## and creation of results on disk. Then, with system.file() it defines 
#' ## the path to the folder "DATASET" in the subdirectory "example" 
#' ## of the package "RGMQL" and opens such folder as a GMQL dataset 
#' ## named "data"
#' 
#' init_gmql()
#' test_path <- system.file("example", "DATASET", package = "RGMQL")
#' data = read_gmql(test_path)
#' 
#' ## The following statement materializes the dataset "data", previoulsy read, 
#' ## at the specific destination test_path into local folder "ds1" opportunely 
#' ## created
#' 
#' collect(data, dir_out = test_path)
#' 
#' ## This statement executes GMQL query.
#' \dontrun{
#' 
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
    val <- response[2]
    if(error)
        stop(val)
    else
    {
        if(remote_proc)
        {
            url <- WrappeR$get_url()
            .download_or_upload()
            res <- serialize_query(url,FALSE,val)
        }
    }
}

.download_or_upload <- function()
{
    WrappeR <- J("it/polimi/genomics/r/Wrapper")
    datasets <- .jevalArray(WrappeR$get_dataset_list(),simplify = TRUE)
    data_list <- apply(datasets, 1, as.list)
    url <- WrappeR$get_url()
    remote <- WrappeR$is_remote_processing()
    if(remote)
    {
        lapply(data_list,function(x){
            if(!is.null(x[[1]]) && !is.na(x[[1]]))
                upload_dataset(url,x[[2]],x[[1]],x[[3]],FALSE)})
    }
    else
    {
        lapply(data_list,function(x){
            if(!is.null(x[[2]]) && !is.na(x[[2]]))
                download_dataset(url,x[[2]],x[[1]])})
    }
}

collect.GMQLDataset <- function(x, dir_out = getwd(), name = "ds1")
{
    ptr_data <- value(x)
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
#' @param dir_out destination folder path. By default it is the current 
#' working directory of the R process
#' @param name name of the result dataset. By default it is the string "ds1"
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
#' ## "data" using CustomParser
#'
#' init_gmql()
#' test_path <- system.file("example", "DATASET", package = "RGMQL")
#' data = read_gmql(test_path)
#' 
#' ## The following statement materializes the dataset 'data', previoulsy read, 
#' ## at the specific destination test_path into local folder "ds1" opportunely 
#' ## created
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
        res_dir_out <- file.path(dir_out,name)
        if(!dir.exists(res_dir_out))
            dir.create(res_dir_out)
    }
    else
        res_dir_out <- dir_out
    
    response <- WrappeR$materialize(input_data, res_dir_out)
    error <- strtoi(response[1])
    val <- response[2]
    if(error)
        stop(val)
    else
        invisible(NULL)
}


#' Method take
#'
#' It saves the content of a dataset that contains samples metadata 
#' and regions as GRangesList.
#' It is normally used to store in memory the content of any dataset 
#' generated during a GMQL query. The operation can be very time-consuming.
#' If you invoked any materialization before take function, 
#' all those datasets are materialized as folders.
#'
#' @importFrom GenomicRanges makeGRangesFromDataFrame
#' @importFrom S4Vectors metadata
#' @importFrom stats setNames
#' @importFrom rJava J .jevalArray
#' @importFrom GenomicRanges GRangesList
#' 
#' @param .data returned object from any GMQL function
#' @param rows number of regions rows for each sample that you want to 
#' retrieve and store in memory.
#' By default it is 0, that means take all rows for each sample
#' 
#' @param ... Additional arguments for use in other specific methods of the 
#' generic take function
#' 
#' @return GRangesList with associated metadata
#'
#' @examples
#' ## This statement initializes and runs the GMQL server for local execution 
#' ## and creation of results on disk. Then, with system.file() it defines 
#' ## the path to the folder "DATASET" in the subdirectory "example"
#' ## of the package "RGMQL" and opens such folder as a GMQL dataset 
#' ## named "rd" using CustomParser
#' 
#' init_gmql()
#' test_path <- system.file("example", "DATASET", package = "RGMQL")
#' rd = read_gmql(test_path)
#' 
#' ## This statement creates a dataset called 'aggr' which contains one 
#' ## sample for each antibody_target and cell value found within the metadata 
#' ## of the 'rd' dataset sample; each created sample contains all regions 
#' ## from all 'rd' samples with a specific value for their 
#' ## antibody_target and cell metadata attributes.
#'  
#' aggr = aggregate(rd, conds(c("antibody_target", "cell")))
#' 
#' ## This statement performs the query and returns the resulted dataset as 
#' ## GRangesList named 'taken'. It returns only the first 45 regions of 
#' ## each sample present into GRangesList and all the medatata associated 
#' ## with each sample
#' 
#' taken <- take(aggr, rows = 45)
#' 
#' @name take
#' @rdname take
#' @aliases take-method
#' @export
setMethod("take", "GMQLDataset",
            function(.data, rows = 0L)
            {
                ptr_data <- value(.data)
                gmql_take(ptr_data, rows)
            })

gmql_take <- function(input_data, rows)
{
    rows <- as.integer(rows[1])
    if(rows<0)
        stop("rows cannot be negative")
    
    WrappeR <- J("it/polimi/genomics/r/Wrapper")
    response <- WrappeR$take(input_data, rows)
    error <- strtoi(response[1])
    data <- response[2]
    if(error)
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
    seq_name <- c("seqname","start","end","strand",schema)
    
    sampleList <- lapply(list, function(x){
        x <- x[-1]
        names(x) <- seq_name
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



