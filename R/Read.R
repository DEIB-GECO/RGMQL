#' Init GMQL Server
#'
#' Initialize and run GMQL server for executing GMQL query
#' It is also perform a login to GMQL REST services suite if needed
#' 
#' @importFrom rJava J
#' 
#' @param output_format string identifies the output format of sample files.
#' Can be TAB, GTF or COLLECT
#' \itemize{
#' \item{TAB: tab delimited file format}
#' \item{GTF: file format used to hold information about gene structure.
#' It is a tab-delimited text format based on the general feature format}
#' \item{COLLECT: used for storing output in memory}
#' }
#' @param remote_processing logical value specifying the processing mode.
#' True for processing on cluster (remote), false for local processing.
#' 
#' @param url string url of server: It must contain the server address 
#' and base url; service name is added automatically.
#' If null, no login is performed.
#' You can always perform it, calling the function \code{\link{login_gmql}} 
#' explicitly
#' 
#' @param username string name used during signup
#' @param password string password used during signup
#' 
#' @return None
#'
#' @examples
#'
#' ## initialize GMQL with local processing with sample files output format 
#' ## as Tab delimited
#' library(rJava)
#' init_gmql("tab", FALSE)
#' 
#' \dontrun{
#' 
#' ## initialize GMQL with remote processing
#' remote_url = "http://130.186.13.219/gmql-rest"
#' init_gmql(remote_processing = TRUE, url = remote_url)
#' 
#' }
#' 
#' @export
#'
init_gmql <- function(output_format = "gtf", remote_processing = FALSE, 
                        url = NULL, username = NULL, password = NULL)
{
    out_format <- toupper(output_format)
    if(!identical(out_format,"TAB") && !identical(out_format,"GTF") && 
        !identical(out_format,"COLLECT"))
        stop("output_format must be TAB, GTF or COLLECT")
    .check_logical(remote_processing)
  
    # mettere attesa da input keyboard, controllare se token già esiste 
    # da sessione precedente
    if(!is.null(url) && !exists("authToken",envir = .GlobalEnv))
        login_gmql(url,username,password)
  
    WrappeR <- J("it/polimi/genomics/r/Wrapper")
    WrappeR$initGMQL(out_format,remote_processing)
}

#' GMQL Function: READ
#'
#' Read a GMQL dataset or any other folder containig some homogenus sample
#' from disk, saving in Scala memory that can be referenced in R
#' Also used to read a repository dataset in case of remote processing.
#' 
#' @importFrom rJava .jnull
#' @importFrom rJava .jarray
#' @importFrom methods is
#' @importFrom rJava J
#' 
#' @param dataset folder path for GMQL dataset or datasetname on repository
#' @param parser string used to parsing dataset files
#' The Parser's available are:
#' \itemize{
#' \item{BedParser}
#' \item{ANNParser}
#' \item{BroadProjParser}
#' \item{BedParser}
#' \item{NarrowPeakParser}
#' \item{RnaSeqParser}
#' \item{CustomParser.}
#' }
#' Default is CustomParser.
#' @param is_local logical value indicating local or remote dataset
#' @param is_GMQL logical value indicating if is a GMQL dataset or not 
#' 
#' @return DataSet class object. It contains the value to use as input 
#' for the subsequent GMQL function
#' 
#' @details
#' Normally a GMQL dataset contains an XML schema file that contains
#' name of column header. (e.g chr, start, stop, strand)
#' The CustomParser read this XML schema; 
#' if you already know what kind of schema your files are, use one of the 
#' parser defined without reading any XML schema
#'
#' @examples
#' 
#' ## read local dataset with CustomParser
#' init_gmql()
#' test_path <- system.file("example", "DATASET", package = "RGMQL")
#' r = read_dataset(test_path)
#' 
#' \dontrun{
#' 
#' ## read local dataset with other Parser
#' init_gmql()
#' test_path <- system.file("example", "DATASET", package = "RGMQL")
#' r = read_dataset(test_path,"ANNParser")
#' 
#' ## read remote public dataset stored into GMQL system repository 
#' 
#' r2 = read_dataset("public.HG19_TCGA_dnaseq",is_local = FALSE)
#' 
#' }
#' 
#' @export
#'
read_dataset <- function(dataset, parser = "CustomParser", is_local=TRUE, 
                        is_GMQL=TRUE)
{
    .check_input(dataset)
    .check_logical(is_local)
    .check_logical(is_GMQL)
    WrappeR <- J("it/polimi/genomics/r/Wrapper")
    if(is_local)
    {
        if(!dir.exists(dataset))
            stop("folder does not exist")
    
        dataset <- sub("/*[/]$","",dataset)
        if(basename(dataset) !="files")
            dataset <- paste0(dataset,"/files")
        
        schema_matrix <- .jnull("java/lang/String")
        url <- .jnull("java/lang/String")
    }
    else
    {
        url <- WrappeR$get_url()
        if(is.null(url))
            stop("You have to log on using login function")
    
        if(!exists("authToken",envir = .GlobalEnv))
            stop("You have to log on using login function")
   
        list <- show_schema(url,dataset)
        schema_names <- sapply(list$fields, function(x){x$name})
        schema_type <- sapply(list$fields, function(x){x$type})
        schema_matrix <- cbind(schema_type,schema_names)
        #schema_type <- list$type
    
        if(is.null(schema_matrix) || length(schema_matrix)==0)
            schema_matrix <- .jnull("java/lang/String")
        else
            schema_matrix <- .jarray(schema_matrix, dispatch = TRUE)
    }

    parser_name <- .check_parser(parser)
    response <- WrappeR$readDataset(dataset,parser_name, is_local, is_GMQL, 
                                    schema_matrix)
    error <- strtoi(response[1])
    data <- response[2]
    if(error!=0)
        stop(data)
    else
        DataSet(data)
}

#' GMQL Function: READ
#'
#' Read a GrangesList saving in scala memory that can be referenced in R
#'
#' @importFrom S4Vectors metadata
#' @importFrom rJava J
#' @importFrom rJava .jarray
#' 
#' @param samples GrangesList
#' 
#' @return DataSet class object. It contains the value to use as input 
#' for the subsequent GMQL function
#' 
#' @examples
#' 
#' library("GenomicRanges")
#' gr1 <- GRanges(seqnames = "chr2", ranges = IRanges(103, 106),
#' strand = "+", score = 5L, GC = 0.45)
#' gr2 <- GRanges(seqnames = c("chr1", "chr1"), ranges = IRanges(c(107, 113), 
#' width = 3), strand = c("+", "-"), score = 3:4, GC = c(0.3, 0.5))
#' 
#' grl <- GRangesList("txA" = gr1, "txB" = gr2)
#' 
#' data_out <- read(grl)
#'
#' @export
#'
read <- function(samples)
{
    if(!is(samples,"GRangesList"))
        stop("only GrangesList")

    meta <- S4Vectors::metadata(samples)
    if(is.null(meta) || length(meta)==0) {
        #repeat meta for each sample in samples list
        len <- length(samples)
        warning("GrangesList has no metadata.
We provide two metadata for you")
        index_meta <- rep(1:len,each = len)
        rep_meta <- rep(c("Provider","Polimi", "Application", "R-GMQL"),
                        times=len)
        meta_matrix <- matrix(rep_meta,ncol = 2,byrow = TRUE)
        meta_matrix <- cbind(index_meta,meta_matrix)
    }
    else {
        unlist_meta <- unlist(meta)
        names_meta <- names(unlist_meta)
        group_names <- gsub(".*_([0-9]*)\\..*","\\1", names_meta)
        names(unlist_meta) <- NULL
        meta_matrix <- cbind(group_names,names_meta,unlist_meta)
    }
    df <- data.frame(samples)
    df <- df[-2] #delete group_name
    region_matrix <- as.matrix(sapply(df, as.character))
    region_matrix[is.na(region_matrix)] <- "NA"
    region_matrix <- region_matrix[,setdiff(colnames(region_matrix),"width")]
    col_types <- sapply(df,class)
    col_names <- names(col_types)
    #re order the schema?
    if("phase" %in% col_names) # if GTF, change
    {
        col_names <- plyr::revalue(col_names,c(type = "feature", 
                                        phase = "frame", seqnames = "seqname"))
        schema_matrix <- cbind(toupper(col_types),col_names)
        schema_matrix<- schema_matrix[setdiff(rownames(schema_matrix),
                                        c("group","width")),]
    }
    else
    {
        col_names <- plyr::revalue(col_names,c(start = "left", 
                                        end = "right", seqnames = "chr"))
        schema_matrix <- cbind(toupper(col_types),col_names)
        schema_matrix<- schema_matrix[setdiff(rownames(schema_matrix),
                                        c("group","width")),]
    }
    rownames(schema_matrix) <- NULL
    colnames(schema_matrix) <- NULL
  
    schema_matrix <- .jarray(schema_matrix,dispatch = TRUE)
    meta_matrix <- .jarray(meta_matrix,dispatch = TRUE)
    region_matrix <- .jarray(region_matrix,dispatch = TRUE)
  
    WrappeR <- J("it/polimi/genomics/r/Wrapper")
    response <- WrappeR$read(meta_matrix,region_matrix,schema_matrix)
    DataSet(response)
}


.check_parser <- function(parser)
{
    parser <- toupper(parser)
    if(!identical(parser,"BEDPARSER") && !identical(parser,"ANNPARSER") &&
        !identical(parser,"BROADPROJPARSER") && !identical(parser,"BASICPARSER") 
        && !identical(parser,"NARROWPEAKPARSER") && 
        !identical(parser,"RNASEQPARSER") && !identical(parser,"CUSTOMPARSER"))
        stop("parser not defined")
    
    parser
}

#' Disable or Enable remote processing
#'
#' It allows to enable or disable remote processing
#' 
#' @details 
#' The invocation of this function allow to change mode of processing.
#' after materialization is not possbile to switch the processing mode, 
#' 
#' @importFrom rJava J
#' 
#' @param is_remote logical value used in order to set the processing mode.
#' TRUE you will set a remote query processing mode otherwise will be local,
#' 
#' @return None
#' 
#' @examples
#' 
#' # initialize with remote processing off
#' init_gmql("tab",remote_processing=FALSE)
#' 
#' # change processing mode to remote
#' remote_processing(TRUE)
#'
#' @export
#'
remote_processing<-function(is_remote)
{
    WrappeR <- J("it/polimi/genomics/r/Wrapper")
    .check_logical(is_remote)
    response <- WrappeR$remote_processing(is_remote)
    print(response)
}


