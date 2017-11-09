#' FILTER AND EXTRACT 
#'
#' This function let user to create a new GRangeslist with fixed information: 
#' seqnames,ranges ans strand and a variable part made up by the regions 
#' defined as input.
#' The metadata and metadata_prefix are used to filter the data and choose 
#' only the samples that match at least one metdatata with its prefix.
#' The regions are shown for each sample obtained from filtering.
#'
#' @import xml2
#' @importFrom dplyr bind_cols
#' @importFrom data.table fread
#' @importFrom rtracklayer import
#'
#' @param data string GMQL dataset folder path or GrangesList 
#' object
#' @param metadata vector of character containing names of metadata 
#' to be searched for in metadata files.
#' data will be extracted if at least one condition is satisfied:
#' this condition will be logically "ANDed" with prefix filtering (see below)
#' if NULL no filtering action occured 
#' (i.e every sample will be taken for regions filtering)
#' @param metadata_prefix vector of character that will filter metadata 
#' containing rispectively every element of this vector.
#' number of elelment in both vector must match
#' @param regions vector of character that will extract only region 
#' attribute specified; if NULL no regions will be taken and the output 
#' will be only GRanges made up by the first attribute
#' (seqnames,start,end,strand)
#'
#' 
#' @details 
#' This function works only with datatset or Grangeslist that have the same 
#' information about regions attribute (but of course different value)
#' 
#' @return granges with selected regions (if any) in elementMetadata
#'
#' @examples
#'
#' test_path <- system.file("example", "DATASET", package = "RGMQL")
#' filter_and_extract(test_path,regions = c("pvalue", "peak"))
#' 
#' grl <- importGMQL.gtf(test_path)
#' filter_and_extract(grl, regions = c("pvalue", "peak"))
#'
#'
#' @export
#'
filter_and_extract <- function(data, metadata = NULL, 
                               metadata_prefix = NULL, regions = NULL)
{
    if(is(data,"GRangesList"))
        .extract_from_GRangesList(data,metadata,metadata_prefix,regions)
    else
        .extract_from_dataset(data,metadata,metadata_prefix,regions)
}




.extract_from_dataset <- function(datasetName, metadata = NULL, 
                        metadata_prefix = NULL, regions = NULL)
{
    datasetName <- sub("/*[/]$","",datasetName)
    if(basename(datasetName) !="files")
        datasetName <- paste0(datasetName,"/files")
    
    if(!dir.exists(datasetName))
        stop("Directory does not exists")

    gdm_meta_files <- list.files(datasetName, pattern = "*.gdm.meta$",
                                    full.names = TRUE)
    gtf_meta_files <- list.files(datasetName, pattern = "*.gtf.meta$",
                                    full.names = TRUE)

    if(length(gdm_meta_files)==0 && length(gtf_meta_files)==0)
        stop("no samples present or no files format supported")

    if(length(gdm_meta_files)>=1 && length(gtf_meta_files)>=1)
        stop("GMQL dataset cannot be mixed dataset: no GTF and GDM together")

    vector_field <- .schema_header(datasetName)

    if(length(gdm_meta_files)>0)
    {
        samples_file <- .check_metadata_files(metadata,metadata_prefix,
                                                gdm_meta_files)
        samples_to_read <- unlist(samples_file)
        if(length(samples_to_read)>0)
            samples_to_read <- gsub(".meta$", "", samples_to_read)
        else
            samples_to_read <- gsub(".meta$", "", gdm_meta_files)

        granges <- .parse_gdm_files(vector_field,samples_to_read,regions)
    }
    else
    {
        samples_file <- .check_metadata_files(metadata,metadata_prefix,
                                                gtf_meta_files)
        samples_to_read <- unlist(samples_file)
        if(length(samples_to_read)>0)
            samples_to_read <- gsub(".meta$", "", samples_to_read)
        else
            samples_to_read <- gsub(".meta$", "", gtf_meta_files)

        granges <- .parse_gtf_files(samples_to_read,regions)
    }
}

.extract_from_GRangesList <- function(rangesList, metadata = NULL,
                                        metadata_prefix = NULL, regions = NULL)
{
    if(!is(rangesList,"GRangesList"))
        stop("only GrangesList admitted")

    if(length(rangesList)<=0)
        stop("rangesList empty")

    meta_list <- metadata(rangesList)
    samples <- .check_metadata_list(metadata, metadata_prefix,meta_list)
    if(length(unlist(samples))<=0)
        samples <- rangesList
    else
    {
        index <- unlist(samples)
        samples <- rangesList[c(index)]
    }
    granges <- .parse_Granges(samples,regions)
}

.parse_Granges <- function(region_list,regions)
{
    g1 <- region_list[[1]]
    elementMetadata(g1) <- NULL
    if(!is.null(regions))
    {
        DF_list <- lapply(region_list, function(g_x){
            meta <- elementMetadata(g_x)[regions]
            data.frame(meta)
        })
        DF_only_regions <- dplyr::bind_cols(DF_list)
        elementMetadata(g1) <- DF_only_regions
    }
    g1
}

.check_metadata_list <- function(metadata,metadata_prefix,meta_list)
{
    vec_meta <- paste0(metadata_prefix,metadata)
    list <- mapply(function(x,index){
        vec_names <- names(x)
        a <- sapply(vec_meta, function(y) {
            which(y==vec_names)
        })
        ## we would like that manage more index from grep
        found <- as.logical(length(unlist(a)))
        #if found retrieve samples that has at least one choosen metadata
        if(found){index}
    }, meta_list, seq_along(meta_list))
}

.check_metadata_files <- function(metadata,metadata_prefix,meta_files)
{
    vec_meta <- paste0(metadata_prefix,metadata)
    meta_list <- lapply(meta_files, function(x){
        list <- .add_metadata(x)
        vec_names <- names(list)
        a <- sapply(vec_meta, function(y) {
            grep(y,vec_names) 
        })

        ## we would like that manage more index from grep
        found <- as.logical(length(unlist(a)))
        #if found retrieve samples that has at least one choosen metadata
        if(found){x}
    })
}


.parse_gtf_files <- function(gtf_region_files,regions)
{
    g1 <- rtracklayer::import(con = gtf_region_files[1], format = "gtf")
    elementMetadata(g1) <- NULL
    if(!is.null(regions))
    {
        DF_list <- lapply(gtf_region_files, function(x){
            g_x <- rtracklayer::import(con = x, format = "gtf")
            meta <- elementMetadata(g_x)[regions]
            data.frame(meta)
        })
        DF_only_regions <- dplyr::bind_cols(DF_list)
        elementMetadata(g1) <- DF_only_regions
    }
    g1
}

.parse_gdm_files <- function(vector_field,gdm_region_files,regions)
{
    #read first sample cause chromosome regions are the same for all samples
    df <- data.table::fread(gdm_region_files[1],col.names = vector_field,
                                header = FALSE,sep = '\t')
    col_names <- names(df)
    df <-  df[c("chr","left","right","strand")]

    if(!is.null(regions))
    {
        df_list <- lapply(gdm_region_files,function(x,regions,vector_field){
            region_frame <- data.table::fread(x,col.names = vector_field,
                                                header = FALSE,sep = '\t')
            col_names <- names(region_frame)
            #delete column not choosen by input
            if(!is.null(regions))
                col_names <- col_names[col_names %in% regions] 
            r <- region_frame[col_names]
        },regions,vector_field)


        df_only_regions <- dplyr::bind_cols(df_list)
        complete_df <- dplyr::bind_cols(df,df_only_regions)
        g <- GenomicRanges::makeGRangesFromDataFrame(complete_df,
                                            keep.extra.columns = TRUE,
                                            start.field = "left",
                                            end.field = "right")
    }
    else
        g <- GenomicRanges::makeGRangesFromDataFrame(df,
                                            keep.extra.columns = TRUE,
                                            start.field = "left",
                                            end.field = "right")

}


